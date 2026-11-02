unit aiplane_pi;

{
  Pi adapter using Pi's own RPC mode (no ACP bridge needed):

    pi --mode rpc

  JSON lines on stdin/stdout, one long-lived process per conversation.
  Commands carry an optional "id" that the "response" repeats; session
  events (message_update, tool_execution_*, agent_settled, ...) stream in
  between. A turn ends with agent_settled.

  Model and thinking level change through set_model / set_thinking_level,
  without restarting the process. Extension dialogs (confirm, select) are
  shown as permission cards; free text dialogs (input, editor) are
  cancelled.

  Protocol: docs/rpc.md, rpc-commands.md and json.md in the Pi package.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, aiplane_types, aiplane_process;

type

  { TPiRpcAdapter }

  TPiRpcAdapter = class(TAIAdapter)
  private
    FChannel: TAIProcessChannel;
    FNextId: integer;
    FPending: TStringList;      // request id -> command
    FPermissions: TStringList;  // key -> extension_ui_request id
    FPermKinds: TStringList;    // key -> confirm | select
    FStopReason: string;
    FModelDirty: boolean;
    FEffortDirty: boolean;
    FCurrentModel: string;
    procedure ChannelLine(Sender: TObject; const ALine: string);
    procedure ChannelStderr(Sender: TObject; const ALine: string);
    procedure ChannelExit(Sender: TObject; AExitCode: integer);
    procedure WriteJSON(AMessage: TJSONObject);
    function SendCommand(const AType: string; AFields: TJSONObject = nil): string;
    procedure HandleResponse(AMessage: TJSONObject);
    procedure HandleMessageUpdate(AMessage: TJSONObject);
    procedure HandleMessageEnd(AMessage: TJSONObject);
    procedure HandleToolStart(AMessage: TJSONObject);
    procedure HandleToolEnd(AMessage: TJSONObject);
    procedure HandleUIRequest(AMessage: TJSONObject);
    procedure ReadModels(AData: TJSONObject);
    procedure ReadState(AData: TJSONObject);
    procedure ApplyModel;
    procedure ApplyEffort;
    procedure FinishTurn(const AStopReason: string);
    procedure CancelPermissions;
    procedure StopChannel;
  protected
    procedure ModelChanged; override;
    procedure EffortChanged; override;
    procedure BuildArguments(AArgs: TStrings); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Prompt(const AText: string); override;
    procedure Cancel; override;
    function Poll: boolean; override;
    procedure RespondPermission(const AKey, AOptionId: string); override;
    procedure NewSession; override;
    { Feeds one protocol line as if it came from Pi (tests). }
    procedure HandleLine(const ALine: string);
    class function AdapterName: string; override;
  end;

{ Splits "provider/model-id" (the id may contain more slashes). }
procedure PiSplitModel(const AModel: string; out AProvider, AModelId: string);
{ Maps the AI Plane effort level to a Pi thinking level ('' = leave as is). }
function PiThinkingLevel(const AEffort: string): string;

implementation

const
  OPTION_YES = 'yes';
  OPTION_NO = 'no';
  MAX_TOOL_OUTPUT = 4000;

function JStr(AObject: TJSONObject; const AName: string; const ADefault: string = ''): string;
var
  d: TJSONData;
begin
  Result := ADefault;
  if not Assigned(AObject) then
    Exit;
  d := AObject.Find(AName);
  if Assigned(d) and (d.JSONType in [jtString, jtNumber, jtBoolean]) then
    Result := d.AsString;
end;

function JBool(AObject: TJSONObject; const AName: string): boolean;
var
  d: TJSONData;
begin
  Result := False;
  if not Assigned(AObject) then
    Exit;
  d := AObject.Find(AName);
  if Assigned(d) and (d.JSONType = jtBoolean) then
    Result := d.AsBoolean;
end;

function JObj(AObject: TJSONObject; const AName: string): TJSONObject;
var
  d: TJSONData;
begin
  Result := nil;
  if Assigned(AObject) then
  begin
    d := AObject.Find(AName);
    if Assigned(d) and (d.JSONType = jtObject) then
      Result := TJSONObject(d);
  end;
end;

function JArr(AObject: TJSONObject; const AName: string): TJSONArray;
var
  d: TJSONData;
begin
  Result := nil;
  if Assigned(AObject) then
  begin
    d := AObject.Find(AName);
    if Assigned(d) and (d.JSONType = jtArray) then
      Result := TJSONArray(d);
  end;
end;

{ Text blocks of a tool result: {"content": [{"type":"text","text":...}]} }
function ResultText(AResult: TJSONObject): string;
var
  arr: TJSONArray;
  i: integer;
begin
  Result := '';
  arr := JArr(AResult, 'content');
  if Assigned(arr) then
    for i := 0 to arr.Count - 1 do
      if (arr[i].JSONType = jtObject) and (JStr(TJSONObject(arr[i]), 'type') = 'text') then
        Result := Result + JStr(TJSONObject(arr[i]), 'text');
  if Length(Result) > MAX_TOOL_OUTPUT then
    Result := Copy(Result, 1, MAX_TOOL_OUTPUT) + LineEnding + '...';
end;

procedure PiSplitModel(const AModel: string; out AProvider, AModelId: string);
var
  p: integer;
begin
  p := Pos('/', AModel);
  if p > 0 then
  begin
    AProvider := Copy(AModel, 1, p - 1);
    AModelId := Copy(AModel, p + 1, MaxInt);
  end
  else
  begin
    AProvider := '';
    AModelId := AModel;
  end;
end;

function PiThinkingLevel(const AEffort: string): string;
begin
  case LowerCase(AEffort) of
    'off', 'minimal', 'low', 'medium', 'high', 'xhigh', 'max':
      Result := LowerCase(AEffort);
    else
      Result := '';
  end;
end;

{ TPiRpcAdapter }

constructor TPiRpcAdapter.Create;
begin
  inherited Create;
  Command := 'pi';
  FPending := TStringList.Create;
  FPermissions := TStringList.Create;
  FPermKinds := TStringList.Create;
end;

destructor TPiRpcAdapter.Destroy;
begin
  if Assigned(FChannel) then
    FChannel.OnExit := nil;
  FreeAndNil(FChannel);
  FPermKinds.Free;
  FPermissions.Free;
  FPending.Free;
  inherited Destroy;
end;

class function TPiRpcAdapter.AdapterName: string;
begin
  Result := 'Pi (RPC)';
end;

procedure TPiRpcAdapter.BuildArguments(AArgs: TStrings);
begin
  AArgs.Add('--mode');
  AArgs.Add('rpc');
  AArgs.AddStrings(Arguments);
end;

procedure TPiRpcAdapter.Start;
var
  args: TStringList;
begin
  if Assigned(FChannel) and FChannel.Running then
    Exit;
  StopChannel;
  FPending.Clear;
  args := TStringList.Create;
  try
    BuildArguments(args);
    FChannel := TAIProcessChannel.Create;
    FChannel.OnLine := @ChannelLine;
    FChannel.OnStderr := @ChannelStderr;
    FChannel.OnExit := @ChannelExit;
    try
      FChannel.Start(Command, args, WorkingDir);
    except
      FreeAndNil(FChannel);
      SetState(aasFailed);
      raise;
    end;
  finally
    args.Free;
  end;
  // Pi accepts commands right away; ask what it has
  SetState(aasReady);
  DoEvent(aekStatus, 'agent: Pi');
  SendCommand('get_available_models');
  SendCommand('get_state');
  FModelDirty := Model <> '';
  FEffortDirty := PiThinkingLevel(Effort) <> '';
  ApplyModel;
  ApplyEffort;
end;

procedure TPiRpcAdapter.StopChannel;
begin
  CancelPermissions;
  if Assigned(FChannel) then
  begin
    FChannel.OnExit := nil;
    FChannel.Stop;
    FreeAndNil(FChannel);
  end;
end;

procedure TPiRpcAdapter.Stop;
begin
  StopChannel;
  FSessionId := '';
  SetState(aasStopped);
end;

procedure TPiRpcAdapter.Prompt(const AText: string);
var
  fields: TJSONObject;
begin
  if FState = aasBusy then
    raise EAIAdapter.Create('A prompt is already running');
  try
    Start;
  except
    on E: Exception do
    begin
      DoEvent(aekError, E.Message);
      FinishTurn('error');
      Exit;
    end;
  end;
  ApplyModel;
  ApplyEffort;
  FStopReason := 'end_turn';
  fields := TJSONObject.Create;
  fields.Add('message', AText);
  SetState(aasBusy);
  SendCommand('prompt', fields);
end;

procedure TPiRpcAdapter.Cancel;
begin
  if FState <> aasBusy then
    Exit;
  CancelPermissions;
  FStopReason := 'cancelled';
  SendCommand('abort');
end;

function TPiRpcAdapter.Poll: boolean;
begin
  Result := Assigned(FChannel) and FChannel.Poll;
end;

procedure TPiRpcAdapter.RespondPermission(const AKey, AOptionId: string);
var
  i: integer;
  requestId, kind: string;
  msg: TJSONObject;
begin
  i := FPermissions.IndexOfName(AKey);
  if i < 0 then
    Exit;
  requestId := FPermissions.ValueFromIndex[i];
  FPermissions.Delete(i);
  kind := FPermKinds.Values[AKey];
  i := FPermKinds.IndexOfName(AKey);
  if i >= 0 then
    FPermKinds.Delete(i);

  msg := TJSONObject.Create;
  msg.Add('type', 'extension_ui_response');
  msg.Add('id', requestId);
  if AOptionId = '' then
    msg.Add('cancelled', True)
  else if kind = 'confirm' then
    msg.Add('confirmed', AOptionId = OPTION_YES)
  else
    msg.Add('value', AOptionId);
  WriteJSON(msg);
end;

procedure TPiRpcAdapter.NewSession;
begin
  if FState = aasBusy then
    raise EAIAdapter.Create('Cannot start a new session while a prompt is running');
  FSessionId := '';
  if Assigned(FChannel) and FChannel.Running then
  begin
    SendCommand('new_session');
    SendCommand('get_state');
  end;
end;

procedure TPiRpcAdapter.ModelChanged;
begin
  FModelDirty := True;
  if (FState = aasReady) and Assigned(FChannel) then
    ApplyModel;
end;

procedure TPiRpcAdapter.EffortChanged;
begin
  FEffortDirty := True;
  if (FState = aasReady) and Assigned(FChannel) then
    ApplyEffort;
end;

procedure TPiRpcAdapter.ApplyModel;
var
  fields: TJSONObject;
  provider, modelId: string;
  i: integer;
begin
  if not FModelDirty or (Model = '') then
    Exit;
  FModelDirty := False;
  PiSplitModel(Model, provider, modelId);
  // a bare id: find its provider in the model list
  if provider = '' then
    for i := 0 to High(FModels) do
      if Copy(FModels[i].Id, Pos('/', FModels[i].Id) + 1, MaxInt) = modelId then
      begin
        PiSplitModel(FModels[i].Id, provider, modelId);
        Break;
      end;
  fields := TJSONObject.Create;
  if provider <> '' then
    fields.Add('provider', provider);
  fields.Add('modelId', modelId);
  SendCommand('set_model', fields);
end;

procedure TPiRpcAdapter.ApplyEffort;
var
  level: string;
  fields: TJSONObject;
begin
  if not FEffortDirty then
    Exit;
  FEffortDirty := False;
  level := PiThinkingLevel(Effort);
  if level = '' then
    Exit;  // auto: keep Pi's own setting
  fields := TJSONObject.Create;
  fields.Add('level', level);
  SendCommand('set_thinking_level', fields);
end;

procedure TPiRpcAdapter.WriteJSON(AMessage: TJSONObject);
var
  s: string;
begin
  try
    s := AMessage.AsJSON;
  finally
    AMessage.Free;
  end;
  DoEvent(aekLog, '>> ' + s);
  if Assigned(FChannel) and FChannel.Running then
    FChannel.WriteLine(s);
end;

function TPiRpcAdapter.SendCommand(const AType: string; AFields: TJSONObject): string;
var
  msg: TJSONObject;
  i: integer;
begin
  Inc(FNextId);
  Result := 'fp' + IntToStr(FNextId);
  FPending.Values[Result] := AType;
  // "id" and "type" first, for a readable log
  msg := TJSONObject.Create(['id', Result, 'type', AType]);
  if Assigned(AFields) then
  try
    for i := 0 to AFields.Count - 1 do
      msg.Add(AFields.Names[i], AFields.Items[i].Clone);
  finally
    AFields.Free;
  end;
  WriteJSON(msg);
end;

procedure TPiRpcAdapter.ChannelLine(Sender: TObject; const ALine: string);
begin
  HandleLine(ALine);
end;

procedure TPiRpcAdapter.ChannelStderr(Sender: TObject; const ALine: string);
begin
  DoEvent(aekLog, 'stderr: ' + ALine);
end;

procedure TPiRpcAdapter.ChannelExit(Sender: TObject; AExitCode: integer);
begin
  FPermissions.Clear;
  FPermKinds.Clear;
  FPending.Clear;
  if FState = aasBusy then
  begin
    DoEvent(aekError, Format('Pi exited (code %d)', [AExitCode]));
    SetState(aasFailed);
    FinishTurn('error');
  end
  else
    SetState(aasStopped);
end;

procedure TPiRpcAdapter.HandleLine(const ALine: string);
var
  data: TJSONData;
  msg: TJSONObject;
begin
  DoEvent(aekLog, '<< ' + ALine);
  try
    data := GetJSON(ALine);
  except
    Exit;
  end;
  try
    if data.JSONType <> jtObject then
      Exit;
    msg := TJSONObject(data);
    case JStr(msg, 'type') of
      'response':
        HandleResponse(msg);
      'message_update':
        HandleMessageUpdate(msg);
      'message_end':
        HandleMessageEnd(msg);
      'tool_execution_start':
        HandleToolStart(msg);
      'tool_execution_end':
        HandleToolEnd(msg);
      'agent_settled':
        if FState = aasBusy then
        begin
          if JBool(msg, 'aborted') then
            FStopReason := 'cancelled';
          if FStopReason = '' then
            FStopReason := 'end_turn';
          FinishTurn(FStopReason);
          FStopReason := '';
        end;
      'extension_ui_request':
        HandleUIRequest(msg);
      'extension_error':
        DoEvent(aekError, 'Extension ' + ExtractFileName(JStr(msg, 'extensionPath')) +
          ': ' + JStr(msg, 'error'));
      'thinking_level_changed':
        DoEvent(aekStatus, 'thinking: ' + JStr(msg, 'level'));
    end;
  finally
    data.Free;
  end;
end;

procedure TPiRpcAdapter.HandleResponse(AMessage: TJSONObject);
var
  id, cmd, error: string;
  i: integer;
  data: TJSONObject;
begin
  id := JStr(AMessage, 'id');
  cmd := JStr(AMessage, 'command');
  i := FPending.IndexOfName(id);
  if i >= 0 then
    FPending.Delete(i);
  data := JObj(AMessage, 'data');
  if not JBool(AMessage, 'success') then
  begin
    error := JStr(AMessage, 'error', 'unknown error');
    case cmd of
      'prompt':
      begin
        DoEvent(aekError, error);
        FinishTurn('error');
      end;
      'set_thinking_level':
        DoEvent(aekLog, 'thinking level not applied: ' + error);
      'get_available_models', 'get_state':
        DoEvent(aekLog, cmd + ' failed: ' + error);
      else
        DoEvent(aekError, cmd + ' failed: ' + error);
    end;
    Exit;
  end;
  case cmd of
    'prompt':
      // handled by an extension command: no run follows
      if (JStr(data, 'disposition') = 'handled') and (FState = aasBusy) then
        FinishTurn('end_turn');
    'get_available_models':
      ReadModels(data);
    'get_state':
      ReadState(data);
    'set_model':
      if Assigned(data) then
      begin
        FCurrentModel := JStr(data, 'provider') + '/' + JStr(data, 'id');
        DoEvent(aekStatus, 'model: ' + FCurrentModel);
      end;
  end;
end;

procedure TPiRpcAdapter.ReadModels(AData: TJSONObject);
var
  arr: TJSONArray;
  i, n: integer;
  m: TJSONObject;
begin
  arr := JArr(AData, 'models');
  if not Assigned(arr) then
    Exit;
  SetLength(FModels, 0);
  for i := 0 to arr.Count - 1 do
  begin
    if arr[i].JSONType <> jtObject then
      Continue;
    m := TJSONObject(arr[i]);
    n := Length(FModels);
    SetLength(FModels, n + 1);
    FModels[n].Id := JStr(m, 'provider') + '/' + JStr(m, 'id');
    FModels[n].Name := JStr(m, 'name', FModels[n].Id);
  end;
  // the prompt itself reports a missing login clearly; just log it here
  if Length(FModels) = 0 then
    DoEvent(aekLog, 'Pi has no models available (run "pi" once and use /login, ' +
      'or set an API key for a provider)');
  // lets the UI refresh its model list
  if FCurrentModel <> '' then
    DoEvent(aekStatus, 'model: ' + FCurrentModel);
end;

procedure TPiRpcAdapter.ReadState(AData: TJSONObject);
var
  m: TJSONObject;
begin
  if not Assigned(AData) then
    Exit;
  m := JObj(AData, 'model');
  if Assigned(m) then
  begin
    FCurrentModel := JStr(m, 'provider') + '/' + JStr(m, 'id');
    DoEvent(aekStatus, 'model: ' + FCurrentModel);
  end;
  FSessionId := JStr(AData, 'sessionId', FSessionId);
  if FSessionId <> '' then
    DoEvent(aekStatus, 'session: ' + FSessionId);
end;

procedure TPiRpcAdapter.HandleMessageUpdate(AMessage: TJSONObject);
var
  ev: TJSONObject;
  e: TAIEvent;
begin
  ev := JObj(AMessage, 'assistantMessageEvent');
  case JStr(ev, 'type') of
    'text_delta':
      DoEvent(aekText, JStr(ev, 'delta'));
    'thinking_delta':
      DoEvent(aekThinking, JStr(ev, 'delta'));
    'toolcall_start':
    begin
      e := AIEvent(aekToolCall);
      e.ToolCallId := JStr(ev, 'id');
      e.ToolTitle := JStr(ev, 'toolName');
      e.ToolKind := e.ToolTitle;
      e.ToolStatus := atsPending;
      DoEventRec(e);
    end;
  end;
end;

procedure TPiRpcAdapter.HandleMessageEnd(AMessage: TJSONObject);
var
  m, usage, cost: TJSONObject;
  reason, costText: string;
  fs: TFormatSettings;
  d: TJSONData;
begin
  m := JObj(AMessage, 'message');
  if JStr(m, 'role') <> 'assistant' then
    Exit;
  reason := JStr(m, 'stopReason');
  case reason of
    'error':
    begin
      DoEvent(aekError, JStr(m, 'errorMessage', 'The model returned an error'));
      FStopReason := 'error';
    end;
    'aborted': FStopReason := 'cancelled';
    'length': FStopReason := 'max_tokens';
  end;
  usage := JObj(m, 'usage');
  if Assigned(usage) then
  begin
    costText := '0';
    cost := JObj(usage, 'cost');
    fs := DefaultFormatSettings;
    fs.DecimalSeparator := '.';
    if Assigned(cost) then
    begin
      d := cost.Find('total');
      if Assigned(d) and (d.JSONType = jtNumber) then
        costText := FormatFloat('0.0000', d.AsFloat, fs);
    end;
    DoEvent(aekUsage, Format('in %s, out %s, cost $%s',
      [JStr(usage, 'input', '0'), JStr(usage, 'output', '0'), costText]));
  end;
end;

procedure TPiRpcAdapter.HandleToolStart(AMessage: TJSONObject);
var
  e: TAIEvent;
  args: TJSONObject;
begin
  e := AIEvent(aekToolUpdate);
  e.ToolCallId := JStr(AMessage, 'toolCallId');
  e.ToolTitle := JStr(AMessage, 'toolName');
  e.ToolKind := e.ToolTitle;
  e.ToolStatus := atsRunning;
  args := JObj(AMessage, 'args');
  if Assigned(args) then
  begin
    e.ToolPath := JStr(args, 'path', JStr(args, 'file_path'));
    e.Text := args.AsJSON;
  end;
  DoEventRec(e);
end;

procedure TPiRpcAdapter.HandleToolEnd(AMessage: TJSONObject);
var
  e: TAIEvent;
begin
  e := AIEvent(aekToolUpdate);
  e.ToolCallId := JStr(AMessage, 'toolCallId');
  e.ToolTitle := JStr(AMessage, 'toolName');
  if JBool(AMessage, 'isError') then
    e.ToolStatus := atsFailed
  else
    e.ToolStatus := atsCompleted;
  e.Text := ResultText(JObj(AMessage, 'result'));
  DoEventRec(e);
end;

procedure TPiRpcAdapter.HandleUIRequest(AMessage: TJSONObject);
var
  method, id: string;
  perm: TAIPermissionRequest;
  options: TJSONArray;
  i: integer;
  reply: TJSONObject;
begin
  method := JStr(AMessage, 'method');
  id := JStr(AMessage, 'id');
  case method of
    'confirm', 'select':
    begin
      Inc(FNextId);
      perm := Default(TAIPermissionRequest);
      perm.Key := 'perm' + IntToStr(FNextId);
      perm.ToolTitle := JStr(AMessage, 'title', 'Pi asks');
      perm.ToolKind := method;
      perm.Detail := JStr(AMessage, 'message');
      if method = 'confirm' then
      begin
        SetLength(perm.Options, 2);
        perm.Options[0].Id := OPTION_YES;
        perm.Options[0].Name := 'Yes';
        perm.Options[0].Kind := 'allow_once';
        perm.Options[1].Id := OPTION_NO;
        perm.Options[1].Name := 'No';
        perm.Options[1].Kind := 'reject_once';
      end
      else
      begin
        options := JArr(AMessage, 'options');
        if Assigned(options) then
        begin
          SetLength(perm.Options, options.Count);
          for i := 0 to options.Count - 1 do
          begin
            perm.Options[i].Id := options[i].AsString;
            perm.Options[i].Name := options[i].AsString;
            perm.Options[i].Kind := 'allow_once';
          end;
        end;
      end;
      FPermissions.Values[perm.Key] := id;
      FPermKinds.Values[perm.Key] := method;
      DoPermission(perm);
    end;
    'input', 'editor':
    begin
      // no free text dialog in the chat yet
      DoEvent(aekLog, 'Pi asked for text input (' + JStr(AMessage, 'title') + '); cancelled');
      reply := TJSONObject.Create;
      reply.Add('type', 'extension_ui_response');
      reply.Add('id', id);
      reply.Add('cancelled', True);
      WriteJSON(reply);
    end;
    'notify':
      DoEvent(aekStatus, 'pi: ' + JStr(AMessage, 'message', JStr(AMessage, 'title')));
  end;
end;

procedure TPiRpcAdapter.FinishTurn(const AStopReason: string);
var
  ev: TAIEvent;
begin
  CancelPermissions;
  if FState = aasBusy then
    SetState(aasReady);
  ev := AIEvent(aekDone);
  ev.StopReason := AStopReason;
  DoEventRec(ev);
end;

procedure TPiRpcAdapter.CancelPermissions;
begin
  while FPermissions.Count > 0 do
    RespondPermission(FPermissions.Names[0], '');
end;

end.

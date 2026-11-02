unit aiplane_acp;

{
  Agent Client Protocol (ACP) adapter.

  ACP is JSON-RPC 2.0 over the runtime's stdin/stdout, one message per line.
  The IDE is the client: it starts the session, sends prompts, receives
  session/update notifications while the agent works, and answers the agent's
  own requests (read a file, ask the user for permission).

  Handshake: initialize -> session/new -> ready. A prompt sent before the
  handshake finishes is queued and sent once the session exists.

  Spec: https://agentclientprotocol.com
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, aiplane_types, aiplane_process;

const
  ACP_PROTOCOL_VERSION = 1;

type

  { TACPAdapter }

  TACPAdapter = class(TAIAdapter)
  private
    FChannel: TAIProcessChannel;
    FNextId: integer;
    FPending: TStringList;      // request id -> method
    FPermissions: TStringList;  // permission key -> JSON-RPC id (as JSON)
    FQueuedPrompt: string;
    FHasQueuedPrompt: boolean;
    FModelConfigId: string;
    FEffortConfigId: string;
    FEffortValues: TStringList;
    FAgentName: string;
    FModelDirty: boolean;
    FEffortDirty: boolean;
    procedure ChannelLine(Sender: TObject; const ALine: string);
    procedure ChannelStderr(Sender: TObject; const ALine: string);
    procedure ChannelExit(Sender: TObject; AExitCode: integer);
    procedure Send(AMessage: TJSONObject);
    function SendRequest(const AMethod: string; AParams: TJSONObject): integer;
    procedure SendNotification(const AMethod: string; AParams: TJSONObject);
    procedure SendResult(const AIdJSON: string; AResult: TJSONData);
    procedure SendError(const AIdJSON: string; ACode: integer; const AMessage: string);
    procedure HandleResponse(AMessage: TJSONObject);
    procedure HandleRequest(AMessage: TJSONObject);
    procedure HandleSessionUpdate(AUpdate: TJSONObject);
    procedure HandleReadFile(const AIdJSON: string; AParams: TJSONObject);
    procedure HandlePermission(const AIdJSON: string; AParams: TJSONObject);
    procedure ReadSessionOptions(AResult: TJSONObject);
    procedure SessionReady;
    procedure ApplyModel;
    procedure ApplyEffort;
    procedure SendPrompt(const AText: string);
    procedure NewSessionRequest;
    procedure FinishTurn(const AStopReason: string);
    procedure CancelPermissions;
  protected
    procedure ModelChanged; override;
    procedure EffortChanged; override;
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
    { Feeds one protocol line as if it came from the runtime (tests). }
    procedure HandleLine(const ALine: string);
    class function AdapterName: string; override;
    property AgentName: string read FAgentName;
  end;

implementation

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

function JObj(AObject: TJSONObject; const AName: string): TJSONObject;
var
  d: TJSONData;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;
  d := AObject.Find(AName);
  if Assigned(d) and (d.JSONType = jtObject) then
    Result := TJSONObject(d);
end;

function JArr(AObject: TJSONObject; const AName: string): TJSONArray;
var
  d: TJSONData;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;
  d := AObject.Find(AName);
  if Assigned(d) and (d.JSONType = jtArray) then
    Result := TJSONArray(d);
end;

// Text of an ACP content block, or of a tool call content entry that wraps
// one: {"type":"content","content":{...}}.
function ContentText(AData: TJSONData): string;
var
  o: TJSONObject;
begin
  Result := '';
  if not Assigned(AData) or (AData.JSONType <> jtObject) then
    Exit;
  o := TJSONObject(AData);
  if JStr(o, 'type') = 'content' then
    Exit(ContentText(o.Find('content')));
  if JStr(o, 'type') = 'text' then
    Result := JStr(o, 'text')
  else if JStr(o, 'type') = 'diff' then
    Result := 'diff: ' + JStr(o, 'path');
end;

{ TACPAdapter }

constructor TACPAdapter.Create;
begin
  inherited Create;
  FPending := TStringList.Create;
  FPermissions := TStringList.Create;
  FEffortValues := TStringList.Create;
end;

destructor TACPAdapter.Destroy;
begin
  FreeAndNil(FChannel);
  FEffortValues.Free;
  FPermissions.Free;
  FPending.Free;
  inherited Destroy;
end;

class function TACPAdapter.AdapterName: string;
begin
  Result := 'ACP';
end;

procedure TACPAdapter.Start;
var
  params, caps, fs, info: TJSONObject;
begin
  if Assigned(FChannel) then
  begin
    if FChannel.Running and (FState in [aasStarting, aasReady, aasBusy]) then
      Exit;
    FChannel.OnExit := nil;
    FChannel.Stop;
    FreeAndNil(FChannel);
  end;
  FPending.Clear;
  FPermissions.Clear;
  FSessionId := '';
  FChannel := TAIProcessChannel.Create;
  FChannel.OnLine := @ChannelLine;
  FChannel.OnStderr := @ChannelStderr;
  FChannel.OnExit := @ChannelExit;
  SetState(aasStarting);
  try
    FChannel.Start(Command, Arguments, WorkingDir);
  except
    on E: Exception do
    begin
      FreeAndNil(FChannel);
      SetState(aasFailed);
      raise;
    end;
  end;

  fs := TJSONObject.Create;
  fs.Add('readTextFile', True);
  fs.Add('writeTextFile', False);
  caps := TJSONObject.Create;
  caps.Add('fs', fs);
  caps.Add('terminal', False);
  info := TJSONObject.Create;
  info.Add('name', 'fastplaz-ai-plane');
  info.Add('version', '0.1');
  params := TJSONObject.Create;
  params.Add('protocolVersion', ACP_PROTOCOL_VERSION);
  params.Add('clientCapabilities', caps);
  params.Add('clientInfo', info);
  SendRequest('initialize', params);
end;

procedure TACPAdapter.Stop;
begin
  CancelPermissions;
  if Assigned(FChannel) then
  begin
    FChannel.OnExit := nil;
    FChannel.Stop;
    FreeAndNil(FChannel);
  end;
  FHasQueuedPrompt := False;
  FSessionId := '';
  SetState(aasStopped);
end;

procedure TACPAdapter.Prompt(const AText: string);
begin
  case FState of
    aasBusy:
      raise EAIAdapter.Create('A prompt is already running');
    aasReady:
      SendPrompt(AText);
    aasStarting:
    begin
      FQueuedPrompt := AText;
      FHasQueuedPrompt := True;
    end;
    else
    begin
      FQueuedPrompt := AText;
      FHasQueuedPrompt := True;
      try
        Start;
      except
        on E: Exception do
        begin
          FHasQueuedPrompt := False;
          DoEvent(aekError, E.Message);
          FinishTurn('error');
        end;
      end;
    end;
  end;
end;

procedure TACPAdapter.Cancel;
var
  params: TJSONObject;
begin
  if FHasQueuedPrompt then
  begin
    FHasQueuedPrompt := False;
    FinishTurn('cancelled');
    Exit;
  end;
  if (FState <> aasBusy) or (FSessionId = '') then
    Exit;
  // The spec requires pending permission requests to be answered first.
  CancelPermissions;
  params := TJSONObject.Create;
  params.Add('sessionId', FSessionId);
  SendNotification('session/cancel', params);
end;

function TACPAdapter.Poll: boolean;
begin
  Result := Assigned(FChannel) and FChannel.Poll;
end;

procedure TACPAdapter.RespondPermission(const AKey, AOptionId: string);
var
  i: integer;
  idJSON: string;
  outcome, result: TJSONObject;
begin
  i := FPermissions.IndexOfName(AKey);
  if i < 0 then
    Exit;
  idJSON := FPermissions.ValueFromIndex[i];
  FPermissions.Delete(i);
  outcome := TJSONObject.Create;
  if AOptionId = '' then
    outcome.Add('outcome', 'cancelled')
  else
  begin
    outcome.Add('outcome', 'selected');
    outcome.Add('optionId', AOptionId);
  end;
  result := TJSONObject.Create;
  result.Add('outcome', outcome);
  SendResult(idJSON, result);
end;

procedure TACPAdapter.NewSession;
begin
  if Assigned(FChannel) and FChannel.Running and (FState = aasReady) then
  begin
    FSessionId := '';
    SetState(aasStarting);
    NewSessionRequest;
  end
  else if FState = aasBusy then
    raise EAIAdapter.Create('Cannot start a new session while a prompt is running');
end;

procedure TACPAdapter.HandleLine(const ALine: string);
var
  data: TJSONData;
  msg: TJSONObject;
begin
  DoEvent(aekLog, '<< ' + ALine);
  try
    data := GetJSON(ALine);
  except
    // Some runtimes print banners on stdout; ignore anything that is not JSON.
    Exit;
  end;
  try
    if data.JSONType <> jtObject then
      Exit;
    msg := TJSONObject(data);
    if Assigned(msg.Find('method')) then
    begin
      if Assigned(msg.Find('id')) then
        HandleRequest(msg)
      else if JStr(msg, 'method') = 'session/update' then
        HandleSessionUpdate(JObj(JObj(msg, 'params'), 'update'));
    end
    else if Assigned(msg.Find('id')) then
      HandleResponse(msg);
  finally
    data.Free;
  end;
end;

procedure TACPAdapter.ChannelLine(Sender: TObject; const ALine: string);
begin
  HandleLine(ALine);
end;

procedure TACPAdapter.ChannelStderr(Sender: TObject; const ALine: string);
begin
  DoEvent(aekLog, 'stderr: ' + ALine);
end;

procedure TACPAdapter.ChannelExit(Sender: TObject; AExitCode: integer);
var
  wasBusy: boolean;
begin
  wasBusy := (FState = aasBusy) or FHasQueuedPrompt;
  FPermissions.Clear;
  FPending.Clear;
  FSessionId := '';
  FHasQueuedPrompt := False;
  if wasBusy or (FState = aasStarting) then
  begin
    DoEvent(aekError, Format('Runtime exited (code %d)', [AExitCode]));
    SetState(aasFailed);
    if wasBusy then
      FinishTurn('error');
  end
  else
    SetState(aasStopped);
end;

procedure TACPAdapter.Send(AMessage: TJSONObject);
var
  s: string;
begin
  try
    AMessage.Add('jsonrpc', '2.0');
    s := AMessage.AsJSON;
  finally
    AMessage.Free;
  end;
  DoEvent(aekLog, '>> ' + s);
  if Assigned(FChannel) and FChannel.Running then
    FChannel.WriteLine(s);
end;

function TACPAdapter.SendRequest(const AMethod: string; AParams: TJSONObject): integer;
var
  msg: TJSONObject;
begin
  Inc(FNextId);
  Result := FNextId;
  FPending.Values[IntToStr(Result)] := AMethod;
  msg := TJSONObject.Create;
  msg.Add('id', Result);
  msg.Add('method', AMethod);
  msg.Add('params', AParams);
  Send(msg);
end;

procedure TACPAdapter.SendNotification(const AMethod: string; AParams: TJSONObject);
var
  msg: TJSONObject;
begin
  msg := TJSONObject.Create;
  msg.Add('method', AMethod);
  msg.Add('params', AParams);
  Send(msg);
end;

procedure TACPAdapter.SendResult(const AIdJSON: string; AResult: TJSONData);
var
  msg: TJSONObject;
begin
  msg := TJSONObject.Create;
  msg.Add('id', GetJSON(AIdJSON));
  if Assigned(AResult) then
    msg.Add('result', AResult)
  else
    msg.Add('result', TJSONNull.Create);
  Send(msg);
end;

procedure TACPAdapter.SendError(const AIdJSON: string; ACode: integer;
  const AMessage: string);
var
  msg, err: TJSONObject;
begin
  err := TJSONObject.Create;
  err.Add('code', ACode);
  err.Add('message', AMessage);
  msg := TJSONObject.Create;
  msg.Add('id', GetJSON(AIdJSON));
  msg.Add('error', err);
  Send(msg);
end;

procedure TACPAdapter.HandleResponse(AMessage: TJSONObject);
var
  idText, method, errText: string;
  i: integer;
  result, err: TJSONObject;
begin
  idText := JStr(AMessage, 'id');
  i := FPending.IndexOfName(idText);
  if i < 0 then
    Exit;
  method := FPending.ValueFromIndex[i];
  FPending.Delete(i);
  result := JObj(AMessage, 'result');
  err := JObj(AMessage, 'error');
  errText := '';
  if Assigned(err) then
    errText := Format('%s failed: %s', [method, JStr(err, 'message', 'unknown error')]);

  case method of
    'initialize':
      if errText <> '' then
      begin
        DoEvent(aekError, errText);
        // The channel is still in its Poll call here, so do not free it;
        // the next Start replaces it.
        SetState(aasFailed);
        if FHasQueuedPrompt then
        begin
          FHasQueuedPrompt := False;
          FinishTurn('error');
        end;
      end
      else
      begin
        FAgentName := JStr(JObj(result, 'agentInfo'), 'name', Command);
        DoEvent(aekStatus, 'agent: ' + FAgentName + ' ' +
          JStr(JObj(result, 'agentInfo'), 'version'));
        NewSessionRequest;
      end;
    'session/new':
      if errText <> '' then
      begin
        DoEvent(aekError, errText);
        SetState(aasFailed);
        if FHasQueuedPrompt then
        begin
          FHasQueuedPrompt := False;
          FinishTurn('error');
        end;
      end
      else
      begin
        FSessionId := JStr(result, 'sessionId');
        ReadSessionOptions(result);
        SessionReady;
      end;
    'session/prompt':
      if errText <> '' then
      begin
        DoEvent(aekError, errText);
        FinishTurn('error');
      end
      else
        FinishTurn(JStr(result, 'stopReason', 'end_turn'));
    'session/set_config_option':
      if errText <> '' then
        DoEvent(aekError, errText)
      else
        ReadSessionOptions(result);
    else
      if errText <> '' then
        DoEvent(aekError, errText);
  end;
end;

procedure TACPAdapter.HandleRequest(AMessage: TJSONObject);
var
  idJSON, method: string;
  params: TJSONObject;
begin
  idJSON := AMessage.Find('id').AsJSON;
  method := JStr(AMessage, 'method');
  params := JObj(AMessage, 'params');
  case method of
    'fs/read_text_file':
      HandleReadFile(idJSON, params);
    'session/request_permission':
      HandlePermission(idJSON, params);
    else
      SendError(idJSON, -32601, 'Method not supported: ' + method);
  end;
end;

procedure TACPAdapter.HandleReadFile(const AIdJSON: string; AParams: TJSONObject);
var
  path, content: string;
  lines: TStringList;
  first, limit, i: integer;
  result: TJSONObject;
begin
  path := JStr(AParams, 'path');
  try
    content := ReadTextFile(path);
  except
    on E: Exception do
    begin
      SendError(AIdJSON, -32002, 'Cannot read ' + path + ': ' + E.Message);
      Exit;
    end;
  end;
  first := StrToIntDef(JStr(AParams, 'line'), 0);
  limit := StrToIntDef(JStr(AParams, 'limit'), 0);
  if (first > 1) or (limit > 0) then
  begin
    lines := TStringList.Create;
    try
      lines.Text := content;
      if first < 1 then
        first := 1;
      if (limit <= 0) or (first - 1 + limit > lines.Count) then
        limit := lines.Count - first + 1;
      content := '';
      for i := first - 1 to first - 2 + limit do
        content := content + lines[i] + #10;
    finally
      lines.Free;
    end;
  end;
  DoEvent(aekLog, 'read file: ' + path);
  result := TJSONObject.Create;
  result.Add('content', content);
  SendResult(AIdJSON, result);
end;

procedure TACPAdapter.HandlePermission(const AIdJSON: string; AParams: TJSONObject);
var
  request: TAIPermissionRequest;
  tool, opt: TJSONObject;
  options: TJSONArray;
  i: integer;
begin
  request := Default(TAIPermissionRequest);
  Inc(FNextId);
  request.Key := 'perm' + IntToStr(FNextId);
  tool := JObj(AParams, 'toolCall');
  request.ToolCallId := JStr(tool, 'toolCallId');
  request.ToolTitle := JStr(tool, 'title', 'tool');
  request.ToolKind := JStr(tool, 'kind');
  if Assigned(tool) and Assigned(tool.Find('rawInput')) then
    request.Detail := tool.Find('rawInput').AsJSON;
  options := JArr(AParams, 'options');
  if Assigned(options) then
  begin
    SetLength(request.Options, options.Count);
    for i := 0 to options.Count - 1 do
    begin
      if options[i].JSONType <> jtObject then
        Continue;
      opt := TJSONObject(options[i]);
      request.Options[i].Id := JStr(opt, 'optionId');
      request.Options[i].Name := JStr(opt, 'name');
      request.Options[i].Kind := JStr(opt, 'kind');
    end;
  end;
  FPermissions.Values[request.Key] := AIdJSON;
  DoPermission(request);
end;

procedure TACPAdapter.HandleSessionUpdate(AUpdate: TJSONObject);
var
  kind, s: string;
  ev: TAIEvent;
  arr: TJSONArray;
  i: integer;
  entry: TJSONObject;
begin
  if not Assigned(AUpdate) then
    Exit;
  kind := JStr(AUpdate, 'sessionUpdate');
  case kind of
    'agent_message_chunk':
      DoEvent(aekText, ContentText(AUpdate.Find('content')));
    'agent_thought_chunk':
      DoEvent(aekThinking, ContentText(AUpdate.Find('content')));
    'tool_call', 'tool_call_update':
    begin
      ev := AIEvent(aekToolUpdate);
      if kind = 'tool_call' then
        ev.Kind := aekToolCall;
      ev.ToolCallId := JStr(AUpdate, 'toolCallId');
      ev.ToolTitle := JStr(AUpdate, 'title');
      ev.ToolKind := JStr(AUpdate, 'kind');
      ev.ToolStatus := AIToolStatusFromString(JStr(AUpdate, 'status', 'pending'));
      arr := JArr(AUpdate, 'locations');
      if Assigned(arr) and (arr.Count > 0) and (arr[0].JSONType = jtObject) then
        ev.ToolPath := JStr(TJSONObject(arr[0]), 'path');
      arr := JArr(AUpdate, 'content');
      if Assigned(arr) then
        for i := 0 to arr.Count - 1 do
        begin
          s := ContentText(arr[i]);
          if s <> '' then
            ev.Text := ev.Text + s + LineEnding;
        end;
      DoEventRec(ev);
    end;
    'plan':
    begin
      s := '';
      arr := JArr(AUpdate, 'entries');
      if Assigned(arr) then
        for i := 0 to arr.Count - 1 do
          if arr[i].JSONType = jtObject then
          begin
            entry := TJSONObject(arr[i]);
            s := s + '[' + JStr(entry, 'status') + '] ' + JStr(entry, 'content') + LineEnding;
          end;
      DoEvent(aekPlan, s);
    end;
    'usage_update':
      DoEvent(aekUsage, JStr(AUpdate, 'used') + '/' + JStr(AUpdate, 'size'));
    'config_option_update':
      ReadSessionOptions(AUpdate);
  end;
end;

procedure TACPAdapter.ReadSessionOptions(AResult: TJSONObject);

  procedure AddModel(AItem: TJSONObject; const AIdField: string);
  var
    n: integer;
  begin
    n := Length(FModels);
    SetLength(FModels, n + 1);
    FModels[n].Id := JStr(AItem, AIdField);
    FModels[n].Name := JStr(AItem, 'name', FModels[n].Id);
  end;

  procedure ReadSelect(AOptions: TJSONArray; AModels: boolean);
  var
    i: integer;
    item: TJSONObject;
  begin
    if not Assigned(AOptions) then
      Exit;
    for i := 0 to AOptions.Count - 1 do
    begin
      if AOptions[i].JSONType <> jtObject then
        Continue;
      item := TJSONObject(AOptions[i]);
      // Grouped select: {"group": "...", "options": [...]}
      if Assigned(JArr(item, 'options')) then
        ReadSelect(JArr(item, 'options'), AModels)
      else if AModels then
        AddModel(item, 'value')
      else
        FEffortValues.Add(JStr(item, 'value'));
    end;
  end;

var
  options, list: TJSONArray;
  modelsObj, opt: TJSONObject;
  i: integer;
  category: string;
begin
  if not Assigned(AResult) then
    Exit;
  options := JArr(AResult, 'configOptions');
  if Assigned(options) then
  begin
    for i := 0 to options.Count - 1 do
    begin
      if options[i].JSONType <> jtObject then
        Continue;
      opt := TJSONObject(options[i]);
      category := JStr(opt, 'category', JStr(opt, 'id'));
      if category = 'model' then
      begin
        FModelConfigId := JStr(opt, 'id');
        SetLength(FModels, 0);
        ReadSelect(JArr(opt, 'options'), True);
        DoEvent(aekStatus, 'model: ' + JStr(opt, 'currentValue'));
      end
      else if category = 'thought_level' then
      begin
        FEffortConfigId := JStr(opt, 'id');
        FEffortValues.Clear;
        ReadSelect(JArr(opt, 'options'), False);
      end;
    end;
    Exit;
  end;
  // Older agents: {"models": {"availableModels": [...], "currentModelId": ""}}
  modelsObj := JObj(AResult, 'models');
  list := JArr(modelsObj, 'availableModels');
  if Assigned(list) then
  begin
    SetLength(FModels, 0);
    for i := 0 to list.Count - 1 do
      if list[i].JSONType = jtObject then
        AddModel(TJSONObject(list[i]), 'modelId');
    DoEvent(aekStatus, 'model: ' + JStr(modelsObj, 'currentModelId'));
  end;
end;

procedure TACPAdapter.SessionReady;
var
  text: string;
begin
  DoEvent(aekStatus, 'session: ' + FSessionId);
  SetState(aasReady);
  if Model <> '' then
    ApplyModel;
  if (Effort <> '') and (Effort <> 'auto') then
    ApplyEffort;
  if FHasQueuedPrompt then
  begin
    FHasQueuedPrompt := False;
    text := FQueuedPrompt;
    FQueuedPrompt := '';
    SendPrompt(text);
  end;
end;

procedure TACPAdapter.ApplyModel;
var
  params: TJSONObject;
begin
  FModelDirty := False;
  if (Model = '') or (FSessionId = '') then
    Exit;
  params := TJSONObject.Create;
  params.Add('sessionId', FSessionId);
  if FModelConfigId <> '' then
  begin
    params.Add('configId', FModelConfigId);
    params.Add('value', Model);
    SendRequest('session/set_config_option', params);
  end
  else if Length(FModels) > 0 then
  begin
    params.Add('modelId', Model);
    SendRequest('session/set_model', params);
  end
  else
  begin
    params.Free;
    DoEvent(aekError, 'This runtime does not support choosing a model');
  end;
end;

procedure TACPAdapter.ApplyEffort;
var
  params: TJSONObject;
  i: integer;
begin
  FEffortDirty := False;
  if (FSessionId = '') or (Effort = '') or (Effort = 'auto') then
    Exit;
  if FEffortConfigId = '' then
  begin
    DoEvent(aekLog, 'effort ignored: runtime has no thought_level option');
    Exit;
  end;
  i := FEffortValues.IndexOf(Effort);
  if i < 0 then
  begin
    DoEvent(aekLog, 'effort ignored: runtime does not offer ' + Effort);
    Exit;
  end;
  params := TJSONObject.Create;
  params.Add('sessionId', FSessionId);
  params.Add('configId', FEffortConfigId);
  params.Add('value', FEffortValues[i]);
  SendRequest('session/set_config_option', params);
end;

procedure TACPAdapter.SendPrompt(const AText: string);
var
  params, block: TJSONObject;
  promptArr: TJSONArray;
begin
  if FModelDirty then
    ApplyModel;
  if FEffortDirty then
    ApplyEffort;
  block := TJSONObject.Create;
  block.Add('type', 'text');
  block.Add('text', AText);
  promptArr := TJSONArray.Create;
  promptArr.Add(block);
  params := TJSONObject.Create;
  params.Add('sessionId', FSessionId);
  params.Add('prompt', promptArr);
  SetState(aasBusy);
  SendRequest('session/prompt', params);
end;

procedure TACPAdapter.NewSessionRequest;
var
  params: TJSONObject;
  dir: string;
begin
  dir := WorkingDir;
  if dir = '' then
    dir := GetCurrentDir;
  params := TJSONObject.Create;
  params.Add('cwd', ExpandFileName(ExcludeTrailingPathDelimiter(dir)));
  params.Add('mcpServers', TJSONArray.Create);
  SendRequest('session/new', params);
end;

procedure TACPAdapter.FinishTurn(const AStopReason: string);
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

procedure TACPAdapter.CancelPermissions;
begin
  while FPermissions.Count > 0 do
    RespondPermission(FPermissions.Names[0], '');
end;

procedure TACPAdapter.ModelChanged;
begin
  FModelDirty := True;
  if FState = aasReady then
    ApplyModel;
end;

procedure TACPAdapter.EffortChanged;
begin
  FEffortDirty := True;
  if FState = aasReady then
    ApplyEffort;
end;

end.

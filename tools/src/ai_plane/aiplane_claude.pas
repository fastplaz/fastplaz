unit aiplane_claude;

{
  Claude Code adapter using its own stream-json mode (no ACP bridge, so no
  Node.js is needed):

    claude -p --input-format stream-json --output-format stream-json
           --verbose --include-partial-messages --permission-prompt-tool stdio

  The process stays alive between turns; every user message is one JSON line
  on stdin. Text arrives as partial "stream_event" deltas, tool calls as
  "assistant"/"user" messages, and each turn ends with a "result" message.
  Permission prompts arrive as control_request/can_use_tool and are answered
  with control_response.

  Model and effort are command line flags, so changing them restarts the
  process and resumes the same session (--resume).
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, aiplane_types, aiplane_process;

type

  { TClaudeStreamAdapter }

  TClaudeStreamAdapter = class(TAIAdapter)
  private
    FChannel: TAIProcessChannel;
    FPermissions: TStringList;  // key -> request_id
    FPermInputs: TStringList;   // key -> tool input JSON
    FSawTextDelta: boolean;
    FRestartNeeded: boolean;
    FNextId: integer;
    procedure ChannelLine(Sender: TObject; const ALine: string);
    procedure ChannelStderr(Sender: TObject; const ALine: string);
    procedure ChannelExit(Sender: TObject; AExitCode: integer);
    procedure WriteJSON(AMessage: TJSONObject);
    procedure HandleStreamEvent(AEvent: TJSONObject);
    procedure HandleAssistant(AMessage: TJSONObject);
    procedure HandleUser(AMessage: TJSONObject);
    procedure HandleResult(AMessage: TJSONObject);
    procedure HandleControlRequest(AMessage: TJSONObject);
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
    procedure HandleLine(const ALine: string);
    class function AdapterName: string; override;
  end;

{ Maps the AI Plane effort level to a claude --effort value ('' = omit). }
function ClaudeEffortArg(const AEffort: string): string;

implementation

const
  PERMISSION_ALLOW = 'allow';
  PERMISSION_DENY = 'deny';
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

{ tool_result content is either a string or an array of text blocks. }
function ToolResultText(AData: TJSONData): string;
var
  i: integer;
  arr: TJSONArray;
begin
  Result := '';
  if not Assigned(AData) then
    Exit;
  if AData.JSONType = jtString then
    Result := AData.AsString
  else if AData.JSONType = jtArray then
  begin
    arr := TJSONArray(AData);
    for i := 0 to arr.Count - 1 do
      if (arr[i].JSONType = jtObject) and (JStr(TJSONObject(arr[i]), 'type') = 'text') then
        Result := Result + JStr(TJSONObject(arr[i]), 'text');
  end;
  if Length(Result) > MAX_TOOL_OUTPUT then
    Result := Copy(Result, 1, MAX_TOOL_OUTPUT) + LineEnding + '...';
end;

function ClaudeEffortArg(const AEffort: string): string;
begin
  case LowerCase(AEffort) of
    'off', 'minimal', 'low': Result := 'low';
    'medium': Result := 'medium';
    'high': Result := 'high';
    'xhigh': Result := 'xhigh';
    'max': Result := 'max';
    else
      Result := '';
  end;
end;

{ TClaudeStreamAdapter }

constructor TClaudeStreamAdapter.Create;
const
  ALIASES: array[0..2] of string = ('opus', 'sonnet', 'haiku');
var
  i: integer;
begin
  inherited Create;
  Command := 'claude';
  FPermissions := TStringList.Create;
  FPermInputs := TStringList.Create;
  // Claude Code has no command that lists models; the aliases always point
  // at the latest model of each family. A full model id also works.
  SetLength(FModels, Length(ALIASES));
  for i := 0 to High(ALIASES) do
  begin
    FModels[i].Id := ALIASES[i];
    FModels[i].Name := ALIASES[i];
  end;
end;

destructor TClaudeStreamAdapter.Destroy;
begin
  if Assigned(FChannel) then
    FChannel.OnExit := nil;
  FreeAndNil(FChannel);
  FPermInputs.Free;
  FPermissions.Free;
  inherited Destroy;
end;

class function TClaudeStreamAdapter.AdapterName: string;
begin
  Result := 'Claude Code (stream-json)';
end;

procedure TClaudeStreamAdapter.BuildArguments(AArgs: TStrings);
var
  effortArg: string;
begin
  AArgs.Add('-p');
  AArgs.Add('--input-format');
  AArgs.Add('stream-json');
  AArgs.Add('--output-format');
  AArgs.Add('stream-json');
  AArgs.Add('--verbose');
  AArgs.Add('--include-partial-messages');
  AArgs.Add('--permission-prompt-tool');
  AArgs.Add('stdio');
  if Model <> '' then
  begin
    AArgs.Add('--model');
    AArgs.Add(Model);
  end;
  effortArg := ClaudeEffortArg(Effort);
  if effortArg <> '' then
  begin
    AArgs.Add('--effort');
    AArgs.Add(effortArg);
  end;
  if FSessionId <> '' then
  begin
    AArgs.Add('--resume');
    AArgs.Add(FSessionId);
  end;
  AArgs.AddStrings(Arguments);
end;

procedure TClaudeStreamAdapter.Start;
var
  args: TStringList;
begin
  if Assigned(FChannel) and FChannel.Running and not FRestartNeeded then
    Exit;
  StopChannel;
  FRestartNeeded := False;
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
    DoEvent(aekLog, 'started: ' + Command + ' ' + StringReplace(Trim(args.Text),
      LineEnding, ' ', [rfReplaceAll]));
  finally
    args.Free;
  end;
  // Claude reads stdin before it prints anything, so there is no handshake
  // to wait for: the system/init message follows the first prompt.
  SetState(aasReady);
end;

procedure TClaudeStreamAdapter.StopChannel;
begin
  CancelPermissions;
  if Assigned(FChannel) then
  begin
    FChannel.OnExit := nil;
    FChannel.Stop;
    FreeAndNil(FChannel);
  end;
end;

procedure TClaudeStreamAdapter.Stop;
begin
  StopChannel;
  SetState(aasStopped);
end;

procedure TClaudeStreamAdapter.Prompt(const AText: string);
var
  msg, message, block: TJSONObject;
  content: TJSONArray;
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
  block := TJSONObject.Create;
  block.Add('type', 'text');
  block.Add('text', AText);
  content := TJSONArray.Create;
  content.Add(block);
  message := TJSONObject.Create;
  message.Add('role', 'user');
  message.Add('content', content);
  msg := TJSONObject.Create;
  msg.Add('type', 'user');
  msg.Add('message', message);
  SetState(aasBusy);
  WriteJSON(msg);
end;

procedure TClaudeStreamAdapter.Cancel;
var
  msg, request: TJSONObject;
begin
  if FState <> aasBusy then
    Exit;
  CancelPermissions;
  Inc(FNextId);
  request := TJSONObject.Create;
  request.Add('subtype', 'interrupt');
  msg := TJSONObject.Create;
  msg.Add('type', 'control_request');
  msg.Add('request_id', 'fastplaz_' + IntToStr(FNextId));
  msg.Add('request', request);
  WriteJSON(msg);
end;

function TClaudeStreamAdapter.Poll: boolean;
begin
  Result := Assigned(FChannel) and FChannel.Poll;
end;

procedure TClaudeStreamAdapter.RespondPermission(const AKey, AOptionId: string);
var
  i: integer;
  requestId, inputJSON: string;
  msg, response, inner: TJSONObject;
begin
  i := FPermissions.IndexOfName(AKey);
  if i < 0 then
    Exit;
  requestId := FPermissions.ValueFromIndex[i];
  FPermissions.Delete(i);
  i := FPermInputs.IndexOfName(AKey);
  inputJSON := '{}';
  if i >= 0 then
  begin
    inputJSON := FPermInputs.ValueFromIndex[i];
    FPermInputs.Delete(i);
  end;

  inner := TJSONObject.Create;
  if AOptionId = PERMISSION_ALLOW then
  begin
    inner.Add('behavior', 'allow');
    inner.Add('updatedInput', GetJSON(inputJSON));
  end
  else
  begin
    inner.Add('behavior', 'deny');
    inner.Add('message', 'The user denied this action.');
  end;
  response := TJSONObject.Create;
  response.Add('subtype', 'success');
  response.Add('request_id', requestId);
  response.Add('response', inner);
  msg := TJSONObject.Create;
  msg.Add('type', 'control_response');
  msg.Add('response', response);
  WriteJSON(msg);
end;

procedure TClaudeStreamAdapter.NewSession;
begin
  if FState = aasBusy then
    raise EAIAdapter.Create('Cannot start a new session while a prompt is running');
  StopChannel;
  FSessionId := '';
  SetState(aasIdle);
end;

procedure TClaudeStreamAdapter.ModelChanged;
begin
  FRestartNeeded := Assigned(FChannel);
end;

procedure TClaudeStreamAdapter.EffortChanged;
begin
  FRestartNeeded := Assigned(FChannel);
end;

procedure TClaudeStreamAdapter.WriteJSON(AMessage: TJSONObject);
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

procedure TClaudeStreamAdapter.ChannelLine(Sender: TObject; const ALine: string);
begin
  HandleLine(ALine);
end;

procedure TClaudeStreamAdapter.ChannelStderr(Sender: TObject; const ALine: string);
begin
  DoEvent(aekLog, 'stderr: ' + ALine);
end;

procedure TClaudeStreamAdapter.ChannelExit(Sender: TObject; AExitCode: integer);
begin
  FPermissions.Clear;
  FPermInputs.Clear;
  if FState = aasBusy then
  begin
    DoEvent(aekError, Format('Claude Code exited (code %d)', [AExitCode]));
    SetState(aasFailed);
    FinishTurn('error');
  end
  else
    SetState(aasStopped);
end;

procedure TClaudeStreamAdapter.HandleLine(const ALine: string);
var
  data: TJSONData;
  msg: TJSONObject;
  kind: string;
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
    kind := JStr(msg, 'type');
    case kind of
      'system':
        if JStr(msg, 'subtype') = 'init' then
        begin
          FSessionId := JStr(msg, 'session_id', FSessionId);
          DoEvent(aekStatus, 'model: ' + JStr(msg, 'model'));
          DoEvent(aekStatus, 'session: ' + FSessionId);
        end;
      'stream_event':
        HandleStreamEvent(JObj(msg, 'event'));
      'assistant':
        HandleAssistant(JObj(msg, 'message'));
      'user':
        HandleUser(JObj(msg, 'message'));
      'result':
        HandleResult(msg);
      'control_request':
        HandleControlRequest(msg);
    end;
  finally
    data.Free;
  end;
end;

procedure TClaudeStreamAdapter.HandleStreamEvent(AEvent: TJSONObject);
var
  delta, block: TJSONObject;
  ev: TAIEvent;
begin
  if not Assigned(AEvent) then
    Exit;
  case JStr(AEvent, 'type') of
    'message_start':
      FSawTextDelta := False;
    'content_block_start':
    begin
      block := JObj(AEvent, 'content_block');
      if JStr(block, 'type') = 'tool_use' then
      begin
        ev := AIEvent(aekToolCall);
        ev.ToolCallId := JStr(block, 'id');
        ev.ToolTitle := JStr(block, 'name');
        ev.ToolKind := JStr(block, 'name');
        ev.ToolStatus := atsPending;
        DoEventRec(ev);
      end;
    end;
    'content_block_delta':
    begin
      delta := JObj(AEvent, 'delta');
      case JStr(delta, 'type') of
        'text_delta':
        begin
          FSawTextDelta := True;
          DoEvent(aekText, JStr(delta, 'text'));
        end;
        'thinking_delta':
          DoEvent(aekThinking, JStr(delta, 'thinking'));
      end;
    end;
  end;
end;

procedure TClaudeStreamAdapter.HandleAssistant(AMessage: TJSONObject);
var
  content: TJSONArray;
  block, input: TJSONObject;
  i: integer;
  ev: TAIEvent;
begin
  content := JArr(AMessage, 'content');
  if not Assigned(content) then
    Exit;
  for i := 0 to content.Count - 1 do
  begin
    if content[i].JSONType <> jtObject then
      Continue;
    block := TJSONObject(content[i]);
    case JStr(block, 'type') of
      'text':
        // Only when partial messages were not streamed, to avoid doubles.
        if not FSawTextDelta then
          DoEvent(aekText, JStr(block, 'text'));
      'tool_use':
      begin
        ev := AIEvent(aekToolUpdate);
        ev.ToolCallId := JStr(block, 'id');
        ev.ToolTitle := JStr(block, 'name');
        ev.ToolKind := JStr(block, 'name');
        ev.ToolStatus := atsRunning;
        input := JObj(block, 'input');
        if Assigned(input) then
        begin
          ev.ToolPath := JStr(input, 'file_path', JStr(input, 'path'));
          ev.Text := input.AsJSON;
        end;
        DoEventRec(ev);
      end;
    end;
  end;
end;

procedure TClaudeStreamAdapter.HandleUser(AMessage: TJSONObject);
var
  content: TJSONArray;
  block: TJSONObject;
  i: integer;
  ev: TAIEvent;
begin
  content := JArr(AMessage, 'content');
  if not Assigned(content) then
    Exit;
  for i := 0 to content.Count - 1 do
  begin
    if content[i].JSONType <> jtObject then
      Continue;
    block := TJSONObject(content[i]);
    if JStr(block, 'type') <> 'tool_result' then
      Continue;
    ev := AIEvent(aekToolUpdate);
    ev.ToolCallId := JStr(block, 'tool_use_id');
    if JStr(block, 'is_error') = 'True' then
      ev.ToolStatus := atsFailed
    else
      ev.ToolStatus := atsCompleted;
    ev.Text := ToolResultText(block.Find('content'));
    DoEventRec(ev);
  end;
end;

procedure TClaudeStreamAdapter.HandleResult(AMessage: TJSONObject);
var
  usage: TJSONObject;
  cost: TJSONData;
  costText: string;
  fs: TFormatSettings;
begin
  FSessionId := JStr(AMessage, 'session_id', FSessionId);
  usage := JObj(AMessage, 'usage');
  if Assigned(usage) then
  begin
    // TJSONFloatNumber.AsString gives 2.39E-003; format it for people.
    costText := '0';
    cost := AMessage.Find('total_cost_usd');
    fs := DefaultFormatSettings;
    fs.DecimalSeparator := '.';
    if Assigned(cost) and (cost.JSONType = jtNumber) then
      costText := FormatFloat('0.0000', cost.AsFloat, fs);
    DoEvent(aekUsage, Format('in %s, out %s, cost $%s', [JStr(usage, 'input_tokens'),
      JStr(usage, 'output_tokens'), costText]));
  end;
  if JStr(AMessage, 'is_error') = 'True' then
  begin
    DoEvent(aekError, JStr(AMessage, 'result', JStr(AMessage, 'subtype')));
    FinishTurn('error');
  end
  else
    FinishTurn(JStr(AMessage, 'stop_reason', 'end_turn'));
end;

procedure TClaudeStreamAdapter.HandleControlRequest(AMessage: TJSONObject);
var
  request: TJSONObject;
  perm: TAIPermissionRequest;
  input: TJSONData;
begin
  request := JObj(AMessage, 'request');
  if JStr(request, 'subtype') <> 'can_use_tool' then
    Exit;
  Inc(FNextId);
  perm := Default(TAIPermissionRequest);
  perm.Key := 'perm' + IntToStr(FNextId);
  perm.ToolCallId := JStr(request, 'tool_use_id');
  perm.ToolTitle := JStr(request, 'tool_name');
  perm.ToolKind := perm.ToolTitle;
  input := request.Find('input');
  if Assigned(input) then
    perm.Detail := input.AsJSON
  else
    perm.Detail := '{}';
  SetLength(perm.Options, 2);
  perm.Options[0].Id := PERMISSION_ALLOW;
  perm.Options[0].Name := 'Allow';
  perm.Options[0].Kind := 'allow_once';
  perm.Options[1].Id := PERMISSION_DENY;
  perm.Options[1].Name := 'Deny';
  perm.Options[1].Kind := 'reject_once';
  FPermissions.Values[perm.Key] := JStr(AMessage, 'request_id');
  FPermInputs.Values[perm.Key] := perm.Detail;
  DoPermission(perm);
end;

procedure TClaudeStreamAdapter.FinishTurn(const AStopReason: string);
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

procedure TClaudeStreamAdapter.CancelPermissions;
begin
  while FPermissions.Count > 0 do
    RespondPermission(FPermissions.Names[0], '');
end;

end.

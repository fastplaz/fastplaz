unit aiplane_mcpserver;

{
  MCP server for Agent Plane: the IDE offers its own tools (open a file, compile
  the project, ...) to the coding agent over the MCP Streamable HTTP
  transport, in its simplest form: POST /mcp with one JSON-RPC message, JSON
  response, no SSE stream.

  - Bound to 127.0.0.1, Bearer token required, non-loopback Origin refused
    (DNS rebinding protection).
  - Tools are registered by the host (RegisterTool). A handler gets a
    TAIMcpToolCall and finishes it at once or later (Finish/Fail).
  - Tools marked mtpAsk / mtpAskSession raise OnPermission first; the host
    shows a card and calls RespondPermission. Until then the HTTP request
    waits. The card text is the tool title, the tool's own summary
    (SetToolDescriber) and the arguments as "name: value" lines.
  - Poll from a timer; everything runs in the caller's thread.

  Design: docs/AI_PLANE_TOOL_RUNDOWN.md. No LCL or IDEIntf here.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, aiplane_types, aiplane_httpserver;

const
  AI_MCP_DEFAULT_PORT = 17845;
  AI_MCP_PORT_TRIES = 10;
  AI_MCP_PATH = '/mcp';
  AI_MCP_LATEST_PROTOCOL = '2025-06-18';
  AI_MCP_MAX_RESULT = 32 * 1024;

  // permission option ids
  AI_MCP_ALLOW_ONCE = 'allow_once';
  AI_MCP_ALLOW_SESSION = 'allow_always';
  AI_MCP_REJECT = 'reject_once';

type
  TAIMcpServer = class;

  TAIMcpToolPermission = (
    mtpAuto,        // runs without asking
    mtpAsk,         // asks every time
    mtpAskSession   // asks once, "allow for this session" is remembered
    );

  { TAIMcpToolCall }

  TAIMcpToolCall = class
  private
    FServer: TAIMcpServer;
    FRequest: TAIHttpRequest;
    FIdJSON: string;
    FName: string;
    FArgs: TJSONObject;
    FFinished: boolean;
    FPermissionKey: string;
  public
    destructor Destroy; override;
    function Str(const AName: string; const ADefault: string = ''): string;
    function Int(const AName: string; ADefault: integer = 0): integer;
    function Bool(const AName: string; ADefault: boolean = False): boolean;
    { Text result. Long text is cut at AI_MCP_MAX_RESULT bytes. }
    procedure Finish(const AText: string);
    { JSON result, sent as formatted text. Takes ownership of AData. }
    procedure FinishJSON(AData: TJSONData);
    { Tool error (isError = true): the agent reads AMessage. }
    procedure Fail(const AMessage: string);
    property Name: string read FName;
    property Args: TJSONObject read FArgs;
    property Finished: boolean read FFinished;
  end;

  TAIMcpToolHandler = procedure(ACall: TAIMcpToolCall) of object;
  { Says in words what a call is about to do, for the permission card
    (e.g. the file it creates). '' adds nothing. }
  TAIMcpToolDescriber = function(ACall: TAIMcpToolCall): string of object;

  TAIMcpTool = record
    Name: string;
    Title: string;
    Description: string;
    InputSchema: string; // JSON object text
    Permission: TAIMcpToolPermission;
    Handler: TAIMcpToolHandler;
    Describe: TAIMcpToolDescriber;
  end;

  TAIMcpLogEvent = procedure(Sender: TObject; const AText: string) of object;

  { TAIMcpServer }

  TAIMcpServer = class
  private
    FHttp: TAIHttpServer;
    FTools: array of TAIMcpTool;
    FToken: string;
    FSessions: TStringList;
    FCalls: TList;               // running or waiting TAIMcpToolCall
    FAllowed: TStringList;       // tools allowed for this session
    FNextPermission: integer;
    FOnPermission: TAIPermissionHandler;
    FOnLog: TAIMcpLogEvent;
    FInstructions: string;
    procedure HttpRequest(Sender: TObject; ARequest: TAIHttpRequest);
    procedure HandleMcp(ARequest: TAIHttpRequest);
    procedure HandleMessage(ARequest: TAIHttpRequest; AMessage: TJSONObject);
    procedure HandleInitialize(ARequest: TAIHttpRequest; const AIdJSON: string;
      AParams: TJSONObject);
    procedure HandleToolsList(ARequest: TAIHttpRequest; const AIdJSON: string);
    procedure HandleToolsCall(ARequest: TAIHttpRequest; const AIdJSON: string;
      AParams: TJSONObject);
    procedure RunCall(ACall: TAIMcpToolCall);
    procedure AskPermission(ACall: TAIMcpToolCall; const ATool: TAIMcpTool);
    procedure SendResult(ARequest: TAIHttpRequest; const AIdJSON: string;
      AResult: TJSONData; AHeaders: TStrings = nil);
    procedure SendError(ARequest: TAIHttpRequest; const AIdJSON: string;
      ACode: integer; const AMessage: string);
    procedure CallFinished(ACall: TAIMcpToolCall; AResult: TJSONObject);
    function OriginAllowed(const AOrigin: string): boolean;
    function FindTool(const AName: string; out ATool: TAIMcpTool): boolean;
    procedure Log(const AText: string);
    function GetPort: word;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterTool(const AName, ATitle, ADescription, AInputSchema: string;
      APermission: TAIMcpToolPermission; AHandler: TAIMcpToolHandler);
    { Adds a summary to the permission card of a registered tool. }
    procedure SetToolDescriber(const AName: string; ADescribe: TAIMcpToolDescriber);
    { Starts on APort, or the next free port (AI_MCP_PORT_TRIES). A new token
      is made at every start. }
    function Start(APort: word = AI_MCP_DEFAULT_PORT): boolean;
    { Stops listening; waiting calls are dropped. }
    procedure Stop;
    function Active: boolean;
    function Poll: boolean;
    { Answers a permission request raised through OnPermission. }
    procedure RespondPermission(const AKey, AOptionId: string);
    { Forgets "allow for this session" choices. }
    procedure ResetAllowed;
    function ToolCount: integer;
    function Url: string;
    function LastError: string;
    property Port: word read GetPort;
    property Token: string read FToken;
    { Sent to the agent in the initialize result. }
    property Instructions: string read FInstructions write FInstructions;
    property OnPermission: TAIPermissionHandler read FOnPermission write FOnPermission;
    property OnLog: TAIMcpLogEvent read FOnLog write FOnLog;
  end;

{ Random hex string from two GUIDs (CoCreateGuid / /dev/urandom). }
function AIMcpNewToken: string;
{ Tool arguments as "name: value" lines for people: strings without quotes,
  other values as JSON, empty strings and nulls left out. }
function AIMcpArgsText(AArgs: TJSONObject): string;

implementation

const
  KNOWN_PROTOCOLS: array[0..3] of string =
    ('2025-11-25', '2025-06-18', '2025-03-26', '2024-11-05');

  JSONRPC_PARSE_ERROR = -32700;
  JSONRPC_INVALID_REQUEST = -32600;
  JSONRPC_METHOD_NOT_FOUND = -32601;
  JSONRPC_INVALID_PARAMS = -32602;

function AIMcpNewToken: string;

  function Hex(const AGuid: TGUID): string;
  var
    b: array[0..15] of byte absolute AGuid;
    i: integer;
  begin
    Result := '';
    for i := 0 to 15 do
      Result := Result + IntToHex(b[i], 2);
  end;

var
  g1, g2: TGUID;
begin
  CreateGUID(g1);
  CreateGUID(g2);
  Result := LowerCase(Hex(g1) + Hex(g2));
end;

function AIMcpArgsText(AArgs: TJSONObject): string;
var
  i: integer;
  v: TJSONData;
  value: string;
begin
  Result := '';
  if not Assigned(AArgs) then
    Exit;
  for i := 0 to AArgs.Count - 1 do
  begin
    v := AArgs.Items[i];
    case v.JSONType of
      jtNull: Continue;
      jtString: value := v.AsString;
      else
        value := v.AsJSON;
    end;
    if value = '' then
      Continue;
    // one line per argument: a multi-line value is indented
    value := StringReplace(StringReplace(value, #13#10, #10, [rfReplaceAll]),
      #10, LineEnding + '    ', [rfReplaceAll]);
    if Result <> '' then
      Result := Result + LineEnding;
    Result := Result + AArgs.Names[i] + ': ' + value;
  end;
end;

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

// Cuts at a UTF-8 character boundary.
function CutUTF8(const AText: string; AMax: integer): string;
var
  n: integer;
begin
  if Length(AText) <= AMax then
    Exit(AText);
  n := AMax;
  while (n > 0) and ((Ord(AText[n + 1]) and $C0) = $80) do
    Dec(n);
  Result := Copy(AText, 1, n) + LineEnding +
    Format('... (truncated, %d of %d bytes)', [n, Length(AText)]);
end;

function TextResult(const AText: string; AIsError: boolean): TJSONObject;
var
  content: TJSONArray;
begin
  content := TJSONArray.Create;
  content.Add(TJSONObject.Create(['type', 'text',
    'text', CutUTF8(AText, AI_MCP_MAX_RESULT)]));
  Result := TJSONObject.Create;
  Result.Add('content', content);
  Result.Add('isError', AIsError);
end;

{ TAIMcpToolCall }

destructor TAIMcpToolCall.Destroy;
begin
  FArgs.Free;
  inherited Destroy;
end;

function TAIMcpToolCall.Str(const AName: string; const ADefault: string): string;
begin
  Result := JStr(FArgs, AName, ADefault);
end;

function TAIMcpToolCall.Int(const AName: string; ADefault: integer): integer;
var
  d: TJSONData;
begin
  Result := ADefault;
  d := FArgs.Find(AName);
  if Assigned(d) then
    case d.JSONType of
      jtNumber: Result := d.AsInteger;
      jtString: Result := StrToIntDef(d.AsString, ADefault);
    end;
end;

function TAIMcpToolCall.Bool(const AName: string; ADefault: boolean): boolean;
var
  d: TJSONData;
begin
  Result := ADefault;
  d := FArgs.Find(AName);
  if Assigned(d) then
    case d.JSONType of
      jtBoolean: Result := d.AsBoolean;
      jtString: Result := SameText(d.AsString, 'true') or (d.AsString = '1');
      jtNumber: Result := d.AsInteger <> 0;
    end;
end;

procedure TAIMcpToolCall.Finish(const AText: string);
begin
  if FFinished then
    Exit;
  FFinished := True;
  FServer.CallFinished(Self, TextResult(AText, False));
end;

procedure TAIMcpToolCall.FinishJSON(AData: TJSONData);
var
  s: string;
begin
  try
    s := AData.FormatJSON([], 2);
  finally
    AData.Free;
  end;
  Finish(s);
end;

procedure TAIMcpToolCall.Fail(const AMessage: string);
begin
  if FFinished then
    Exit;
  FFinished := True;
  FServer.CallFinished(Self, TextResult(AMessage, True));
end;

{ TAIMcpServer }

constructor TAIMcpServer.Create;
begin
  FHttp := TAIHttpServer.Create;
  FHttp.OnRequest := @HttpRequest;
  FSessions := TStringList.Create;
  FSessions.Sorted := True;
  FCalls := TList.Create;
  FAllowed := TStringList.Create;
  FAllowed.Sorted := True;
  FAllowed.CaseSensitive := False;
end;

destructor TAIMcpServer.Destroy;
begin
  Stop;
  FHttp.Free;
  FCalls.Free;
  FSessions.Free;
  FAllowed.Free;
  inherited Destroy;
end;

procedure TAIMcpServer.RegisterTool(const AName, ATitle, ADescription,
  AInputSchema: string; APermission: TAIMcpToolPermission; AHandler: TAIMcpToolHandler);
var
  n: integer;
begin
  n := Length(FTools);
  SetLength(FTools, n + 1);
  FTools[n].Name := AName;
  FTools[n].Title := ATitle;
  FTools[n].Description := ADescription;
  FTools[n].InputSchema := AInputSchema;
  FTools[n].Permission := APermission;
  FTools[n].Handler := AHandler;
  FTools[n].Describe := nil;
end;

procedure TAIMcpServer.SetToolDescriber(const AName: string; ADescribe: TAIMcpToolDescriber);
var
  i: integer;
begin
  for i := 0 to High(FTools) do
    if FTools[i].Name = AName then
      FTools[i].Describe := ADescribe;
end;

function TAIMcpServer.Start(APort: word): boolean;
begin
  Stop;
  FToken := AIMcpNewToken;
  Result := FHttp.Start(APort, AI_MCP_PORT_TRIES);
  if Result then
    Log(Format('MCP server on %s (%d tools)', [Url, Length(FTools)]))
  else
    Log('MCP server not started: ' + FHttp.LastError);
end;

procedure TAIMcpServer.Stop;
var
  i: integer;
  call: TAIMcpToolCall;
begin
  for i := FCalls.Count - 1 downto 0 do
  begin
    call := TAIMcpToolCall(FCalls[i]);
    call.FFinished := True;
    // also frees the waiting HTTP request
    FHttp.Respond(call.FRequest, 503, 'text/plain', 'Server stopped');
    call.Free;
  end;
  FCalls.Clear;
  FHttp.Stop;
  FSessions.Clear;
end;

function TAIMcpServer.Active: boolean;
begin
  Result := FHttp.Active;
end;

function TAIMcpServer.Poll: boolean;
begin
  Result := FHttp.Poll;
end;

function TAIMcpServer.ToolCount: integer;
begin
  Result := Length(FTools);
end;

function TAIMcpServer.GetPort: word;
begin
  Result := FHttp.Port;
end;

function TAIMcpServer.Url: string;
begin
  if Active then
    Result := Format('http://127.0.0.1:%d%s', [Port, AI_MCP_PATH])
  else
    Result := '';
end;

function TAIMcpServer.LastError: string;
begin
  Result := FHttp.LastError;
end;

procedure TAIMcpServer.ResetAllowed;
begin
  FAllowed.Clear;
end;

procedure TAIMcpServer.Log(const AText: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, AText);
end;

function TAIMcpServer.FindTool(const AName: string; out ATool: TAIMcpTool): boolean;
var
  i: integer;
begin
  for i := 0 to High(FTools) do
    if FTools[i].Name = AName then
    begin
      ATool := FTools[i];
      Exit(True);
    end;
  ATool := Default(TAIMcpTool);
  Result := False;
end;

// Browsers send Origin; agents usually do not. A page on another host must
// not reach the IDE through DNS rebinding.
function TAIMcpServer.OriginAllowed(const AOrigin: string): boolean;
var
  host: string;
  p: integer;
begin
  if (AOrigin = '') or SameText(AOrigin, 'null') then
    Exit(True);
  host := LowerCase(AOrigin);
  p := Pos('://', host);
  if p > 0 then
    Delete(host, 1, p + 2);
  p := Pos('/', host);
  if p > 0 then
    host := Copy(host, 1, p - 1);
  if (host <> '') and (host[1] = '[') then
    host := Copy(host, 1, Pos(']', host))
  else
  begin
    p := Pos(':', host);
    if p > 0 then
      host := Copy(host, 1, p - 1);
  end;
  Result := (host = '127.0.0.1') or (host = 'localhost') or (host = '[::1]');
end;

procedure TAIMcpServer.HttpRequest(Sender: TObject; ARequest: TAIHttpRequest);
var
  path, sid: string;
  p: integer;
begin
  path := ARequest.Path;
  p := Pos('?', path);
  if p > 0 then
    path := Copy(path, 1, p - 1);
  if path <> AI_MCP_PATH then
  begin
    FHttp.Respond(ARequest, 404, 'text/plain', 'Not found');
    Exit;
  end;
  if not OriginAllowed(ARequest.Header('Origin')) then
  begin
    Log('refused request with Origin ' + ARequest.Header('Origin'));
    FHttp.Respond(ARequest, 403, 'text/plain', 'Origin not allowed');
    Exit;
  end;
  if ARequest.Header('Authorization') <> 'Bearer ' + FToken then
  begin
    FHttp.Respond(ARequest, 401, 'text/plain', 'Unauthorized');
    Exit;
  end;
  case ARequest.Method of
    'POST':
      HandleMcp(ARequest);
    'DELETE':
    begin
      sid := ARequest.Header('Mcp-Session-Id');
      p := FSessions.IndexOf(sid);
      if p >= 0 then
        FSessions.Delete(p);
      FHttp.Respond(ARequest, 200, '', '');
    end;
    else
      // No server-initiated SSE stream; allowed by the spec.
      FHttp.Respond(ARequest, 405, 'text/plain', 'Method not allowed');
  end;
end;

procedure TAIMcpServer.HandleMcp(ARequest: TAIHttpRequest);
var
  data: TJSONData;
  sid: string;
begin
  sid := ARequest.Header('Mcp-Session-Id');
  // An unknown session (the IDE restarted the server) tells the client to
  // initialize again.
  if (sid <> '') and (FSessions.IndexOf(sid) < 0) then
  begin
    FHttp.Respond(ARequest, 404, 'text/plain', 'Unknown session');
    Exit;
  end;
  try
    data := GetJSON(ARequest.Body);
  except
    on E: Exception do
    begin
      SendError(ARequest, 'null', JSONRPC_PARSE_ERROR, 'Parse error: ' + E.Message);
      Exit;
    end;
  end;
  try
    if data.JSONType <> jtObject then
      SendError(ARequest, 'null', JSONRPC_INVALID_REQUEST,
        'Expected one JSON-RPC message (batches are not supported)')
    else
      HandleMessage(ARequest, TJSONObject(data));
  finally
    data.Free;
  end;
end;

procedure TAIMcpServer.HandleMessage(ARequest: TAIHttpRequest; AMessage: TJSONObject);
var
  method, idJSON: string;
  idData: TJSONData;
  params: TJSONObject;
begin
  method := JStr(AMessage, 'method');
  idData := AMessage.Find('id');
  params := JObj(AMessage, 'params');
  if not Assigned(idData) then
  begin
    // notifications (initialized, cancelled) and responses: accepted
    FHttp.Respond(ARequest, 202, '', '');
    Exit;
  end;
  idJSON := idData.AsJSON;
  if method = '' then
  begin
    // a response to a server request; this server sends none
    FHttp.Respond(ARequest, 202, '', '');
    Exit;
  end;
  case method of
    'initialize':
      HandleInitialize(ARequest, idJSON, params);
    'ping':
      SendResult(ARequest, idJSON, TJSONObject.Create);
    'tools/list':
      HandleToolsList(ARequest, idJSON);
    'tools/call':
      HandleToolsCall(ARequest, idJSON, params);
    else
      SendError(ARequest, idJSON, JSONRPC_METHOD_NOT_FOUND, 'Method not found: ' + method);
  end;
end;

procedure TAIMcpServer.HandleInitialize(ARequest: TAIHttpRequest;
  const AIdJSON: string; AParams: TJSONObject);
var
  requested, version, sid, client: string;
  i: integer;
  result, caps, tools, info: TJSONObject;
  headers: TStringList;
begin
  requested := JStr(AParams, 'protocolVersion');
  version := AI_MCP_LATEST_PROTOCOL;
  for i := 0 to High(KNOWN_PROTOCOLS) do
    if requested = KNOWN_PROTOCOLS[i] then
      version := requested;
  client := JStr(JObj(AParams, 'clientInfo'), 'name', '?') + ' ' +
    JStr(JObj(AParams, 'clientInfo'), 'version');
  sid := AIMcpNewToken;
  FSessions.Add(sid);
  Log(Format('MCP client %s connected (protocol %s)', [Trim(client), version]));

  tools := TJSONObject.Create;
  tools.Add('listChanged', False);
  caps := TJSONObject.Create;
  caps.Add('tools', tools);
  info := TJSONObject.Create;
  info.Add('name', 'fastplaz');
  info.Add('title', 'Lazarus IDE (FastPlaz Agent Plane)');
  info.Add('version', '0.1');
  result := TJSONObject.Create;
  result.Add('protocolVersion', version);
  result.Add('capabilities', caps);
  result.Add('serverInfo', info);
  if FInstructions <> '' then
    result.Add('instructions', FInstructions);
  headers := TStringList.Create;
  try
    headers.Add('Mcp-Session-Id: ' + sid);
    SendResult(ARequest, AIdJSON, result, headers);
  finally
    headers.Free;
  end;
end;

procedure TAIMcpServer.HandleToolsList(ARequest: TAIHttpRequest; const AIdJSON: string);
var
  list: TJSONArray;
  item: TJSONObject;
  schema: TJSONData;
  i: integer;
  result: TJSONObject;
begin
  list := TJSONArray.Create;
  for i := 0 to High(FTools) do
  begin
    item := TJSONObject.Create;
    item.Add('name', FTools[i].Name);
    if FTools[i].Title <> '' then
      item.Add('title', FTools[i].Title);
    item.Add('description', FTools[i].Description);
    schema := nil;
    try
      schema := GetJSON(FTools[i].InputSchema);
    except
      schema := nil;
    end;
    if not Assigned(schema) or (schema.JSONType <> jtObject) then
    begin
      schema.Free;
      schema := TJSONObject.Create(['type', 'object']);
    end;
    item.Add('inputSchema', schema);
    list.Add(item);
  end;
  result := TJSONObject.Create;
  result.Add('tools', list);
  SendResult(ARequest, AIdJSON, result);
end;

procedure TAIMcpServer.HandleToolsCall(ARequest: TAIHttpRequest;
  const AIdJSON: string; AParams: TJSONObject);
var
  toolName: string;
  tool: TAIMcpTool;
  call: TAIMcpToolCall;
  args: TJSONObject;
begin
  toolName := JStr(AParams, 'name');
  if not FindTool(toolName, tool) then
  begin
    SendError(ARequest, AIdJSON, JSONRPC_INVALID_PARAMS, 'Unknown tool: ' + toolName);
    Exit;
  end;
  args := JObj(AParams, 'arguments');
  call := TAIMcpToolCall.Create;
  call.FServer := Self;
  call.FRequest := ARequest;
  call.FIdJSON := AIdJSON;
  call.FName := toolName;
  if Assigned(args) then
    call.FArgs := TJSONObject(args.Clone)
  else
    call.FArgs := TJSONObject.Create;
  FCalls.Add(call);
  Log('tool ' + toolName + ' ' + call.FArgs.AsJSON);

  if (tool.Permission = mtpAuto) or
    ((tool.Permission = mtpAskSession) and (FAllowed.IndexOf(toolName) >= 0)) then
    RunCall(call)
  else
    AskPermission(call, tool);
end;

procedure TAIMcpServer.AskPermission(ACall: TAIMcpToolCall; const ATool: TAIMcpTool);
var
  req: TAIPermissionRequest;
  n: integer;
  summary, args: string;
begin
  if not Assigned(FOnPermission) then
  begin
    ACall.Fail('Not allowed: the IDE has no way to ask the user for permission.');
    Exit;
  end;
  Inc(FNextPermission);
  ACall.FPermissionKey := 'mcp:' + IntToStr(FNextPermission);
  req := Default(TAIPermissionRequest);
  req.Key := ACall.FPermissionKey;
  req.ToolTitle := 'FastPlaz: ' + ATool.Name;
  req.ToolKind := 'mcp';
  summary := '';
  if Assigned(ATool.Describe) then
    try
      summary := Trim(ATool.Describe(ACall));
    except
      on E: Exception do
        Log('describe ' + ATool.Name + ': ' + E.Message);
    end;
  args := AIMcpArgsText(ACall.FArgs);
  req.Detail := ATool.Title;
  if summary <> '' then
    req.Detail := req.Detail + LineEnding + summary;
  if args <> '' then
    req.Detail := req.Detail + LineEnding + LineEnding + args;
  n := 2;
  if ATool.Permission = mtpAskSession then
    n := 3;
  SetLength(req.Options, n);
  req.Options[0].Id := AI_MCP_ALLOW_ONCE;
  req.Options[0].Name := 'Allow';
  req.Options[0].Kind := 'allow_once';
  if n = 3 then
  begin
    req.Options[1].Id := AI_MCP_ALLOW_SESSION;
    req.Options[1].Name := 'Allow for this session';
    req.Options[1].Kind := 'allow_always';
  end;
  req.Options[n - 1].Id := AI_MCP_REJECT;
  req.Options[n - 1].Name := 'Deny';
  req.Options[n - 1].Kind := 'reject_once';
  FOnPermission(Self, req);
end;

procedure TAIMcpServer.RespondPermission(const AKey, AOptionId: string);
var
  i: integer;
  call: TAIMcpToolCall;
begin
  for i := 0 to FCalls.Count - 1 do
  begin
    call := TAIMcpToolCall(FCalls[i]);
    if call.FPermissionKey <> AKey then
      Continue;
    call.FPermissionKey := '';
    if AOptionId = AI_MCP_ALLOW_SESSION then
      FAllowed.Add(call.FName);
    if (AOptionId = AI_MCP_ALLOW_ONCE) or (AOptionId = AI_MCP_ALLOW_SESSION) then
      RunCall(call)
    else
      call.Fail('The user denied this tool call.');
    Exit;
  end;
end;

procedure TAIMcpServer.RunCall(ACall: TAIMcpToolCall);
var
  tool: TAIMcpTool;
begin
  if not FindTool(ACall.FName, tool) or not Assigned(tool.Handler) then
  begin
    ACall.Fail('Tool not available: ' + ACall.FName);
    Exit;
  end;
  try
    tool.Handler(ACall);
  except
    on E: Exception do
      ACall.Fail(E.ClassName + ': ' + E.Message);
  end;
end;

procedure TAIMcpServer.CallFinished(ACall: TAIMcpToolCall; AResult: TJSONObject);
begin
  FCalls.Remove(ACall);
  if ACall.FRequest.Connected then
    SendResult(ACall.FRequest, ACall.FIdJSON, AResult)
  else
  begin
    // the agent went away (cancelled); free the orphaned request
    AResult.Free;
    FHttp.Respond(ACall.FRequest, 200, '', '');
  end;
  ACall.Free;
end;

procedure TAIMcpServer.SendResult(ARequest: TAIHttpRequest; const AIdJSON: string;
  AResult: TJSONData; AHeaders: TStrings);
var
  msg: TJSONObject;
  s: string;
begin
  msg := TJSONObject.Create;
  try
    msg.Add('jsonrpc', '2.0');
    msg.Add('id', GetJSON(AIdJSON));
    msg.Add('result', AResult);
    s := msg.AsJSON;
  finally
    msg.Free;
  end;
  FHttp.Respond(ARequest, 200, 'application/json', s, AHeaders);
end;

procedure TAIMcpServer.SendError(ARequest: TAIHttpRequest; const AIdJSON: string;
  ACode: integer; const AMessage: string);
var
  msg, err: TJSONObject;
  s: string;
begin
  err := TJSONObject.Create;
  err.Add('code', ACode);
  err.Add('message', AMessage);
  msg := TJSONObject.Create;
  try
    msg.Add('jsonrpc', '2.0');
    msg.Add('id', GetJSON(AIdJSON));
    msg.Add('error', err);
    s := msg.AsJSON;
  finally
    msg.Free;
  end;
  FHttp.Respond(ARequest, 200, 'application/json', s);
end;

end.

unit test_aiplane_mcp;

{
  Tests for the AI Plane MCP server (aiplane_httpserver, aiplane_mcpserver):
  HTTP handling, token and Origin checks, sessions, tools/list, tools/call,
  permissions answered at once or later, port fallback.

  The server is polled by a background thread (in the IDE a timer does it);
  the test talks to it with a blocking HTTP client or a raw socket.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser, fphttpclient,
  ssockets, aiplane_types, aiplane_mcpserver;

type

  { TMcpTestTools }

  TMcpTestTools = class
  public
    Permissions: integer;
    LastKey: string;
    { '', 'allow_once', 'allow_always', 'reject_once', or 'later' }
    Answer: string;
    procedure Echo(ACall: TAIMcpToolCall);
    procedure Big(ACall: TAIMcpToolCall);
    procedure Boom(ACall: TAIMcpToolCall);
    procedure Risky(ACall: TAIMcpToolCall);
    procedure OnPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
  end;

  { TMcpPollThread }

  TMcpPollThread = class(TThread)
  public
    Server: TAIMcpServer;
    LaterKey: string;
    LaterAt: QWord;
    procedure Execute; override;
  end;

  { TAIMcpServerTest }

  TAIMcpServerTest = class(TTestCase)
  private
    FServer: TAIMcpServer;
    FTools: TMcpTestTools;
    FThread: TMcpPollThread;
    FSession: string;
    function Post(const ABody: string; AToken: boolean = True;
      const AOrigin: string = ''): string;
    function PostStatus(const ABody: string; AToken: boolean = True;
      const AOrigin: string = ''): integer;
    function Rpc(const AMethod: string; AParams: TJSONObject = nil): TJSONObject;
    function CallTool(const AName: string; AArgs: TJSONObject = nil): TJSONObject;
    function ResultText(AResponse: TJSONObject): string;
    function RawRequest(const ARequest: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestServerListensOnLoopback;
    procedure TestPortFallback;
    procedure TestTokenRequired;
    procedure TestOriginChecked;
    procedure TestInitializeGivesSession;
    procedure TestUnknownSessionIs404;
    procedure TestToolsList;
    procedure TestCallTool;
    procedure TestUnknownTool;
    procedure TestToolException;
    procedure TestResultTruncated;
    procedure TestPermissionAllowOnce;
    procedure TestPermissionAllowForSession;
    procedure TestPermissionDenied;
    procedure TestPermissionAnsweredLater;
    procedure TestNotificationAccepted;
    procedure TestGetNotAllowed;
    procedure TestParseError;
    procedure TestChunkedAndPipelined;
    procedure TestNewTokenPerStart;
  end;

implementation

const
  // away from the IDE default (17845) so tests run next to a live IDE
  TEST_PORT = 47845;

{ TMcpTestTools }

procedure TMcpTestTools.Echo(ACall: TAIMcpToolCall);
begin
  ACall.Finish('echo: ' + ACall.Str('text'));
end;

procedure TMcpTestTools.Big(ACall: TAIMcpToolCall);
begin
  ACall.Finish(StringOfChar('x', 40000));
end;

procedure TMcpTestTools.Boom(ACall: TAIMcpToolCall);
begin
  raise Exception.Create('tool exploded');
end;

procedure TMcpTestTools.Risky(ACall: TAIMcpToolCall);
begin
  ACall.Finish('risky done');
end;

procedure TMcpTestTools.OnPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
begin
  Inc(Permissions);
  LastKey := ARequest.Key;
  if (Answer <> '') and (Answer <> 'later') then
    TAIMcpServer(Sender).RespondPermission(ARequest.Key, Answer);
end;

{ TMcpPollThread }

procedure TMcpPollThread.Execute;
begin
  while not Terminated do
  begin
    if (LaterKey <> '') and (GetTickCount64 >= LaterAt) then
    begin
      Server.RespondPermission(LaterKey, AI_MCP_ALLOW_ONCE);
      LaterKey := '';
    end;
    if not Server.Poll then
      Sleep(2);
  end;
end;

{ TAIMcpServerTest }

procedure TAIMcpServerTest.SetUp;
begin
  FTools := TMcpTestTools.Create;
  FServer := TAIMcpServer.Create;
  FServer.OnPermission := @FTools.OnPermission;
  FServer.RegisterTool('echo', 'Echo', 'Returns the text',
    '{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]}',
    mtpAuto, @FTools.Echo);
  FServer.RegisterTool('big', 'Big', 'Returns 40000 bytes', '', mtpAuto, @FTools.Big);
  FServer.RegisterTool('boom', 'Boom', 'Raises', '', mtpAuto, @FTools.Boom);
  FServer.RegisterTool('risky', 'Risky', 'Asks first', '', mtpAskSession, @FTools.Risky);
  FServer.RegisterTool('always', 'Always ask', 'Asks every time', '', mtpAsk,
    @FTools.Risky);
  AssertTrue('server started: ' + FServer.LastError, FServer.Start(TEST_PORT));
  FThread := TMcpPollThread.Create(True);
  FThread.Server := FServer;
  FThread.Start;
  FSession := '';
end;

procedure TAIMcpServerTest.TearDown;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  FreeAndNil(FServer);
  FreeAndNil(FTools);
end;

function TAIMcpServerTest.PostStatus(const ABody: string; AToken: boolean;
  const AOrigin: string): integer;
var
  client: TFPHTTPClient;
  body, response: TStringStream;
begin
  client := TFPHTTPClient.Create(nil);
  body := TStringStream.Create(ABody);
  response := TStringStream.Create('');
  try
    client.IOTimeout := 10000;
    client.AddHeader('Content-Type', 'application/json');
    client.AddHeader('Accept', 'application/json, text/event-stream');
    if AToken then
      client.AddHeader('Authorization', 'Bearer ' + FServer.Token);
    if AOrigin <> '' then
      client.AddHeader('Origin', AOrigin);
    if FSession <> '' then
      client.AddHeader('Mcp-Session-Id', FSession);
    client.RequestBody := body;
    client.HTTPMethod('POST', FServer.Url, response, []);
    Result := client.ResponseStatusCode;
  finally
    response.Free;
    body.Free;
    client.Free;
  end;
end;

function TAIMcpServerTest.Post(const ABody: string; AToken: boolean;
  const AOrigin: string): string;
var
  client: TFPHTTPClient;
  body: TStringStream;
  sid: string;
begin
  client := TFPHTTPClient.Create(nil);
  body := TStringStream.Create(ABody);
  try
    client.IOTimeout := 10000;
    client.AddHeader('Content-Type', 'application/json');
    if AToken then
      client.AddHeader('Authorization', 'Bearer ' + FServer.Token);
    if AOrigin <> '' then
      client.AddHeader('Origin', AOrigin);
    if FSession <> '' then
      client.AddHeader('Mcp-Session-Id', FSession);
    client.RequestBody := body;
    Result := client.Post(FServer.Url);
    sid := client.GetHeader(client.ResponseHeaders, 'Mcp-Session-Id');
    if sid <> '' then
      FSession := sid;
  finally
    body.Free;
    client.Free;
  end;
end;

function TAIMcpServerTest.Rpc(const AMethod: string; AParams: TJSONObject): TJSONObject;
var
  msg: TJSONObject;
  s: string;
begin
  msg := TJSONObject.Create(['jsonrpc', '2.0', 'id', 1, 'method', AMethod]);
  try
    if Assigned(AParams) then
      msg.Add('params', AParams);
    s := Post(msg.AsJSON);
  finally
    msg.Free;
  end;
  Result := GetJSON(s) as TJSONObject;
end;

function TAIMcpServerTest.CallTool(const AName: string; AArgs: TJSONObject): TJSONObject;
var
  params: TJSONObject;
begin
  params := TJSONObject.Create(['name', AName]);
  if Assigned(AArgs) then
    params.Add('arguments', AArgs);
  Result := Rpc('tools/call', params);
end;

function TAIMcpServerTest.ResultText(AResponse: TJSONObject): string;
var
  content: TJSONArray;
begin
  Result := '';
  content := AResponse.Objects['result'].Arrays['content'];
  if content.Count > 0 then
    Result := content.Objects[0].Get('text', '');
end;

function TAIMcpServerTest.RawRequest(const ARequest: string): string;
var
  sock: TInetSocket;
  buf: array[0..4095] of char;
  n: integer;
  started: QWord;
  part: string;
begin
  Result := '';
  sock := TInetSocket.Create('127.0.0.1', FServer.Port);
  try
    sock.IOTimeout := 500;
    sock.WriteBuffer(ARequest[1], Length(ARequest));
    started := GetTickCount64;
    repeat
      n := sock.Read(buf, SizeOf(buf));
      if n > 0 then
      begin
        SetString(part, PChar(@buf[0]), n);
        Result := Result + part;
      end;
    until (GetTickCount64 - started > 3000) or (Pos('echo: pipelined', Result) > 0);
  finally
    sock.Free;
  end;
end;

procedure TAIMcpServerTest.TestServerListensOnLoopback;
begin
  AssertEquals(TEST_PORT, FServer.Port);
  AssertEquals('http://127.0.0.1:' + IntToStr(TEST_PORT) + '/mcp', FServer.Url);
  AssertEquals(64, Length(FServer.Token));
end;

procedure TAIMcpServerTest.TestPortFallback;
var
  second: TAIMcpServer;
begin
  second := TAIMcpServer.Create;
  try
    AssertTrue(second.Start(TEST_PORT));
    AssertTrue('moved to a free port', second.Port <> TEST_PORT);
    AssertTrue(second.Port > TEST_PORT);
    AssertTrue(second.Port < TEST_PORT + AI_MCP_PORT_TRIES);
  finally
    second.Free;
  end;
end;

procedure TAIMcpServerTest.TestTokenRequired;
begin
  AssertEquals(401, PostStatus('{"jsonrpc":"2.0","id":1,"method":"ping"}', False));
  AssertEquals(200, PostStatus('{"jsonrpc":"2.0","id":1,"method":"ping"}', True));
end;

procedure TAIMcpServerTest.TestOriginChecked;
const
  PING = '{"jsonrpc":"2.0","id":1,"method":"ping"}';
begin
  AssertEquals(403, PostStatus(PING, True, 'http://evil.example'));
  AssertEquals(403, PostStatus(PING, True, 'http://127.0.0.1.evil.example'));
  AssertEquals(200, PostStatus(PING, True, 'http://localhost:3000'));
  AssertEquals(200, PostStatus(PING, True, 'http://127.0.0.1:17845'));
end;

procedure TAIMcpServerTest.TestInitializeGivesSession;
var
  res: TJSONObject;
begin
  res := Rpc('initialize', TJSONObject.Create(['protocolVersion', '2025-03-26',
    'capabilities', TJSONObject.Create,
    'clientInfo', TJSONObject.Create(['name', 'test', 'version', '1'])]));
  try
    AssertEquals('2025-03-26', res.Objects['result'].Get('protocolVersion', ''));
    AssertEquals('fastplaz-lazarus',
      res.Objects['result'].Objects['serverInfo'].Get('name', ''));
    AssertTrue(Assigned(res.Objects['result'].Objects['capabilities'].Find('tools')));
    AssertTrue('session id header', FSession <> '');
  finally
    res.Free;
  end;
  res := Rpc('initialize', TJSONObject.Create(['protocolVersion', '1999-01-01']));
  try
    AssertEquals('unknown version -> latest', AI_MCP_LATEST_PROTOCOL,
      res.Objects['result'].Get('protocolVersion', ''));
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestUnknownSessionIs404;
begin
  FSession := 'not-a-session';
  AssertEquals(404, PostStatus('{"jsonrpc":"2.0","id":1,"method":"ping"}'));
end;

procedure TAIMcpServerTest.TestToolsList;
var
  res: TJSONObject;
  tools: TJSONArray;
begin
  res := Rpc('tools/list');
  try
    tools := res.Objects['result'].Arrays['tools'];
    AssertEquals(5, tools.Count);
    AssertEquals('echo', tools.Objects[0].Get('name', ''));
    AssertEquals('Returns the text', tools.Objects[0].Get('description', ''));
    AssertEquals('object', tools.Objects[0].Objects['inputSchema'].Get('type', ''));
    AssertEquals('empty schema -> object', 'object',
      tools.Objects[1].Objects['inputSchema'].Get('type', ''));
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestCallTool;
var
  res: TJSONObject;
begin
  res := CallTool('echo', TJSONObject.Create(['text', 'halo ' + #$E2#$9C#$93]));
  try
    AssertEquals('echo: halo ' + #$E2#$9C#$93, ResultText(res));
    AssertFalse(res.Objects['result'].Get('isError', True));
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestUnknownTool;
var
  res: TJSONObject;
begin
  res := CallTool('nope');
  try
    AssertEquals(-32602, res.Objects['error'].Get('code', 0));
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestToolException;
var
  res: TJSONObject;
begin
  res := CallTool('boom');
  try
    AssertTrue(res.Objects['result'].Get('isError', False));
    AssertTrue(Pos('tool exploded', ResultText(res)) > 0);
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestResultTruncated;
var
  res: TJSONObject;
  text: string;
begin
  res := CallTool('big');
  try
    text := ResultText(res);
    AssertTrue(Length(text) < 40000);
    AssertTrue(Length(text) >= AI_MCP_MAX_RESULT);
    AssertTrue(Pos('truncated', text) > 0);
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestPermissionAllowOnce;
var
  res: TJSONObject;
begin
  FTools.Answer := AI_MCP_ALLOW_ONCE;
  res := CallTool('always');
  try
    AssertEquals('risky done', ResultText(res));
  finally
    res.Free;
  end;
  res := CallTool('always');
  res.Free;
  AssertEquals('asked every time', 2, FTools.Permissions);
end;

procedure TAIMcpServerTest.TestPermissionAllowForSession;
var
  res: TJSONObject;
begin
  FTools.Answer := AI_MCP_ALLOW_SESSION;
  res := CallTool('risky');
  try
    AssertEquals('risky done', ResultText(res));
  finally
    res.Free;
  end;
  res := CallTool('risky');
  try
    AssertEquals('risky done', ResultText(res));
  finally
    res.Free;
  end;
  AssertEquals('asked once', 1, FTools.Permissions);
  FServer.ResetAllowed;
  CallTool('risky').Free;
  AssertEquals('asked again after reset', 2, FTools.Permissions);
end;

procedure TAIMcpServerTest.TestPermissionDenied;
var
  res: TJSONObject;
begin
  FTools.Answer := AI_MCP_REJECT;
  res := CallTool('risky');
  try
    AssertTrue(res.Objects['result'].Get('isError', False));
    AssertTrue(Pos('denied', ResultText(res)) > 0);
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestPermissionAnsweredLater;
var
  res: TJSONObject;
  started: QWord;
begin
  FTools.Answer := 'later';
  // the poll thread answers the first permission key it is given
  FThread.LaterKey := 'mcp:1';
  FThread.LaterAt := GetTickCount64 + 300;
  started := GetTickCount64;
  res := CallTool('risky');
  try
    AssertEquals('risky done', ResultText(res));
    AssertTrue('waited for the answer', GetTickCount64 - started >= 250);
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestNotificationAccepted;
begin
  AssertEquals(202, PostStatus('{"jsonrpc":"2.0","method":"notifications/initialized"}'));
end;

procedure TAIMcpServerTest.TestGetNotAllowed;
var
  client: TFPHTTPClient;
  response: TStringStream;
begin
  client := TFPHTTPClient.Create(nil);
  response := TStringStream.Create('');
  try
    client.AddHeader('Authorization', 'Bearer ' + FServer.Token);
    client.HTTPMethod('GET', FServer.Url, response, []);
    AssertEquals(405, client.ResponseStatusCode);
  finally
    response.Free;
    client.Free;
  end;
end;

procedure TAIMcpServerTest.TestParseError;
var
  res: TJSONObject;
begin
  res := GetJSON(Post('{not json')) as TJSONObject;
  try
    AssertEquals(-32700, res.Objects['error'].Get('code', 0));
  finally
    res.Free;
  end;
end;

procedure TAIMcpServerTest.TestChunkedAndPipelined;
var
  body1, body2, req, reply: string;
begin
  body1 := '{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"echo","arguments":{"text":"chunked"}}}';
  body2 := '{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"echo","arguments":{"text":"pipelined"}}}';
  // first request with a chunked body (split in two chunks), second right
  // behind it on the same connection with Content-Length
  req := 'POST /mcp HTTP/1.1'#13#10 +
    'Host: 127.0.0.1'#13#10 +
    'Authorization: Bearer ' + FServer.Token + #13#10 +
    'Content-Type: application/json'#13#10 +
    'Transfer-Encoding: chunked'#13#10#13#10 +
    IntToHex(10, 1) + #13#10 + Copy(body1, 1, 10) + #13#10 +
    IntToHex(Length(body1) - 10, 1) + #13#10 + Copy(body1, 11, MaxInt) + #13#10 +
    '0'#13#10#13#10 +
    'POST /mcp HTTP/1.1'#13#10 +
    'Host: 127.0.0.1'#13#10 +
    'Authorization: Bearer ' + FServer.Token + #13#10 +
    'Content-Type: application/json'#13#10 +
    'Content-Length: ' + IntToStr(Length(body2)) + #13#10#13#10 + body2;
  reply := RawRequest(req);
  AssertTrue(reply, Pos('echo: chunked', reply) > 0);
  AssertTrue(reply, Pos('echo: pipelined', reply) > 0);
  AssertTrue('order kept', Pos('echo: chunked', reply) < Pos('echo: pipelined', reply));
end;

procedure TAIMcpServerTest.TestNewTokenPerStart;
var
  first: string;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
  first := FServer.Token;
  AssertTrue(FServer.Start(TEST_PORT));
  AssertTrue(first <> FServer.Token);
  FServer.Stop;
  AssertFalse(FServer.Active);
  AssertEquals('', FServer.Url);
end;

initialization
  RegisterTest(TAIMcpServerTest);

end.

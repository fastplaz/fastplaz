unit test_mcp_server;

{
  mcp_server: the MCP (Model Context Protocol) core for applications.
  JSON-RPC handling (initialize, ping, tools, resources, errors), the HTTP
  rules of the stateless Streamable HTTP endpoint (method, Origin, bearer
  token, protocol version header) and concurrent calls on one server.
}

{$mode objfpc}{$H+}

interface

uses
  mcp_server, fpjson, jsonparser, Classes, SysUtils, fpcunit, testregistry;

type

  { TMcpServerTest }

  TMcpServerTest = class(TTestCase)
  private
    FServer: TMcpServer;
    FMethodCalls: integer;
    procedure MethodTool(ACall: TMcpToolCall);
    function MethodResource(const AUri: string; out AText: string): boolean;
    // sends ABody, expects a request answer, returns the parsed message
    function Send(const ABody: string): TJSONObject;
    function Request(const AMethod: string; const AParams: string = '';
      const AId: string = '1'): TJSONObject;
    function CallTool(const AName, AArgs: string): TJSONObject;
    function Post(const ABody: string; const AOrigin: string = '';
      const AHost: string = ''): TMcpHttpResponse;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValidToolName;
    procedure TestHostOf;
    procedure TestGlobalServer;
    procedure TestInitialize;
    procedure TestInitializeUnknownVersion;
    procedure TestInitializeResourcesCapability;
    procedure TestPing;
    procedure TestToolsList;
    procedure TestToolsListAnnotations;
    procedure TestToolCallText;
    procedure TestToolArguments;
    procedure TestToolCallJSON;
    procedure TestToolMethodHandler;
    procedure TestToolError;
    procedure TestToolExceptionHidden;
    procedure TestToolExceptionDetails;
    procedure TestToolCallErrors;
    procedure TestRegisterErrors;
    procedure TestResources;
    procedure TestResourceErrors;
    procedure TestEmptyLists;
    procedure TestNotifications;
    procedure TestInvalidMessages;
    procedure TestMethodNotFound;
    procedure TestIdKept;
    procedure TestHttpMethods;
    procedure TestHttpOrigin;
    procedure TestHttpToken;
    procedure TestHttpProtocolVersion;
    procedure TestHttpAnswers;
    procedure TestUTF8;
    procedure TestConcurrentCalls;
  end;

implementation

type
  EBoom = class(Exception);

  { TCallThread }

  TCallThread = class(TThread)
  private
    FServer: TMcpServer;
    FIndex: integer;
  public
    Failures: integer;
    constructor Create(AServer: TMcpServer; AIndex: integer);
    procedure Execute; override;
  end;

procedure ToolEcho(ACall: TMcpToolCall);
begin
  ACall.Finish('echo: ' + ACall.Str('text'));
end;

procedure ToolArgs(ACall: TMcpToolCall);
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  ACall.Finish(Format('%s|%d|%s|%s|%s', [ACall.Str('s', 'none'),
    ACall.Int('i', -1), FloatToStr(ACall.Float('f', -1), fs),
    BoolToStr(ACall.Bool('b'), 'yes', 'no'),
    BoolToStr(ACall.Has('n'), 'has', 'missing')]));
end;

procedure ToolJSON(ACall: TMcpToolCall);
begin
  ACall.FinishJSON(TJSONObject.Create(['city', ACall.Str('city'), 'temp', 30]));
end;

procedure ToolArray(ACall: TMcpToolCall);
begin
  ACall.FinishJSON(TJSONArray.Create([1, 2]));
end;

procedure ToolFail(ACall: TMcpToolCall);
begin
  raise EMcpToolError.Create('city not found');
end;

procedure ToolBoom(ACall: TMcpToolCall);
begin
  raise EBoom.Create('secret database detail');
end;

procedure ToolSum(ACall: TMcpToolCall);
begin
  ACall.Finish(IntToStr(ACall.Int('a') + ACall.Int('b')));
end;

function ResourceReadme(const AUri: string; out AText: string): boolean;
begin
  AText := 'readme of ' + AUri;
  Result := True;
end;

function ResourceDenied(const AUri: string; out AText: string): boolean;
begin
  AText := 'not today';
  Result := False;
end;

function ResourceBoom(const AUri: string; out AText: string): boolean;
begin
  Result := False;
  raise EBoom.Create('secret path');
end;

const
  ECHO_SCHEMA = '{"type":"object","properties":{"text":{"type":"string"}}}';
  // e-acute (in the Windows ANSI code page) and a CJK character (not in it)
  UTF8_TEXT = #$C3#$A9' '#$E4#$B8#$AD;

{ FastPlaz strings hold UTF-8 bytes; fpjson uses UTF8String. Change the tag
  only, like mcp_server does, so the bytes can be compared. }
function J(const S: RawByteString): TJSONStringType;
var
  r: RawByteString;
begin
  r := S;
  UniqueString(r);
  SetCodePage(r, CP_UTF8, False);
  Result := r;
end;

function Raw(const S: TJSONStringType): string;
var
  r: RawByteString;
begin
  r := S;
  UniqueString(r);
  SetCodePage(r, DefaultSystemCodePage, False);
  Result := r;
end;

{ TCallThread }

constructor TCallThread.Create(AServer: TMcpServer; AIndex: integer);
begin
  FServer := AServer;
  FIndex := AIndex;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TCallThread.Execute;
var
  i, a: integer;
  response: string;
  data: TJSONData;
begin
  for i := 1 to 200 do
  begin
    a := FIndex * 1000 + i;
    if FServer.HandleMessage(Format(
      '{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"sum","arguments":{"a":%d,"b":1}}}',
      [a, a]), response) <> mmkRequest then
    begin
      Inc(Failures);
      Continue;
    end;
    data := GetJSON(response);
    try
      if (data.FindPath('id').AsInteger <> a) or
        (data.FindPath('result.content[0].text').AsString <> IntToStr(a + 1)) then
        Inc(Failures);
    finally
      data.Free;
    end;
  end;
end;

{ TMcpServerTest }

procedure TMcpServerTest.SetUp;
begin
  FServer := TMcpServer.Create;
  FServer.Name := 'test-server';
  FServer.Version := '2.0';
  FMethodCalls := 0;
end;

procedure TMcpServerTest.TearDown;
begin
  FServer.Free;
end;

procedure TMcpServerTest.MethodTool(ACall: TMcpToolCall);
begin
  Inc(FMethodCalls);
  ACall.Finish('method ' + ACall.Name);
end;

function TMcpServerTest.MethodResource(const AUri: string; out AText: string): boolean;
begin
  AText := 'method resource';
  Result := True;
end;

function TMcpServerTest.Send(const ABody: string): TJSONObject;
var
  response: string;
  data: TJSONData;
begin
  AssertTrue('request answered', FServer.HandleMessage(ABody, response) = mmkRequest);
  data := GetJSON(J(response));
  AssertTrue('answer is an object', data.JSONType = jtObject);
  Result := TJSONObject(data);
  AssertEquals('jsonrpc', '2.0', Result.Get('jsonrpc', ''));
end;

function TMcpServerTest.Request(const AMethod: string; const AParams: string;
  const AId: string): TJSONObject;
var
  body: string;
begin
  body := '{"jsonrpc":"2.0","id":' + AId + ',"method":"' + AMethod + '"';
  if AParams <> '' then
    body := body + ',"params":' + AParams;
  Result := Send(body + '}');
end;

function TMcpServerTest.CallTool(const AName, AArgs: string): TJSONObject;
var
  params: string;
begin
  params := '{"name":"' + AName + '"';
  if AArgs <> '' then
    params := params + ',"arguments":' + AArgs;
  Result := Request('tools/call', params + '}');
end;

function TMcpServerTest.Post(const ABody: string; const AOrigin: string;
  const AHost: string): TMcpHttpResponse;
var
  req: TMcpHttpRequest;
begin
  req := Default(TMcpHttpRequest);
  req.Method := 'POST';
  req.Body := ABody;
  req.Origin := AOrigin;
  req.Host := AHost;
  Result := FServer.HandleHttp(req);
end;

procedure TMcpServerTest.TestValidToolName;
begin
  AssertTrue(McpValidToolName('get_weather'));
  AssertTrue(McpValidToolName('Admin.Tools-v2'));
  AssertTrue(McpValidToolName(StringOfChar('a', 128)));
  AssertFalse('empty', McpValidToolName(''));
  AssertFalse('too long', McpValidToolName(StringOfChar('a', 129)));
  AssertFalse('space', McpValidToolName('get weather'));
  AssertFalse('slash', McpValidToolName('a/b'));
  AssertFalse('non ascii', McpValidToolName('cuaca' + #$C3#$A9));
end;

procedure TMcpServerTest.TestHostOf;
begin
  AssertEquals('example.com', McpHostOf('https://Example.com:8080/path'));
  AssertEquals('example.com', McpHostOf('example.com'));
  AssertEquals('localhost', McpHostOf('http://localhost:3000'));
  AssertEquals('[::1]', McpHostOf('http://[::1]:6274'));
  AssertEquals('[::1]', McpHostOf('[::1]'));
  AssertEquals('evil.com', McpHostOf('http://localhost@evil.com'));
  AssertEquals('null', McpHostOf('null'));
  AssertEquals('', McpHostOf(''));
  AssertEquals('broken bracket', '', McpHostOf('http://[::1'));
end;

procedure TMcpServerTest.TestGlobalServer;
begin
  AssertNotNull('global server', McpServer);
  AssertSame('same instance', McpServer, McpServer);
end;

procedure TMcpServerTest.TestInitialize;
var
  msg: TJSONObject;
begin
  FServer.Title := 'Test Server';
  FServer.Instructions := 'Use echo.';
  msg := Request('initialize',
    '{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"t","version":"1"}}');
  try
    AssertEquals('2025-03-26', msg.FindPath('result.protocolVersion').AsString);
    AssertEquals('test-server', msg.FindPath('result.serverInfo.name').AsString);
    AssertEquals('Test Server', msg.FindPath('result.serverInfo.title').AsString);
    AssertEquals('2.0', msg.FindPath('result.serverInfo.version').AsString);
    AssertEquals('Use echo.', msg.FindPath('result.instructions').AsString);
    AssertFalse(msg.FindPath('result.capabilities.tools.listChanged').AsBoolean);
    AssertNull('no resources capability', msg.FindPath('result.capabilities.resources'));
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestInitializeUnknownVersion;
var
  msg: TJSONObject;
begin
  msg := Request('initialize', '{"protocolVersion":"1999-01-01"}');
  try
    AssertEquals(MCP_LATEST_PROTOCOL, msg.FindPath('result.protocolVersion').AsString);
    AssertNull('no title', msg.FindPath('result.serverInfo.title'));
    AssertNull('no instructions', msg.FindPath('result.instructions'));
  finally
    msg.Free;
  end;
  // no params at all
  msg := Request('initialize');
  try
    AssertEquals(MCP_LATEST_PROTOCOL, msg.FindPath('result.protocolVersion').AsString);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestInitializeResourcesCapability;
var
  msg: TJSONObject;
begin
  FServer.RegisterResource('doc://readme', 'readme', '', '', 'text/plain', @ResourceReadme);
  msg := Request('initialize', '{"protocolVersion":"2025-06-18"}');
  try
    AssertNotNull(msg.FindPath('result.capabilities.resources'));
    AssertFalse(msg.FindPath('result.capabilities.resources.subscribe').AsBoolean);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestPing;
var
  msg: TJSONObject;
begin
  msg := Request('ping');
  try
    AssertEquals('{}', msg.FindPath('result').AsJSON);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolsList;
var
  msg: TJSONObject;
  tools: TJSONArray;
begin
  FServer.RegisterTool('echo', 'Echo', 'Echoes text', ECHO_SCHEMA, @ToolEcho);
  FServer.RegisterTool('weather', '', 'Weather', '', @ToolJSON, [],
    '{"type":"object","properties":{"temp":{"type":"number"}}}');
  AssertEquals(2, FServer.ToolCount);
  AssertTrue(FServer.HasTool('echo'));
  AssertFalse(FServer.HasTool('Echo'));
  msg := Request('tools/list');
  try
    tools := TJSONArray(msg.FindPath('result.tools'));
    AssertEquals(2, tools.Count);
    AssertEquals('echo', tools.Objects[0].Get('name', ''));
    AssertEquals('Echo', tools.Objects[0].Get('title', ''));
    AssertEquals('Echoes text', tools.Objects[0].Get('description', ''));
    AssertEquals('string',
      tools.Objects[0].FindPath('inputSchema.properties.text.type').AsString);
    AssertNull('no annotations without hints', tools.Objects[0].Find('annotations'));
    AssertNull('no output schema', tools.Objects[0].Find('outputSchema'));
    AssertNull('no empty title', tools.Objects[1].Find('title'));
    AssertEquals('empty schema becomes an object', 'object',
      tools.Objects[1].FindPath('inputSchema.type').AsString);
    AssertEquals('number',
      tools.Objects[1].FindPath('outputSchema.properties.temp.type').AsString);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolsListAnnotations;
var
  msg: TJSONObject;
  a: TJSONData;
begin
  FServer.RegisterTool('read', 'Read', 'Reads', '', @ToolEcho,
    [mthReadOnly, mthIdempotent]);
  FServer.RegisterTool('drop', '', 'Drops', '', @ToolEcho,
    [mthDestructive, mthOpenWorld]);
  msg := Request('tools/list');
  try
    a := msg.FindPath('result.tools[0].annotations');
    AssertNotNull(a);
    AssertEquals('Read', a.FindPath('title').AsString);
    AssertTrue(a.FindPath('readOnlyHint').AsBoolean);
    AssertFalse(a.FindPath('destructiveHint').AsBoolean);
    AssertTrue(a.FindPath('idempotentHint').AsBoolean);
    AssertFalse(a.FindPath('openWorldHint').AsBoolean);
    a := msg.FindPath('result.tools[1].annotations');
    AssertNull('no empty title', a.FindPath('title'));
    AssertFalse(a.FindPath('readOnlyHint').AsBoolean);
    AssertTrue(a.FindPath('destructiveHint').AsBoolean);
    AssertTrue(a.FindPath('openWorldHint').AsBoolean);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolCallText;
var
  msg: TJSONObject;
begin
  FServer.RegisterTool('echo', 'Echo', 'Echoes text', ECHO_SCHEMA, @ToolEcho);
  msg := CallTool('echo', '{"text":"halo \"dunia\" ' + UTF8_TEXT + '"}');
  try
    AssertEquals('text', msg.FindPath('result.content[0].type').AsString);
    AssertEquals('echo: halo "dunia" ' + UTF8_TEXT,
      Raw(msg.FindPath('result.content[0].text').AsString));
    AssertFalse(msg.FindPath('result.isError').AsBoolean);
    AssertNull(msg.FindPath('result.structuredContent'));
  finally
    msg.Free;
  end;
  // no arguments
  msg := CallTool('echo', '');
  try
    AssertEquals('echo: ', msg.FindPath('result.content[0].text').AsString);
  finally
    msg.Free;
  end;
  // null arguments
  msg := CallTool('echo', 'null');
  try
    AssertEquals('echo: ', msg.FindPath('result.content[0].text').AsString);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolArguments;
var
  msg: TJSONObject;

  function Run(const AArgs: string): string;
  begin
    msg := CallTool('args', AArgs);
    try
      Result := msg.FindPath('result.content[0].text').AsString;
    finally
      FreeAndNil(msg);
    end;
  end;

begin
  FServer.RegisterTool('args', '', 'Arguments', '', @ToolArgs);
  AssertEquals('defaults', 'none|-1|-1|no|missing', Run('{}'));
  AssertEquals('typed', 'x|42|1.5|yes|has',
    Run('{"s":"x","i":42,"f":1.5,"b":true,"n":0}'));
  AssertEquals('strings', '7|7|2.25|yes|missing',
    Run('{"s":7,"i":" 7 ","f":"2.25","b":"TRUE","n":null}'));
  AssertEquals('float to int, number to bool', 'none|3|3|yes|missing',
    Run('{"i":3.9,"f":3,"b":1}'));
  AssertEquals('bad values', 'none|-1|-1|no|missing',
    Run('{"i":"abc","f":"x","b":"no","s":[1]}'));
  AssertEquals('big int', 'none|9007199254740993|-1|no|missing',
    Run('{"i":9007199254740993}'));
end;

procedure TMcpServerTest.TestToolCallJSON;
var
  msg: TJSONObject;
  text: TJSONData;
begin
  FServer.RegisterTool('weather', '', 'Weather', '', @ToolJSON);
  FServer.RegisterTool('list', '', 'List', '', @ToolArray);
  msg := CallTool('weather', '{"city":"Jakarta"}');
  try
    AssertEquals('Jakarta', msg.FindPath('result.structuredContent.city').AsString);
    AssertEquals(30, msg.FindPath('result.structuredContent.temp').AsInteger);
    text := GetJSON(msg.FindPath('result.content[0].text').AsString);
    try
      AssertEquals('text has the same JSON', 'Jakarta', text.FindPath('city').AsString);
    finally
      text.Free;
    end;
  finally
    msg.Free;
  end;
  // an array is text only: structuredContent must be an object
  msg := CallTool('list', '');
  try
    AssertEquals('[1, 2]', msg.FindPath('result.content[0].text').AsString);
    AssertNull(msg.FindPath('result.structuredContent'));
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolMethodHandler;
var
  msg: TJSONObject;
begin
  FServer.RegisterTool('m', '', 'Method', '', @MethodTool);
  msg := CallTool('m', '{}');
  try
    AssertEquals('method m', msg.FindPath('result.content[0].text').AsString);
    AssertEquals(1, FMethodCalls);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolError;
var
  msg: TJSONObject;
begin
  FServer.RegisterTool('fail', '', 'Fails', '', @ToolFail);
  msg := CallTool('fail', '{}');
  try
    AssertNull('a tool error is a result, not a JSON-RPC error', msg.Find('error'));
    AssertTrue(msg.FindPath('result.isError').AsBoolean);
    AssertEquals('city not found', msg.FindPath('result.content[0].text').AsString);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolExceptionHidden;
var
  msg: TJSONObject;
begin
  FServer.RegisterTool('boom', '', 'Boom', '', @ToolBoom);
  msg := CallTool('boom', '{}');
  try
    AssertTrue(msg.FindPath('result.isError').AsBoolean);
    AssertEquals('Internal error in tool boom',
      msg.FindPath('result.content[0].text').AsString);
    AssertFalse('no exception text', Pos('secret', msg.AsJSON) > 0);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolExceptionDetails;
var
  msg: TJSONObject;
begin
  FServer.ExceptionDetails := True;
  FServer.RegisterTool('boom', '', 'Boom', '', @ToolBoom);
  msg := CallTool('boom', '{}');
  try
    AssertEquals('EBoom: secret database detail',
      msg.FindPath('result.content[0].text').AsString);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestToolCallErrors;

  procedure Check(const AParams: string; ACode: integer; const AText: string);
  var
    msg: TJSONObject;
  begin
    msg := Request('tools/call', AParams);
    try
      AssertNull(AParams + ': no result', msg.Find('result'));
      AssertEquals(AParams + ': code', ACode, msg.FindPath('error.code').AsInteger);
      AssertTrue(AParams + ': message ' + msg.FindPath('error.message').AsString,
        Pos(AText, msg.FindPath('error.message').AsString) > 0);
    finally
      msg.Free;
    end;
  end;

begin
  FServer.RegisterTool('echo', '', 'Echo', '', @ToolEcho);
  Check('{"name":"nope"}', JSONRPC_INVALID_PARAMS, 'Unknown tool: nope');
  Check('{}', JSONRPC_INVALID_PARAMS, 'Missing tool name');
  Check('{"name":"echo","arguments":[1]}', JSONRPC_INVALID_PARAMS, 'must be an object');
  Check('{"name":"echo","arguments":"x"}', JSONRPC_INVALID_PARAMS, 'must be an object');
  Check('[1]', JSONRPC_INVALID_PARAMS, 'Params must be an object');
  Check('', JSONRPC_INVALID_PARAMS, 'Missing params');
end;

procedure TMcpServerTest.TestRegisterErrors;

  procedure Expect(const AWhat: string; AOk: boolean);
  begin
    AssertTrue(AWhat + ' raises EMcpServer', AOk);
  end;

var
  raised: boolean;
begin
  FServer.RegisterTool('echo', '', 'Echo', '', @ToolEcho);

  raised := False;
  try
    FServer.RegisterTool('echo', '', 'Again', '', @ToolEcho);
  except
    on EMcpServer do raised := True;
  end;
  Expect('duplicate name', raised);

  raised := False;
  try
    FServer.RegisterTool('bad name', '', '', '', @ToolEcho);
  except
    on EMcpServer do raised := True;
  end;
  Expect('invalid name', raised);

  raised := False;
  try
    FServer.RegisterTool('schema', '', '', '[1,2]', @ToolEcho);
  except
    on EMcpServer do raised := True;
  end;
  Expect('schema not an object', raised);

  raised := False;
  try
    FServer.RegisterTool('schema2', '', '', '{broken', @ToolEcho);
  except
    on EMcpServer do raised := True;
  end;
  Expect('broken schema', raised);

  raised := False;
  try
    FServer.RegisterTool('out', '', '', '', @ToolEcho, [], '"text"');
  except
    on EMcpServer do raised := True;
  end;
  Expect('output schema not an object', raised);

  raised := False;
  try
    FServer.RegisterTool('nil', '', '', '', TMcpToolProc(nil));
  except
    on EMcpServer do raised := True;
  end;
  Expect('no handler', raised);

  FServer.RegisterResource('doc://a', '', '', '', '', @ResourceReadme);
  raised := False;
  try
    FServer.RegisterResource('doc://a', '', '', '', '', @ResourceReadme);
  except
    on EMcpServer do raised := True;
  end;
  Expect('duplicate resource', raised);

  raised := False;
  try
    FServer.RegisterResource(' ', '', '', '', '', @ResourceReadme);
  except
    on EMcpServer do raised := True;
  end;
  Expect('resource without uri', raised);

  AssertEquals('only valid registrations kept', 1, FServer.ToolCount);
  AssertEquals(1, FServer.ResourceCount);
  FServer.Clear;
  AssertEquals(0, FServer.ToolCount);
  AssertEquals(0, FServer.ResourceCount);
end;

procedure TMcpServerTest.TestResources;
var
  msg: TJSONObject;
  list: TJSONArray;
begin
  FServer.RegisterResource('doc://readme', 'readme', 'Read me', 'The readme',
    'text/markdown', @ResourceReadme);
  FServer.RegisterResource('doc://method', '', '', '', '', @MethodResource);
  msg := Request('resources/list');
  try
    list := TJSONArray(msg.FindPath('result.resources'));
    AssertEquals(2, list.Count);
    AssertEquals('doc://readme', list.Objects[0].Get('uri', ''));
    AssertEquals('readme', list.Objects[0].Get('name', ''));
    AssertEquals('Read me', list.Objects[0].Get('title', ''));
    AssertEquals('The readme', list.Objects[0].Get('description', ''));
    AssertEquals('text/markdown', list.Objects[0].Get('mimeType', ''));
    AssertEquals('name falls back to the uri', 'doc://method',
      list.Objects[1].Get('name', ''));
    AssertNull(list.Objects[1].Find('mimeType'));
  finally
    msg.Free;
  end;
  msg := Request('resources/read', '{"uri":"doc://readme"}');
  try
    AssertEquals('doc://readme', msg.FindPath('result.contents[0].uri').AsString);
    AssertEquals('text/markdown', msg.FindPath('result.contents[0].mimeType').AsString);
    AssertEquals('readme of doc://readme', msg.FindPath('result.contents[0].text').AsString);
  finally
    msg.Free;
  end;
  msg := Request('resources/read', '{"uri":"doc://method"}');
  try
    AssertEquals('method resource', msg.FindPath('result.contents[0].text').AsString);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestResourceErrors;
var
  msg: TJSONObject;
begin
  FServer.RegisterResource('doc://denied', '', '', '', '', @ResourceDenied);
  FServer.RegisterResource('doc://boom', '', '', '', '', @ResourceBoom);
  msg := Request('resources/read', '{"uri":"doc://missing"}');
  try
    AssertEquals(MCP_RESOURCE_NOT_FOUND, msg.FindPath('error.code').AsInteger);
  finally
    msg.Free;
  end;
  msg := Request('resources/read', '{"uri":"doc://denied"}');
  try
    AssertEquals(JSONRPC_INTERNAL_ERROR, msg.FindPath('error.code').AsInteger);
    AssertEquals('Resource cannot be read: doc://denied: not today',
      msg.FindPath('error.message').AsString);
  finally
    msg.Free;
  end;
  msg := Request('resources/read', '{"uri":"doc://boom"}');
  try
    AssertEquals(JSONRPC_INTERNAL_ERROR, msg.FindPath('error.code').AsInteger);
    AssertFalse('no exception text', Pos('secret', msg.AsJSON) > 0);
  finally
    msg.Free;
  end;
  msg := Request('resources/read');
  try
    AssertEquals('no params', MCP_RESOURCE_NOT_FOUND, msg.FindPath('error.code').AsInteger);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestEmptyLists;
var
  msg: TJSONObject;
begin
  msg := Request('tools/list');
  try
    AssertEquals('[]', msg.FindPath('result.tools').AsJSON);
  finally
    msg.Free;
  end;
  msg := Request('resources/templates/list');
  try
    AssertEquals('[]', msg.FindPath('result.resourceTemplates').AsJSON);
  finally
    msg.Free;
  end;
  msg := Request('prompts/list');
  try
    AssertEquals('[]', msg.FindPath('result.prompts').AsJSON);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestNotifications;
var
  response: string;
begin
  AssertTrue('initialized',
    FServer.HandleMessage('{"jsonrpc":"2.0","method":"notifications/initialized"}',
    response) = mmkNotification);
  AssertEquals('', response);
  AssertTrue('cancelled',
    FServer.HandleMessage('{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":1}}',
    response) = mmkNotification);
  AssertTrue('unknown notification is ignored',
    FServer.HandleMessage('{"jsonrpc":"2.0","method":"tools/call"}',
    response) = mmkNotification);
  AssertTrue('client response',
    FServer.HandleMessage('{"jsonrpc":"2.0","id":5,"result":{}}',
    response) = mmkNotification);
  AssertTrue('client error response',
    FServer.HandleMessage('{"jsonrpc":"2.0","id":"x","error":{"code":1,"message":"m"}}',
    response) = mmkNotification);
end;

procedure TMcpServerTest.TestInvalidMessages;

  procedure Check(const ABody: string; ACode: integer; const AIdJSON: string);
  var
    response: string;
    data: TJSONData;
  begin
    AssertTrue(ABody + ': invalid',
      FServer.HandleMessage(ABody, response) = mmkInvalid);
    data := GetJSON(response);
    try
      AssertEquals(ABody + ': code', ACode, data.FindPath('error.code').AsInteger);
      AssertEquals(ABody + ': id', AIdJSON, data.FindPath('id').AsJSON);
    finally
      data.Free;
    end;
  end;

begin
  Check('', JSONRPC_PARSE_ERROR, 'null');
  Check('{"jsonrpc":"2.0",', JSONRPC_PARSE_ERROR, 'null');
  Check('[{"jsonrpc":"2.0","id":1,"method":"ping"}]', JSONRPC_INVALID_REQUEST, 'null');
  Check('"ping"', JSONRPC_INVALID_REQUEST, 'null');
  Check('{"id":1,"method":"ping"}', JSONRPC_INVALID_REQUEST, '1');
  Check('{"jsonrpc":"1.0","id":1,"method":"ping"}', JSONRPC_INVALID_REQUEST, '1');
  Check('{"jsonrpc":"2.0","id":null,"method":"ping"}', JSONRPC_INVALID_REQUEST, 'null');
  Check('{"jsonrpc":"2.0","id":{},"method":"ping"}', JSONRPC_INVALID_REQUEST, 'null');
  Check('{"jsonrpc":"2.0","id":2}', JSONRPC_INVALID_REQUEST, '2');
  Check('{"jsonrpc":"2.0"}', JSONRPC_INVALID_REQUEST, 'null');
end;

procedure TMcpServerTest.TestMethodNotFound;
var
  msg: TJSONObject;
begin
  msg := Request('sampling/createMessage');
  try
    AssertEquals(JSONRPC_METHOD_NOT_FOUND, msg.FindPath('error.code').AsInteger);
    AssertEquals('Method not found: sampling/createMessage',
      msg.FindPath('error.message').AsString);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestIdKept;
var
  msg: TJSONObject;
begin
  msg := Request('ping', '', '"abc-1"');
  try
    AssertEquals('"abc-1"', msg.FindPath('id').AsJSON);
  finally
    msg.Free;
  end;
  msg := Request('ping', '', '0');
  try
    AssertEquals('0', msg.FindPath('id').AsJSON);
  finally
    msg.Free;
  end;
  msg := Request('nope', '', '"e"');
  try
    AssertEquals('error keeps the id', '"e"', msg.FindPath('id').AsJSON);
  finally
    msg.Free;
  end;
end;

procedure TMcpServerTest.TestHttpMethods;
const
  METHODS: array[0..2] of string = ('GET', 'DELETE', 'PUT');
var
  req: TMcpHttpRequest;
  res: TMcpHttpResponse;
  m: string;
begin
  for m in METHODS do
  begin
    req := Default(TMcpHttpRequest);
    req.Method := m;
    res := FServer.HandleHttp(req);
    AssertEquals(m, 405, res.Status);
    AssertEquals(m + ' Allow header', 1, Length(res.Headers));
    AssertEquals('Allow: POST', res.Headers[0]);
  end;
  req.Method := 'post';
  req.Body := '{"jsonrpc":"2.0","id":1,"method":"ping"}';
  AssertEquals('method is case-insensitive', 200, FServer.HandleHttp(req).Status);
end;

procedure TMcpServerTest.TestHttpOrigin;
const
  PING = '{"jsonrpc":"2.0","id":1,"method":"ping"}';
begin
  AssertEquals('no origin', 200, Post(PING).Status);
  AssertEquals('localhost', 200, Post(PING, 'http://localhost:6274').Status);
  AssertEquals('127.0.0.1', 200, Post(PING, 'http://127.0.0.1').Status);
  AssertEquals('[::1]', 200, Post(PING, 'http://[::1]:80').Status);
  AssertEquals('same host', 200,
    Post(PING, 'https://api.example.com', 'api.example.com:443').Status);
  AssertEquals('other host', 403,
    Post(PING, 'https://evil.com', 'api.example.com').Status);
  AssertEquals('user info trick', 403, Post(PING, 'http://localhost@evil.com').Status);
  AssertEquals('null origin', 403, Post(PING, 'null').Status);
  AssertEquals('403 body', 'Origin not allowed', Post(PING, 'https://evil.com').Body);

  FServer.AllowedOrigins := 'admin.example.com, https://app.example.com:8443';
  AssertEquals('listed', 200, Post(PING, 'https://admin.example.com').Status);
  AssertEquals('listed with port', 200, Post(PING, 'https://app.example.com').Status);
  AssertEquals('not listed', 403, Post(PING, 'https://example.com').Status);

  FServer.AllowedOrigins := '*';
  AssertEquals('everything', 200, Post(PING, 'https://evil.com').Status);
  AssertEquals('everything incl. null', 200, Post(PING, 'null').Status);
end;

procedure TMcpServerTest.TestHttpToken;
var
  req: TMcpHttpRequest;
  res: TMcpHttpResponse;
begin
  req := Default(TMcpHttpRequest);
  req.Method := 'POST';
  req.Body := '{"jsonrpc":"2.0","id":1,"method":"ping"}';
  AssertEquals('no token configured', 200, FServer.HandleHttp(req).Status);

  FServer.Token := 's3cret';
  res := FServer.HandleHttp(req);
  AssertEquals('missing', 401, res.Status);
  AssertEquals('WWW-Authenticate: Bearer', res.Headers[0]);
  req.Authorization := 'Bearer wrong!';
  AssertEquals('wrong', 401, FServer.HandleHttp(req).Status);
  req.Authorization := 'Bearer s3cre';
  AssertEquals('prefix', 401, FServer.HandleHttp(req).Status);
  req.Authorization := 'Bearer s3cret2';
  AssertEquals('longer', 401, FServer.HandleHttp(req).Status);
  req.Authorization := 's3cret';
  AssertEquals('no scheme', 401, FServer.HandleHttp(req).Status);
  req.Authorization := 'Bearer s3cret';
  AssertEquals('right', 200, FServer.HandleHttp(req).Status);

  req.Method := 'GET';
  req.Authorization := '';
  AssertEquals('token checked before the method', 401, FServer.HandleHttp(req).Status);
end;

procedure TMcpServerTest.TestHttpProtocolVersion;
var
  req: TMcpHttpRequest;
  res: TMcpHttpResponse;
begin
  req := Default(TMcpHttpRequest);
  req.Method := 'POST';
  req.Body := '{"jsonrpc":"2.0","id":1,"method":"ping"}';
  req.ProtocolVersion := '2025-06-18';
  AssertEquals('known', 200, FServer.HandleHttp(req).Status);
  req.ProtocolVersion := '2024-11-05';
  AssertEquals('old known', 200, FServer.HandleHttp(req).Status);
  req.ProtocolVersion := '2030-01-01';
  res := FServer.HandleHttp(req);
  AssertEquals('unknown', 400, res.Status);
  AssertTrue(Pos('Unsupported MCP-Protocol-Version', res.Body) > 0);
end;

procedure TMcpServerTest.TestHttpAnswers;
var
  res: TMcpHttpResponse;
  data: TJSONData;
begin
  FServer.RegisterTool('echo', '', 'Echo', '', @ToolEcho);
  res := Post('{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"echo","arguments":{"text":"hi"}}}');
  AssertEquals(200, res.Status);
  AssertEquals('application/json', res.ContentType);
  AssertEquals('no extra headers, no session id', 0, Length(res.Headers));
  data := GetJSON(res.Body);
  try
    AssertEquals('echo: hi', data.FindPath('result.content[0].text').AsString);
  finally
    data.Free;
  end;

  res := Post('{"jsonrpc":"2.0","method":"notifications/initialized"}');
  AssertEquals('notification', 202, res.Status);
  AssertEquals('text/plain', res.ContentType);
  AssertEquals('', res.Body);

  res := Post('{not json');
  AssertEquals('parse error', 400, res.Status);
  AssertEquals('application/json', res.ContentType);
  AssertTrue(Pos('-32700', res.Body) > 0);

  res := Post('{"jsonrpc":"2.0","id":1,"method":"nope"}');
  AssertEquals('JSON-RPC errors of a request are 200', 200, res.Status);
end;

function ResourceUTF8(const AUri: string; out AText: string): boolean;
begin
  AText := 'isi ' + UTF8_TEXT;
  Result := True;
end;

procedure TMcpServerTest.TestUTF8;

  // the raw answer, as bytes
  function Answer(const ABody: string): string;
  begin
    AssertTrue(FServer.HandleMessage(ABody, Result) = mmkRequest);
  end;

  procedure Contains(const AWhat, AText, AResponse: string);
  begin
    AssertTrue(AWhat + ': ' + AResponse, Pos(AText, AResponse) > 0);
  end;

begin
  FServer.Name := 'server ' + UTF8_TEXT;
  FServer.Instructions := 'petunjuk ' + UTF8_TEXT;
  FServer.RegisterTool('echo', 'Judul ' + UTF8_TEXT, 'Desc ' + UTF8_TEXT,
    '{"type":"object","properties":{"text":{"type":"string","description":"' +
    UTF8_TEXT + '"}}}', @ToolEcho);
  FServer.RegisterResource('doc://' + UTF8_TEXT, 'nama ' + UTF8_TEXT, '', '',
    'text/plain', @ResourceUTF8);

  Contains('serverInfo', '"server ' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"initialize"}'));
  Contains('instructions', '"petunjuk ' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"initialize"}'));
  Contains('tool title', '"Judul ' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"tools/list"}'));
  Contains('tool description', '"Desc ' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"tools/list"}'));
  Contains('schema', '"description" : "' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"tools/list"}'));
  Contains('call result', '"echo: ' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"echo","arguments":{"text":"' +
    UTF8_TEXT + '"}}}'));
  Contains('escaped input', '"echo: ' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"echo","arguments":{"text":"é 中"}}}'));
  Contains('resource list', '"nama ' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"resources/list"}'));
  Contains('resource text', '"isi ' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"doc://' +
    UTF8_TEXT + '"}}'));
  Contains('error message', 'Unknown tool: ' + UTF8_TEXT,
    Answer('{"jsonrpc":"2.0","id":"' + UTF8_TEXT +
    '","method":"tools/call","params":{"name":"' + UTF8_TEXT + '"}}'));
  Contains('string id', '"id" : "' + UTF8_TEXT + '"',
    Answer('{"jsonrpc":"2.0","id":"' + UTF8_TEXT + '","method":"ping"}'));
end;

procedure TMcpServerTest.TestConcurrentCalls;
const
  THREAD_COUNT = 8;
var
  workers: array[0..THREAD_COUNT - 1] of TCallThread;
  i, failures: integer;
begin
  FServer.RegisterTool('sum', '', 'Sum', '', @ToolSum, [mthReadOnly]);
  for i := 0 to THREAD_COUNT - 1 do
    workers[i] := TCallThread.Create(FServer, i + 1);
  failures := 0;
  for i := 0 to THREAD_COUNT - 1 do
  begin
    workers[i].WaitFor;
    Inc(failures, workers[i].Failures);
    workers[i].Free;
  end;
  AssertEquals('every concurrent call answered its own id', 0, failures);
end;

initialization
  RegisterTest(TMcpServerTest);

end.

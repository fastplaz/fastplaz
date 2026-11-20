unit test_mcp_handler;

{
  mcp_handler: the parts of the MCP controller that need no running web
  server. Request built from CGI variables (Authorization fallback for the
  Apache rewrite trick), header splitting, status texts and the "mcp"
  section of config.json (token, env: token, allowed origins, exception
  details, missing keys keep the values set in code). The controller class
  itself is compiled here.
}

{$mode objfpc}{$H+}

interface

uses
  mcp_handler, mcp_server, jsonConf, Classes, SysUtils, fpcunit, testregistry;

type

  { TMcpHandlerTest }

  TMcpHandlerTest = class(TTestCase)
  private
    FServer: TMcpServer;
    FConfigFile: string;
    function LoadConfig(const AJSON: string): TJSONConfig;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRequestFromVariables;
    procedure TestRedirectAuthorization;
    procedure TestSplitHeader;
    procedure TestStatusText;
    procedure TestConfigValues;
    procedure TestConfigEnvToken;
    procedure TestConfigMissingKeepsCode;
    procedure TestConfigNil;
    procedure TestControllerClass;
  end;

implementation

var
  Vars: TStringList;

function FakeVariable(const AName: string): string;
begin
  Result := Vars.Values[AName];
end;

{ TMcpHandlerTest }

procedure TMcpHandlerTest.SetUp;
begin
  FServer := TMcpServer.Create;
  Vars := TStringList.Create;
  FConfigFile := GetTempFileName(GetTempDir, 'mcpcfg');
end;

procedure TMcpHandlerTest.TearDown;
begin
  FServer.Free;
  FreeAndNil(Vars);
  if FileExists(FConfigFile) then
    DeleteFile(FConfigFile);
end;

function TMcpHandlerTest.LoadConfig(const AJSON: string): TJSONConfig;
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  try
    lst.Text := AJSON;
    lst.SaveToFile(FConfigFile);
  finally
    lst.Free;
  end;
  Result := TJSONConfig.Create(nil);
  Result.Filename := FConfigFile;
end;

procedure TMcpHandlerTest.TestRequestFromVariables;
var
  req: TMcpHttpRequest;
begin
  Vars.Values['HTTP_HOST'] := 'api.example.com';
  Vars.Values['HTTP_ORIGIN'] := 'https://app.example.com';
  Vars.Values['HTTP_AUTHORIZATION'] := 'Bearer abc';
  Vars.Values['REDIRECT_HTTP_AUTHORIZATION'] := 'Bearer other';
  Vars.Values['HTTP_MCP_PROTOCOL_VERSION'] := '2025-06-18';
  req := McpRequestFromVariables('POST', '{"x":1}', @FakeVariable);
  AssertEquals('POST', req.Method);
  AssertEquals('{"x":1}', req.Body);
  AssertEquals('api.example.com', req.Host);
  AssertEquals('https://app.example.com', req.Origin);
  AssertEquals('direct header wins', 'Bearer abc', req.Authorization);
  AssertEquals('2025-06-18', req.ProtocolVersion);

  Vars.Clear;
  req := McpRequestFromVariables('GET', '', @FakeVariable);
  AssertEquals('', req.Origin);
  AssertEquals('', req.Authorization);
  AssertEquals('', req.ProtocolVersion);
end;

procedure TMcpHandlerTest.TestRedirectAuthorization;
var
  req: TMcpHttpRequest;
begin
  Vars.Values['REDIRECT_HTTP_AUTHORIZATION'] := 'Bearer xyz';
  req := McpRequestFromVariables('POST', '', @FakeVariable);
  AssertEquals('Bearer xyz', req.Authorization);
end;

procedure TMcpHandlerTest.TestSplitHeader;
var
  n, v: string;
begin
  AssertTrue(McpSplitHeader('Allow: POST', n, v));
  AssertEquals('Allow', n);
  AssertEquals('POST', v);
  AssertTrue(McpSplitHeader('WWW-Authenticate:Bearer realm="a:b"', n, v));
  AssertEquals('WWW-Authenticate', n);
  AssertEquals('only the first colon splits', 'Bearer realm="a:b"', v);
  AssertFalse('no colon', McpSplitHeader('broken', n, v));
  AssertEquals('', n);
  AssertFalse('no name', McpSplitHeader(': value', n, v));
end;

procedure TMcpHandlerTest.TestStatusText;
begin
  AssertEquals('OK', McpStatusText(200));
  AssertEquals('Accepted', McpStatusText(202));
  AssertEquals('Bad Request', McpStatusText(400));
  AssertEquals('Unauthorized', McpStatusText(401));
  AssertEquals('Forbidden', McpStatusText(403));
  AssertEquals('Method Not Allowed', McpStatusText(405));
  AssertEquals('', McpStatusText(299));
end;

procedure TMcpHandlerTest.TestConfigValues;
var
  cfg: TJSONConfig;
begin
  cfg := LoadConfig('{"mcp":{"token":" t0ken ","allowed_origins":"a.example.com, b.example.com","exception_details":true}}');
  try
    McpLoadConfig(FServer, cfg);
  finally
    cfg.Free;
  end;
  AssertEquals('trimmed', 't0ken', FServer.Token);
  AssertEquals('a.example.com, b.example.com', FServer.AllowedOrigins);
  AssertTrue(FServer.ExceptionDetails);
end;

procedure TMcpHandlerTest.TestConfigEnvToken;
var
  cfg: TJSONConfig;
  envName: string;
begin
  // PATH exists on every platform; a real setup uses e.g. MCP_TOKEN
  envName := 'PATH';
  cfg := LoadConfig('{"mcp":{"token":"env:' + envName + '"}}');
  try
    McpLoadConfig(FServer, cfg);
  finally
    cfg.Free;
  end;
  AssertEquals(Trim(GetEnvironmentVariable(envName)), FServer.Token);

  FServer.Token := 'from code';
  cfg := LoadConfig('{"mcp":{"token":"env:FASTPLAZ_MCP_TEST_NOT_SET_42"}}');
  try
    McpLoadConfig(FServer, cfg);
  finally
    cfg.Free;
  end;
  AssertEquals('empty variable keeps the token from code', 'from code', FServer.Token);
end;

procedure TMcpHandlerTest.TestConfigMissingKeepsCode;
var
  cfg: TJSONConfig;
begin
  FServer.Token := 'code';
  FServer.AllowedOrigins := 'code.example.com';
  FServer.ExceptionDetails := True;
  cfg := LoadConfig('{"systems":{"debug":true}}');
  try
    McpLoadConfig(FServer, cfg);
  finally
    cfg.Free;
  end;
  AssertEquals('code', FServer.Token);
  AssertEquals('code.example.com', FServer.AllowedOrigins);
  AssertTrue(FServer.ExceptionDetails);

  cfg := LoadConfig('{"mcp":{"token":"","exception_details":false}}');
  try
    McpLoadConfig(FServer, cfg);
  finally
    cfg.Free;
  end;
  AssertEquals('empty token keeps the code value', 'code', FServer.Token);
  AssertFalse('explicit false wins', FServer.ExceptionDetails);
end;

procedure TMcpHandlerTest.TestConfigNil;
begin
  FServer.Token := 'x';
  McpLoadConfig(FServer, nil);
  McpLoadConfig(nil, nil);
  AssertEquals('x', FServer.Token);
end;

procedure TMcpHandlerTest.TestControllerClass;
begin
  AssertTrue(TMcpController.InheritsFrom(TComponent));
  AssertEquals('TMcpController', TMcpController.ClassName);
end;

initialization
  RegisterTest(TMcpHandlerTest);

end.

unit test_mcp_lib;

{
  mcp_lib: source text of the MCP generators (controller unit, routes.pas
  of an MCP project), controller type names that never hit the base class,
  the "mcp" section added to config.json and the .htaccess rule that passes
  the Authorization header to the CGI program (also on the shipped API
  template).
}

{$mode objfpc}{$H+}

interface

uses
  mcp_lib, fpjson, jsonparser, Classes, SysUtils, fpcunit, testregistry;

type

  { TMcpLibTest }

  TMcpLibTest = class(TTestCase)
  private
    FTempFile: string;
    procedure WriteTemp(const AText: string);
    function ReadTemp: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTypeName;
    procedure TestPascalQuoted;
    procedure TestUses;
    procedure TestInterfaceSource;
    procedure TestControllerWithRoute;
    procedure TestControllerForProject;
    procedure TestRoutesSource;
    procedure TestConfigAddsSection;
    procedure TestConfigKeepsSection;
    procedure TestConfigInvalid;
    procedure TestConfigFile;
    procedure TestHtaccessRule;
    procedure TestHtaccessOnce;
    procedure TestHtaccessWithoutRewrite;
    procedure TestHtaccessTemplate;
    procedure TestHtaccessFile;
  end;

implementation

function Has(const AText, ASub: string): boolean;
begin
  Result := Pos(ASub, AText) > 0;
end;

{ TMcpLibTest }

procedure TMcpLibTest.SetUp;
begin
  FTempFile := GetTempFileName(GetTempDir, 'mcplib');
end;

procedure TMcpLibTest.TearDown;
begin
  if FileExists(FTempFile) then
    DeleteFile(FTempFile);
end;

procedure TMcpLibTest.WriteTemp(const AText: string);
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  try
    lst.Text := AText;
    lst.SaveToFile(FTempFile);
  finally
    lst.Free;
  end;
end;

function TMcpLibTest.ReadTemp: string;
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  try
    lst.LoadFromFile(FTempFile);
    Result := lst.Text;
  finally
    lst.Free;
  end;
end;

procedure TMcpLibTest.TestTypeName;
begin
  AssertEquals('base class avoided', MCP_RENAMED_TYPE_NAME, McpControllerTypeName('Mcp'));
  AssertEquals('case does not matter', MCP_RENAMED_TYPE_NAME, McpControllerTypeName('mcp'));
  AssertEquals('TWeatherController', McpControllerTypeName('weather'));
  AssertEquals('TMcpWeatherController', McpControllerTypeName('mcp weather'));
  AssertEquals('TFastplazmcpController', McpControllerTypeName('fastplazmcp'));
end;

procedure TMcpLibTest.TestPascalQuoted;
begin
  AssertEquals('''abc''', PascalQuoted('abc'));
  AssertEquals('''it''''s''', PascalQuoted('it''s'));
  AssertEquals('''''', PascalQuoted(''));
end;

procedure TMcpLibTest.TestUses;
begin
  AssertTrue(Has(McpControllerUses, 'mcp_server'));
  AssertTrue(Has(McpControllerUses, 'mcp_handler'));
  AssertTrue(Has(McpControllerUses, 'fastplaz_handler'));
  AssertEquals('appended after Classes, SysUtils', ',', Copy(McpControllerUses, 1, 1));
end;

procedure TMcpLibTest.TestInterfaceSource;
var
  s: string;
begin
  s := McpControllerInterfaceSource('TWeatherController');
  AssertTrue(Has(s, 'type'));
  AssertTrue(Has(s, 'TWeatherController = class(TMcpController)'));
end;

procedure TMcpLibTest.TestControllerWithRoute;
var
  s: string;
begin
  s := McpControllerImplementationSource('TWeatherController', 'weather', '', True);
  AssertTrue('example tool', Has(s, 'procedure DoHello(ACall: TMcpToolCall);'));
  AssertTrue('registration', Has(s, 'McpServer.RegisterTool(''hello'''));
  AssertTrue('handler', Has(s, '@DoHello, [mthReadOnly, mthIdempotent]'));
  AssertTrue('initialization', Has(s, 'initialization'));
  AssertTrue('route', Has(s, 'Route[''weather''] := TWeatherController;'));
  AssertTrue('curl hint uses the permalink', Has(s, 'http://localhost/weather'));
  AssertFalse('no server name', Has(s, 'McpServer.Name :='));
  AssertFalse('no implementation keyword (added by the IDE)', Has(s, 'implementation'));
  AssertFalse('no end. (added by the IDE)', Has(s, 'end.'));
end;

procedure TMcpLibTest.TestControllerForProject;
var
  s: string;
begin
  s := McpControllerImplementationSource('TMyAppController', 'mcp', 'it''s app', False);
  AssertFalse('route lives in routes.pas', Has(s, 'Route['));
  AssertTrue('server name quoted', Has(s, 'McpServer.Name := ''it''''s app'';'));
  AssertTrue(Has(s, 'McpServer.Version := ''1.0.0'';'));
end;

procedure TMcpLibTest.TestRoutesSource;
var
  s: string;
begin
  s := McpRoutesImplementationSource('myapp_controller', 'TMyappController', 'mcp');
  AssertTrue(Has(s, 'uses myapp_controller;'));
  AssertTrue(Has(s, 'initialization'));
  AssertTrue(Has(s, 'Route[''mcp''] := TMyappController;'));
end;

procedure TMcpLibTest.TestConfigAddsSection;
var
  s: string;
  data: TJSONData;
begin
  s := ConfigWithMcpSection('{"systems":{"debug":true},"database":{"default":{}}}');
  AssertTrue(s <> '');
  data := GetJSON(s);
  try
    AssertEquals('', data.FindPath('mcp.token').AsString);
    AssertEquals('', data.FindPath('mcp.allowed_origins').AsString);
    AssertFalse(data.FindPath('mcp.exception_details').AsBoolean);
    AssertTrue('other sections kept', data.FindPath('systems.debug').AsBoolean);
    AssertNotNull(data.FindPath('database.default'));
  finally
    data.Free;
  end;
end;

procedure TMcpLibTest.TestConfigKeepsSection;
var
  data: TJSONData;
begin
  data := GetJSON(ConfigWithMcpSection('{"mcp":{"token":"abc"}}'));
  try
    AssertEquals('existing section untouched', 'abc', data.FindPath('mcp.token').AsString);
    AssertNull(data.FindPath('mcp.allowed_origins'));
  finally
    data.Free;
  end;
  // a non-object "mcp" is replaced
  data := GetJSON(ConfigWithMcpSection('{"mcp":"x"}'));
  try
    AssertEquals('', data.FindPath('mcp.token').AsString);
  finally
    data.Free;
  end;
end;

procedure TMcpLibTest.TestConfigInvalid;
begin
  AssertEquals('', ConfigWithMcpSection('{broken'));
  AssertEquals('', ConfigWithMcpSection('[1,2]'));
  AssertEquals('', ConfigWithMcpSection(''));
  AssertTrue('comments and trailing commas are accepted',
    ConfigWithMcpSection('{"a":1, // c' + LineEnding + '}') <> '');
end;

procedure TMcpLibTest.TestConfigFile;
begin
  WriteTemp('{"systems":{}}');
  AssertTrue(SetConfigMcpSection(FTempFile));
  AssertTrue(Has(ReadTemp, '"mcp"'));
  AssertTrue('again: nothing to change', SetConfigMcpSection(FTempFile));
  WriteTemp('{broken');
  AssertFalse(SetConfigMcpSection(FTempFile));
  AssertEquals('broken file left alone', '{broken', Trim(ReadTemp));
  AssertFalse('missing file', SetConfigMcpSection(FTempFile + '.missing'));
end;

procedure TMcpLibTest.TestHtaccessRule;
var
  lines: TStringList;
  i, engine, rule, last: integer;
begin
  lines := TStringList.Create;
  try
    lines.Text := HtaccessWithMcpAuth('# top' + LineEnding + 'RewriteEngine On' +
      LineEnding + 'Options +ExecCGI' + LineEnding +
      'RewriteRule ^(.*)$ app.bin/$1 [QSA,L]');
    engine := -1;
    rule := -1;
    last := -1;
    for i := 0 to lines.Count - 1 do
    begin
      if lines[i] = 'RewriteEngine On' then
        engine := i;
      if lines[i] = 'RewriteRule .* - [E=HTTP_AUTHORIZATION:%{HTTP:Authorization}]' then
        rule := i;
      if Pos('app.bin', lines[i]) > 0 then
        last := i;
    end;
    AssertTrue('rule added', rule >= 0);
    AssertTrue('after RewriteEngine On', rule > engine);
    AssertTrue('before the final rewrite', rule < last);
    AssertTrue('CGIPassAuth mentioned', Has(lines.Text, 'CGIPassAuth On'));
    AssertFalse('CGIPassAuth only as a comment', Has(lines.Text, LineEnding + 'CGIPassAuth'));
  finally
    lines.Free;
  end;
end;

procedure TMcpLibTest.TestHtaccessOnce;
var
  s: string;
begin
  s := HtaccessWithMcpAuth('RewriteEngine On' + LineEnding);
  AssertEquals('unchanged the second time', s, HtaccessWithMcpAuth(s));
end;

procedure TMcpLibTest.TestHtaccessWithoutRewrite;
var
  s: string;
begin
  s := HtaccessWithMcpAuth('Options +ExecCGI' + LineEnding);
  AssertTrue(Has(s, 'RewriteEngine On'));
  AssertTrue(Pos('RewriteEngine On', s) < Pos(MCP_HTACCESS_MARKER, s));
  AssertTrue('case-insensitive match', Has(HtaccessWithMcpAuth('rewriteengine on'),
    MCP_HTACCESS_MARKER));
end;

procedure TMcpLibTest.TestHtaccessTemplate;
var
  lst: TStringList;
  template, s: string;
begin
  template := '..' + DirectorySeparator + '..' + DirectorySeparator + 'templates' +
    DirectorySeparator + 'api' + DirectorySeparator + '.htaccess';
  if not FileExists(template) then
    Ignore('API template not found: ' + ExpandFileName(template));
  lst := TStringList.Create;
  try
    lst.LoadFromFile(template);
    s := HtaccessWithMcpAuth(lst.Text);
  finally
    lst.Free;
  end;
  AssertTrue(Has(s, MCP_HTACCESS_MARKER));
  AssertTrue('before the catch-all rule',
    Pos(MCP_HTACCESS_MARKER, s) < Pos('RewriteRule ^(.*)$ your_binary_file', s));
end;

procedure TMcpLibTest.TestHtaccessFile;
begin
  WriteTemp('RewriteEngine On');
  AssertTrue(SetHtaccessMcpAuth(FTempFile));
  AssertTrue(Has(ReadTemp, MCP_HTACCESS_MARKER));
  AssertFalse('missing file', SetHtaccessMcpAuth(FTempFile + '.missing'));
end;

initialization
  RegisterTest(TMcpLibTest);

end.

unit mcp_lib;

(*
  Source text and file changes for the MCP generators ("FastPlaz | Create
  MCP Controller" and "File | New | FastPlaz | MCP Server Application").
  No IDE units here, so the text can be tested in tools_test.

  The generated controller derives from TMcpController (FastPlaz/src/mcp/
  mcp_handler.pas) and registers its tools on McpServer (mcp_server.pas).
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner;

const
  MCP_BASE_CLASS = 'TMcpController';
  MCP_DEFAULT_MODULE_NAME = 'Mcp';
  MCP_DEFAULT_PERMALINK = 'mcp';
  // used when the name would be the base class itself
  MCP_RENAMED_TYPE_NAME = 'TMcpEndpointController';
  MCP_HTACCESS_MARKER = 'HTTP_AUTHORIZATION:%{HTTP:Authorization}';

{ ControllerTypeName, but never the base class TMcpController: "Mcp" gives
  TMcpEndpointController. }
function McpControllerTypeName(const AModuleName: string): string;
{ Added to the interface uses clause after "Classes, SysUtils". }
function McpControllerUses: string;
{ The type section of the controller unit. }
function McpControllerInterfaceSource(const ATypeName: string): string;
{ The implementation section: an example tool "hello", the registration in
  initialization and, with AWithRoute, the route line. AServerName '' leaves
  the server name to the default. }
function McpControllerImplementationSource(const ATypeName, APermalink,
  AServerName: string; AWithRoute: boolean): string;
{ Implementation section of routes.pas for an MCP project. }
function McpRoutesImplementationSource(const AControllerUnit, ATypeName,
  APermalink: string): string;
{ AConfigJSON with an "mcp" section (token, allowed_origins,
  exception_details) when it has none; '' when it is not a JSON object. }
function ConfigWithMcpSection(const AConfigJSON: string): string;
function SetConfigMcpSection(const AConfigFile: string): boolean;
{ AHtaccess with a rewrite rule that passes the Authorization header to the
  CGI program (Apache drops it), placed after "RewriteEngine On"; unchanged
  when the rule is there already. }
function HtaccessWithMcpAuth(const AHtaccess: string): string;
function SetHtaccessMcpAuth(const AHtaccessFile: string): boolean;
{ A Pascal string literal for AText (quotes doubled). }
function PascalQuoted(const AText: string): string;

implementation

uses tools_lib;

function McpControllerTypeName(const AModuleName: string): string;
begin
  Result := ControllerTypeName(AModuleName);
  if SameText(Result, MCP_BASE_CLASS) then
    Result := MCP_RENAMED_TYPE_NAME;
end;

function PascalQuoted(const AText: string): string;
begin
  Result := '''' + StringReplace(AText, '''', '''''', [rfReplaceAll]) + '''';
end;

function McpControllerUses: string;
begin
  Result := ', fpjson, fastplaz_handler, mcp_server, mcp_handler';
end;

function McpControllerInterfaceSource(const ATypeName: string): string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    str.Add('type');
    str.Add('');
    str.Add('  { ' + ATypeName + ': the MCP endpoint. TMcpController passes every request');
    str.Add('    to McpServer; the tools are registered in initialization below. }');
    str.Add('');
    str.Add('  ' + ATypeName + ' = class(TMcpController)');
    str.Add('  end;');
    str.Add('');
    Result := str.Text;
  finally
    str.Free;
  end;
end;

function McpControllerImplementationSource(const ATypeName, APermalink,
  AServerName: string; AWithRoute: boolean): string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    str.Add('// Tool "hello". Test it with:');
    str.Add('//   curl -X POST http://localhost/' + APermalink +
      ' -H "Content-Type: application/json" -d ''{"jsonrpc":"2.0","id":1,' +
      '"method":"tools/call","params":{"name":"hello","arguments":{"name":"World"}}}''');
    str.Add('// In standalone mode tools run in several threads at once: keep them');
    str.Add('// free of shared state, or protect it.');
    str.Add('procedure DoHello(ACall: TMcpToolCall);');
    str.Add('begin');
    str.Add('  if ACall.Str(''name'') = '''' then');
    str.Add('    raise EMcpToolError.Create(''"name" is required''); // tool error, isError = true');
    str.Add('  ACall.Finish(''Hello '' + ACall.Str(''name'') + '', from FastPlaz!'');');
    str.Add('end;');
    str.Add('');
    str.Add('initialization');
    if AWithRoute then
    begin
      str.Add('  // -> http://yourdomainname/' + APermalink);
      str.Add('  // The following line should be moved to a file "routes.pas"');
      str.Add('  Route[' + PascalQuoted(APermalink) + '] := ' + ATypeName + ';');
      str.Add('');
    end;
    if AServerName <> '' then
    begin
      str.Add('  McpServer.Name := ' + PascalQuoted(AServerName) + ';');
      str.Add('  McpServer.Version := ''1.0.0'';');
    end;
    str.Add('  // McpServer.Instructions := ''What this server is for, sent to the client.'';');
    str.Add('  // Token, allowed origins: config/config.json, section "mcp".');
    str.Add('');
    str.Add('  McpServer.RegisterTool(''hello'', ''Hello'', ''Greets someone by name.'',');
    str.Add('    ''{"type":"object","properties":{"name":{"type":"string","description":"Who to greet"}},"required":["name"]}'',');
    str.Add('    @DoHello, [mthReadOnly, mthIdempotent]);');
    str.Add('');
    Result := str.Text;
  finally
    str.Free;
  end;
end;

function McpRoutesImplementationSource(const AControllerUnit, ATypeName,
  APermalink: string): string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    str.Add('uses ' + AControllerUnit + ';');
    str.Add('');
    str.Add('initialization');
    str.Add('  // MCP endpoint: POST http://yourdomainname/' + APermalink);
    str.Add('  Route[' + PascalQuoted(APermalink) + '] := ' + ATypeName + ';');
    str.Add('');
    Result := str.Text;
  finally
    str.Free;
  end;
end;

function ConfigWithMcpSection(const AConfigJSON: string): string;
var
  parser: TJSONParser;
  root: TJSONData;
begin
  Result := '';
  root := nil;
  try
    parser := TJSONParser.Create(AConfigJSON, [joUTF8, joComments, joIgnoreTrailingComma]);
    try
      root := parser.Parse;
    finally
      parser.Free;
    end;
  except
    on E: Exception do
      Exit;
  end;
  try
    if not (root is TJSONObject) then
      Exit;
    if not (TJSONObject(root).Find('mcp') is TJSONObject) then
    begin
      TJSONObject(root).Delete('mcp');
      TJSONObject(root).Add('mcp', TJSONObject.Create([
        'token', '',
        'allowed_origins', '',
        'exception_details', False]));
    end;
    Result := root.FormatJSON + LineEnding;
  finally
    root.Free;
  end;
end;

type
  TTextChange = function(const AText: string): string;

function UpdateTextFile(const AFileName: string; AChange: TTextChange): boolean;
var
  lst: TStringList;
  s: string;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;
  lst := TStringList.Create;
  try
    try
      lst.LoadFromFile(AFileName);
      s := AChange(lst.Text);
      if s = '' then
        Exit;
      if s <> lst.Text then
      begin
        lst.Text := s;
        lst.SaveToFile(AFileName);
      end;
      Result := True;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    lst.Free;
  end;
end;

function ConfigWithMcpSectionText(const AText: string): string;
begin
  Result := ConfigWithMcpSection(AText);
end;

function SetConfigMcpSection(const AConfigFile: string): boolean;
begin
  Result := UpdateTextFile(AConfigFile, @ConfigWithMcpSectionText);
end;

function HtaccessWithMcpAuth(const AHtaccess: string): string;
var
  lines: TStringList;
  i, at: integer;
begin
  if Pos(MCP_HTACCESS_MARKER, AHtaccess) > 0 then
    Exit(AHtaccess);
  lines := TStringList.Create;
  try
    lines.Text := AHtaccess;
    at := -1;
    for i := 0 to lines.Count - 1 do
      if SameText(Trim(lines[i]), 'RewriteEngine On') then
      begin
        at := i + 1;
        Break;
      end;
    if at < 0 then
    begin
      lines.Add('');
      lines.Add('RewriteEngine On');
      at := lines.Count;
    end;
    lines.Insert(at, '');
    lines.Insert(at + 1, '# MCP bearer token (config.json "mcp/token"): Apache does not pass the');
    lines.Insert(at + 2, '# Authorization header to CGI programs. This rule passes it on');
    lines.Insert(at + 3, '# (REDIRECT_HTTP_AUTHORIZATION); with "AllowOverride AuthConfig" the line');
    lines.Insert(at + 4, '# "CGIPassAuth On" (Apache 2.4.13+) does the same.');
    lines.Insert(at + 5, 'RewriteRule .* - [E=' + MCP_HTACCESS_MARKER + ']');
    Result := lines.Text;
  finally
    lines.Free;
  end;
end;

function SetHtaccessMcpAuth(const AHtaccessFile: string): boolean;
begin
  Result := UpdateTextFile(AHtaccessFile, @HtaccessWithMcpAuth);
end;

end.

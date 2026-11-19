unit mcp_server;

(*
  MCP (Model Context Protocol) server core for FastPlaz applications.

  Transport: MCP Streamable HTTP in its simplest form, POST /mcp with one
  JSON-RPC message and a JSON answer, no SSE stream. Stateless: no
  Mcp-Session-Id is issued and nothing is kept between requests, so the same
  code serves CGI (one process per request) and standalone mode.

  The core knows nothing about CGI or fpweb: HandleHttp() takes the parts of
  the HTTP request it needs and returns status, content type, headers and
  body. HandleMessage() handles one JSON-RPC message without HTTP.

  Usage, in the initialization section of a unit:

    McpServer.Name := 'weather';
    McpServer.RegisterTool('get_weather', 'Get weather',
      'Current weather of a city',
      '{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}',
      @DoGetWeather, [mthReadOnly]);

    procedure DoGetWeather(ACall: TMcpToolCall);
    begin
      ACall.Finish('Sunny in ' + ACall.Str('city'));
    end;

  Threads: standalone mode serves requests in several threads with one
  McpServer. Register tools and resources in initialization sections only;
  after that the registry is only read. HandleHttp/HandleMessage keep no
  state in fields. Tool and resource handlers must be thread-safe too.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

const
  MCP_PATH = '/mcp';
  MCP_LATEST_PROTOCOL = '2025-06-18';
  // used when the client sends no MCP-Protocol-Version header (spec rule)
  MCP_DEFAULT_HEADER_PROTOCOL = '2025-03-26';
  MCP_PROTOCOL_VERSIONS: array[0..3] of string =
    ('2025-11-25', '2025-06-18', '2025-03-26', '2024-11-05');

  JSONRPC_PARSE_ERROR = -32700;
  JSONRPC_INVALID_REQUEST = -32600;
  JSONRPC_METHOD_NOT_FOUND = -32601;
  JSONRPC_INVALID_PARAMS = -32602;
  JSONRPC_INTERNAL_ERROR = -32603;
  MCP_RESOURCE_NOT_FOUND = -32002;

type
  EMcpServer = class(Exception);

  { Raise in a tool handler: the call ends as a tool error (isError) and the
    client reads the message. Other exceptions are reported without their
    text unless TMcpServer.ExceptionDetails is set. }
  EMcpToolError = class(Exception);

  { MCP tool annotations. A tool registered with no hints sends no
    annotations (the client then assumes the MCP defaults). }
  TMcpToolHint = (
    mthReadOnly,    // changes nothing
    mthDestructive, // deletes or overwrites what cannot be restored
    mthIdempotent,  // calling again with the same arguments changes nothing more
    mthOpenWorld    // talks to the outside world (web, other services)
    );
  TMcpToolHints = set of TMcpToolHint;

  { TMcpToolCall: one tools/call, created per request. }

  TMcpToolCall = class
  private
    FName: string;
    FArgs: TJSONObject;
    FContent: TJSONArray;
    FStructured: TJSONObject;
    FIsError: boolean;
  public
    constructor Create(const AName: string; AArgs: TJSONObject);
    destructor Destroy; override;
    function Has(const AName: string): boolean;
    function Str(const AName: string; const ADefault: string = ''): string;
    function Int(const AName: string; ADefault: int64 = 0): int64;
    function Float(const AName: string; ADefault: double = 0): double;
    function Bool(const AName: string; ADefault: boolean = False): boolean;
    { Adds a text content item to the result. }
    procedure Finish(const AText: string);
    { JSON result: sent as text, and as structuredContent when AData is an
      object. Takes ownership of AData. }
    procedure FinishJSON(AData: TJSONData);
    { Tool error (isError = true): the client reads AMessage. }
    procedure Fail(const AMessage: string);
    { The CallToolResult object; the caller owns it. }
    function BuildResult: TJSONObject;
    property Name: string read FName;
    { Arguments of the call, never nil. }
    property Args: TJSONObject read FArgs;
    property IsError: boolean read FIsError;
  end;

  TMcpToolProc = procedure(ACall: TMcpToolCall);
  TMcpToolMethod = procedure(ACall: TMcpToolCall) of object;

  { Gives the text of resource AUri. False: AText is the reason it cannot be
    read. }
  TMcpResourceProc = function(const AUri: string; out AText: string): boolean;
  TMcpResourceMethod = function(const AUri: string; out AText: string): boolean of object;

  TMcpErrorEvent = procedure(Sender: TObject; const AContext: string;
    E: Exception) of object;

  TMcpTool = record
    Name: string;
    Title: string;
    Description: string;
    InputSchema: string;  // normalized JSON object text
    OutputSchema: string; // '' or JSON object text
    Hints: TMcpToolHints;
    Proc: TMcpToolProc;
    Method: TMcpToolMethod;
  end;

  TMcpResource = record
    Uri: string;
    Name: string;
    Title: string;
    Description: string;
    MimeType: string;
    Proc: TMcpResourceProc;
    Method: TMcpResourceMethod;
  end;

  TMcpMessageKind = (
    mmkRequest,      // a request: AResponse holds the answer
    mmkNotification, // a notification or a client response: no answer
    mmkInvalid       // not JSON / not a JSON-RPC message: AResponse holds the error
    );

  { The parts of an HTTP request the MCP endpoint looks at. }
  TMcpHttpRequest = record
    Method: string;          // GET, POST, ...
    Body: string;
    Host: string;            // Host header, compared with Origin
    Origin: string;          // Origin header, '' when absent
    Authorization: string;   // Authorization header
    ProtocolVersion: string; // MCP-Protocol-Version header, '' when absent
  end;

  TMcpHttpResponse = record
    Status: integer;
    ContentType: string;
    Body: string;
    Headers: array of string; // extra headers, 'Name: value'
  end;

  { TMcpServer }

  TMcpServer = class
  private
    FTools: array of TMcpTool;
    FResources: array of TMcpResource;
    FName: string;
    FTitle: string;
    FVersion: string;
    FInstructions: string;
    FToken: string;
    FAllowedOrigins: string;
    FExceptionDetails: boolean;
    FOnError: TMcpErrorEvent;
    procedure AddTool(const AName, ATitle, ADescription, AInputSchema: string;
      AHints: TMcpToolHints; AProc: TMcpToolProc; AMethod: TMcpToolMethod;
      const AOutputSchema: string);
    procedure AddResource(const AUri, AName, ATitle, ADescription, AMimeType: string;
      AProc: TMcpResourceProc; AMethod: TMcpResourceMethod);
    function FindTool(const AName: string; out ATool: TMcpTool): boolean;
    function DoInitialize(AParams: TJSONObject): TJSONObject;
    function DoToolsList: TJSONObject;
    function DoToolsCall(AParams: TJSONObject; out AErrorCode: integer;
      out AErrorMessage: string): TJSONObject;
    function DoResourcesList: TJSONObject;
    function DoResourcesRead(AParams: TJSONObject; out AErrorCode: integer;
      out AErrorMessage: string): TJSONObject;
    procedure ReportError(const AContext: string; E: Exception);
  public
    constructor Create;
    procedure RegisterTool(const AName, ATitle, ADescription, AInputSchema: string;
      AHandler: TMcpToolProc; AHints: TMcpToolHints = [];
      const AOutputSchema: string = ''); overload;
    procedure RegisterTool(const AName, ATitle, ADescription, AInputSchema: string;
      AHandler: TMcpToolMethod; AHints: TMcpToolHints = [];
      const AOutputSchema: string = ''); overload;
    { A text resource for resources/list and resources/read; the handler is
      asked for the text at every read. }
    procedure RegisterResource(const AUri, AName, ATitle, ADescription,
      AMimeType: string; AHandler: TMcpResourceProc); overload;
    procedure RegisterResource(const AUri, AName, ATitle, ADescription,
      AMimeType: string; AHandler: TMcpResourceMethod); overload;
    { Removes every tool and resource (tests, or a host that rebuilds the
      registry before it serves requests). }
    procedure Clear;
    function ToolCount: integer;
    function ResourceCount: integer;
    function HasTool(const AName: string): boolean;

    { One JSON-RPC message, no HTTP rules. }
    function HandleMessage(const ABody: string; out AResponse: string): TMcpMessageKind;
    { One HTTP request to the MCP endpoint: method, Origin, token and
      protocol version checks, then HandleMessage. }
    function HandleHttp(const ARequest: TMcpHttpRequest): TMcpHttpResponse;
    { Origin check (DNS rebinding protection): no Origin, a loopback origin,
      the request's own host, or one listed in AllowedOrigins. }
    function OriginAllowed(const AOrigin, AHost: string): boolean;

    { serverInfo sent at initialize }
    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    property Version: string read FVersion write FVersion;
    { Sent to the client in the initialize result. }
    property Instructions: string read FInstructions write FInstructions;
    { When set, every HTTP request needs "Authorization: Bearer <Token>". }
    property Token: string read FToken write FToken;
    { Extra allowed Origin hosts, separated by comma or space, e.g.
      "app.example.com, https://admin.example.com"; "*" allows every origin. }
    property AllowedOrigins: string read FAllowedOrigins write FAllowedOrigins;
    { Send the text of unexpected tool/resource exceptions to the client. }
    property ExceptionDetails: boolean read FExceptionDetails write FExceptionDetails;
    { Unexpected exceptions in handlers, for logging. Must be thread-safe. }
    property OnError: TMcpErrorEvent read FOnError write FOnError;
  end;

{ The application's MCP server, created when this unit is initialized. }
function McpServer: TMcpServer;

{ True when AName is a valid MCP tool name: 1..128 of A-Z a-z 0-9 _ - . }
function McpValidToolName(const AName: string): boolean;
{ True when AVersion is a protocol version this server speaks. }
function McpKnownProtocol(const AVersion: string): boolean;
{ Host part of an Origin or Host value, lower case, IPv6 kept in brackets:
  "https://Example.com:8080" -> "example.com", "[::1]:80" -> "[::1]". }
function McpHostOf(const AValue: string): string;
{ A JSON-RPC error message; AIdJSON is the request id as JSON ('null' when
  unknown). }
function McpErrorJSON(const AIdJSON: string; ACode: integer;
  const AMessage: string): string;

implementation

var
  GlobalServer: TMcpServer = nil;

function McpServer: TMcpServer;
begin
  Result := GlobalServer;
end;

function McpValidToolName(const AName: string): boolean;
var
  i: integer;
begin
  Result := (Length(AName) >= 1) and (Length(AName) <= 128);
  if Result then
    for i := 1 to Length(AName) do
      if not (AName[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.']) then
        Exit(False);
end;

function McpKnownProtocol(const AVersion: string): boolean;
var
  i: integer;
begin
  for i := 0 to High(MCP_PROTOCOL_VERSIONS) do
    if AVersion = MCP_PROTOCOL_VERSIONS[i] then
      Exit(True);
  Result := False;
end;

function McpHostOf(const AValue: string): string;
var
  p: integer;
begin
  Result := LowerCase(Trim(AValue));
  p := Pos('://', Result);
  if p > 0 then
    Delete(Result, 1, p + 2);
  p := Pos('/', Result);
  if p > 0 then
    Result := Copy(Result, 1, p - 1);
  p := Pos('@', Result); // user info
  if p > 0 then
    Delete(Result, 1, p);
  if (Result <> '') and (Result[1] = '[') then
  begin
    p := Pos(']', Result);
    if p > 0 then
      Result := Copy(Result, 1, p)
    else
      Result := '';
  end
  else
  begin
    p := Pos(':', Result);
    if p > 0 then
      Result := Copy(Result, 1, p - 1);
  end;
end;

{ Code pages. FastPlaz strings carry UTF-8 bytes whatever their code page
  tag (request bodies, templates, database), while fpjson works with
  UTF8String. An implicit conversion between the two would re-encode the
  bytes when the system code page is not UTF-8 (Windows): "é" becomes "Ã©",
  and characters outside the ANSI code page become "?". These two helpers
  only change the tag. }

// a FastPlaz string (UTF-8 bytes) for fpjson
function ToJ(const S: RawByteString): TJSONStringType;
var
  r: RawByteString;
begin
  r := S;
  if (r <> '') and (StringCodePage(r) <> CP_UTF8) then
  begin
    UniqueString(r);
    SetCodePage(r, CP_UTF8, False);
  end;
  Result := r;
end;

// an fpjson string as a FastPlaz string, same bytes
function FromJ(const S: TJSONStringType): string;
var
  r: RawByteString;
begin
  r := S;
  if r <> '' then
  begin
    UniqueString(r);
    SetCodePage(r, DefaultSystemCodePage, False);
  end;
  Result := r;
end;

function JStr(AObject: TJSONObject; const AName: string;
  const ADefault: string = ''): string;
var
  d: TJSONData;
begin
  Result := ADefault;
  if not Assigned(AObject) then
    Exit;
  d := AObject.Find(AName);
  if Assigned(d) and (d.JSONType in [jtString, jtNumber, jtBoolean]) then
    Result := FromJ(d.AsString);
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

// Parses AText as a JSON object; nil when it is not one.
function ParseObject(const AText: string): TJSONObject;
var
  d: TJSONData;
begin
  Result := nil;
  if Trim(AText) = '' then
    Exit;
  try
    d := GetJSON(ToJ(AText));
  except
    Exit;
  end;
  if d.JSONType = jtObject then
    Result := TJSONObject(d)
  else
    d.Free;
end;

function McpErrorJSON(const AIdJSON: string; ACode: integer;
  const AMessage: string): string;
var
  msg, err: TJSONObject;
  id: TJSONData;
begin
  err := TJSONObject.Create;
  err.Add('code', ACode);
  err.Add('message', ToJ(AMessage));
  msg := TJSONObject.Create;
  try
    msg.Add('jsonrpc', '2.0');
    try
      id := GetJSON(ToJ(AIdJSON));
    except
      id := TJSONNull.Create;
    end;
    msg.Add('id', id);
    msg.Add('error', err);
    Result := FromJ(msg.AsJSON);
  finally
    msg.Free;
  end;
end;

function ResultJSON(const AIdJSON: string; AResult: TJSONData): string;
var
  msg: TJSONObject;
begin
  msg := TJSONObject.Create;
  try
    msg.Add('jsonrpc', '2.0');
    msg.Add('id', GetJSON(ToJ(AIdJSON)));
    msg.Add('result', AResult);
    Result := FromJ(msg.AsJSON);
  finally
    msg.Free;
  end;
end;

{ TMcpToolCall }

constructor TMcpToolCall.Create(const AName: string; AArgs: TJSONObject);
begin
  FName := AName;
  if Assigned(AArgs) then
    FArgs := TJSONObject(AArgs.Clone)
  else
    FArgs := TJSONObject.Create;
  FContent := TJSONArray.Create;
end;

destructor TMcpToolCall.Destroy;
begin
  FArgs.Free;
  FContent.Free;
  FStructured.Free;
  inherited Destroy;
end;

function TMcpToolCall.Has(const AName: string): boolean;
var
  d: TJSONData;
begin
  d := FArgs.Find(AName);
  Result := Assigned(d) and (d.JSONType <> jtNull);
end;

function TMcpToolCall.Str(const AName: string; const ADefault: string): string;
begin
  Result := JStr(FArgs, AName, ADefault);
end;

function TMcpToolCall.Int(const AName: string; ADefault: int64): int64;
var
  d: TJSONData;
begin
  Result := ADefault;
  d := FArgs.Find(AName);
  if Assigned(d) then
    case d.JSONType of
      jtNumber:
        if TJSONNumber(d).NumberType = ntFloat then
          Result := Trunc(d.AsFloat)
        else
          Result := d.AsInt64;
      jtString: Result := StrToInt64Def(Trim(d.AsString), ADefault);
    end;
end;

function TMcpToolCall.Float(const AName: string; ADefault: double): double;
var
  d: TJSONData;
  fs: TFormatSettings;
begin
  Result := ADefault;
  d := FArgs.Find(AName);
  if Assigned(d) then
    case d.JSONType of
      jtNumber: Result := d.AsFloat;
      jtString:
      begin
        fs := DefaultFormatSettings;
        fs.DecimalSeparator := '.';
        Result := StrToFloatDef(Trim(d.AsString), ADefault, fs);
      end;
    end;
end;

function TMcpToolCall.Bool(const AName: string; ADefault: boolean): boolean;
var
  d: TJSONData;
begin
  Result := ADefault;
  d := FArgs.Find(AName);
  if Assigned(d) then
    case d.JSONType of
      jtBoolean: Result := d.AsBoolean;
      jtString: Result := SameText(d.AsString, 'true') or (d.AsString = '1');
      jtNumber: Result := d.AsFloat <> 0;
    end;
end;

procedure TMcpToolCall.Finish(const AText: string);
begin
  FContent.Add(TJSONObject.Create(['type', 'text', 'text', TJSONString.Create(ToJ(AText))]));
end;

procedure TMcpToolCall.FinishJSON(AData: TJSONData);
begin
  if not Assigned(AData) then
  begin
    Finish('null');
    Exit;
  end;
  try
    Finish(FromJ(AData.AsJSON));
    if (AData.JSONType = jtObject) and not Assigned(FStructured) then
    begin
      FStructured := TJSONObject(AData);
      AData := nil;
    end;
  finally
    AData.Free;
  end;
end;

procedure TMcpToolCall.Fail(const AMessage: string);
begin
  FIsError := True;
  Finish(AMessage);
end;

function TMcpToolCall.BuildResult: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('content', TJSONArray(FContent.Clone));
  if Assigned(FStructured) then
    Result.Add('structuredContent', TJSONObject(FStructured.Clone));
  Result.Add('isError', FIsError);
end;

{ TMcpServer }

constructor TMcpServer.Create;
begin
  FName := 'fastplaz-mcp';
  FVersion := '1.0.0';
end;

procedure TMcpServer.AddTool(const AName, ATitle, ADescription,
  AInputSchema: string; AHints: TMcpToolHints; AProc: TMcpToolProc;
  AMethod: TMcpToolMethod; const AOutputSchema: string);
var
  schema, output: TJSONObject;
  n: integer;
begin
  if not McpValidToolName(AName) then
    raise EMcpServer.CreateFmt('Invalid MCP tool name "%s"', [AName]);
  if HasTool(AName) then
    raise EMcpServer.CreateFmt('MCP tool "%s" is already registered', [AName]);
  if Trim(AInputSchema) = '' then
    schema := TJSONObject.Create(['type', 'object'])
  else
    schema := ParseObject(AInputSchema);
  if not Assigned(schema) then
    raise EMcpServer.CreateFmt('MCP tool "%s": input schema is not a JSON object', [AName]);
  output := nil;
  try
    if Trim(AOutputSchema) <> '' then
    begin
      output := ParseObject(AOutputSchema);
      if not Assigned(output) then
        raise EMcpServer.CreateFmt('MCP tool "%s": output schema is not a JSON object', [AName]);
    end;
    n := Length(FTools);
    SetLength(FTools, n + 1);
    FTools[n].Name := AName;
    FTools[n].Title := ATitle;
    FTools[n].Description := ADescription;
    FTools[n].InputSchema := FromJ(schema.AsJSON);
    if Assigned(output) then
      FTools[n].OutputSchema := FromJ(output.AsJSON)
    else
      FTools[n].OutputSchema := '';
    FTools[n].Hints := AHints;
    FTools[n].Proc := AProc;
    FTools[n].Method := AMethod;
  finally
    schema.Free;
    output.Free;
  end;
end;

procedure TMcpServer.RegisterTool(const AName, ATitle, ADescription,
  AInputSchema: string; AHandler: TMcpToolProc; AHints: TMcpToolHints;
  const AOutputSchema: string);
begin
  if not Assigned(AHandler) then
    raise EMcpServer.CreateFmt('MCP tool "%s" has no handler', [AName]);
  AddTool(AName, ATitle, ADescription, AInputSchema, AHints, AHandler, nil,
    AOutputSchema);
end;

procedure TMcpServer.RegisterTool(const AName, ATitle, ADescription,
  AInputSchema: string; AHandler: TMcpToolMethod; AHints: TMcpToolHints;
  const AOutputSchema: string);
begin
  if not Assigned(AHandler) then
    raise EMcpServer.CreateFmt('MCP tool "%s" has no handler', [AName]);
  AddTool(AName, ATitle, ADescription, AInputSchema, AHints, nil, AHandler,
    AOutputSchema);
end;

procedure TMcpServer.AddResource(const AUri, AName, ATitle, ADescription,
  AMimeType: string; AProc: TMcpResourceProc; AMethod: TMcpResourceMethod);
var
  i, n: integer;
begin
  if Trim(AUri) = '' then
    raise EMcpServer.Create('MCP resource without URI');
  for i := 0 to High(FResources) do
    if FResources[i].Uri = AUri then
      raise EMcpServer.CreateFmt('MCP resource "%s" is already registered', [AUri]);
  n := Length(FResources);
  SetLength(FResources, n + 1);
  FResources[n].Uri := AUri;
  if AName <> '' then
    FResources[n].Name := AName
  else
    FResources[n].Name := AUri;
  FResources[n].Title := ATitle;
  FResources[n].Description := ADescription;
  FResources[n].MimeType := AMimeType;
  FResources[n].Proc := AProc;
  FResources[n].Method := AMethod;
end;

procedure TMcpServer.RegisterResource(const AUri, AName, ATitle,
  ADescription, AMimeType: string; AHandler: TMcpResourceProc);
begin
  if not Assigned(AHandler) then
    raise EMcpServer.CreateFmt('MCP resource "%s" has no handler', [AUri]);
  AddResource(AUri, AName, ATitle, ADescription, AMimeType, AHandler, nil);
end;

procedure TMcpServer.RegisterResource(const AUri, AName, ATitle,
  ADescription, AMimeType: string; AHandler: TMcpResourceMethod);
begin
  if not Assigned(AHandler) then
    raise EMcpServer.CreateFmt('MCP resource "%s" has no handler', [AUri]);
  AddResource(AUri, AName, ATitle, ADescription, AMimeType, nil, AHandler);
end;

procedure TMcpServer.Clear;
begin
  SetLength(FTools, 0);
  SetLength(FResources, 0);
end;

function TMcpServer.ToolCount: integer;
begin
  Result := Length(FTools);
end;

function TMcpServer.ResourceCount: integer;
begin
  Result := Length(FResources);
end;

function TMcpServer.HasTool(const AName: string): boolean;
var
  tool: TMcpTool;
begin
  Result := FindTool(AName, tool);
end;

function TMcpServer.FindTool(const AName: string; out ATool: TMcpTool): boolean;
var
  i: integer;
begin
  for i := 0 to High(FTools) do
    if FTools[i].Name = AName then
    begin
      ATool := FTools[i];
      Exit(True);
    end;
  ATool := Default(TMcpTool);
  Result := False;
end;

procedure TMcpServer.ReportError(const AContext: string; E: Exception);
begin
  if Assigned(FOnError) then
    try
      FOnError(Self, AContext, E);
    except
      // a failing logger must not break the answer
    end;
end;

function TMcpServer.OriginAllowed(const AOrigin, AHost: string): boolean;
var
  host: string;
  list: TStringList;
  i: integer;
begin
  if AOrigin = '' then
    Exit(True); // not a browser
  if Trim(FAllowedOrigins) = '*' then
    Exit(True);
  host := McpHostOf(AOrigin);
  if (host = '') or (host = 'null') then
    Exit(False);
  if (host = '127.0.0.1') or (host = 'localhost') or (host = '[::1]') then
    Exit(True);
  if (AHost <> '') and (host = McpHostOf(AHost)) then
    Exit(True);
  Result := False;
  list := TStringList.Create;
  try
    list.Delimiter := ',';
    list.StrictDelimiter := True;
    list.DelimitedText := StringReplace(Trim(FAllowedOrigins), ' ', ',', [rfReplaceAll]);
    for i := 0 to list.Count - 1 do
      if (Trim(list[i]) <> '') and (McpHostOf(list[i]) = host) then
        Exit(True);
  finally
    list.Free;
  end;
end;

function TMcpServer.DoInitialize(AParams: TJSONObject): TJSONObject;
var
  requested, negotiated: string;
  caps, info: TJSONObject;
begin
  requested := JStr(AParams, 'protocolVersion');
  if McpKnownProtocol(requested) then
    negotiated := requested
  else
    negotiated := MCP_LATEST_PROTOCOL;

  caps := TJSONObject.Create;
  caps.Add('tools', TJSONObject.Create(['listChanged', False]));
  if Length(FResources) > 0 then
    caps.Add('resources', TJSONObject.Create(['subscribe', False, 'listChanged', False]));
  info := TJSONObject.Create;
  info.Add('name', ToJ(FName));
  if FTitle <> '' then
    info.Add('title', ToJ(FTitle));
  info.Add('version', ToJ(FVersion));
  Result := TJSONObject.Create;
  Result.Add('protocolVersion', negotiated);
  Result.Add('capabilities', caps);
  Result.Add('serverInfo', info);
  if FInstructions <> '' then
    Result.Add('instructions', ToJ(FInstructions));
end;

function TMcpServer.DoToolsList: TJSONObject;
var
  list: TJSONArray;
  item, annotations: TJSONObject;
  i: integer;
begin
  list := TJSONArray.Create;
  for i := 0 to High(FTools) do
  begin
    item := TJSONObject.Create;
    item.Add('name', ToJ(FTools[i].Name));
    if FTools[i].Title <> '' then
      item.Add('title', ToJ(FTools[i].Title));
    item.Add('description', ToJ(FTools[i].Description));
    item.Add('inputSchema', GetJSON(ToJ(FTools[i].InputSchema)));
    if FTools[i].OutputSchema <> '' then
      item.Add('outputSchema', GetJSON(ToJ(FTools[i].OutputSchema)));
    if FTools[i].Hints <> [] then
    begin
      annotations := TJSONObject.Create;
      if FTools[i].Title <> '' then
        annotations.Add('title', ToJ(FTools[i].Title));
      annotations.Add('readOnlyHint', mthReadOnly in FTools[i].Hints);
      annotations.Add('destructiveHint', mthDestructive in FTools[i].Hints);
      annotations.Add('idempotentHint', mthIdempotent in FTools[i].Hints);
      annotations.Add('openWorldHint', mthOpenWorld in FTools[i].Hints);
      item.Add('annotations', annotations);
    end;
    list.Add(item);
  end;
  Result := TJSONObject.Create(['tools', list]);
end;

function TMcpServer.DoToolsCall(AParams: TJSONObject; out AErrorCode: integer;
  out AErrorMessage: string): TJSONObject;
var
  toolName: string;
  tool: TMcpTool;
  args: TJSONData;
  call: TMcpToolCall;
begin
  Result := nil;
  AErrorCode := 0;
  AErrorMessage := '';
  toolName := JStr(AParams, 'name');
  if toolName = '' then
  begin
    AErrorCode := JSONRPC_INVALID_PARAMS;
    AErrorMessage := 'Missing tool name';
    Exit;
  end;
  if not FindTool(toolName, tool) then
  begin
    AErrorCode := JSONRPC_INVALID_PARAMS;
    AErrorMessage := 'Unknown tool: ' + toolName;
    Exit;
  end;
  args := AParams.Find('arguments');
  if Assigned(args) and not (args.JSONType in [jtObject, jtNull]) then
  begin
    AErrorCode := JSONRPC_INVALID_PARAMS;
    AErrorMessage := 'Tool arguments must be an object';
    Exit;
  end;
  if Assigned(args) and (args.JSONType = jtNull) then
    args := nil;

  call := TMcpToolCall.Create(toolName, TJSONObject(args));
  try
    try
      if Assigned(tool.Method) then
        tool.Method(call)
      else
        tool.Proc(call);
    except
      on E: EMcpToolError do
        call.Fail(E.Message);
      on E: Exception do
      begin
        ReportError('tool ' + toolName, E);
        if FExceptionDetails then
          call.Fail(E.ClassName + ': ' + E.Message)
        else
          call.Fail('Internal error in tool ' + toolName);
      end;
    end;
    Result := call.BuildResult;
  finally
    call.Free;
  end;
end;

function TMcpServer.DoResourcesList: TJSONObject;
var
  list: TJSONArray;
  item: TJSONObject;
  i: integer;
begin
  list := TJSONArray.Create;
  for i := 0 to High(FResources) do
  begin
    item := TJSONObject.Create;
    item.Add('uri', ToJ(FResources[i].Uri));
    item.Add('name', ToJ(FResources[i].Name));
    if FResources[i].Title <> '' then
      item.Add('title', ToJ(FResources[i].Title));
    if FResources[i].Description <> '' then
      item.Add('description', ToJ(FResources[i].Description));
    if FResources[i].MimeType <> '' then
      item.Add('mimeType', ToJ(FResources[i].MimeType));
    list.Add(item);
  end;
  Result := TJSONObject.Create(['resources', list]);
end;

function TMcpServer.DoResourcesRead(AParams: TJSONObject;
  out AErrorCode: integer; out AErrorMessage: string): TJSONObject;
var
  uri, text: string;
  i: integer;
  ok: boolean;
  content: TJSONObject;
begin
  Result := nil;
  AErrorCode := 0;
  AErrorMessage := '';
  uri := JStr(AParams, 'uri');
  for i := 0 to High(FResources) do
    if FResources[i].Uri = uri then
    begin
      text := '';
      try
        if Assigned(FResources[i].Method) then
          ok := FResources[i].Method(uri, text)
        else
          ok := FResources[i].Proc(uri, text);
      except
        on E: Exception do
        begin
          ReportError('resource ' + uri, E);
          ok := False;
          if FExceptionDetails then
            text := E.ClassName + ': ' + E.Message
          else
            text := 'internal error';
        end;
      end;
      if not ok then
      begin
        AErrorCode := JSONRPC_INTERNAL_ERROR;
        AErrorMessage := 'Resource cannot be read: ' + uri;
        if text <> '' then
          AErrorMessage := AErrorMessage + ': ' + text;
        Exit;
      end;
      content := TJSONObject.Create;
      content.Add('uri', ToJ(uri));
      if FResources[i].MimeType <> '' then
        content.Add('mimeType', ToJ(FResources[i].MimeType));
      content.Add('text', ToJ(text));
      Result := TJSONObject.Create(['contents', TJSONArray.Create([content])]);
      Exit;
    end;
  AErrorCode := MCP_RESOURCE_NOT_FOUND;
  AErrorMessage := 'Resource not found: ' + uri;
end;

function TMcpServer.HandleMessage(const ABody: string;
  out AResponse: string): TMcpMessageKind;
var
  data, idData, paramsData: TJSONData;
  msg, params, answer: TJSONObject;
  method, idJSON, errorMessage: string;
  errorCode: integer;
begin
  AResponse := '';
  try
    data := GetJSON(ToJ(ABody));
  except
    data := nil;
  end;
  if not Assigned(data) then // GetJSON('') gives nil
  begin
    AResponse := McpErrorJSON('null', JSONRPC_PARSE_ERROR, 'Parse error');
    Exit(mmkInvalid);
  end;
  try
    if data.JSONType <> jtObject then
    begin
      AResponse := McpErrorJSON('null', JSONRPC_INVALID_REQUEST,
        'Expected one JSON-RPC message object (batches are not supported)');
      Exit(mmkInvalid);
    end;
    msg := TJSONObject(data);
    idData := msg.Find('id');
    if Assigned(idData) and not (idData.JSONType in [jtString, jtNumber]) then
    begin
      AResponse := McpErrorJSON('null', JSONRPC_INVALID_REQUEST,
        'The id must be a string or a number');
      Exit(mmkInvalid);
    end;
    if Assigned(idData) then
      idJSON := FromJ(idData.AsJSON)
    else
      idJSON := 'null';
    if JStr(msg, 'jsonrpc') <> '2.0' then
    begin
      AResponse := McpErrorJSON(idJSON, JSONRPC_INVALID_REQUEST,
        'Not a JSON-RPC 2.0 message');
      Exit(mmkInvalid);
    end;
    method := JStr(msg, 'method');
    if method = '' then
    begin
      // a response from the client (result or error with an id): accepted,
      // this server sends no requests of its own
      if Assigned(idData) and
        (Assigned(msg.Find('result')) or Assigned(msg.Find('error'))) then
        Exit(mmkNotification);
      AResponse := McpErrorJSON(idJSON, JSONRPC_INVALID_REQUEST, 'Missing method');
      Exit(mmkInvalid);
    end;
    if not Assigned(idData) then
      Exit(mmkNotification); // notifications/initialized, cancelled, ...

    paramsData := msg.Find('params');
    if Assigned(paramsData) and not (paramsData.JSONType in [jtObject, jtNull]) then
    begin
      AResponse := McpErrorJSON(idJSON, JSONRPC_INVALID_PARAMS,
        'Params must be an object');
      Exit(mmkRequest);
    end;
    params := JObj(msg, 'params');

    Result := mmkRequest;
    answer := nil;
    errorCode := 0;
    errorMessage := '';
    case method of
      'initialize':
        answer := DoInitialize(params);
      'ping':
        answer := TJSONObject.Create;
      'tools/list':
        answer := DoToolsList;
      'tools/call':
        if Assigned(params) then
          answer := DoToolsCall(params, errorCode, errorMessage)
        else
        begin
          errorCode := JSONRPC_INVALID_PARAMS;
          errorMessage := 'Missing params';
        end;
      'resources/list':
        answer := DoResourcesList;
      'resources/read':
        answer := DoResourcesRead(params, errorCode, errorMessage);
      'resources/templates/list':
        answer := TJSONObject.Create(['resourceTemplates', TJSONArray.Create]);
      'prompts/list':
        answer := TJSONObject.Create(['prompts', TJSONArray.Create]);
      else
      begin
        errorCode := JSONRPC_METHOD_NOT_FOUND;
        errorMessage := 'Method not found: ' + method;
      end;
    end;
    if Assigned(answer) then
      AResponse := ResultJSON(idJSON, answer)
    else
      AResponse := McpErrorJSON(idJSON, errorCode, errorMessage);
  finally
    data.Free;
  end;
end;

// Compares in a time that depends on the lengths only.
function SameSecret(const A, B: string): boolean;
var
  i: integer;
  diff: byte;
begin
  diff := Ord(Length(A) <> Length(B));
  for i := 1 to Length(A) do
    if i <= Length(B) then
      diff := diff or (Ord(A[i]) xor Ord(B[i]));
  Result := diff = 0;
end;

function HttpAnswer(AStatus: integer; const AContentType, ABody: string;
  const AHeader: string = ''): TMcpHttpResponse;
begin
  Result := Default(TMcpHttpResponse);
  Result.Status := AStatus;
  Result.ContentType := AContentType;
  Result.Body := ABody;
  if AHeader <> '' then
  begin
    SetLength(Result.Headers, 1);
    Result.Headers[0] := AHeader;
  end;
end;

function TMcpServer.HandleHttp(const ARequest: TMcpHttpRequest): TMcpHttpResponse;
var
  response: string;
begin
  if not OriginAllowed(ARequest.Origin, ARequest.Host) then
    Exit(HttpAnswer(403, 'text/plain', 'Origin not allowed'));
  if (FToken <> '') and not SameSecret(ARequest.Authorization, 'Bearer ' + FToken) then
    Exit(HttpAnswer(401, 'text/plain', 'Unauthorized', 'WWW-Authenticate: Bearer'));
  // no server-initiated SSE stream (GET) and no session to end (DELETE)
  if not SameText(ARequest.Method, 'POST') then
    Exit(HttpAnswer(405, 'text/plain', 'Method not allowed', 'Allow: POST'));
  if (ARequest.ProtocolVersion <> '') and
    not McpKnownProtocol(Trim(ARequest.ProtocolVersion)) then
    Exit(HttpAnswer(400, 'application/json', McpErrorJSON('null',
      JSONRPC_INVALID_REQUEST, 'Unsupported MCP-Protocol-Version: ' +
      ARequest.ProtocolVersion)));
  case HandleMessage(ARequest.Body, response) of
    mmkRequest:
      Result := HttpAnswer(200, 'application/json', response);
    mmkNotification:
      Result := HttpAnswer(202, 'text/plain', '');
    else
      Result := HttpAnswer(400, 'application/json', response);
  end;
end;

initialization
  GlobalServer := TMcpServer.Create;

finalization
  FreeAndNil(GlobalServer);

end.

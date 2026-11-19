unit mcp_handler;

(*
  FastPlaz controller for the MCP endpoint: connects mcp_server (the
  protocol core) to a FastPlaz request, in CGI and in standalone mode.

  Route it, and register tools in an initialization section:

    Route.Add('mcp', '^/mcp/?$', TMcpController);
    McpServer.RegisterTool('echo', 'Echo', 'Echoes text', '', @DoEcho);

  or derive a class (TMyMcpController = class(TMcpController)) to serve
  another TMcpServer instance (property Server) or to add CORS headers.

  Headers are read through fastplaz_app.ServerVariable (HTTP_ORIGIN,
  HTTP_HOST, HTTP_AUTHORIZATION, HTTP_MCP_PROTOCOL_VERSION), which gives the
  same values in CGI and standalone mode. Apache drops the Authorization
  header for CGI programs unless "CGIPassAuth On" is set; the rewrite trick
  (REDIRECT_HTTP_AUTHORIZATION) is read too.

  config/config.json, applied once to the server on the first request;
  only keys that are present change what the code set:

    "mcp": {
      "token": "env:MCP_TOKEN",          // or the token itself; "" = no auth
      "allowed_origins": "app.example.com",
      "exception_details": false
    }
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsonConf, fastplaz_handler, mcp_server;

type
  { reads a CGI variable such as HTTP_ORIGIN }
  TMcpVariableFunc = function(const AName: string): string;

  { TMcpController }

  TMcpController = class(TMyCustomController)
  private
    FServer: TMcpServer;
  protected
    procedure Serve; virtual;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    procedure Get; override;
    procedure Post; override;
    procedure Put; override;
    procedure Delete; override;
    procedure Patch; override;
    procedure Options; override;
    { The server this endpoint serves; McpServer unless set in the
      constructor of a derived class. }
    property Server: TMcpServer read FServer write FServer;
  end;

{ The parts of the current request MCP needs, from CGI variables. }
function McpRequestFromVariables(const AMethod, ABody: string;
  AVariable: TMcpVariableFunc): TMcpHttpRequest;
{ "Allow: POST" -> "Allow", "POST". False when there is no colon. }
function McpSplitHeader(const AHeader: string; out AName, AValue: string): boolean;
function McpStatusText(AStatus: integer): string;
{ Applies the "mcp" section of AConfig to AServer (see the unit header).
  A token "env:NAME" is read from the environment variable NAME. }
procedure McpLoadConfig(AServer: TMcpServer; AConfig: TJSONConfig);

implementation

uses
  common, fastplaz_app;

var
  ConfigLock: TRTLCriticalSection;
  ConfiguredServers: TList = nil;

function McpRequestFromVariables(const AMethod, ABody: string;
  AVariable: TMcpVariableFunc): TMcpHttpRequest;
begin
  Result := Default(TMcpHttpRequest);
  Result.Method := AMethod;
  Result.Body := ABody;
  Result.Host := AVariable('HTTP_HOST');
  Result.Origin := AVariable('HTTP_ORIGIN');
  Result.Authorization := AVariable('HTTP_AUTHORIZATION');
  if Result.Authorization = '' then
    Result.Authorization := AVariable('REDIRECT_HTTP_AUTHORIZATION');
  Result.ProtocolVersion := AVariable('HTTP_MCP_PROTOCOL_VERSION');
end;

function McpSplitHeader(const AHeader: string; out AName, AValue: string): boolean;
var
  p: integer;
begin
  p := Pos(':', AHeader);
  Result := p > 1;
  if Result then
  begin
    AName := Trim(Copy(AHeader, 1, p - 1));
    AValue := Trim(Copy(AHeader, p + 1, MaxInt));
  end
  else
  begin
    AName := '';
    AValue := '';
  end;
end;

function McpStatusText(AStatus: integer): string;
begin
  case AStatus of
    200: Result := 'OK';
    202: Result := 'Accepted';
    204: Result := 'No Content';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    500: Result := 'Internal Server Error';
    else
      Result := '';
  end;
end;

procedure McpLoadConfig(AServer: TMcpServer; AConfig: TJSONConfig);
var
  token, origins: string;
begin
  if not Assigned(AServer) or not Assigned(AConfig) then
    Exit;
  token := Trim(string(AConfig.GetValue('mcp/token', '')));
  if SameText(Copy(token, 1, 4), 'env:') then
    token := Trim(GetEnvironmentVariable(Trim(Copy(token, 5, MaxInt))));
  if token <> '' then
    AServer.Token := token;
  origins := Trim(string(AConfig.GetValue('mcp/allowed_origins', '')));
  if origins <> '' then
    AServer.AllowedOrigins := origins;
  AServer.ExceptionDetails :=
    AConfig.GetValue('mcp/exception_details', AServer.ExceptionDetails);
end;

// Once per server and process; requests may run in several threads.
procedure ApplyConfigOnce(AServer: TMcpServer);
begin
  EnterCriticalSection(ConfigLock);
  try
    if ConfiguredServers.IndexOf(AServer) >= 0 then
      Exit;
    ConfiguredServers.Add(AServer);
    try
      McpLoadConfig(AServer, Config);
    except
      // a broken config must not take the endpoint down; values from code stay
    end;
  finally
    LeaveCriticalSection(ConfigLock);
  end;
end;

{ TMcpController }

constructor TMcpController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FServer := McpServer;
end;

procedure TMcpController.Serve;
var
  answer: TMcpHttpResponse;
  i: integer;
  headerName, headerValue: string;
begin
  if not Assigned(FServer) then
    FServer := McpServer;
  ApplyConfigOnce(FServer);
  answer := FServer.HandleHttp(McpRequestFromVariables(Request.Method,
    Request.Content, @ServerVariable));
  Response.Code := answer.Status;
  Response.CodeText := McpStatusText(answer.Status);
  if answer.ContentType <> '' then
    Response.ContentType := answer.ContentType;
  for i := 0 to High(answer.Headers) do
    if McpSplitHeader(answer.Headers[i], headerName, headerValue) then
      Response.SetCustomHeader(headerName, headerValue);
  Response.Content := answer.Body;
end;

procedure TMcpController.Get;
begin
  Serve;
end;

procedure TMcpController.Post;
begin
  Serve;
end;

procedure TMcpController.Put;
begin
  Serve;
end;

procedure TMcpController.Delete;
begin
  Serve;
end;

procedure TMcpController.Patch;
begin
  Serve;
end;

procedure TMcpController.Options;
begin
  Serve;
end;

initialization
  InitCriticalSection(ConfigLock);
  ConfiguredServers := TList.Create;

finalization
  FreeAndNil(ConfiguredServers);
  DoneCriticalSection(ConfigLock);

end.

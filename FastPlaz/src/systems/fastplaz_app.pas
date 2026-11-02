unit fastplaz_app;

{
  Application wrapper: one binary runs either as CGI or as a standalone HTTP
  server, chosen at runtime.

  Mode, in order of priority:
    1. command line   --mode=cgi|http  --port=8080  --address=127.0.0.1
                      --root=<dir>  (working directory, default: the
                      executable's directory in standalone mode)
    2. GATEWAY_INTERFACE set by the web server -> CGI
    3. config/config.json  systems/server/mode, port, address
    4. default: HTTP on 127.0.0.1:8080

  Use this unit instead of fpcgi. "Application" keeps the CGI style
  Request / Response / EnvironmentVariable members (class helper below), so
  code written for fpcgi compiles unchanged; they always refer to the request
  being handled, whatever the mode.

  A program that still "uses fpcgi" keeps working as CGI: its application is
  the one that receives the requests, and Application returns it while a
  request is being handled.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, custweb, custcgi, custhttpapp;

const
  FASTPLAZ_HTTP_PORT_DEFAULT = 8080;
  FASTPLAZ_HTTP_ADDRESS_DEFAULT = '127.0.0.1';

type
  TFastPlazAppMode = (amCGI, amHTTP);

  TFastPlazRequestEvent = procedure(ARequest: TRequest; AResponse: TResponse);

  { TFastPlazApplicationHelper }

  TFastPlazApplicationHelper = class helper for TCustomWebApplication
  private
    function GetCurrentRequest: TRequest;
    function GetCurrentResponse: TResponse;
    function GetServerVariable(const AName: string): string;
  public
    // the request / response being handled (nil outside a request)
    property Request: TRequest read GetCurrentRequest;
    property Response: TResponse read GetCurrentResponse;
    // CGI variable of the current request (REMOTE_ADDR, HTTP_USER_AGENT, ...)
    property EnvironmentVariable[const AName: string]: string read GetServerVariable;
  end;

  { TFastPlazCGIHandler }

  TFastPlazCGIHandler = class(TCGIHandler)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TFastPlazHTTPHandler }

  TFastPlazHTTPHandler = class(TFPHTTPServerHandler)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TFastPlazHTTPApplication }

  TFastPlazHTTPApplication = class(TCustomHTTPApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

// the web application: the one handling the current request, otherwise the
// one created for the selected mode
function Application: TCustomWebApplication;

function AppMode: TFastPlazAppMode;
// True when running as a long-lived HTTP server (state survives requests)
function IsStandalone: boolean;
function HTTPPort: integer;
function HTTPAddress: string;

function CurrentRequest: TRequest;
function CurrentResponse: TResponse;

// CGI variable of the current request. In CGI mode this is the process
// environment; in standalone mode it is derived from the request, and names
// that are not request related (TEMP, PATH, ...) still come from the process.
function ServerVariable(const AName: string): string;

// "X-Requested-With" <-> "HTTP_X_REQUESTED_WITH"
function CGINameToHeaderName(const AName: string): string;

// call after the response has been sent: ends the process in CGI mode, does
// nothing in standalone mode (the server keeps running)
procedure StopRequest;

var
  // called around every request, in every mode
  OnBeginRequest: TFastPlazRequestEvent = nil;
  OnEndRequest: TFastPlazRequestEvent = nil;

implementation

uses
  CustApp, fpjson, jsonparser, jsonscanner;

const
  CONFIG_FILE = 'config' + DirectorySeparator + 'config.json';

var
  FApp: TCustomWebApplication = nil;       // created here, freed here
  FRunningApp: TCustomWebApplication = nil; // handling the current request
  FModeResolved: boolean = False;
  FMode: TFastPlazAppMode = amHTTP;
  FPort: integer = FASTPLAZ_HTTP_PORT_DEFAULT;
  FAddress: string = FASTPLAZ_HTTP_ADDRESS_DEFAULT;
  FRequest: TRequest = nil;
  FResponse: TResponse = nil;

function CommandLineValue(const AName: string; out AValue: string): boolean;
var
  i: integer;
  prefix: string;
begin
  Result := False;
  AValue := '';
  prefix := '--' + AName + '=';
  for i := 1 to ParamCount do
    if SameText(Copy(ParamStr(i), 1, Length(prefix)), prefix) then
    begin
      AValue := Copy(ParamStr(i), Length(prefix) + 1, MaxInt);
      Result := True;
    end;
end;

function ParseMode(const AValue: string; var AMode: TFastPlazAppMode): boolean;
begin
  Result := True;
  if SameText(AValue, 'cgi') then
    AMode := amCGI
  else if SameText(AValue, 'http') then
    AMode := amHTTP
  else
    Result := False;
end;

// systems/server from config/config.json; a missing or broken file is not
// an error here, common.pas reports it
procedure ReadServerConfig(out AMode: string; out APort: integer;
  out AAddress: string);
var
  lst: TStringList;
  parser: TJSONParser;
  root: TJSONData;
  server: TJSONData;
begin
  AMode := '';
  APort := 0;
  AAddress := '';
  if not FileExists(CONFIG_FILE) then
    Exit;
  root := nil;
  lst := TStringList.Create;
  try
    try
      lst.LoadFromFile(CONFIG_FILE);
      parser := TJSONParser.Create(lst.Text, [joUTF8, joComments, joIgnoreTrailingComma]);
      try
        root := parser.Parse;
      finally
        parser.Free;
      end;
      if not (root is TJSONObject) then
        Exit;
      server := TJSONObject(root).FindPath('systems.server');
      if not (server is TJSONObject) then
        Exit;
      AMode := TJSONObject(server).Get('mode', '');
      APort := TJSONObject(server).Get('port', 0);
      AAddress := TJSONObject(server).Get('address', '');
    except
      // ignored, see above
    end;
  finally
    root.Free;
    lst.Free;
  end;
end;

procedure ResolveMode;
var
  s, cfgMode, cfgAddress: string;
  cfgPort: integer;
  modeSet: boolean;
begin
  if FModeResolved then
    Exit;
  FModeResolved := True;

  ReadServerConfig(cfgMode, cfgPort, cfgAddress);
  if cfgPort > 0 then
    FPort := cfgPort;
  if cfgAddress <> '' then
    FAddress := cfgAddress;

  modeSet := CommandLineValue('mode', s) and ParseMode(s, FMode);
  if not modeSet then
  begin
    if GetEnvironmentVariable('GATEWAY_INTERFACE') <> '' then
    begin
      FMode := amCGI;
      modeSet := True;
    end;
  end;
  if not modeSet then
    modeSet := ParseMode(cfgMode, FMode);
  if not modeSet then
    FMode := amHTTP;

  if CommandLineValue('port', s) and (StrToIntDef(s, 0) > 0) then
    FPort := StrToInt(s);
  if CommandLineValue('address', s) and (s <> '') then
    FAddress := s;
end;

function AppMode: TFastPlazAppMode;
begin
  if FRunningApp <> nil then
  begin
    if FRunningApp is TCustomHTTPApplication then
      Exit(amHTTP)
    else
      Exit(amCGI);
  end;
  ResolveMode;
  Result := FMode;
end;

function IsStandalone: boolean;
begin
  Result := AppMode = amHTTP;
end;

function HTTPPort: integer;
begin
  ResolveMode;
  Result := FPort;
end;

function HTTPAddress: string;
begin
  ResolveMode;
  Result := FAddress;
end;

function Application: TCustomWebApplication;
var
  http: TFastPlazHTTPApplication;
begin
  if FRunningApp <> nil then
    Exit(FRunningApp);
  if FApp = nil then
  begin
    ResolveMode;
    // an application created by fpcgi (old style .lpr) is the one that runs
    if (FMode = amCGI) and (CustomApplication is TCustomWebApplication) then
      Exit(TCustomWebApplication(CustomApplication));
    if FMode = amCGI then
      FApp := TCustomCGIApplication.Create(nil)
    else
    begin
      http := TFastPlazHTTPApplication.Create(nil);
      http.Port := FPort;
      http.Address := FAddress;
      http.Threaded := False;
      FApp := http;
    end;
    // keep an application created by fpcgi registered: it is the one that
    // handles requests if the .lpr runs it
    if CustomApplication = nil then
      CustomApplication := FApp;
  end;
  Result := FApp;
end;

function CurrentRequest: TRequest;
begin
  Result := FRequest;
  // fpcgi application whose handler is not ours (should not happen, the
  // handler class is replaced below, kept as a safety net)
  if (Result = nil) and (CustomApplication is TCustomCGIApplication) then
    Result := TCustomCGIApplication(CustomApplication).Request;
end;

function CurrentResponse: TResponse;
begin
  Result := FResponse;
  if (Result = nil) and (CustomApplication is TCustomCGIApplication) then
    Result := TCustomCGIApplication(CustomApplication).Response;
end;

function CGINameToHeaderName(const AName: string): string;
var
  i: integer;
  upNext: boolean;
begin
  Result := UpperCase(Trim(AName));
  if Copy(Result, 1, 5) = 'HTTP_' then
    Delete(Result, 1, 5);
  Result := LowerCase(StringReplace(Result, '_', '-', [rfReplaceAll]));
  upNext := True;
  for i := 1 to Length(Result) do
  begin
    if upNext then
      Result[i] := UpCase(Result[i]);
    upNext := Result[i] = '-';
  end;
end;

function HostWithoutPort(const AHost: string): string;
var
  i: integer;
begin
  Result := AHost;
  if (Result <> '') and (Result[1] = '[') then
  begin
    // [::1]:8080
    i := Pos(']', Result);
    if i > 0 then
      Result := Copy(Result, 1, i);
    Exit;
  end;
  i := Pos(':', Result);
  if i > 0 then
    Result := Copy(Result, 1, i - 1);
end;

function ServerVariable(const AName: string): string;
var
  r: TRequest;
  n: string;
begin
  r := CurrentRequest;
  if (r = nil) or (not IsStandalone) then
    Exit(GetEnvironmentVariable(AName));

  n := UpperCase(Trim(AName));
  case n of
    'REQUEST_METHOD': Result := r.Method;
    'QUERY_STRING': Result := r.QueryString;
    'REQUEST_URI': Result := r.URI;
    'PATH_INFO': Result := r.PathInfo;
    'SCRIPT_NAME': Result := r.ScriptName;
    'SCRIPT_FILENAME': Result := ParamStr(0);
    'DOCUMENT_ROOT': Result := ExcludeTrailingPathDelimiter(GetCurrentDir);
    'REMOTE_ADDR': Result := r.RemoteAddr;
    'REMOTE_HOST': Result := r.RemoteHost;
    'SERVER_NAME': Result := HostWithoutPort(r.Host);
    'SERVER_PORT': Result := IntToStr(r.ServerPort);
    'SERVER_PROTOCOL':
    begin
      Result := r.ProtocolVersion;
      if (Result <> '') and (Pos('HTTP', UpperCase(Result)) <> 1) then
        Result := 'HTTP/' + Result;
    end;
    'SERVER_SOFTWARE': Result := 'FastPlaz';
    'GATEWAY_INTERFACE': Result := '';
    'CONTENT_TYPE': Result := r.ContentType;
    'CONTENT_LENGTH':
    begin
      Result := '';
      if r.ContentLength > 0 then
        Result := IntToStr(r.ContentLength);
    end;
    else
      if Copy(n, 1, 5) = 'HTTP_' then
        Result := r.GetFieldByName(CGINameToHeaderName(n))
      else
        Result := GetEnvironmentVariable(AName);
  end;
end;

procedure StopRequest;
begin
  if not IsStandalone then
    Application.Terminate;
end;

procedure BeginRequest(AHandler: TWebHandler; ARequest: TRequest;
  AResponse: TResponse);
begin
  if AHandler.Owner is TCustomWebApplication then
    FRunningApp := TCustomWebApplication(AHandler.Owner);
  FRequest := ARequest;
  FResponse := AResponse;
  if Assigned(OnBeginRequest) then
    OnBeginRequest(ARequest, AResponse);
end;

procedure EndRequest(ARequest: TRequest; AResponse: TResponse);
begin
  try
    if Assigned(OnEndRequest) then
      OnEndRequest(ARequest, AResponse);
  finally
    FRequest := nil;
    FResponse := nil;
  end;
end;

{ TFastPlazApplicationHelper }

function TFastPlazApplicationHelper.GetCurrentRequest: TRequest;
begin
  Result := CurrentRequest;
end;

function TFastPlazApplicationHelper.GetCurrentResponse: TResponse;
begin
  Result := CurrentResponse;
end;

function TFastPlazApplicationHelper.GetServerVariable(const AName: string): string;
begin
  Result := ServerVariable(AName);
end;

{ TFastPlazCGIHandler }

procedure TFastPlazCGIHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  BeginRequest(Self, ARequest, AResponse);
  try
    inherited HandleRequest(ARequest, AResponse);
  finally
    EndRequest(ARequest, AResponse);
  end;
end;

{ TFastPlazHTTPHandler }

procedure TFastPlazHTTPHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  BeginRequest(Self, ARequest, AResponse);
  try
    inherited HandleRequest(ARequest, AResponse);
  finally
    EndRequest(ARequest, AResponse);
  end;
end;

{ TFastPlazHTTPApplication }

function TFastPlazHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TFastPlazHTTPHandler.Create(Self);
end;

procedure ChangeToRootDir;
var
  dir: string;
begin
  // config/, themes/, ztemp/ are relative to the working directory. A web
  // server starts a CGI program in its own directory, a standalone server
  // may be started from anywhere: --root wins, otherwise move to the
  // executable's directory when the config is there and not here.
  if CommandLineValue('root', dir) and (dir <> '') then
  begin
    if DirectoryExists(dir) then
      SetCurrentDir(dir);
    Exit;
  end;
  if GetEnvironmentVariable('GATEWAY_INTERFACE') <> '' then
    Exit;
  if FileExists(CONFIG_FILE) then
    Exit;
  dir := ExtractFilePath(ParamStr(0));
  if (dir <> '') and FileExists(IncludeTrailingPathDelimiter(dir) + CONFIG_FILE) then
    SetCurrentDir(dir);
end;

initialization
  // every CGI application, ours or one created by fpcgi, gets a handler that
  // tracks the current request
  CGIWebHandlerClass := TFastPlazCGIHandler;
  ChangeToRootDir;

finalization
  if (FApp <> nil) and (CustomApplication = FApp) then
    CustomApplication := nil;
  FreeAndNil(FApp);

end.

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

  // what the standalone banner shows, see ShowStandaloneBanner
  TStandaloneBannerInfo = record
    Title: string;        // Application.Title
    URL: string;          // http://address:port/
    Address: string;
    Port: integer;
    Development: boolean; // config.json systems/development
    RootDir: string;      // working directory (config/, themes/, ...)
  end;

  TStandaloneBannerProc = procedure(const AInfo: TStandaloneBannerInfo);

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
    // installs the Ctrl+C / SIGTERM handlers once the server really runs
    procedure DoRun; override;
  end;

// the web application: the one handling the current request, otherwise the
// one created for the selected mode
function Application: TCustomWebApplication;

// the mode rule: --mode (when valid), then CGI when the web server set
// GATEWAY_INTERFACE, then systems/server/mode (when valid), else HTTP
function SelectAppMode(const ACommandLineMode, AGatewayInterface,
  AConfigMode: string): TFastPlazAppMode;

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
// the CGI variable AName derived from ARequest (standalone rules)
function RequestServerVariable(ARequest: TRequest; const AName: string): string;

// "X-Requested-With" <-> "HTTP_X_REQUESTED_WITH"
function CGINameToHeaderName(const AName: string): string;

// call after the response has been sent: ends the process in CGI mode, does
// nothing in standalone mode (the server keeps running)
procedure StopRequest;

// standalone mode: serve ARequest from a file under the working directory
// when it names one (theme assets, images, ...). Only asset extensions are
// served; dot files, ".." and ztemp/ never are. False when the request is
// not for such a file and must go to the application.
function ServeStaticFile(ARequest: TRequest; AResponse: TResponse): boolean;
// the relative file ServeStaticFile would send for a URL path, '' if none
function StaticFileForPath(const AURLPath: string): string;
// systems/server/static or --static=on|off (default on)
function StaticFilesEnabled: boolean;
// systems/trust_proxy (default off): in standalone mode HTTPS, HTTP_HOST,
// SERVER_NAME and SERVER_PORT then follow X-Forwarded-Proto/-Host/-Port.
// Only enable it when the server is reachable through the proxy alone.
function TrustProxy: boolean;
procedure SetTrustProxy(AValue: boolean);
// systems/server/max_requests or --max-requests=N: a standalone server stops
// after N requests (0 = never), so a process manager can start a fresh one
function MaxRequests: integer;
// requests handled by this process
function RequestCount: integer;

// standalone mode: stop accepting and let Application.Run return after the
// current request; safe to call from a signal handler (Ctrl+C, SIGTERM)
procedure RequestShutdown;

// Startup banner of the standalone server, written to the console: the
// FASTPLAZ logo, the local URL, mode (development/production), working
// directory and FPC version.
//
// Does nothing in CGI mode (the output would become the response). Call it
// from the .lpr after Application.Title is set. To change the look, assign
// OnStandaloneBanner (it receives StandaloneBannerInfo) or call your own
// procedure instead; StandaloneBannerText and ConsoleColorEnabled help.
procedure ShowStandaloneBanner;
function StandaloneBannerInfo: TStandaloneBannerInfo;
// banner text (UTF-8); AColor adds ANSI colors
function StandaloneBannerText(const AInfo: TStandaloneBannerInfo;
  AColor: boolean): UTF8String;
// writes UTF-8 text to the console as is, whatever the console code page
procedure WriteConsoleUTF8(const AText: UTF8String);
// True when stdout is a console that understands ANSI colors (enabled on
// Windows 10+) and NO_COLOR is not set
function ConsoleColorEnabled: boolean;

const
  // UTF-8 box drawing characters as raw bytes; StandaloneBannerText keeps
  // them intact (a plain string may convert them to the system code page)
  FASTPLAZ_BANNER_LOGO: array[0..2] of string = (
    '╔═╗ ╔═╗ ╔═╗ ╔╦╗ ╔═╗ ╦   ╔═╗ ╔═╗',
    '╠╣  ╠═╣ ╚═╗  ║  ╠═╝ ║   ╠═╣ ╔═╝',
    '╚   ╩ ╩ ╚═╝  ╩  ╩   ╩═╝ ╩ ╩ ╚═╝'
  );

var
  // called around every request, in every mode
  OnBeginRequest: TFastPlazRequestEvent = nil;
  OnEndRequest: TFastPlazRequestEvent = nil;
  // replaces the default banner of ShowStandaloneBanner
  OnStandaloneBanner: TStandaloneBannerProc = nil;

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  CustApp, DateUtils, fpjson, jsonparser, jsonscanner;

const
  CONFIG_FILE = 'config' + DirectorySeparator + 'config.json';

  // extension -> content type of the files served in standalone mode;
  // anything else (html templates, json, binaries, ...) goes to the app
  STATIC_TYPES: array[0..25, 0..1] of string = (
    ('css', 'text/css'),
    ('js', 'application/javascript'),
    ('mjs', 'application/javascript'),
    ('map', 'application/json'),
    ('png', 'image/png'),
    ('jpg', 'image/jpeg'),
    ('jpeg', 'image/jpeg'),
    ('gif', 'image/gif'),
    ('svg', 'image/svg+xml'),
    ('webp', 'image/webp'),
    ('avif', 'image/avif'),
    ('ico', 'image/x-icon'),
    ('bmp', 'image/bmp'),
    ('woff', 'font/woff'),
    ('woff2', 'font/woff2'),
    ('ttf', 'font/ttf'),
    ('otf', 'font/otf'),
    ('eot', 'application/vnd.ms-fontobject'),
    ('txt', 'text/plain'),
    ('pdf', 'application/pdf'),
    ('mp4', 'video/mp4'),
    ('webm', 'video/webm'),
    ('mp3', 'audio/mpeg'),
    ('ogg', 'audio/ogg'),
    ('wav', 'audio/wav'),
    ('swf', 'application/x-shockwave-flash')
  );

var
  FApp: TCustomWebApplication = nil;       // created here, freed here
  FRunningApp: TCustomWebApplication = nil; // handling the current request
  FModeResolved: boolean = False;
  FMode: TFastPlazAppMode = amHTTP;
  FPort: integer = FASTPLAZ_HTTP_PORT_DEFAULT;
  FAddress: string = FASTPLAZ_HTTP_ADDRESS_DEFAULT;
  FStatic: boolean = True;
  FMaxRequests: integer = 0;
  FRequestCount: integer = 0;
  FShutdownRequested: boolean = False;
  FShutdownHandlersInstalled: boolean = False;
  FTrustProxy: boolean = False;
  FDevelopment: boolean = False;
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
  out AAddress: string; out AStatic: boolean; out AMaxRequests: integer;
  out ATrustProxy, ADevelopment: boolean);
var
  lst: TStringList;
  parser: TJSONParser;
  root: TJSONData;
  server: TJSONData;
begin
  AMode := '';
  APort := 0;
  AAddress := '';
  AStatic := True;
  AMaxRequests := 0;
  ATrustProxy := False;
  ADevelopment := False;
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
      server := TJSONObject(root).FindPath('systems.trust_proxy');
      if server is TJSONBoolean then
        ATrustProxy := server.AsBoolean;
      server := TJSONObject(root).FindPath('systems.development');
      if server is TJSONBoolean then
        ADevelopment := server.AsBoolean;
      server := TJSONObject(root).FindPath('systems.server');
      if not (server is TJSONObject) then
        Exit;
      AMode := TJSONObject(server).Get('mode', '');
      APort := TJSONObject(server).Get('port', 0);
      AAddress := TJSONObject(server).Get('address', '');
      AStatic := TJSONObject(server).Get('static', True);
      AMaxRequests := TJSONObject(server).Get('max_requests', 0);
    except
      // ignored, see above
    end;
  finally
    root.Free;
    lst.Free;
  end;
end;

function SelectAppMode(const ACommandLineMode, AGatewayInterface,
  AConfigMode: string): TFastPlazAppMode;
begin
  Result := amHTTP;
  if ParseMode(ACommandLineMode, Result) then
    Exit;
  if AGatewayInterface <> '' then
    Exit(amCGI);
  if ParseMode(AConfigMode, Result) then
    Exit;
  Result := amHTTP;
end;

procedure ResolveMode;
var
  s, cfgMode, cfgAddress: string;
  cfgPort: integer;
begin
  if FModeResolved then
    Exit;
  FModeResolved := True;

  ReadServerConfig(cfgMode, cfgPort, cfgAddress, FStatic, FMaxRequests,
    FTrustProxy, FDevelopment);
  if cfgPort > 0 then
    FPort := cfgPort;
  if cfgAddress <> '' then
    FAddress := cfgAddress;

  if not CommandLineValue('mode', s) then
    s := '';
  FMode := SelectAppMode(s, SysUtils.GetEnvironmentVariable('GATEWAY_INTERFACE'),
    cfgMode);

  if CommandLineValue('port', s) and (StrToIntDef(s, 0) > 0) then
    FPort := StrToInt(s);
  if CommandLineValue('address', s) and (s <> '') then
    FAddress := s;
  if CommandLineValue('static', s) then
    FStatic := not (SameText(s, 'off') or SameText(s, 'false') or (s = '0'));
  if CommandLineValue('max-requests', s) then
    FMaxRequests := StrToIntDef(s, FMaxRequests);
  if FMaxRequests < 0 then
    FMaxRequests := 0;
end;

function MaxRequests: integer;
begin
  ResolveMode;
  Result := FMaxRequests;
end;

function RequestCount: integer;
begin
  Result := FRequestCount;
end;

function TrustProxy: boolean;
begin
  ResolveMode;
  Result := FTrustProxy;
end;

procedure SetTrustProxy(AValue: boolean);
begin
  ResolveMode;
  FTrustProxy := AValue;
end;

function StaticFilesEnabled: boolean;
begin
  ResolveMode;
  Result := FStatic;
end;

function StaticContentType(const AFileName: string): string;
var
  ext: string;
  i: integer;
begin
  Result := '';
  ext := LowerCase(ExtractFileExt(AFileName));
  if (ext = '') or (ext = '.') then
    Exit;
  Delete(ext, 1, 1);
  for i := Low(STATIC_TYPES) to High(STATIC_TYPES) do
    if STATIC_TYPES[i, 0] = ext then
      Exit(STATIC_TYPES[i, 1]);
end;

function StaticFileForPath(const AURLPath: string): string;
var
  path, segment, root, full: string;
  parts: TStringArray;
  i: integer;
begin
  Result := '';
  path := AURLPath;
  i := Pos('?', path);
  if i > 0 then
    path := Copy(path, 1, i - 1);
  path := HTTPDecode(path);
  // backslashes, drive letters / NTFS streams and NUL never name an asset
  if (path = '') or (Pos('\', path) > 0) or (Pos(':', path) > 0) or
    (Pos(#0, path) > 0) then
    Exit;
  parts := path.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  if Length(parts) = 0 then
    Exit;
  for i := 0 to High(parts) do
  begin
    segment := parts[i];
    // "..", ".htaccess", ".git", ...
    if segment[1] = '.' then
      Exit;
  end;
  // logs, sessions, cache
  if SameText(parts[0], 'ztemp') then
    Exit;
  if StaticContentType(parts[High(parts)]) = '' then
    Exit;

  path := string.Join(DirectorySeparator, parts);
  root := IncludeTrailingPathDelimiter(ExpandFileName(GetCurrentDir));
  full := ExpandFileName(root + path);
  // must stay inside the root, whatever the file system made of the name
  if not SameFileName(Copy(full, 1, Length(root)), root) then
    Exit;
  if (not FileExists(full)) or DirectoryExists(full) then
    Exit;
  Result := path;
end;

// RFC 7231 date: "Mon, 02 Nov 2026 10:39:58 GMT"
function HTTPDateString(AUniversal: TDateTime): string;
const
  DAYS: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MONTHS: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
  Result := Format('%s, %s %s %s GMT', [DAYS[DayOfWeek(AUniversal)],
    FormatDateTime('dd', AUniversal), MONTHS[MonthOf(AUniversal)],
    FormatDateTime('yyyy hh:nn:ss', AUniversal)]);
end;

function ServeStaticFile(ARequest: TRequest; AResponse: TResponse): boolean;
var
  fileName, lastModified: string;
  modified: TDateTime;
  stream: TFileStream;
begin
  Result := False;
  if not (SameText(ARequest.Method, 'GET') or SameText(ARequest.Method, 'HEAD')) then
    Exit;
  if not StaticFilesEnabled then
    Exit;
  fileName := StaticFileForPath(ARequest.URI);
  if fileName = '' then
    Exit;
  if not FileAge(fileName, modified) then
    Exit;
  Result := True;

  AResponse.Code := 200;
  AResponse.CodeText := 'OK';
  AResponse.ContentType := StaticContentType(fileName);
  lastModified := HTTPDateString(LocalTimeToUniversal(modified));
  AResponse.LastModified := lastModified;
  // browsers send back the Last-Modified value they got
  if ARequest.GetFieldByName('If-Modified-Since') = lastModified then
  begin
    AResponse.Code := 304;
    AResponse.CodeText := 'Not Modified';
    AResponse.ContentLength := 0;
    AResponse.SendContent;
    Exit;
  end;

  stream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  AResponse.ContentLength := stream.Size;
  if SameText(ARequest.Method, 'HEAD') then
  begin
    stream.Free;
    AResponse.SendHeaders;
    Exit;
  end;
  AResponse.FreeContentStream := True;
  AResponse.ContentStream := stream;
  AResponse.SendContent;
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

procedure RequestShutdown;
begin
  FShutdownRequested := True;
  if (FRunningApp is TCustomHTTPApplication) then
    FRunningApp.Terminate
  else if (FApp is TCustomHTTPApplication) then
    FApp.Terminate;
end;

{$IFDEF MSWINDOWS}
function ConsoleCtrlHandler(ACtrlType: DWORD): WINBOOL; stdcall;
begin
  case ACtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_SHUTDOWN_EVENT:
    begin
      // a second Ctrl+C while stopping ends the process the default way
      if FShutdownRequested then
        Exit(False);
      RequestShutdown;
      Result := True;
    end;
    else
      Result := False;
  end;
end;
{$ENDIF}

{$IFDEF UNIX}
procedure ShutdownSignalHandler(ASignal: longint); cdecl;
begin
  if FShutdownRequested then
    Halt(1);
  RequestShutdown;
end;
{$ENDIF}

procedure InstallShutdownHandlers;
begin
  {$IFDEF MSWINDOWS}
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);
  {$ENDIF}
  {$IFDEF UNIX}
  fpSignal(SIGTERM, signalhandler(@ShutdownSignalHandler));
  fpSignal(SIGINT, signalhandler(@ShutdownSignalHandler));
  {$ENDIF}
end;

{ standalone banner }

const
  ANSI_RESET = #27'[0m';
  ANSI_BOLD = #27'[1m';
  ANSI_DIM = #27'[2m';
  ANSI_GREEN = #27'[32m';
  ANSI_YELLOW = #27'[33m';
  ANSI_CYAN = #27'[36m';
  BANNER_ARROW = '➜';
  {$IFDEF MSWINDOWS}
  VIRTUAL_TERMINAL_PROCESSING = $0004;
  {$ENDIF}

var
  FConsoleChecked: boolean = False;
  FConsoleColor: boolean = False;

function ConsoleColorEnabled: boolean;
{$IFDEF MSWINDOWS}
var
  handle: THandle;
  consoleMode: DWORD;
{$ENDIF}
begin
  if FConsoleChecked then
    Exit(FConsoleColor);
  FConsoleChecked := True;
  FConsoleColor := False;
  if SysUtils.GetEnvironmentVariable('NO_COLOR') <> '' then
    Exit(False);
  {$IFDEF MSWINDOWS}
  handle := GetStdHandle(STD_OUTPUT_HANDLE);
  consoleMode := 0;
  // fails when stdout is redirected to a file or a pipe
  if (handle = INVALID_HANDLE_VALUE) or (not GetConsoleMode(handle, consoleMode)) then
    Exit(False);
  if (consoleMode and VIRTUAL_TERMINAL_PROCESSING) = 0 then
    FConsoleColor := SetConsoleMode(handle, consoleMode or VIRTUAL_TERMINAL_PROCESSING)
  else
    FConsoleColor := True;
  {$ENDIF}
  {$IFDEF UNIX}
  FConsoleColor := (fpIsATTY(StdOutputHandle) = 1) and
    (SysUtils.GetEnvironmentVariable('TERM') <> 'dumb');
  {$ENDIF}
  Result := FConsoleColor;
end;

function StandaloneBannerInfo: TStandaloneBannerInfo;
var
  host: string;
begin
  ResolveMode;
  Result.Title := '';
  if FApp <> nil then
    Result.Title := FApp.Title;
  Result.Address := FAddress;
  Result.Port := FPort;
  // a server on all interfaces is reached locally through the loopback
  host := FAddress;
  if (host = '') or (host = '0.0.0.0') then
    host := '127.0.0.1'
  else if Pos(':', host) > 0 then
    host := '[' + host + ']';
  Result.URL := Format('http://%s:%d/', [host, FPort]);
  Result.Development := FDevelopment;
  Result.RootDir := GetCurrentDir;
end;

// a literal of this unit holds UTF-8 bytes but carries the system code
// page: relabel it without converting
function UTF8Literal(const AText: RawByteString): UTF8String;
var
  raw: RawByteString;
begin
  raw := AText;
  UniqueString(raw);
  SetCodePage(raw, CP_UTF8, False);
  Result := raw;
end;

function StandaloneBannerText(const AInfo: TStandaloneBannerInfo;
  AColor: boolean): UTF8String;

  function C(const ACode: string; const AText: UTF8String): UTF8String;
  begin
    if AColor then
      Result := ACode + AText + ANSI_RESET
    else
      Result := AText;
  end;

  function Line(const ALabel: string; const AValue: UTF8String): UTF8String;
  begin
    Result := '   ' + C(ANSI_DIM, Format('%-9s', [ALabel])) + AValue + LineEnding;
  end;

var
  i: integer;
  modeName, title, rootDir: UTF8String;
begin
  Result := '';
  for i := Low(FASTPLAZ_BANNER_LOGO) to High(FASTPLAZ_BANNER_LOGO) do
    Result := Result + ' ' + C(ANSI_CYAN + ANSI_BOLD,
      UTF8Literal(FASTPLAZ_BANNER_LOGO[i])) + LineEnding;
  Result := Result + LineEnding;
  title := AInfo.Title;
  if title <> '' then
    Result := Result + '   ' + C(ANSI_BOLD, title) + LineEnding;
  Result := Result + ' ' + C(ANSI_GREEN, UTF8Literal(BANNER_ARROW)) + ' ' +
    C(ANSI_DIM, Format('%-9s', ['Local:'])) + C(ANSI_CYAN, AInfo.URL) + LineEnding;
  if (AInfo.Address = '0.0.0.0') or (AInfo.Address = '::') then
    Result := Result + Line('Network:', 'all interfaces, port ' + IntToStr(AInfo.Port));
  if AInfo.Development then
    modeName := C(ANSI_YELLOW, 'development')
  else
    modeName := 'production';
  Result := Result + Line('Mode:', modeName);
  rootDir := AInfo.RootDir;
  Result := Result + Line('Root:', rootDir);
  Result := Result + Line('FPC:', {$i %FPCVERSION%} + ' ' + {$i %FPCTARGETCPU%} +
    '-' + {$i %FPCTARGETOS%});
  Result := Result + LineEnding + '   ' + C(ANSI_DIM, 'Press Ctrl+C to stop') +
    LineEnding;
end;

procedure WriteConsoleUTF8(const AText: UTF8String);
begin
  {$IFDEF MSWINDOWS}
  // only a console has an output code page
  if GetConsoleOutputCP <> 0 then
    SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}
  // no conversion on the way out; later output of the program is converted
  // to UTF-8 too, which matches the console now
  SetTextCodePage(Output, CP_UTF8);
  Write(Output, AText);
  Flush(Output);
end;

procedure ShowStandaloneBanner;
var
  info: TStandaloneBannerInfo;
begin
  if not IsStandalone then
    Exit;
  info := StandaloneBannerInfo;
  if Assigned(OnStandaloneBanner) then
  begin
    OnStandaloneBanner(info);
    Exit;
  end;
  WriteConsoleUTF8(StandaloneBannerText(info, ConsoleColorEnabled));
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
      // the accept loop wakes up every second, so a shutdown request from
      // a signal handler is noticed without a new connection
      http.AcceptIdleTimeout := 1000;
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

// first entry of a proxy header ("https, http" -> "https"), '' when the
// proxy is not trusted
function ForwardedValue(ARequest: TRequest; const AHeader: string): string;
var
  i: integer;
begin
  Result := '';
  if not TrustProxy then
    Exit;
  Result := ARequest.GetFieldByName(AHeader);
  i := Pos(',', Result);
  if i > 0 then
    Result := Copy(Result, 1, i - 1);
  Result := Trim(Result);
end;

function RequestHost(ARequest: TRequest): string;
begin
  Result := ForwardedValue(ARequest, 'X-Forwarded-Host');
  if Result = '' then
    Result := ARequest.Host;
end;

function RequestServerVariable(ARequest: TRequest; const AName: string): string;
var
  r: TRequest;
  n, s: string;
  i: integer;
begin
  r := ARequest;
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
    'SERVER_NAME': Result := HostWithoutPort(RequestHost(r));
    'SERVER_PORT':
    begin
      Result := ForwardedValue(r, 'X-Forwarded-Port');
      if StrToIntDef(Result, 0) <= 0 then
      begin
        // the port of the host the client used, else the one we listen on
        s := RequestHost(r);
        i := LastDelimiter(':]', s);
        if (i > 0) and (s[i] = ':') then
          Result := Copy(s, i + 1, MaxInt)
        else if (ForwardedValue(r, 'X-Forwarded-Host') <> '') or
          (ForwardedValue(r, 'X-Forwarded-Proto') <> '') then
        begin
          if SameText(ForwardedValue(r, 'X-Forwarded-Proto'), 'https') then
            Result := '443'
          else
            Result := '80';
        end
        else
          Result := IntToStr(r.ServerPort);
      end;
    end;
    'HTTPS':
      if SameText(ForwardedValue(r, 'X-Forwarded-Proto'), 'https') then
        Result := 'on'
      else
        Result := '';
    'HTTP_HOST': Result := RequestHost(r);
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
        Result := SysUtils.GetEnvironmentVariable(AName);
  end;
end;

function ServerVariable(const AName: string): string;
var
  r: TRequest;
begin
  r := CurrentRequest;
  if (r = nil) or (not IsStandalone) then
    Result := SysUtils.GetEnvironmentVariable(AName)
  else
    Result := RequestServerVariable(r, AName);
end;

procedure StopRequest;
begin
  if not IsStandalone then
    Application.Terminate;
end;

// not BeginRequest/EndRequest: inside the handlers those names resolve to
// TWebHandler.EndRequest, which frees the request and the response
procedure RequestStarted(AHandler: TWebHandler; ARequest: TRequest;
  AResponse: TResponse);
begin
  if AHandler.Owner is TCustomWebApplication then
    FRunningApp := TCustomWebApplication(AHandler.Owner);
  FRequest := ARequest;
  FResponse := AResponse;
  if Assigned(OnBeginRequest) then
    OnBeginRequest(ARequest, AResponse);
end;

procedure RequestFinished(ARequest: TRequest; AResponse: TResponse);
begin
  try
    if Assigned(OnEndRequest) then
      OnEndRequest(ARequest, AResponse);
  finally
    FRequest := nil;
    FResponse := nil;
    Inc(FRequestCount);
    if IsStandalone and (MaxRequests > 0) and (FRequestCount >= MaxRequests) then
      RequestShutdown;
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
  RequestStarted(Self, ARequest, AResponse);
  try
    inherited HandleRequest(ARequest, AResponse);
  finally
    RequestFinished(ARequest, AResponse);
  end;
end;

{ TFastPlazHTTPHandler }

procedure TFastPlazHTTPHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  // assets are answered before the application sees the request, like a
  // web server would (nginx try_files)
  try
    if ServeStaticFile(ARequest, AResponse) then
      Exit;
  except
    on E: Exception do
    begin
      ShowRequestException(AResponse, E);
      Exit;
    end;
  end;
  RequestStarted(Self, ARequest, AResponse);
  try
    inherited HandleRequest(ARequest, AResponse);
  finally
    RequestFinished(ARequest, AResponse);
  end;
end;

{ TFastPlazHTTPApplication }

function TFastPlazHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TFastPlazHTTPHandler.Create(Self);
end;

procedure TFastPlazHTTPApplication.DoRun;
begin
  if not FShutdownHandlersInstalled then
  begin
    FShutdownHandlersInstalled := True;
    InstallShutdownHandlers;
  end;
  inherited DoRun;
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
  if SysUtils.GetEnvironmentVariable('GATEWAY_INTERFACE') <> '' then
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

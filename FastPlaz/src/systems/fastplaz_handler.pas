unit fastplaz_handler;

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  {$IFDEF HEAPTRACE}
  heaptrc,
  {$ENDIF}
  RegExpr,
  sqldb, gettext, session_controller, module_controller,
  config_lib,
  fpcgi, httpdefs, fpHTTP, fpWeb,
  //webutil,
  custweb, dateutils, variants,
  SysUtils, Classes;

const
  LF = #13#10;
  TDateTimeEpsilon = 2.2204460493e-16;

resourcestring
  __ErrNoModuleNameForRequest = 'Could not determine HTTP module name for request';
  __Err_Http_InvalidMethod = 'Invalid method request';

  // theme
  __Err_App_Init =
    '<h3>This is the first time using fastplaz?</h3><a href="%s">click here</a> if you need to initialize your webapp''s structure.<br>Make sure target directory is writeable';
  __Err_Theme_Not_Exists = 'file layout "%s" does not exist in theme "%s"';
  __Err_Theme_Tag_NotImplemented = 'Template tag [%s] does not implemented yet.';
  __Err_Theme_Modul_NotFond = 'Modul "%s" not found';
  __Err_Theme_ForeachNotImplemented = 'foreach array still not implemented';
  __Err_Theme_Template_NotFound = 'Unable to open file "%s"';

  __Result_Default_Get = 'Default GET Result';
  __Result_Default_Post = 'Default POST Result';
  __Result_Default_Put = 'Default PUT Result';
  __Result_Default_Delete = 'Default DELETE Result';
  __Result_Default_Patch = 'Default PATCH Result';
  __Result_Default_Options = 'Default OPTIONS Result';
  __Result_Default_Handler = 'Fastplaz Request Handler Default';

  __Content_Not_Found = 'Nothing Found';
  __Tag_Content_Not_Found = 'Tags Content "%s" Not Found';

  // methode
  ALL = '';
  METHOD_GET = 'GET';
  METHOD_POST = 'POST';
  METHOD_PUT = 'PUT';
  METHOD_PATCH = 'PATCH';
  METHOD_DELETE = 'DELETE';
  HEAD = 'HEAD';
  OPTIONS = 'OPTIONS';


type

  { TMainData }

  TMainData = record
    module, modtype, func: string;
    sitename,
    slogan,
    baseUrl,
    admin_email,
    language,
    tempDir: string;
    themeEnable: boolean;
    theme: string;
    cacheType: string;
    cacheWrite: boolean;
    cacheTime: integer;
    tablePrefix: string;
    SessionID: string;
    SessionDir: string;
    SessionStorage: integer; // 1: file; 2 database
    hitStorage: string;
    databaseRead,
    databaseWrite: string;
    databaseActive,
    useDatabase,
    initialized,
    debug: boolean;
    debugLevel: integer;
    isReady: boolean;
    cookiePath: string;
  end;

  TOnBlockController = procedure(Sender: TObject; FunctionName: string;
    Parameter: TStrings; var ResponseString: string) of object;
  TTagCallback = function(const ATagName: string;
    AParams: TStringList): string of object;

  TOnMenu = function(RequestHeader: TRequest): string of object;
  TOnSearch = function(Keyword: string; RequestHeader: TRequest): string of object;
  TOnNotification = function(NotifType: string;
    RequestHeader: TRequest): string of object;

  { TMyCustomWebModule }

  //TMyCustomWebModule  = class(TFPWebModule)
  TMyCustomWebModule = class(TCustomFPWebModule)
  private
    FFiles: TUploadedFiles;
    FisJSON, FCreateSession: boolean;
    FOnBlockController: TOnBlockController;
    FOnMenu: TOnMenu;
    FOnNotification: TOnNotification;
    FOnSearch: TOnSearch;

    function GetBaseURL: string;
    function GetCSRFFailedCount: integer;
    function GetCustomHeader(const KeyName: string): string;
    function GetEnvirontment(const KeyName: string): string;
    function GetHeader(const KeyName: string): string;
    function GetIsActive: boolean;
    function GetIsAjax: boolean;
    function GetIsDelete: boolean;
    function GetIsGet: boolean;
    function GetIsPatch: boolean;
    function GetIsPost: boolean;
    function GetIsPut: boolean;
    function GetIsValidCSRF: boolean;
    function getModuleActive: string;
    function GetSession: TSessionController;
    function GetSessionID: string;
    function GetTablePrefix: string;
    function GetTag(const TagName: string): TTagCallback;
    function GetTimeUsage: integer;
    function GetURI: string;
    procedure SetCustomHeader(const KeyName: string; AValue: string);
    procedure SetFlashMessage(AValue: string);
    procedure SetTag(const TagName: string; AValue: TTagCallback);

    procedure RequestHandlerDefault(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  public
    RouteRegex: string;
    VisibleModuleName: string;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;

    procedure Get; virtual;
    procedure Post; virtual;
    procedure Put; virtual;
    procedure Delete; virtual;
    procedure Patch; virtual;
    procedure Options; virtual;

    procedure LanguageInit;
    procedure CloseConnection( const AResponseContent: string = ''; ACode: Integer = 200);

    property URI: string read GetURI;
    property Environtment[const KeyName: string]: string read GetEnvirontment;
    property Header[const KeyName: string]: string read GetHeader;
    property CustomHeader[const KeyName: string]: string read GetCustomHeader write SetCustomHeader;

    property Tags[const TagName: string]: TTagCallback read GetTag write SetTag; default;
    procedure TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);
    property BaseURL: string read GetBaseURL;
    property OnBlockController: TOnBlockController
      read FOnBlockController write FOnBlockController;

    property OnMenu: TOnMenu read FOnMenu write FOnMenu;
    property OnSearch: TOnSearch read FOnSearch write FOnSearch;
    property OnNotification: TOnNotification read FOnNotification write FOnNotification;

    property ModuleActive: string read getModuleActive;
    property isActive: boolean read GetIsActive;
    property isPost: boolean read GetIsPost;
    property isGet: boolean read GetIsGet;
    property isPut: boolean read GetIsPut;
    property isPatch: boolean read GetIsPatch;
    property isDelete: boolean read GetIsDelete;
    property isValidCSRF: boolean read GetIsValidCSRF;
    property isAjax: boolean read GetIsAjax;

    property FlashMessages: string write SetFlashMessage;
    property CSRFFailedCount: integer read GetCSRFFailedCount;

    property CreateSession: boolean read FCreateSession write FCreateSession;
    property Session: TSessionController read GetSession;
    property SessionID: string read GetSessionID;

    property TablePrefix: string read GetTablePrefix;
    property TimeUsage: integer read GetTimeUsage;
  published
    property Files : TUploadedFiles Read FFiles;
  end;

  TMyCustomController = class(TMyCustomWebModule);

  TMyCustomWebModuleClass = class of TMyCustomWebModule;

  { TModuleLoadedItem }

  TModuleLoadedItem = class(TCollectionItem)
  private
    FModuleClass: TMyCustomWebModuleClass;
    FModuleName: string;
    FModuleStore: TMyCustomWebModule;
    FSkipStreaming: boolean;
  public
    property ModuleClass: TMyCustomWebModuleClass read FModuleClass write FModuleClass;
    property ModuleStore: TMyCustomWebModule read FModuleStore write FModuleStore;
    property ModuleName: string read FModuleName write FModuleName;
    property SkipStreaming: boolean read FSkipStreaming write FSkipStreaming;
  end;

  { TModuleLoaded }

  TModuleLoaded = class(TCollection)
  private
    function GetModule(Index: integer): TModuleLoadedItem;
    procedure SetModule(Index: integer; AValue: TModuleLoadedItem);
  public
    property Modules[Index: integer]: TModuleLoadedItem read GetModule write SetModule;
      default;
    function IndexOfModule(const AModuleName: string): integer;
  end;

  { TFastPlasAppandler }

  TFastPlasAppandler = class(TComponent)
  private
    FIsDisplayError: boolean;
    function GetURI: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property URI: string read GetURI;
    property isDisplayError: boolean read FIsDisplayError write FIsDisplayError;

    function GetActiveModuleName(Arequest: TRequest): string;
    procedure OnGetModule(Sender: TObject; ARequest: TRequest;
      var ModuleClass: TCustomHTTPModuleClass);
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    function Tag_InternalContent_Handler(const TagName: string;
      Params: TStringList): string;

    procedure AddLoadModule(const ModuleName: string;
      ModuleClass: TMyCustomWebModuleClass; SkipStreaming: boolean = True);
    function LoadModule(const ModuleName: string): TMyCustomWebModule;
    procedure RegisterModule(const ModuleName: string;
      ModuleClass: TMyCustomWebModuleClass; LoadOnStart: boolean = True;
      SkipStreaming: boolean = False);
    function FindModule(ModuleClass: TCustomHTTPModuleClass): TCustomHTTPModule;
    procedure AddLog(Message: string);
    procedure DieRaise(const Fmt: string; const Args: array of const);
  end;

  { TGET }

  TGET = class
  private
    function GetCount: integer;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name: string; AValue: string);
  public
    property Values[Name: string]: string read GetValue write SetValue; default;
    property Count: integer read GetCount;
  end;

  { TPOST }

  TPOST = class
  private
    function GetCount: integer;
    function GetValue(const Variable: string): string;
    procedure SetValue(const Variable: string; AValue: string);
  public
    property Values[variable: string]: string read GetValue write SetValue; default;
    property Count: integer read GetCount;
  end;

  { TREQUESTVAR }

  TREQUESTVAR = class
  private
    function GetValue(variable: string): string;
  public
    property Values[variable: string]: string read GetValue; default;
  end;

  { TSESSION }

  TSESSION = class
  private
    function GetValue(variable: string): variant;
    procedure SetValue(variable: string; AValue: variant);
  public
    property Values[variable: string]: variant read GetValue write SetValue; default;
    function ReadDateTime(const variable: string): TDateTime;
  end;

  { TSERVER }

  TSERVER = class
  private
    function GetValue(const variable: string): string;
  public
    property Values[variable: string]: string read GetValue; default;
  end;

  { TRoute }

  TRoute = class
  private
    procedure SetRouteValue(KeyName: string; AValue: TMyCustomWebModuleClass);
  public
    procedure Add(const ModuleName: string; const PatternURL: string;
      ModuleClass: TMyCustomWebModuleClass; Method: string = '';
      LoadOnStart: boolean = False; SkipStreaming: boolean = True);
    procedure Add(const ModuleName: string; ModuleClass: TMyCustomWebModuleClass;
      Method: string = ''; LoadOnStart: boolean = False; SkipStreaming: boolean = True);

    property RouteValue[KeyName: string]: TMyCustomWebModuleClass
      write SetRouteValue; default;
  end;

procedure InitializeFastPlaz(Sender: TObject = nil);
procedure Redirect(const URL: string; const FlashMessage: string = ''; AStatusCode: Integer = 302);

procedure DisplayError(const Message: string; const Layout: string = 'error');
procedure Debug(const Message: integer; const Key: string = '');
procedure Debug(const Message: string; const Key: string = '');
procedure Debug(const Sender: TObject; const Key: string = '');

var
  AppData: TMainData;
  Config: TMyConfig;
  Sanitize: boolean;
  SessionController: TSessionController;
  FastPlasAppandler: TFastPlasAppandler;
  ModuleLoaded: TModuleLoaded;
  ModUtil: TModUtil;
  Route: TRoute;
  _GET: TGET;
  _POST: TPOST;
  _SESSION: TSESSION;
  _REQUEST: TREQUESTVAR;
  _SERVER: TSERVER;
  _DebugInfo: TStringList;
  StartTime, StopTime, ElapsedTime: cardinal;
  MemoryAllocated: integer;

implementation

uses common, language_lib, database_lib, logutil_lib, theme_controller, html_lib;

var
  MethodMap: TStringList;
  RouteRegexMap: TStringList;

function _CleanVar(const Value: string): string;
begin
  Result := mysql_real_escape_string(Value);
  Result := Trim(Result);
end;


procedure InitializeFastPlaz(Sender: TObject);
var
  _: String;
begin
  if AppData.initialized then
    Exit;
  AppData.initialized := True;

  //-- compatibility with old-style
  AppData.module := _GET['mod'];
  AppData.modtype := _GET['type'];
  AppData.func := _GET['func'];
  AppData.useDatabase := False;
  AppData.databaseActive := False;
  if AppData.modtype = '' then
    AppData.modtype := 'user';
  if AppData.func = '' then
    AppData.func := 'main';
  try
    AppData.SessionID := TMyCustomWebModule(Sender).Session.SessionID;
  except
  end;

  if Config.Status = 2 then
    die(Config.Message);
  AppData.sitename := string(Config.GetValue(_SYSTEM_SITENAME, _APP));
  AppData.slogan := string(Config.GetValue(_SYSTEM_SLOGAN, _APP));
  AppData.baseUrl := string(Config.GetValue(_SYSTEM_BASEURL, ''));
  AppData.admin_email := string(
    Config.GetValue(_SYSTEM_WEBMASTER_EMAIL, UnicodeString( Application.Email)));
  AppData.language := string(Config.GetValue(_SYSTEM_LANGUAGE_DEFAULT, 'en'));
  AppData.themeEnable := Config.GetValue(_SYSTEM_THEME_ENABLE, True);
  AppData.theme := string(Config.GetValue(_SYSTEM_THEME, 'default'));
  AppData.debug := Config.GetValue(_SYSTEM_DEBUG, False);
  AppData.debugLevel := Config.GetValue(_SYSTEM_DEBUGLEVEL, 0);
  AppData.cacheType := string(Config.GetValue(_SYSTEM_CACHE_TYPE, 'file'));
  AppData.cacheWrite := Config.GetValue(_SYSTEM_CACHE_WRITE, True);

  AppData.cacheTime := Config.GetValue(_SYSTEM_CACHE_TIME, 3);
  AppData.tempDir := string(Config.GetValue(_SYSTEM_TEMP_DIR, 'ztemp'));
  AppData.cookiePath := string(Config.GetValue(_SYSTEM_COOKIE_PATH, ''));

  if AppData.baseUrl = '' then
  begin
    AppData.baseUrl := 'http://' + GetEnvironmentVariable('SERVER_NAME') +
      ExtractFilePath(GetEnvironmentVariable('SCRIPT_NAME'));
  end;

  AppData.hitStorage := string(Config.GetValue(_SYSTEM_HIT_STORAGE, ''));
  try
    if AppData.hitStorage <> '' then
    begin
      if AppData.hitStorage = 'file' then
        ThemeUtil.HitType := htFile;
      if AppData.hitStorage = 'database' then
        ThemeUtil.HitType := htDatabase;
      if AppData.hitStorage = 'sqlite' then
        ThemeUtil.HitType := htSQLite;
    end;
  except
    on e: Exception do
    begin
      LogUtil.add(E.Message, 'hitstorage-init');
    end;
  end;

  if AppData.themeEnable then
  begin
    ThemeUtil := TThemeUtil.Create;
  end;

  //-- session
  AppData.SessionDir := string(Config.GetValue(_SYSTEM_SESSION_DIR, ''));
  if AppData.SessionDir <> '' then
    SessionController.SessionDir := AppData.SessionDir;
  AppData.SessionStorage := _SESSION_STORAGE_FILE;
  if string(Config.GetValue(_SYSTEM_SESSION_STORAGE, 'file')) = 'database' then
    AppData.SessionStorage := _SESSION_STORAGE_DATABASE;
  SessionController.Storage := AppData.SessionStorage;
  // force session with cookie
  if SessionController.CookieID.IsEmpty then
  begin
    _ := Application.Request.CookieFields.Values['_'];
    if _.IsEmpty then
    begin
      _ := RandomString(40, 'fastplaz');
    end;
    with Application.Response.Cookies.Add do
    begin
      Name := '_';
      Value := _;
      if not AppData.cookiePath.IsEmpty then
        Path := AppData.cookiePath;
      Expires := dateutils.IncDay(Now,3);
    end;
    SessionController.SessionID := _;
  end;
  if AppData.SessionStorage = _SESSION_STORAGE_DATABASE then
  begin
    DataBaseInit;
  end;
  SessionController.TimeOut :=
    Config.GetValue(_SYSTEM_SESSION_TIMEOUT, _SESSION_TIMEOUT_DEFAULT);
  if not SessionController.StartSession then
  begin
    //SessionController.EndSession;
    //SessionController.StartSession;
  end;
  //-- session - end

  //todo: auto clean-up session


  //-- language

  AppData.isReady := True;
  {$ifdef DEBUG}
  if AppData.debug and (AppData.debugLevel = 1) then
    LogUtil.Add('--== initialize: ' + Application.Request.PathInfo, 'init');
  {$endif}
end;

procedure Redirect(const URL: string; const FlashMessage: string;
  AStatusCode: Integer);
begin
  if FlashMessage <> '' then
    ThemeUtil.FlashMessages := FlashMessage;
  Application.Response.Content := '';
  //Application.Response.SendRedirect(URL);
  //Application.Response.SendResponse;
  Application.Response.Code := AStatusCode;
  Application.Response.Location := URL;
  Application.Response.SendHeaders;
  Application.Destroy;
end;


procedure DisplayError(const Message: string; const Layout: string);
begin
  FastPlasAppandler.isDisplayError := True;
  if not AppData.themeEnable then
  begin
    die(Message);
  end;

  if (not AppData.useDatabase) or (AppData.useDatabase and AppData.databaseActive) then
  begin
    Application.Response.Contents.Text :=
      ThemeUtil.Render(@ThemeUtil.TagDefault, Layout);
    Application.Response.Contents.Text :=
      ReplaceAll(Application.Response.Contents.Text,
      [ThemeUtil.StartDelimiter + 'maincontent' + ThemeUtil.EndDelimiter],
      '<div class="box error">' + Message + '</div>');
  end
  else
  begin
    Application.Response.Contents.Text := '<div class="box error">' + Message + '</div>';
  end;

  Application.Response.SendContent;
  Application.Terminate;
end;

procedure Debug(const Message: integer; const Key: string);
begin
  if key <> '' then
    echo('<pre>' + Key + ': ' + i2s(Message) + '</pre>')
  else
    echo('<pre>' + i2s(Message) + '</pre>');
end;

procedure Debug(const Message: string; const Key: string);
begin
  if key <> '' then
    echo('<pre>' + Key + ': ' + Message + '</pre>')
  else
    echo('<pre>' + Message + '</pre>');
end;

procedure Debug(const Sender: TObject; const Key: string);
var
  prefix, suffic, html: string;
begin
  if not Assigned(Sender) then
  begin
    echo('Sender is not assigned');
    Exit;
  end;
  prefix := '<pre>';
  suffic := '</pre>';
  html := Sender.ClassName;
  if Key <> '' then
    html := html + '(' + Key + '): '
  else
    html := html + ': ';

  //if Sender is String then
  //  html := (Sender as String);
  if Sender is TStrings then
    html := html + (Sender as TStrings).Text;
  if Sender is TSimpleModel then
  begin
    with (Sender as TSimpleModel) do
    begin

    end;
  end;//- TSimpleModel

  echo(prefix + html + suffic);
end;

{ TModuleLoaded }

function TModuleLoaded.GetModule(Index: integer): TModuleLoadedItem;
begin
  Result := TModuleLoadedItem(Items[Index]);
end;

procedure TModuleLoaded.SetModule(Index: integer; AValue: TModuleLoadedItem);
begin
  Items[Index] := AValue;
end;

function TModuleLoaded.IndexOfModule(const AModuleName: string): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (CompareText(Modules[Result].ModuleName, AModuleName) <> 0) do
    Dec(Result);
end;

{ TRoute }

// SkipStreaming: True -> skip the streaming support - which means you don't require Lazarus!!!
procedure TRoute.Add(const ModuleName: string; ModuleClass: TMyCustomWebModuleClass;
  Method: string; LoadOnStart: boolean; SkipStreaming: boolean);
var
  moduleNameReal: string;
begin
  if isRegex(ModuleName) then
  begin
    // next;
  end;
  moduleNameReal := CleanUrl(ModuleName);
  moduleNameReal := ReplaceAll(moduleNameReal, ['_'], '');
  moduleNameReal := ReplaceAll(moduleNameReal, ['-', '|'], '_');
  Add(moduleNameReal, ModuleName, ModuleClass, Method, LoadOnStart, SkipStreaming);
end;

procedure TRoute.SetRouteValue(KeyName: string; AValue: TMyCustomWebModuleClass
  );
begin
  Add( KeyName, AValue);
end;

procedure TRoute.Add(const ModuleName: string; const PatternURL: string;
  ModuleClass: TMyCustomWebModuleClass; Method: string; LoadOnStart: boolean;
  SkipStreaming: boolean);
//var
  //i: integer;
  //mi: TModuleItem;
  //mc: TCustomHTTPModuleClass;
  //m: TCustomHTTPModule;
begin
  if not Assigned(MethodMap) then
    MethodMap := TStringList.Create;
  if not Assigned(RouteRegexMap) then
    RouteRegexMap := TStringList.Create;

  MethodMap.Values[ModuleName] := Method;
  RouteRegexMap.Values[ModuleName] := PatternURL;

  //RegisterHTTPModule( ModuleName, ModuleClass, SkipStreaming);
  FastPlasAppandler.RegisterModule(ModuleName, ModuleClass, LoadOnStart, SkipStreaming);

  //mi := ModuleFactory.FindModule( moduleName);
  {
  i := ModuleFactory.IndexOfModule( ModuleName);
  if i <> -1 then
  begin
    mi := ModuleFactory[I];
    mc := mi.ModuleClass;
    m := FastPlasAppandler.FindModule(mc);
    if m <> nil then
    begin
      //--
    end;
  end;
  }

  //  if LoadOnStart then
  //    FastPlasAppandler.AddLoadModule( ModuleName, TMyCustomWebModuleClass(ModuleClass), SkipStreaming);

end;

{ TSESSION }

function TSESSION.GetValue(variable: string): variant;
begin
  Result := '';
  try
    Result := SessionController[variable];
  except
  end;
end;

procedure TSESSION.SetValue(variable: string; AValue: variant);
begin
  SessionController[variable] := AValue;
  if ((VarType(AValue) = varSmallInt) or (VarType(AValue) = varinteger) or
    (VarType(AValue) = varint64)) then
    SessionController[variable] := i2s(AValue);
end;

function TSESSION.ReadDateTime(const variable: string): TDateTime;
begin
  Result := SessionController.ReadDateTime(variable);
end;

{$if fpc_fullversion >= 20701}
{
class function TStringHash.hash(s: string; n: integer): integer;
var
  c: char;
begin
  Result := 0;
  for c in s do
    Inc(Result, Ord(c));
  Result := Result mod n;
end;
}
{$endif fpc_fullversion >= 20701}

{ TMyCustomWebModule }

function TMyCustomWebModule.GetBaseURL: string;
begin
  Result := ThemeUtil.BaseURL;
end;

function TMyCustomWebModule.GetCSRFFailedCount: integer;
begin
  Result := s2i(_SESSION[__HTML_CSRF_TOKEN_KEY_FAILEDCOUNT]);
end;

function TMyCustomWebModule.GetCustomHeader(const KeyName: string): string;
begin
  Result := Response.GetCustomHeader( KeyName);
end;

function TMyCustomWebModule.GetEnvirontment(const KeyName: string): string;
begin
  Result := Application.EnvironmentVariable[KeyName];
end;

function TMyCustomWebModule.GetHeader(const KeyName: string): string;
begin
  Result := GetEnvironmentVariable( KeyName);
end;

function TMyCustomWebModule.GetIsActive: boolean;
var
  i: integer;
begin
  Result := False;
  i := ModuleFactory.IndexOfModule(FastPlasAppandler.GetActiveModuleName(
    Application.Request));
  if i = -1 then
    Exit;
  if ClassName = ModuleFactory[i].ModuleClass.ClassName then
    Result := True;
end;

function TMyCustomWebModule.GetIsAjax: boolean;
begin
  Result := False;
  if (LowerCase(Application.EnvironmentVariable['HTTP_X_REQUESTED_WITH']) =
    'xmlhttprequest') then
    Result := True;
end;

function TMyCustomWebModule.GetIsDelete: boolean;
begin
  Result := False;
  if Application.Request.Method = METHOD_DELETE then
    Result := True;
end;

function TMyCustomWebModule.GetIsGet: boolean;
begin
  Result := False;
  if Application.Request.Method = METHOD_GET then
    Result := True;
end;

function TMyCustomWebModule.GetIsPatch: boolean;
begin
  Result := False;
  if Application.Request.Method = METHOD_PATCH then
    Result := True;
end;

function TMyCustomWebModule.GetIsPost: boolean;
begin
  Result := False;
  if Application.Request.Method = METHOD_POST then
    Result := True;
end;

function TMyCustomWebModule.GetIsPut: boolean;
begin
  Result := False;
  if Application.Request.Method = METHOD_PUT then
    Result := True;
end;

function TMyCustomWebModule.GetIsValidCSRF: boolean;
var
  i: integer;
begin
  Result := False;
  if not isPost then
    Exit;

  if _POST['csrftoken'] = '' then
    Exit;

  if _POST['csrftoken'] = _SESSION[__HTML_CSRF_TOKEN_KEY] then
    Result := True;

  if Result then
  begin
    HTMLUtil.ResetCSRF;
  end
  else
  begin
    i := s2i(_SESSION[__HTML_CSRF_TOKEN_KEY_FAILEDCOUNT]) + 1;
    _SESSION[__HTML_CSRF_TOKEN_KEY_FAILEDCOUNT] := i;
  end;

  SessionController.ForceUpdate;
end;

function TMyCustomWebModule.getModuleActive: string;
var
  i: integer;
begin
  Result := '';
  i := ModuleFactory.IndexOfModule(FastPlasAppandler.GetActiveModuleName(
    Application.Request));
  if i = -1 then
    Exit;
  Result := ModuleFactory[i].ModuleClass.ClassName;
end;

function TMyCustomWebModule.GetSession: TSessionController;
begin
  Result := SessionController;
end;

function TMyCustomWebModule.GetSessionID: string;
begin
  Result := SessionController.SessionID;
end;

function TMyCustomWebModule.GetTablePrefix: string;
begin
  Result := AppData.tablePrefix;
end;

function TMyCustomWebModule.GetTag(const TagName: string): TTagCallback;
begin
  if AppData.themeEnable then
    Result := ___TagCallbackMap[TagName];
end;

function TMyCustomWebModule.GetTimeUsage: integer;
begin
  StopTime:= _GetTickCount;
  ElapsedTime:= StopTime - StartTime;
  Result := ElapsedTime;
end;

function TMyCustomWebModule.GetURI: string;
begin
  Result := FastPlasAppandler.URI;
end;

procedure TMyCustomWebModule.SetCustomHeader(const KeyName: string;
  AValue: string);
begin
  Response.SetCustomHeader( KeyName, AValue);
end;

procedure TMyCustomWebModule.SetFlashMessage(AValue: string);
begin
  ThemeUtil.FlashMessages := AValue;
end;

procedure TMyCustomWebModule.SetTag(const TagName: string; AValue: TTagCallback);
begin
  if AppData.themeEnable then
    ___TagCallbackMap[TagName] := AValue;
end;

constructor TMyCustomWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FCreateSession := False;
  FisJSON := False;
  ActionVar := 'act';
  FOnMenu := nil;
  FOnSearch := nil;
  FOnNotification := nil;
  VisibleModuleName := ClassName;
  //_Initialize( self);
  {$ifdef DEBUG}
  if ((AppData.debug) and (AppData.debugLevel <= 1)) then
    LogUtil.Add(ClassName + '.create()', 'init');
  {$endif}
end;

destructor TMyCustomWebModule.Destroy;
begin
  inherited Destroy;
end;

procedure TMyCustomWebModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  moduleName, methodDefault: string;
begin
  moduleName := FastPlasAppandler.GetActiveModuleName(ARequest);
  methodDefault := MethodMap.Values[moduleName];
  {$ifdef DEBUG}
  if ((AppData.debug) and (AppData.debugLevel <= 1)) then
    LogUtil.Add('handle request: mod=' + moduleName, 'init');
  {$endif}

  // CSRF Security
  if isPost then
  begin
    if not HTMLUtil.CheckCSRF(False) then
    begin
      //----
    end;
  end;

  if not Assigned(OnRequest) then
  begin
    OnRequest := @RequestHandlerDefault;
  end;//-- if not Assigned(OnRequest)

  if methodDefault = '' then
  begin
    AppData.isReady := True;
    inherited HandleRequest(ARequest, AResponse);
  end
  else
  begin
    if Application.Request.Method = methodDefault then
    begin
      AppData.isReady := True;
      inherited HandleRequest(ARequest, AResponse);
    end
    else
    begin
      {$ifdef DEBUG}
      if ((AppData.debug) and (AppData.debugLevel <= 1)) then
        LogUtil.Add('handle request: ' + __(__Err_Http_InvalidMethod), 'init');
      {$endif}
      FastPlasAppandler.DieRaise(__(__Err_Http_InvalidMethod), []);
    end;
  end;
end;

procedure TMyCustomWebModule.RequestHandlerDefault(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  //AResponse.Content:= __Result_Default_Handler;
  AResponse.Content := '';

  case ARequest.Method of
    'GET':
    begin
      Get;
    end;
    'POST':
    begin
      Post;
    end;
    'PUT':
    begin
      Put;
    end;
    'PATCH':
    begin
      Patch;
    end;
    'DELETE':
    begin
      Delete;
    end;
    'OPTIONS':
    begin
      Options;
    end;

    else
    begin
      AResponse.Content := __Result_Default_Handler + ': ' + ARequest.Method;
    end;
  end;

  // add header TimeUsage
  StopTime:= _GetTickCount;
  ElapsedTime:= StopTime - StartTime;
  Application.Response.SetCustomHeader( 'TimeUsage', i2s( ElapsedTime));;

  Handled := True;
end;

procedure TMyCustomWebModule.Get;
begin
  Response.Content := __Result_Default_Get;
end;

procedure TMyCustomWebModule.Post;
begin
  Response.Content := __Result_Default_Post;
end;

procedure TMyCustomWebModule.Put;
begin
  Response.Content := __Result_Default_Put;
end;

procedure TMyCustomWebModule.Patch;
begin
  Response.Content := __Result_Default_Patch;
end;

procedure TMyCustomWebModule.Delete;
begin
  Response.Content := __Result_Default_Delete;
end;

procedure TMyCustomWebModule.Options;
begin
  Response.Content := __Result_Default_Options;
end;

procedure TMyCustomWebModule.LanguageInit;
begin
  if Application.Request.QueryFields.Values['lang'] <> '' then
    LANG := Application.Request.QueryFields.Values['lang']
  else
  begin
    if _SESSION['lang'] <> '' then
    begin
      if Length(_SESSION['lang']) < 5 then
        LANG := _SESSION['lang'];
    end
    else
      LANG := AppData.language;
  end;
  if LANG = 'us' then
    LANG := 'en';
  _SESSION['lang'] := LANG;
end;

procedure TMyCustomWebModule.CloseConnection(const AResponseContent: string;
  ACode: Integer);
begin
  Response.Code := ACode;
  Response.Content := AResponseContent;
  CustomHeader['Connection'] := 'close';
  Response.SendContent;
end;

procedure TMyCustomWebModule.TagController(Sender: TObject;
  const TagString: string; TagParams: TStringList; out ReplaceText: string);
begin
  ThemeUtil.TagController(Sender, TagString, TagParams, ReplaceText);
end;


{ TREQUESTVAR }

function TREQUESTVAR.GetValue(variable: string): string;
begin
  if Application.Request.Method = 'GET' then
    Result := _GET[variable]
  else
  begin
    Result := _POST[variable];
    if Result = '' then
      Result := _GET[variable];
  end;
  Result := _CleanVar(Result);
end;

{ TPOST }

function TPOST.GetCount: integer;
begin
  Result := Application.Request.ContentFields.Count;
end;

function TPOST.GetValue(const Variable: string): string;
var
  _l: TStringList;
  s, postType: string;
begin
  Result := '';
  if Application.Request.Method = 'GET' then
    Exit;
  s := '';
  try
    _l := TStringList.Create;
    _l.Delimiter := ';';
    _l.StrictDelimiter := True;
    _l.DelimitedText := Application.Request.ContentType;
    postType := _l[0];
  except
    postType := '';
  end;
  FreeAndNil(_l);
  if postType = 'text/plain' then
  begin
    Result := Application.Request.Content;
    Exit;
  end;
  if Application.Request.ContentFields.IndexOfName(Variable) = -1 then
  begin
    Result := '';
    Exit;
  end;
  try
    case postType of
      'multipart/form-data':
      begin
        s := Application.Request.ContentFields.Values[Variable];
      end;
      'application/x-www-form-urlencoded':
      begin
        s := Application.Request.ContentFields.Values[Variable];
      end;
    end;
  except
  end;
  if Sanitize then
    Result := _CleanVar(s)
  else
    Result := s
end;

procedure TPOST.SetValue(const Variable: string; AValue: string);
begin
  Application.Request.ContentFields.Values[Variable] := AValue;
end;

{ TGET }

function TGET.GetCount: integer;
begin
  Result := Application.Request.QueryFields.Count;
end;

function TGET.GetValue(const Name: string): string;
begin
  Result := '';
  if Application.Request.QueryFields.IndexOfName(Name) = -1 then
    Exit;
  try
    if Sanitize then
      Result := _CleanVar(Application.Request.QueryFields.Values[Name])
    else
      Result := Application.Request.QueryFields.Values[Name];
  except
  end;
end;

procedure TGET.SetValue(const Name: string; AValue: string);
begin
  Application.Request.QueryFields.Values[Name] := AValue;
end;

{ TSERVER }

function TSERVER.GetValue(const variable: string): string;
begin
  Result := GetEnvironmentVariable(variable);
end;



{ TLazCMSAppandler }

function TFastPlasAppandler.GetURI: string;
begin
  //Result := ThemeUtil.BaseURL + Application.EnvironmentVariable['REQUEST_URI'];
  //Result := ThemeUtil.BaseURL;
  Result := Application.EnvironmentVariable['REQUEST_URI'];
end;

constructor TFastPlasAppandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsDisplayError := False;
  Application.OnException := @ExceptionHandler;
end;

destructor TFastPlasAppandler.Destroy;
begin
  if AppData.themeEnable then
  begin
    FreeAndNil(ThemeUtil);
  end;
  inherited Destroy;
end;

function TFastPlasAppandler.GetActiveModuleName(Arequest: TRequest): string;

  function GetDefaultModuleName: string;
  begin
    if (Application.DefaultModuleName <> '') then
      Result := Application.DefaultModuleName
    else if (ModuleFactory.Count = 1) then
      Result := ModuleFactory[0].ModuleName;
  end;

var
  S: string;
  //reg: TRegExpr;
begin
{
  Result := ARequest.QueryFields.Values[Application.ModuleVariable];
  if (Result = '') then
  begin
    try
      pathInfo := ARequest.PathInfo;
      pathInfo := ExcludeLeadingPathDelimiter(pathInfo);
      reg := TRegExpr.Create;
      for i := 0 to RouteRegexMap.Count - 1 do
      begin
        reg.Expression := RouteRegexMap.ValueFromIndex[i];
        if reg.Exec(pathInfo) then
        begin
          s := RouteRegexMap.Names[i];
          if ModuleFactory.FindModule(S) <> nil then
          begin
            Result := s;
            Break;
          end;
        end;
      end;
    except
      on E: Exception do
        die('OnGetModule: ' + E.Message);
    end;
  end;
  reg.Free;
  if (Result = '') then
  begin
    //if not Application.AllowDefaultModule then
      raise EFPWebError.Create(__(__ErrNoModuleNameForRequest));
    //Result := GetDefaultModuleName;
  end;
  pr( Result);
  die('zzzz');
  Exit;
}

  //--- OLD STYLE
  Result := ARequest.QueryFields.Values[Application.ModuleVariable];
  if (Result = '') then
  begin
    {
    S := ARequest.PathInfo;
    if (Length(S) > 0) and (S[1] = '/') then
      Delete(S, 1, 1);                      //Delete the leading '/' if exists
    I := Length(S);
    if (I > 0) and (S[I] = '/') then
      Delete(S, I, 1);                      //Delete the trailing '/' if exists
    I := Pos('/', S);
    //if (I>0) or Application.PreferModuleName then
    //  Result:=ARequest.GetNextPathInfo;
    if i > 0 then
    begin
      s := Copy(s, 1, i - 1);
    end;
    }

    S := ARequest.PathInfo;
    s := ExcludeLeadingPathDelimiter(s);
    Result := S;
  end;
  if (Result = '') then
  begin
    if not Application.AllowDefaultModule then
      raise EFPWebError.Create(__(__ErrNoModuleNameForRequest));
    Result := GetDefaultModuleName;
  end;

end;

procedure TFastPlasAppandler.OnGetModule(Sender: TObject; ARequest: TRequest;
  var ModuleClass: TCustomHTTPModuleClass);
var
  s, pathInfo: string;
  i, j: integer;
  reg: TRegExpr;
  //m: TCustomHTTPModule;
  //mi: TModuleItem;
  //mc: TCustomHTTPModuleClass;
begin
  InitializeFastPlaz(Sender);
  s := GetActiveModuleName(ARequest);
  if ModuleFactory.FindModule(S) = nil then
  begin
    pathInfo := ARequest.PathInfo;
    pathInfo := IncludeLeadingPathDelimiter(pathInfo);

    // check with Regex - redefine variable
    try
      reg := TRegExpr.Create;
      for i := 0 to RouteRegexMap.Count - 1 do
      begin
        reg.Expression := RouteRegexMap.ValueFromIndex[i];
        if reg.Exec(pathInfo) then
        begin
          s := RouteRegexMap.Names[i];
          if ModuleFactory.FindModule(S) <> nil then
          begin
            ARequest.QueryFields.Values[Application.ModuleVariable] := s;
            for j := 1 to reg.SubExprMatchCount do
            begin
              ARequest.QueryFields.Values['$' + i2s(j)] := reg.Match[j];
              if j = 3 then
                ARequest.QueryFields.Values['act'] := reg.Match[j];
            end;
            Break;
          end;
        end;
      end;
    except
      on E: Exception do
        die('OnGetModule: ' + E.Message);
    end;
    reg.Free;


    //_Redirect( '/?url='+ copy( ARequest.PathInfo, 2, Length(ARequest.PathInfo)-1) + '&' + ARequest.QueryString );

    {
    i := ModuleFactory.IndexOfModule('main');
    If (I=-1) then begin
    end else begin
      mi := ModuleFactory[i];
      mi.SkipStreaming:= True;
      mc := mi.ModuleClass;
      ModuleClass:= mi.ModuleClass;
    end;
    }

    {
    mi := ModuleFactory.FindModule('main');
    mi.SkipStreaming:= True;
    mc := mi.ModuleClass;
    ModuleClass:= mc;
    }

  end //-- if ModuleFactory.FindModule(S) = nil
  else
  begin
  end; //-- if ModuleFactory.FindModule(S) = nil

  // Load Module @startup
  if ModuleLoaded.Count > 0 then
  begin
    for i := 0 to ModuleLoaded.Count - 1 do
    begin
      ModuleLoaded[i].ModuleStore := LoadModule(ModuleLoaded[i].ModuleName);
    end;
  end;

end;

procedure TFastPlasAppandler.ExceptionHandler(Sender: TObject; E: Exception);
begin
  //Application.ShowException(E);
  //Application.Terminate;
  LogUtil.Add(Sender.ClassName + ': ' + E.Message, 'exception');
end;

function TFastPlasAppandler.Tag_InternalContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := 'Tag_InternalContent_Handler';
end;

procedure TFastPlasAppandler.AddLoadModule(const ModuleName: string;
  ModuleClass: TMyCustomWebModuleClass; SkipStreaming: boolean);
var
  i: integer;
  mi: TModuleLoadedItem;
begin
  i := ModuleLoaded.IndexOfModule(ModuleName);
  if i = -1 then
  begin
    mi := ModuleLoaded.Add as TModuleLoadedItem;
    mi.ModuleName := ModuleName;
    mi.ModuleStore := nil;
  end
  else
    mi := ModuleLoaded[i];

  mi.SkipStreaming := SkipStreaming;
end;

function TFastPlasAppandler.LoadModule(const ModuleName: string): TMyCustomWebModule;
var
  m: TCustomHTTPModule;
  mi: TModuleItem;
  mc: TCustomHTTPModuleClass;
  i: integer;
begin
  Result := nil;
  i := ModuleFactory.IndexOfModule(ModuleName);
  if i <> -1 then
  begin
    mi := ModuleFactory[I];
    mc := mi.ModuleClass;
    m := FastPlasAppandler.FindModule(mc);
    if m = nil then
    begin
      if assigned(mi) and mi.SkipStreaming then
        M := MC.CreateNew(Self)
      else
        M := MC.Create(Self);
      M.Name := ModuleName;
      Result := TMyCustomWebModule(M);
    end;
  end;
end;

procedure TFastPlasAppandler.RegisterModule(const ModuleName: string;
  ModuleClass: TMyCustomWebModuleClass; LoadOnStart: boolean; SkipStreaming: boolean);
var
  I: integer;
  MI: TModuleItem;
begin
  I := ModuleFactory.IndexOfModule(ModuleName);
  if (I = -1) then
  begin
    MI := ModuleFactory.Add as TModuleItem;
    MI.ModuleName := ModuleName;
  end
  else
    MI := ModuleFactory[I];
  MI.ModuleClass := ModuleClass;
  MI.SkipStreaming := SkipStreaming;

  if LoadOnStart then
  begin
    AddLoadModule(ModuleName, ModuleClass, SkipStreaming);
  end; //-- LoadOnStart

end;

function TFastPlasAppandler.FindModule(ModuleClass: TCustomHTTPModuleClass):
TCustomHTTPModule;
var
  i: integer;
begin
  i := Application.ComponentCount - 1;
  while (i >= 0) and (not ((Application.Components[i] is ModuleClass) and
      (TCustomHTTPModule(Application.Components[i]).Kind <> wkOneShot))) do
    Dec(i);
  if (i >= 0) then
    Result := Application.Components[i] as TCustomHTTPModule
  else
    Result := nil;
end;

procedure TFastPlasAppandler.AddLog(Message: string);
begin
  if LogUtil = nil then
    LogUtil := TLogUtil.Create;
  LogUtil.Add(Message);
end;

procedure TFastPlasAppandler.DieRaise(const Fmt: string; const Args: array of const);
begin
  Die('<div class="warning">' + Format(Fmt, Args) + '</div>');
end;

initialization
  AppData.isReady := False;
  StartTime := _GetTickCount;
  Sanitize := True;
  //MemoryAllocated := GetHeapStatus.TotalAllocated;
  MemoryAllocated := SysGetHeapStatus.TotalAllocated;
  SessionController := TSessionController.Create();
  FastPlasAppandler := TFastPlasAppandler.Create(nil);
  ModuleLoaded := TModuleLoaded.Create(TModuleLoadedItem);
  Route := TRoute.Create;
  ModUtil := TModUtil.Create;
  _DebugInfo := TStringList.Create;
  _REQUEST := TREQUESTVAR.Create;
  _POST := TPOST.Create;
  _GET := TGET.Create;
  _SESSION := TSESSION.Create;
  _SERVER := TSERVER.Create;
  MethodMap := TStringList.Create;

finalization
  FreeAndNil(RouteRegexMap);
  FreeAndNil(MethodMap);
  FreeAndNil(_SESSION);
  FreeAndNil(_GET);
  FreeAndNil(_POST);
  FreeAndNil(_REQUEST);
  FreeAndNil(_SERVER);
  FreeAndNil(_DebugInfo);
  FreeAndNil(ModUtil);
  FreeAndNil(Route);
  FreeAndNil(ModuleLoaded);
  FreeAndNil(FastPlasAppandler);
  FreeAndNil(SessionController);
  {$IFDEF HEAPTRACE}
  DeleteFile(AppData.tempDir + DirectorySeparator + 'HEAP.TXT');
  SetHeapTraceOutput(AppData.tempDir + DirectorySeparator + 'HEAP.TXT');
  {$ENDIF}

end.

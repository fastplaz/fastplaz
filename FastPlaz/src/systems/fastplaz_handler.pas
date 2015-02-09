unit fastplaz_handler;

{$mode objfpc}{$H+}
{$include ../../define.inc}

interface

uses
  {$IFDEF HEAPTRACE}
  heaptrc,
  {$ENDIF}
  sqldb, gettext, session_controller, module_controller,
  config_lib,
  fpcgi, httpdefs, fpHTTP, fpWeb, webutil, custweb, dateutils,
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

  __Content_Not_Found = 'Nothing Found';
  __Tag_Content_Not_Found = 'Tags Content "%s" Not Found';

  // methode
  ALL = '';
  GET = 'GET';
  POST = 'POST';
  PUT = 'PUT';
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
    hitStorage: string;
    databaseRead,
    databaseWrite: string;
    databaseActive,
    useDatabase,
    initialized,
    debug: boolean;
    isReady: boolean;
  end;

  TOnBlockController = procedure(Sender: TObject; FunctionName: string; Parameter: TStrings;
    var ResponseString: string) of object;
  TTagCallback = function(const ATagName: string; AParams: TStringList): string of object;

  { TMyCustomWebModule }

  //TMyCustomWebModule  = class(TFPWebModule)
  TMyCustomWebModule = class(TCustomFPWebModule)
  private
    FisJSON, FCreateSession: boolean;
    FOnBlockController: TOnBlockController;

    function GetBaseURL: string;
    function GetEnvirontment(const KeyName: string): string;
    function GetIsDelete: boolean;
    function GetIsGet: boolean;
    function GetIsPost: boolean;
    function GetIsPut: boolean;
    function GetSession: TSessionController;
    function GetSessionID: string;
    function GetTag(const TagName: string): TTagCallback;
    function GetURI: string;
    procedure SetTag(const TagName: string; AValue: TTagCallback);

  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;

    procedure LanguageInit;

    property URI: string read GetURI;
    property Environtment[const KeyName: string]: string read GetEnvirontment;

    property Tags[const TagName: string]: TTagCallback read GetTag write SetTag; default;
    procedure TagController(Sender: TObject; const TagString: string; TagParams: TStringList;
      Out ReplaceText: string);
    property BaseURL: string read GetBaseURL;
    property OnBlockController: TOnBlockController read FOnBlockController write FOnBlockController;

    property isPost: boolean read GetIsPost;
    property isGet: boolean read GetIsGet;
    property isPut: boolean read GetIsPut;
    property isDelete: boolean read GetIsDelete;

    property CreateSession: boolean read FCreateSession write FCreateSession;
    property Session: TSessionController read GetSession;
    property SessionID: string read GetSessionID;
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
    procedure OnGetModule(Sender: TObject; ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    function Tag_InternalContent_Handler(const TagName: string; Params: TStringList): string;

    function FindModule(ModuleClass: TCustomHTTPModuleClass): TCustomHTTPModule;
    procedure AddLog(Message: string);
    procedure DieRaise(const Fmt: string; const Args: array of const);
  end;

  TGET = class
  private
    function GetValue(const Name: string): string;
    procedure SetValue(const Name: string; AValue: string);
  public
    property Values[Name: string]: string read GetValue write SetValue; default;
  end;

  { TPOST }

  TPOST = class
  private
    function GetValue(const Variable: string): string;
    procedure SetValue(const Variable: string; AValue: string);
  public
    property Values[variable: string]: string read GetValue write SetValue; default;
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

  { TRoute }

  TRoute = class
  private
  public
    procedure Add(const PatternURL: string; ModuleClass: TCustomHTTPModuleClass; Method: string = '';
      SkipStreaming: boolean = True);
  end;

procedure InitializeFastPlaz(Sender: TObject = nil);
procedure Redirect(const URL: string);

procedure DisplayError(const Message: string);
procedure Debug(const Message: integer; const Key: string = '');
procedure Debug(const Message: string; const Key: string = '');
procedure Debug(const Sender: TObject; const Key: string = '');

var
  AppData: TMainData;
  Config: TMyConfig;
  SessionController: TSessionController;
  FastPlasAppandler: TFastPlasAppandler;
  ModUtil: TModUtil;
  Route: TRoute;
  _GET: TGET;
  _POST: TPOST;
  _SESSION: TSESSION;
  _REQUEST: TREQUESTVAR;
  _DebugInfo: TStringList;
  StartTime, StopTime, ElapsedTime: cardinal;

implementation

uses common, language_lib, database_lib, logutil_lib, theme_controller,
  about_controller;

var
  MethodMap: TStringList;

function _CleanVar(const Value: string): string;
begin
  Result := mysql_real_escape_string(Value);
end;


procedure InitializeFastPlaz(Sender: TObject);
begin
  if AppData.initialized then
    Exit;
  AppData.initialized := True;
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
  AppData.sitename := Config.GetValue(_SYSTEM_SITENAME, _APP);
  AppData.slogan := Config.GetValue(_SYSTEM_SLOGAN, _APP);
  AppData.baseUrl := Config.GetValue(_SYSTEM_BASEURL, '');
  AppData.admin_email := Config.GetValue(_SYSTEM_WEBMASTER_EMAIL, Application.Email);
  AppData.language := Config.GetValue(_SYSTEM_LANGUAGE_DEFAULT, 'en');
  AppData.themeEnable := Config.GetValue(_SYSTEM_THEME_ENABLE, True);
  AppData.theme := Config.GetValue(_SYSTEM_THEME, 'default');
  AppData.debug := Config.GetValue(_SYSTEM_DEBUG, False);
  AppData.cacheType := Config.GetValue(_SYSTEM_CACHE_TYPE, 'file');
  AppData.cacheWrite := Config.GetValue(_SYSTEM_CACHE_WRITE, True);

  AppData.cacheTime := Config.GetValue(_SYSTEM_CACHE_TIME, 3);
  AppData.tempDir := Config.GetValue(_SYSTEM_TEMP_DIR, 'ztemp');
  AppData.SessionDir := Config.GetValue(_SYSTEM_SESSION_DIR, '');
  AppData.hitStorage := Config.GetValue(_SYSTEM_HIT_STORAGE, '');

  if AppData.baseUrl = '' then
  begin
    AppData.baseUrl := 'http://' + GetEnvironmentVariable('SERVER_NAME') +
      ExtractFilePath(GetEnvironmentVariable('SCRIPT_NAME'));
  end;

  if AppData.hitStorage = 'file' then
    ThemeUtil.HitType := htFile;
  if AppData.hitStorage = 'database' then
    ThemeUtil.HitType := htDatabase;
  if AppData.hitStorage = 'sqlite' then
    ThemeUtil.HitType := htSQLite;

  if AppData.themeEnable then
  begin
    ThemeUtil := TThemeUtil.Create;
  end;

  //LogUtil.registerError('auw');
  //-- process the homepage
  if AppData.module = '' then
  begin

  end;

  //-- session
  if AppData.SessionDir <> '' then
    SessionController.SessionDir := AppData.SessionDir;
  if not SessionController.StartSession then
  begin
    //SessionController.EndSession;
    //SessionController.StartSession;
  end;
  SessionController.TimeOut := Config.GetValue(_SYSTEM_SESSION_TIMEOUT, 0);
  ;
  //todo: auto clean-up session
  //-- session - end

  //-- language

  AppData.isReady := True;
end;

procedure Redirect(const URL: string);
begin
  Application.Response.Content := '';
  //Application.Response.SendRedirect(URL);
  //Application.Response.SendResponse;
  Application.Response.Location := URL;
  Application.Response.SendHeaders;
end;


procedure DisplayError(const Message: string);
begin
  FastPlasAppandler.isDisplayError := True;
  if not AppData.themeEnable then
  begin
    die(Message);
  end;

  if (not AppData.useDatabase) or (AppData.useDatabase and AppData.databaseActive) then
  begin
    Application.Response.Contents.Text := ThemeUtil.Render( @ThemeUtil.TagDefault, 'error');
    Application.Response.Contents.Text :=
      ReplaceAll(Application.Response.Contents.Text, [ThemeUtil.StartDelimiter + '$maincontent' +
      ThemeUtil.EndDelimiter], '<div class="box error">' + Message + '</div>');
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

{ TRoute }

// SkipStreaming: True -> skip the streaming support - which means you don't require Lazarus!!!
procedure TRoute.Add(const PatternURL: string; ModuleClass: TCustomHTTPModuleClass; Method: string;
  SkipStreaming: boolean);
var
  moduleName: string;
  pattern_url: TStrings;
  i: integer;
  mi: TModuleItem;
  mc: TCustomHTTPModuleClass;
  m: TCustomHTTPModule;
begin

  // prepare pattern-url for next version
  pattern_url := Explode(PatternURL, '/');
  moduleName := pattern_url[0];

  if not Assigned(MethodMap) then
    MethodMap := TStringList.Create;
  MethodMap.Values[moduleName] := Method;

  RegisterHTTPModule(moduleName, ModuleClass, SkipStreaming);

  //mi := ModuleFactory.FindModule( moduleName);
  i := ModuleFactory.IndexOfModule(moduleName);
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

end;

{ TSESSION }

function TSESSION.GetValue(variable: string): variant;
begin
  Result := SessionController[variable];
end;

procedure TSESSION.SetValue(variable: string; AValue: variant);
begin
  SessionController[variable] := AValue;
end;

function TSESSION.ReadDateTime(const variable: string): TDateTime;
begin
  Result := SessionController.ReadDateTime(variable);
end;

{$if fpc_fullversion >= 20701}
class function TStringHash.hash(s: string; n: integer): integer;
var
  c: char;
begin
  Result := 0;
  for c in s do
    Inc(Result, Ord(c));
  Result := Result mod n;
end;

{$endif fpc_fullversion >= 20701}

{ TMyCustomWebModule }

function TMyCustomWebModule.GetBaseURL: string;
begin
  Result := ThemeUtil.BaseURL;
end;

function TMyCustomWebModule.GetEnvirontment(const KeyName: string): string;
begin
  Result := Application.EnvironmentVariable[KeyName];
end;

function TMyCustomWebModule.GetIsDelete: boolean;
begin
  Result := False;
  if Application.Request.Method = 'DELETE' then
    Result := True;
end;

function TMyCustomWebModule.GetIsGet: boolean;
begin
  Result := False;
  if Application.Request.Method = GET then
    Result := True;
end;

function TMyCustomWebModule.GetIsPost: boolean;
begin
  Result := False;
  if Application.Request.Method = POST then
    Result := True;
end;

function TMyCustomWebModule.GetIsPut: boolean;
begin
  Result := False;
  if Application.Request.Method = PUT then
    Result := True;
end;

function TMyCustomWebModule.GetSession: TSessionController;
begin
  Result := SessionController;
end;

function TMyCustomWebModule.GetSessionID: string;
begin
  Result := SessionController.SessionID;
end;

function TMyCustomWebModule.GetTag(const TagName: string): TTagCallback;
begin
  if AppData.themeEnable then
    Result := ___TagCallbackMap[TagName];
end;

function TMyCustomWebModule.GetURI: string;
begin
  Result := FastPlasAppandler.URI;
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
  //_Initialize( self);
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
  if methodDefault = '' then
  begin
    AppData.isReady:=True;
    inherited HandleRequest(ARequest, AResponse);
  end
  else
  begin
    if Application.Request.Method = methodDefault then
      inherited HandleRequest(ARequest, AResponse)
    else
    begin
      FastPlasAppandler.DieRaise(__(__Err_Http_InvalidMethod), []);
    end;
  end;
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

procedure TMyCustomWebModule.TagController(Sender: TObject; const TagString: string;
  TagParams: TStringList; out ReplaceText: string);
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

function TPOST.GetValue(const Variable: string): string;
var
  _l: TStringList;
  s, postType: string;
begin
  Result := '';
  if Application.Request.Method <> 'POST' then
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
  Result := _CleanVar(s);
end;

procedure TPOST.SetValue(const Variable: string; AValue: string);
begin
  Application.Request.ContentFields.Values[Variable] := AValue;
end;

{ TGET }

function TGET.GetValue(const Name: string): string;
begin
  if Application.Request.QueryFields.IndexOfName(Name) = -1 then
    Result := ''
  else
    Result := _CleanVar(Application.Request.QueryFields.Values[Name]);
end;

procedure TGET.SetValue(const Name: string; AValue: string);
begin
  Application.Request.QueryFields.Values[Name] := AValue;
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
end;

destructor TFastPlasAppandler.Destroy;
begin
  if AppData.themeEnable then
  begin
    FreeAndNil(ThemeUtil);
    ;
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
  I: integer;
begin
  Result := ARequest.QueryFields.Values[Application.ModuleVariable];
  if (Result = '') then
  begin
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
  s: string;
  //  m  : TCustomHTTPModule;
  //  mi : TModuleItem;
  //  mc : TCustomHTTPModuleClass;
begin
  InitializeFastPlaz(Sender);
  s := GetActiveModuleName(ARequest);
  if ModuleFactory.FindModule(S) = nil then
  begin
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

  end
  else
  begin
  end;

end;

procedure TFastPlasAppandler.ExceptionHandler(Sender: TObject; E: Exception);
begin
  die(e.Message);

end;

function TFastPlasAppandler.Tag_InternalContent_Handler(const TagName: string; Params: TStringList): string;
begin
  Result := 'Tag_InternalContent_Handler';
end;

function TFastPlasAppandler.FindModule(ModuleClass: TCustomHTTPModuleClass): TCustomHTTPModule;
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
  SessionController := TSessionController.Create();
  FastPlasAppandler := TFastPlasAppandler.Create(nil);
  Route := TRoute.Create;
  ModUtil := TModUtil.Create;
  _DebugInfo := TStringList.Create;
  _REQUEST := TREQUESTVAR.Create;
  _POST := TPOST.Create;
  _GET := TGET.Create;
  _SESSION := TSESSION.Create;
  MethodMap := TStringList.Create;

finalization
  FreeAndNil(MethodMap);
  FreeAndNil(_SESSION);
  FreeAndNil(_GET);
  FreeAndNil(_POST);
  FreeAndNil(_REQUEST);
  FreeAndNil(_DebugInfo);
  FreeAndNil(ModUtil);
  FreeAndNil(Route);
  FreeAndNil(FastPlasAppandler);
  FreeAndNil(SessionController);
  {$IFDEF HEAPTRACE}
  DeleteFile(AppData.tempDir + DirectorySeparator + 'HEAP.TXT');
  SetHeapTraceOutput(AppData.tempDir + DirectorySeparator + 'HEAP.TXT');
  {$ENDIF}

end.

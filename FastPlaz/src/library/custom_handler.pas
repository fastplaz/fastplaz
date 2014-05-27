unit custom_handler;

{$mode objfpc}{$H+}
{$include define.inc}

interface

uses
  sqldb, gettext, session_controller,
  fpcgi, httpdefs, fpHTTP, fpWeb, webutil, custweb, dateutils,
  SysUtils, Classes;

const
  LF = #13#10;
  TDateTimeEpsilon = 2.2204460493e-16;

resourcestring
  __ErrNoModuleNameForRequest = 'Could not determine HTTP module name for request';

  // theme
  __Err_App_Init = '<a href="%s">click here</a> if you need to initialize your webapp''s structure.<br>Make sure target directory is writeable';
  __Err_Theme_Not_Exists = 'file ''%s'' does not exist in theme ''%s''';
  __Err_Theme_Tag_NotImplemented = 'Template tag [%s] does not implemented yet.';
  __Err_Theme_Modul_NotFond = 'Modul "%s" not found';

  __Content_Not_Found = 'Nothing Found';
  __Tag_Content_Not_Found = 'Tags Content "%s" Not Found';

Type

  TMainData = record
    module, modtype, func : string;
    sitename,
    language,
    theme,
    cache_type,
    temp_dir: string;
    table_prefix: string;
    SessionID: string;
    SessionDir: string;
    initialized,
    debug : boolean;
  end;

  TOnBlockController = Procedure (Sender : TObject; FunctionName: string; Parameter:TStrings; var ResponseString : string ) of object;
  TTagCallback = function (const ATagName: String; AParams: TStringList): String of object;

  { TMyCustomWebModule }

  //TMyCustomWebModule  = class(TFPWebModule)
  TMyCustomWebModule  = class(TCustomFPWebModule)
  private
    FCreateSession: Boolean;
    FOnBlockController: TOnBlockController;

    function GetBaseURL: string;
    function GetSession: TSessionController;
    function GetSessionID: string;
    function GetTag(const TagName: String): TTagCallback;
    procedure SetTag(const TagName: String; AValue: TTagCallback);

  public
    Constructor CreateNew(AOwner : TComponent; CreateMode : Integer); override;
    destructor Destroy; override;
    procedure LanguageInit;

    property  Tags[const TagName: String]: TTagCallback read GetTag write SetTag; default;
    procedure TagController(Sender: TObject; const TagString:String; TagParams: TStringList; Out ReplaceText: String);
    property BaseURL : string Read GetBaseURL;
    property OnBlockController: TOnBlockController Read FOnBlockController write FOnBlockController;

    property CreateSession : Boolean Read FCreateSession Write FCreateSession;
    property Session : TSessionController Read GetSession;
    property SessionID : string Read GetSessionID;
  end;

  { TLazCMSAppandler }

  TFastPlasAppandler  = class(TComponent)
  private
  public
    function _GetModuleName(Arequest: TRequest): string;
    Procedure OnGetModule(Sender : TObject; ARequest : TRequest; Var ModuleClass : TCustomHTTPModuleClass);
  end;

  TGET = class
  private
    function GetValue(const Name: string): string;
    procedure SetValue(const Name: string; AValue: string);
  public
    property Values[ Name: string]: string read GetValue write SetValue; default;
  end;

  { TPOST }

  TPOST = class
  private
    function GetValue(const variable: string): string;
    procedure SetValue(const variable: string; AValue: string);
  public
    property Values[ variable: string]: string read GetValue write SetValue; default;
  end;

  { TREQUESTVAR }

  TREQUESTVAR = class
  private
    function GetValue( variable: string): string;
  public
    property Values[ variable: string]: string read GetValue; default;
  end;

  { TSESSION }

  TSESSION = class
  private
    function GetValue( variable: string): string;
    procedure SetValue( variable: string; AValue: string);
  public
    property Values[ variable: string]: string read GetValue write SetValue; default;
    function ReadDateTime(const variable:string):TDateTime;
  end;

function _CleanVar( const variable:string):string;
procedure echo( const Message:string);
Procedure _Initialize( Sender: TObject = nil);
procedure _Redirect( const URL:string);
procedure Debug( const Message: Integer; const Key:string = '');
procedure Debug( const Message: string; const Key:string = '');
procedure Debug( const Sender: TObject; const Key:string = '');
Procedure AddRoute(Const ModuleName : String; ModuleClass : TCustomHTTPModuleClass; SkipStreaming : Boolean = true);


var
  AppData : TMainData;
  SessionController : TSessionController;
  FastPlasAppandler: TFastPlasAppandler;
  _GET : TGET;
  _POST : TPOST;
  _SESSION : TSESSION;
  _REQUEST : TREQUESTVAR;
  _DebugInfo : TStringList;
  _StartTime, _StopTime, _ElapsedTime : Cardinal;

implementation

uses common, language_lib, database_lib, logutil_lib, theme_controller;

function _CleanVar(const variable: string): string;
begin
  // code for secure variable
  Result := variable;
end;

procedure echo(const Message: string);
begin
  Application.Response.Contents.Add( Message);
end;

procedure _Initialize(Sender: TObject);
begin
  if AppData.initialized then Exit;
  AppData.initialized:= true;
  AppData.module:= _GET[ 'mod'];
  AppData.modtype:= _GET[ 'type'];
  AppData.func:= _GET[ 'func'];
  if AppData.modtype='' then AppData.modtype := 'user';
  if AppData.func='' then AppData.func := 'main';
  try
    AppData.SessionID := TMyCustomWebModule( Sender).Session.SessionID;
  except
  end;

  AppData.sitename := Config.GetValue( _SYSTEM_SITENAME, _APP);
  AppData.language := Config.GetValue( _SYSTEM_LANGUAGE_DEFAULT, 'en');
  AppData.theme := Config.GetValue( _SYSTEM_THEME, 'default');
  AppData.debug := Config.GetValue( _SYSTEM_DEBUG, false);
  AppData.table_prefix:= Config.GetValue( _DATABASE_TABLE_PREFIX, '');
  AppData.cache_type := Config.GetValue( _SYSTEM_CACHE_TYPE, 'file');
  AppData.temp_dir := Config.GetValue( _SYSTEM_TEMP_DIR, 'ztemp');
  AppData.SessionDir:= Config.GetValue( _SYSTEM_SESSION_DIR, '');

  //LogUtil.registerError('auw');
  //-- process the homepage
  if AppData.module = '' then begin

  end;

  //-- session
  if AppData.SessionDir <> '' then
  begin
    SessionController.SessionDir:=AppData.SessionDir;
  end;
  SessionController.StartSession;
  SessionController.IsExpired;
  //-- session - end

  //-- language

end;

procedure _Redirect(const URL: string);
begin
  Application.Response.SendRedirect( URL);
  Application.Response.SendResponse;
end;

procedure Debug(const Message: Integer; const Key: string);
begin
  if key <> '' then
    echo( '<pre>' + Key + ': ' + i2s(Message)+ '</pre>')
  else
    echo( '<pre>' + i2s(Message) + '</pre>');
  die('jleeeb');
end;

procedure Debug(const Message: string; const Key: string);
begin
  if key <> '' then
    echo( '<pre>' + Key + ': ' + Message + '</pre>')
  else
    echo( '<pre>' + Message + '</pre>');
end;

procedure Debug(const Sender: TObject; const Key: string);
var
  prefix, suffic, html : string;
begin
  if not Assigned(Sender) then
  begin
    echo( 'Sender is not assigned');
    Exit;
  end;
  prefix := '<pre>';
  suffic := '</pre>';
  html := Sender.ClassName;
  if Key <> '' then html := html + '('+Key+'): ' else html:= html + ': ';

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

  echo( prefix + html + suffic);
end;

procedure AddRoute(const ModuleName: String; ModuleClass: TCustomHTTPModuleClass; SkipStreaming: Boolean);
begin
  RegisterHTTPModule( ModuleName, ModuleClass, SkipStreaming);
end;

{ TSESSION }

function TSESSION.GetValue(variable: string): string;
begin
  Result:=SessionController[variable];
end;

procedure TSESSION.SetValue( variable: string; AValue: string);
begin
  SessionController[variable]:=AValue;
end;

function TSESSION.ReadDateTime(const variable: string): TDateTime;
begin
  Result:=SessionController.ReadDateTime(variable);
end;

{$if fpc_fullversion >= 20701}
class function TStringHash.hash(s: String; n: Integer): Integer;
var
  c: Char;
begin
  Result := 0;
  for c in s do
    Inc(Result,Ord(c));
  Result := Result mod n;
end;
{$endif fpc_fullversion >= 20701}

{ TMyCustomWebModule }

function TMyCustomWebModule.GetBaseURL: string;
begin
  Result:=ThemeUtil.BaseURL;
end;


function TMyCustomWebModule.GetSession: TSessionController;
begin
  Result := SessionController;
end;

function TMyCustomWebModule.GetSessionID: string;
begin
  Result:=SessionController.SessionID;
end;

function TMyCustomWebModule.GetTag(const TagName: String): TTagCallback;
begin
  Result := ___TagCallbackMap[TagName];
end;

procedure TMyCustomWebModule.SetTag(const TagName: String; AValue: TTagCallback );
begin
  ___TagCallbackMap[TagName] := AValue;
end;

constructor TMyCustomWebModule.CreateNew(AOwner: TComponent; CreateMode: Integer );
begin
  inherited CreateNew( AOwner, CreateMode);
  FCreateSession := false;
  //_Initialize( self);
end;

destructor TMyCustomWebModule.Destroy;
begin
  inherited Destroy;
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
        LANG := _SESSION['lang']
    end
    else
      LANG := AppData.language;
  end;
  if LANG = 'us' then LANG := 'en';
  _SESSION['lang'] := LANG;
end;

procedure TMyCustomWebModule.TagController(Sender: TObject;
  const TagString: String; TagParams: TStringList; out ReplaceText: String);
begin
  ThemeUtil.TagController( Sender,TagString, TagParams, ReplaceText);
end;


{ TREQUESTVAR }

function TREQUESTVAR.GetValue( variable: string): string;
begin
  if Application.Request.Method = 'GET' then
    Result := _GET[ variable]
  else begin
    Result := _POST[ variable];
    if Result = '' then Result := _GET[ variable];
  end;
  Result := _CleanVar( Result);
end;

{ TPOST }

function TPOST.GetValue(const variable: string): string;
var
  _l : TStringList;
  _s, _type : string;
begin
  if Application.Request.Method <> 'POST' then Exit;
  _s := '';
  _l := TStringList.Create;
  _l.Delimiter:=';';
  _l.StrictDelimiter := true;
  _l.DelimitedText:= Application.Request.ContentType;;
  _type :=_l[0];
  FreeAndNil( _l);
  if _type = 'text/plain' then begin
    Result := Application.Request.Content;
    Exit;
  end;
  if Application.Request.ContentFields.IndexOfName( variable) = -1 then begin
    Result := '';
    Exit;
  end;
  case _type of
    'multipart/form-data' : begin
      _s := Application.Request.ContentFields.Values[ variable];
    end;
    'application/x-www-form-urlencoded' : begin
      _s := Application.Request.ContentFields.Values[ variable];
    end;
  end;
  Result := _CleanVar(_s);
end;

procedure TPOST.SetValue(const variable: string; AValue: string);
begin
  Application.Request.ContentFields.Values[ variable] := AValue;
end;

{ TGET }

function TGET.GetValue(const Name: string): string;
begin
  if Application.Request.QueryFields.IndexOfName( Name) = -1 then
    Result := ''
  else
    Result := _CleanVar( Application.Request.QueryFields.Values[ Name]);
end;

procedure TGET.SetValue(const Name: string; AValue: string);
begin
  Application.Request.QueryFields.Values[ Name] := AValue;
end;

{ TLazCMSAppandler }

function TFastPlasAppandler._GetModuleName(Arequest: TRequest): string;
  function GetDefaultModuleName : String;
  begin
  if (Application.DefaultModuleName<>'') then
    Result:=Application.DefaultModuleName
  else if (ModuleFactory.Count=1) then
    Result:=ModuleFactory[0].ModuleName;
  end;
var
  S : String;
  I : Integer;
begin
  Result:=ARequest.QueryFields.Values[Application.ModuleVariable];
  If (Result='') then begin
    S:=ARequest.PathInfo;
    If (Length(S)>0) and (S[1]='/') then
      Delete(S,1,1);                      //Delete the leading '/' if exists
    I:=Length(S);
    If (I>0) and (S[I]='/') then
      Delete(S,I,1);                      //Delete the trailing '/' if exists
    I:=Pos('/',S);
    //if (I>0) or Application.PreferModuleName then
    //  Result:=ARequest.GetNextPathInfo;
    if i > 0 then begin
      s := Copy(s,1,i-1);
    end;
    Result := S;
  end;
  If (Result='') then begin
    if Not Application.AllowDefaultModule then
      Raise EFPWebError.Create( __(__ErrNoModuleNameForRequest));
    Result:=GetDefaultModuleName;
  end;

end;

procedure TFastPlasAppandler.OnGetModule(Sender: TObject; ARequest: TRequest;
  var ModuleClass: TCustomHTTPModuleClass);
var
  s : string;
//  m  : TCustomHTTPModule;
//  mi : TModuleItem;
//  mc : TCustomHTTPModuleClass;
begin
  _Initialize( Sender);
  s := _GetModuleName( ARequest);
  if ModuleFactory.FindModule( S) = nil then begin
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

  end else begin
  end;

end;

initialization
  _StartTime := _GetTickCount;
  SessionController := TSessionController.Create();
  FastPlasAppandler := TFastPlasAppandler.Create(nil);
  _DebugInfo := TStringList.Create;
  _REQUEST := TREQUESTVAR.Create;
  _POST := TPOST.Create;
  _GET := TGET.Create;
  _SESSION := TSESSION.Create;

finalization
  FreeAndNil(_SESSION);
  FreeAndNil(_GET);
  FreeAndNil(_POST);
  FreeAndNil(_REQUEST);
  FreeAndNil(_DebugInfo);
  FreeAndNil(SessionController);
  FreeAndNil(FastPlasAppandler);

end.


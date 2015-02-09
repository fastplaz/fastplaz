unit theme_controller;

{$mode objfpc}{$H+}

interface

uses
  {$if fpc_fullversion >= 20701}
    ghashmap,
  {$else fpc_fullversion >= 20701}
    fgl,
  {$endif fpc_fullversion >= 20701}
  fpcgi, fpTemplate, fphttp, fpWeb, fpjson, HTTPDefs, dateutils,
  Regex, RegExpr, sqldb,
  common, fastplaz_handler, database_lib, datetime_lib,
  Classes, SysUtils;

const
  // if use { ... }
  //__FOREACH_START = '{foreach([\.\$A-Za-z= ]+)}';
  //__FOREACH_END = '\{/foreach[\.\$A-Za-z0-9= ]+}';

  __FOREACH_START = '\[foreach([\.\$A-Za-z0-9=_ ]+)\]';
  __FOREACH_END = '\[/foreach[\.\$A-Za-z0-9=_ ]+\]';

  __CONDITIONAL_IF_START = '\[if([\.\$A-Za-z_0-9=\ ]+)\]';
  __CONDITIONAL_IF_VALUE = '([\.\$A-Za-z0-9=_ ]+)(\[else\]+)?([\.\$A-Za-z0-9=_ ]+)';
  __CONDITIONAL_IF_END = '\[/if\]';

  __HITS_FILENAME = 'hits.log';

  __ERR_THEME_NOT_ENABLED = 'Using Theme''s features but it is not enabled. Check your config.json file.';

  __ERR_RECAPTCHA_INVALID_KEY = 'Invalid recaptcha publickey.<br />Create Key from %s';
  ReCaptcha_SignupUrl = 'https://www.google.com/recaptcha/admin';

type

  // based on qtemplate
  {$if fpc_fullversion >= 20701}
    { TStringHash }
    TStringHash = class
      class function hash(s: String; n: Integer): Integer;
    end;
    generic TStringHashMap<T> = class(specialize THashMap<String,T,TStringHash>) end;
  {$else fpc_fullversion >= 20701}
    generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
  {$endif fpc_fullversion >= 20701}

  TAssignVarMap = specialize TStringHashMap<Pointer>;
  TTagCallbackMap = specialize TStringHashMap<TTagCallback>; // based on qtemplate

  TRenderType = ( rtNone, rtSmarty, rtFPTemplate);

  { THTMLHead }

  THTMLHead = class
  private
  public
    JS, CSS, Meta : TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure AddJS( const FileName:string);
    procedure AddCSS( const FileName:string; const Media:string='all');
    procedure AddMeta( const Name:string; const Content:string; const MetaType:string = 'name');
  end;

  THitType = (
    htNone,
    htFile,
    htDatabase,
    htSQLite
  );

  { TThemeUtil }

  TThemeUtil = class
  private
    FHits : TStringList;
    FHitType : THitType;
    FBaseURL : string;
    FCacheTime: integer;
    FEndDelimiter, FStartDelimiter, FParamValueSeparator: string;
    FIsJSON: boolean;
    FThemeName, FLayout, FThemeExtension: string;
    FRenderType : TRenderType;
    FHTMLHead : THTMLHead;
    FTrimForce: boolean;
    FTrimWhiteSpace: boolean;
    isRendering,
    isRenderingModule : boolean;
    function GetAssignVar(const TagName: String): Pointer;
    function GetBaseURL: string;
    function GetHitCount(const URL: String): integer;
    function GetRenderType: TRenderType;
    function GetThemeName: string;
    function GetActiveModuleName(Arequest: TRequest): string;
    function getVarValue(const VariableName: string): variant;
    procedure SetAssignVar(const TagName: String; AValue: Pointer);
    procedure SetCacheTime(AValue: integer);
    procedure SetIsJSON(AValue: boolean);
    procedure SetRenderType(AValue: TRenderType);
    procedure SetThemeName(AValue: string);
    procedure SetTrimForce(AValue: boolean);
    procedure SetTrimWhiteSpace(AValue: boolean);
    procedure AddHit( const URL:string);

    function wpautop( Content:string; BR:boolean = true):string;
    function FilterOutput( Content, Filter:string):string;
    function BlockController( const ModuleName:string; const FunctionName:string; Parameter:TStrings):string;
    function GetDebugInfo( DebugType:string):string;
    function DoTrimWhiteSpace(const Content:string;ForceTrim:boolean=false):string;

    //- cache
    function getCacheFileName: string;
    function isCacheExpired: boolean;
    function LoadCache: string;
    procedure SaveCache(Content: string);

    //-- [if ] processor
    function ConditionalIfProcessor( TagProcessor: TReplaceTagEvent; Content:string):string;
    function conditionIsEqual( Value1, Value2: variant): boolean;


    //-- foreach
    function ForeachProcessor( TagProcessor: TReplaceTagEvent; Content:string):string;
    function ForeachProcessor_Table( TagProcessor: TReplaceTagEvent; KeyName, Content: string):string;
    procedure ForeachProcessor_Table_TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);
  public
    isCleanTag : boolean;
    constructor Create;
    destructor Destroy; override;
    property ThemeName: string read GetThemeName write SetThemeName;
    property Layout: string read FLayout write FLayout;
    property Extension: string read FThemeExtension write FThemeExtension;
    property StartDelimiter: string read FStartDelimiter write FStartDelimiter;
    property EndDelimiter: string read FEndDelimiter write FEndDelimiter;
    property BaseURL : string Read GetBaseURL;
    property IsJSON:boolean read FIsJSON write SetIsJSON;
    property RenderType : TRenderType read GetRenderType write SetRenderType;
    function GetVersionInfo():boolean;
    property CacheTime : integer read FCacheTime write SetCacheTime;// in hours

    procedure TagController(Sender: TObject; const TagString:String; TagParams: TStringList; Out ReplaceText: String);
    procedure TagCleaner(Sender: TObject; const TagString:String; TagParams: TStringList; Out ReplaceText: String);
    procedure TagDefault(Sender: TObject; const TagString:String; TagParams: TStringList; Out ReplaceText: String);

    property AssignVar[const TagName: String]: Pointer read GetAssignVar write SetAssignVar;
    property Hit[const URL: String]: integer read GetHitCount;
    property HitType : THitType read FHitType write FHitType;
    property VarValue[const VariableName: string]: variant read getVarValue;

    procedure Assign(const KeyName: string; Value:string);
    procedure Assign(const KeyName: string; const Address: pointer = nil);
    procedure Assign(const KeyName: string; Value:TSimpleModel);
    procedure Assign(const KeyName: string; Value:TSQLQuery);
    function Render(TagProcessorAddress: TReplaceTagEvent=nil; const LayoutTemplateFile: string = '';
      Cache: boolean = False; SubModule:boolean =false): string;
    function RenderFromContent(TagProcessorAddress: TReplaceTagEvent; const Content: string;
      TemplateFile: string = ''): string;

    property TrimWhiteSpace:boolean read FTrimWhiteSpace write SetTrimWhiteSpace;
    property TrimForce:boolean read FTrimForce write SetTrimForce;
    procedure AddJS( const FileName:string);
    procedure AddCSS( const FileName:string; const Media:string='all');
    procedure AddMeta( const Name:string; const Content:string; const MetaType:string = 'name');
  end;

var
  ThemeUtil: TThemeUtil;
  ___TagCallbackMap: TTagCallbackMap;

implementation

uses config_lib, logutil_lib, language_lib, versioninfo_lib, html_lib,
  initialize_controller;

var
  assignVarMap : TAssignVarMap;
  FAssignVarStringMap : TStringList;
  ForeachTable_Keyname,
  ForeachTable_Itemname : string;
  FTagAssign_Variable : TStringList;

{ THTMLHead }

constructor THTMLHead.Create;
begin
  JS := TStringList.Create;
  CSS := TStringList.Create;
  Meta := TStringList.Create;
end;

destructor THTMLHead.Destroy;
begin
  FreeAndNil(Meta);
  FreeAndNil(CSS);
  FreeAndNil(JS);
  inherited Destroy;
end;

procedure THTMLHead.AddJS(const FileName: string);
begin
  if FileName='' then Exit;
  JS.Add('<script type="text/javascript" src="'+FileName+'"></script>');
end;

procedure THTMLHead.AddCSS(const FileName: string; const Media: string);
begin
  if FileName='' then Exit;
  CSS.Add('<link rel="stylesheet" href="'+FileName+'" type="text/css" media="'+Media+'" />');
end;

procedure THTMLHead.AddMeta(const Name: string; const Content: string;
  const MetaType: string);
begin
  if Name='' then Exit;
  Meta.Add('<meta '+MetaType+'="'+Name+'" content="'+Content+'" />');
end;

{ TThemeUtil }

function TThemeUtil.GetThemeName: string;
begin
  if FThemeName = '' then
  begin
    FThemeName := string( Config.GetValue(_SYSTEM_THEME, 'default'));
  end;
  Result := FThemeName;
end;

function TThemeUtil.GetAssignVar(const TagName: String): Pointer;
begin
  try
    Result := assignVarMap[TagName];
  except
    Result := nil;
  end;
end;

function TThemeUtil.GetBaseURL: string;
begin
  if FBaseURL = '' then begin
    FBaseURL:= AppData.baseUrl;
  end;
  Result := FBaseURL;
end;

procedure TThemeUtil.SetAssignVar(const TagName: String; AValue: Pointer);
begin
  assignVarMap[TagName] := AValue;
end;

procedure TThemeUtil.SetCacheTime(AValue: integer);
begin
  if FCacheTime=AValue then Exit;
  FCacheTime:=AValue - 1;
end;

procedure TThemeUtil.SetIsJSON(AValue: boolean);
begin
  if FIsJSON=AValue then Exit;
  FIsJSON:=AValue;
end;

procedure TThemeUtil.SetRenderType(AValue: TRenderType);
begin
  FRenderType:= AValue;
end;

procedure TThemeUtil.Assign(const KeyName: string; Value: string);
begin
  FAssignVarStringMap.Values[KeyName] := Value;
end;

procedure TThemeUtil.Assign(const KeyName: string; const Address: pointer);
begin
  if not Assigned(Address) then
    Exit;
  try
    assignVarMap[KeyName] := Address;
    //x := TSQLQuery( assignVarMap[KeyName]^).SQL.Text);
  except
    on e: Exception do
    begin
      FastPlasAppandler.DieRaise(e.Message + ' when "assign" variable "' + KeyName + '"',[]);
    end;
  end;
end;

procedure TThemeUtil.Assign(const KeyName: string; Value: TSimpleModel);
begin
  Assign( KeyName, Value.Data);
end;

procedure TThemeUtil.Assign(const KeyName: string; Value: TSQLQuery);
var
  s : string;
  i : integer;
begin
  for i:=0 to Value.Fields.Count-1 do
  begin
    s := KeyName + '.' + Value.Fields[i].FieldName;
    try
      FAssignVarStringMap.Values[s] := Value.Fields[i].Value;
    except
      FAssignVarStringMap.Values[s] := '';
    end;
  end;
end;

procedure TThemeUtil.SetThemeName(AValue: string);
begin
  FThemeName := AValue;
end;

procedure TThemeUtil.SetTrimForce(AValue: boolean);
begin
  if FTrimForce=AValue then Exit;
  FTrimForce:=AValue;
end;

procedure TThemeUtil.SetTrimWhiteSpace(AValue: boolean);
begin
  if FTrimWhiteSpace=AValue then Exit;
  FTrimWhiteSpace:=AValue;
end;

function TThemeUtil.GetActiveModuleName(Arequest: TRequest): string;
begin
  Result := FastPlasAppandler.GetActiveModuleName( Arequest);
end;

function TThemeUtil.getVarValue(const VariableName: string): variant;
var
  fieldname,
  tmpvalue,
  varname : string;
  vartmp : TStrings;
  f : double;
begin
  vartmp := Explode( VariableName, '.');
  varname := trim(vartmp[0]);
  tmpvalue := '';
  if varname = '' then
  begin
    Result := 'variable '+varname+' not found.';
    exit;
  end;

  // is var1 numeric ?
  try
    f := StrToFloat( varname);
    Result := f;
    Exit;
  except
  end;

  // check from foreach-storage first
  try
    if assignVarMap[ ForeachTable_Keyname] <> nil then
    begin
      fieldname:= vartmp[1];
      tmpvalue := TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(fieldname).AsString;
      Result := tmpvalue;
    end;
  except
    on e: Exception do
    begin
      die( 'ThemeUtil.VarValue['+VariableName+']: ' + e.Message);
    end;
  end;

  FreeAndNil( vartmp);
end;

procedure TThemeUtil.AddHit(const URL: string);
var
  s : string;
  i : integer;
begin
  if FHitType = htNone then Exit;
  s := ReplaceAll( URL, [ 'http://', 'https://'], '');
  s := ReplaceAll( s, ['?', '&', '=', '/', '\', '.'], '-');
  s := UrlEncode( s);
  i := 1;
  case FHitType of
    htFile : begin
      if FileExists( IncludeTrailingPathDelimiter(AppData.tempDir)+__HITS_FILENAME) then
      begin
        FHits.LoadFromFile(IncludeTrailingPathDelimiter(AppData.tempDir)+__HITS_FILENAME);
        i := s2i( FHits.Values[s])+1;
      end;
      FHits.Values[s] := i2s(i);
      try
        FHits.SaveToFile(IncludeTrailingPathDelimiter(AppData.tempDir)+__HITS_FILENAME);
      except
      end;
    end;// htFile

  end;// case FHitType of
end;

{
Replaces double line-breaks with paragraph elements.

ref:
wordpress file "wp-includes/formation.php"
}
function TThemeUtil.wpautop(Content: string; BR: boolean): string;
var
  tmp : TStrings;
  html : widestring;
begin
  if trim( Content) = '' then
  begin
    Result := '';
    Exit;
  end;

  html := #13 + Content + #13;

  {
  html := ' awal <p>ada paragrapbh</p>asddad'#10' asd ad ad.'#13#10'<br />  <br />aasdf'
//    + #13'<blockquote>ini quote</blockquote>'#13#1
    + '<ul><li>satu</li><li>dua</li></ul>'#13
    + '<br />ganti baris.<br />asdadasd <P>paragraph</P>.'#13#10'adsa fsa.'#13#10'123 3453 3455.'#13#10'mulai table<table class="ab" style="xx"><td class=x><br />kolom</td></table>akhir';
  html := #13 + html + #13;
  ta( html);
  }

  tmp := Explode( html, '<pre');
  if ((tmp.Count = 1) and ( Pos( '<pre', tmp.Strings[0])=0) ) then
  begin
    html := preg_replace( '\<br />\s*<br />', #13#13, html);
    html := preg_replace( '(<'+__HTML_ALLBLOCK+'[^>]*>)', #13'$1', html);
    html := preg_replace( '(</'+__HTML_ALLBLOCK+'[^>]*>)', '$1'#13#13, html);

    //check : option, object, source

    html := ReplaceAll( html, [ #10#13, #10], #13, True); // cross-platform newlines
    html := preg_replace( #13'(.*?)'#13, '<p>$1</p>'#13, html);
    html := preg_replace( '<p>\s*</p>', '', html);
    html := preg_replace( '<p>\s*(</?'+__HTML_ALLBLOCK+'[^>]*>)', '$1', html);
    html := preg_replace( '(</?'+__HTML_ALLBLOCK+'[^>]*>)\s*</p>', '$1', html);

    // check: div, address, form, blockquote

    if BR then
    begin
      html := preg_replace( '<br />', '<br />'#13, html);
    end;

    html := preg_replace( '(</?'+__HTML_ALLBLOCK+'[^>]*>)\s*<br />', '$1', html);
    html := preg_replace( '(</?'+__HTML_ALLBLOCK+'[^>]*>)\s*</p>', '$1', html);
    html := preg_replace( '<p>\s*</p>', '', html);

    // check : br before p|li|div|dl|dd|dt|th|pre|td|ul|ol

  end else
  begin
    Result := Content;
    //todo: process if 'pre' exists
  end;

  FreeAndNil( tmp);
  Result := html;
end;

function TThemeUtil.GetHitCount(const URL: String): integer;
var
  s : string;
begin
  Result:=0;
  s := '';
  if FHitType = htNone then Exit;

  case FHitType of
    htFile : begin
      if FileExists( IncludeTrailingPathDelimiter(AppData.tempDir)+__HITS_FILENAME) then
      begin
        if s = '' then s := Application.Request.URL;
        s := ReplaceAll( s, ['?', '&', '=', '/'], ['-', '-', '-', '-']);
        FHits.LoadFromFile(IncludeTrailingPathDelimiter(AppData.tempDir)+__HITS_FILENAME);
        Result := s2i( FHits.Values[s]);
      end;
    end;
  end; // case FHitType of

end;

function TThemeUtil.GetRenderType: TRenderType;
begin
  Result := FRenderType;
end;

function TThemeUtil.FilterOutput(Content, Filter: string): string;
begin
  Result := Content;
  if Filter = '' then
    Exit;
  case Filter of
    'nl2br' : begin
      Result := StringReplace( Content, #13#10, '<br>', [rfReplaceAll]);
    end;
    'wpautop' : Result := wpautop( Content);
    'uppercase' : begin
      Result := UpperCase( Content);
    end;
    'lowercase' : begin
      Result := LowerCase( Content);
    end;
    'ucwords' : begin
      Result := ucwords( Content);
    end;
    'moreless' : begin
      //Result := MoreLess(Content);
    end;
    'dateformathuman' : begin
      Result := DateTimeHuman( Content);
    end;
  end;
end;

function TThemeUtil.BlockController(const ModuleName: string;
  const FunctionName: string; Parameter: TStrings): string;
var
  m  : TCustomHTTPModule;
  mi : TModuleItem;
  mc : TCustomHTTPModuleClass;
  f,
  StringResult : string;
  str : TStringList;
  i : integer;
begin
  mi := ModuleFactory.FindModule( ModuleName);
  if mi = nil then begin
    Result := EchoError( __Err_Theme_Modul_NotFond, [ModuleName]);
    Exit;
  end;
  //i := ModuleFactory.IndexOfModule( ModuleName);
  mc := mi.ModuleClass;
  m:=FastPlasAppandler.FindModule(mc);
  if m = nil then begin
    m:=mc.CreateNew(nil);
  end else begin
  end;

  // if cache, load from cache
  if (Parameter.Values['cache']='1') or (Parameter.Values['cache']='true') then begin
    f := ExtractFileDir( Application.ExeName) + DirectorySeparator + AppData.tempDir + DirectorySeparator + 'cache'+DirectorySeparator + ModuleName + '-' + FunctionName + '.html';
    if FileExists( f) then begin

      i := HoursBetween( FileDateToDateTime( FileAge( f)), now);
      if i = 0 then begin  // cache : 1 hour
        str := TStringList.Create;
        str.LoadFromFile( f);
        Result := str.Text;
        FreeAndNil( str);
        Exit;
      end;
    end;
  end;

  // OnBlockController
  if TMyCustomWebModule(m).OnBlockController <> nil then begin
    TMyCustomWebModule(m).OnBlockController( Self, FunctionName, Parameter, StringResult);
    Result := StringResult;
  end;

  //-- if cache, save to cache
  if (Parameter.Values['cache']='1') or (Parameter.Values['cache']='true') then begin
    if not DirectoryExists( ExtractFileDir( f)) then
      MkDir( ExtractFileDir( f));
    str := TStringList.Create;
    str.Text:= StringResult;
    str.SaveToFile( f);
    FreeAndNil( str);
  end;
end;

function TThemeUtil.GetDebugInfo(DebugType: string): string;
var
  i:integer;
  lst : TStrings;
begin
  case DebugType of
    'sql': begin
      Result := '<div class="debug"><table class="debug">';
      for i:=0 to _DebugInfo.Count-1 do begin
        lst := Explode( StringReplace( _DebugInfo[i], #9#9#9, '<br>', [rfReplaceAll]), '||');
        Result := Result + '<tr>';
        Result := Result + '<td>' + lst[2] + '</td>';
        Result := Result + '<td>' + lst[1] + '</td>';
        Result := Result + '</tr>';
        FreeAndNil(lst);
      end;
      Result := Result + '</table></div>';
    end;
    'time' : begin
      StopTime:= _GetTickCount;
      ElapsedTime:= StopTime - StartTime;
      Result := i2s( ElapsedTime) + 'ms';
    end;
    'memory' : begin
      Result := f2s((GetHeapStatus.TotalAddrSpace div 1024) / 1024) + 'MB';
    end;
  end;
end;

function TThemeUtil.GetVersionInfo: boolean;
begin
  if VersionInfo.FullVersion = '' then
  begin
    try
      with TVersionInfo.Create do
      begin
        Load( HINSTANCE);
        VersionInfo.Major        := FixedInfo.FileVersion[0];
        VersionInfo.Minor        := FixedInfo.FileVersion[1];
        VersionInfo.Revision     := FixedInfo.FileVersion[2];
        VersionInfo.BuldNumber   := FixedInfo.FileVersion[3];

        VersionInfo.Version  :=
          i2s( VersionInfo.Major) + '.' +
          i2s( VersionInfo.Minor) + '.' +
          i2s( VersionInfo.Revision);

        VersionInfo.FullVersion  :=
          i2s( VersionInfo.Major) + '.' +
          i2s( VersionInfo.Minor) + '.' +
          i2s( VersionInfo.Revision) + ' (build ' +
          i2s( VersionInfo.BuldNumber) + ')';
        Free;
      end;
      Result:=True;
    except
      VersionInfo.FullVersion := '-';
      Result:= False;
    end;
  end;
end;

function TThemeUtil.getCacheFileName: string;
var
  pathinfo: string;
begin
  pathinfo := copy(Application.Request.PathInfo, 1,
    length(Application.Request.PathInfo) - 1);
  if pathinfo = '' then
    pathinfo := Application.Request.QueryString;
  if pathinfo = '' then
    pathinfo := 'home';
  if pathinfo[1] = DirectorySeparator then
    pathinfo := copy(pathinfo, 2, length(pathinfo) - 1);
  Result := ExtractFileDir(Application.ExeName) + DirectorySeparator + AppData.tempDir +
    DirectorySeparator + 'cache' + DirectorySeparator + GetActiveModuleName(Application.Request) +
    DirectorySeparator + SafeText( pathinfo) + '-' + LANG + '.html';
end;

function TThemeUtil.isCacheExpired: boolean;
begin
  if HoursBetween(FileDateToDateTime(FileAge(getCacheFileName)), now) > FCacheTime then
    Result := True
  else
    Result := False;
end;

function TThemeUtil.LoadCache: string;
var
  f: string;
begin
  Result:='';
  if AppData.cacheType = 'file' then
  begin
    f := getCacheFileName;
    if FileExists(f) and not isCacheExpired then
    begin
      try
        with TStringList.Create do
        begin
          LoadFromFile(f);
          Result := Text;
          Free;
        end;
      except
      end;
    end;
  end; //-- if AppData.cache_type = 'file'
end;

procedure TThemeUtil.SaveCache(Content: string);
var
  f: string;
begin
  if ((AppData.cacheType = 'file') and ( AppData.cacheWrite)) then
  begin
    f := getCacheFileName;
    try
      if not DirectoryExists(ExtractFileDir(f)) then
        MkDir(ExtractFileDir(f));
      with TStringList.Create do
      begin
        Text := Content;
        SaveToFile(f);
        Free;
      end;
    except
      on E: Exception do
      begin
        if AppData.debug then
        begin
          _DebugInfo.Add(e.Message);
          LogUtil.add(e.Message);
        end;
      end;
    end;
  end;//- if AppData.cache_type = 'file' then
end;

function TThemeUtil.ConditionalIfProcessor(TagProcessor: TReplaceTagEvent; Content: string): string;
var
  parameter : TStrings;
  condition,
  s : string;
  value1, value2 : variant;
begin
  s := '';
  Result := Content;
  with TRegExpr.Create do
  begin
    //Expression := Format('%s(.*?)%s', [ __CONDITIONAL_IF_START, __CONDITIONAL_IF_END]);
    // \[if([\.\$A-Za-z_0-9=\ ]+)\]([a-z]+)(\[else\])?([a-z]+)\[/if\]
    Expression := Format('%s'+__CONDITIONAL_IF_VALUE+'%s', [ __CONDITIONAL_IF_START, __CONDITIONAL_IF_END]);
    if Exec( Content) then
    begin
      parameter := Explode( Match[1], ' ');
      if (parameter.Count < 4) then
      begin
        FreeAndNil( parameter);
        Exit;
      end;

      value1 := VarValue[ parameter[1]];
      value2 := VarValue[ parameter[3]];
      condition := parameter[2];

      if condition = '=' then condition := 'eq';
      case condition of
        'eq':
          begin
            if conditionIsEqual( value1, value2) then
              s := Match[2]
            else
              s := Match[4];
            Result := StringReplace( Content, Match[0], s, [rfReplaceAll]);
          end;

        // development: lt, gt

      end;
      //die( Content);

    end;

    Free;
  end;
end;

function TThemeUtil.conditionIsEqual(Value1, Value2: variant): boolean;
begin
  Result := False;
  if Value1 = Value2 then
    Result := True;
end;

function TThemeUtil.ForeachProcessor(TagProcessor: TReplaceTagEvent;
  Content: string): string;
var
  parameter : TStrings;
  html : string;
begin
  Result := Content;
  with TRegExpr.Create do
  begin
    Expression := Format('%s(.*?)%s', [ __FOREACH_START, __FOREACH_END]);
    if Exec( Content) then
    begin
      parameter := Explode( Match[1], ' ');
      ForeachTable_Keyname  := parameter.Values['from'];
      ForeachTable_Itemname := parameter.Values['item'];
      case parameter.Values['type'] of
        '' : begin
          FastPlasAppandler.DieRaise('field "type" is not define in "foreach ' + Match[1] + '"',[]);
        end;
        'table' : begin
          html := ForeachProcessor_Table(TagProcessor, parameter.Values['from'], Match[2]);
        end;
        'array' : begin
          FastPlasAppandler.DieRaise(__( __Err_Theme_ForeachNotImplemented),[]);
        end;
      end;

      //-- proccess conditional if
      //html := ConditionalIfProcessor( TagProcessor, html);

      ForeachTable_Keyname := '';
      ForeachTable_Itemname := '';

      html := StringReplace( Content, Match[0], html, [rfReplaceAll]);

      //** call parent tag-controller

      ForeachTable_Keyname := '';
      ForeachTable_Itemname:= '';
      FreeAndNil( parameter);
      Result := html;
    end;
    Free;
  end;
end;

function TThemeUtil.ForeachProcessor_Table(TagProcessor: TReplaceTagEvent;
  KeyName, Content: string): string;
var
  html, tmp : string;
  templateEngine : TFPTemplate;
begin
  if ( AssignVar[KeyName] = nil) then
  begin
    Exit;
  end;

  html := '';
  while not TSQLQuery( assignVarMap[KeyName]^).EOF do
  begin

    tmp := RenderFromContent(@TagController, Content);

    templateEngine := TFPTemplate.Create;
    templateEngine.Template := tmp;
    templateEngine.AllowTagParams := True;
    templateEngine.StartDelimiter := FStartDelimiter;
    templateEngine.EndDelimiter := FEndDelimiter;
    templateEngine.ParamValueSeparator := '=';
    templateEngine.OnReplaceTag := @ForeachProcessor_Table_TagController;
    html := html + templateEngine.GetContent;
    FreeAndNil(templateEngine);

    html := RenderFromContent(@TagController, html);

    //-- proccess conditional if
    html := ConditionalIfProcessor( TagProcessor, html);

    TSQLQuery( assignVarMap[KeyName]^).Next;
  end;
  Result := html;
end;

procedure TThemeUtil.ForeachProcessor_Table_TagController(Sender: TObject;
  const TagString: string; TagParams: TStringList; out ReplaceText: string);
var
  tagstring_custom : TStringList;
begin
  if ForeachTable_Keyname = '' then
    Exit;
  tagstring_custom := ExplodeTags( TagString);
  ReplaceText := FStartDelimiter +  TagString + FEndDelimiter;
  if tagstring_custom[0] <> ForeachTable_Itemname then
  begin
    FreeAndNil( tagstring_custom);
    Exit;
  end;
  if tagstring_custom.Values['assignto'] <> '' then
  begin
    FTagAssign_Variable.Values[tagstring_custom.Values['assignto']]:='s|'
      + TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsString;
  end;
  if tagstring_custom.Values['addassignto'] <> '' then
  begin
    if FTagAssign_Variable.Values[tagstring_custom.Values['addassignto']] = '' then
    begin
      FTagAssign_Variable.Values[tagstring_custom.Values['addassignto']]:='s|'
        + TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsString;
    end
    else
    begin
      FTagAssign_Variable.Values[tagstring_custom.Values['addassignto']]:=
        FTagAssign_Variable.Values[tagstring_custom.Values['addassignto']]
        + TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsString;
    end;
  end;
  if tagstring_custom.Values['dateformat'] <> '' then
  begin
    if tagstring_custom.Values['dateformat'] = 'human' then
    begin
      ReplaceText := DateTimeHuman( TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsDateTime);
    end
    else
      ReplaceText := FormatDateTime( tagstring_custom.Values['dateformat'],
        TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsDateTime
      );
  end
  else
  begin
    ReplaceText := TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsString;
  end;
  FreeAndNil( tagstring_custom);
end;

constructor TThemeUtil.Create;
begin
  FThemeExtension := '.html';
  FLayout := '';
  FStartDelimiter := '[';
  FEndDelimiter := ']';
  FParamValueSeparator := '=';
  FTrimWhiteSpace := True;
  FTrimForce := False;
  FIsJSON := False;
  isRendering := False;
  isRenderingModule:= False;
  isCleanTag:= False;
  assignVarMap := TAssignVarMap.Create;
  FAssignVarStringMap := TStringList.Create;
  FTagAssign_Variable := TStringList.Create;
  FHTMLHead := THTMLHead.Create;
  FHits := TStringList.Create;
  FHitType := htNone;
  CacheTime := AppData.cacheTime; // default: 3 hours
  HTMLUtil := THTMLUtil.Create;
  RenderType:= rtSmarty;
  ___TagCallbackMap := TTagCallbackMap.Create;
end;

destructor TThemeUtil.Destroy;
begin
  FreeAndNil(___TagCallbackMap);
  FreeAndNil(HTMLUtil);
  FreeAndNil(FHits);
  FreeAndNil(FHTMLHead);
  FreeAndNil(FTagAssign_Variable);
  FreeAndNil(FAssignVarStringMap);
  FreeAndNil(assignVarMap);
  inherited Destroy;
end;

procedure TThemeUtil.TagController(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
var
  s, tagname, filter : string;
  tagstring_custom : TStringList;
  tag_with_filter : TStrings;
begin
  if Pos( '|', TagString) = 0 then begin
    tagstring_custom := ExplodeTags( TagString);
    filter := tagstring_custom.Values['filter'];
  end
  else begin
    tagstring_custom := TStringList.Create;
    tag_with_filter := Explode( TagString, '|');
    tagstring_custom.Text := tag_with_filter.Text;
    FreeAndNil( tag_with_filter);
    filter := tagstring_custom[1];
  end;
  //ReplaceText := tagstring_custom[0];
  ReplaceText := '';

  // check from AssignVar
  if not FastPlasAppandler.isDisplayError then
  begin
  if tagstring_custom.Values['index']<>'' then
  begin
    if ThemeUtil.AssignVar[tagstring_custom[0]] <> Nil then
    begin
      try
        if tagstring_custom.Values['dateformat'] <> '' then
        begin
          if tagstring_custom.Values['dateformat'] = 'human' then
          begin
            ReplaceText := DateTimeHuman( TSQLQuery(ThemeUtil.AssignVar[tagstring_custom[0]]^).FieldByName(tagstring_custom.Values['index']).AsDateTime);
          end
          else
            ReplaceText := FormatDateTime( tagstring_custom.Values['dateformat'],
              TSQLQuery(ThemeUtil.AssignVar[tagstring_custom[0]]^).FieldByName(tagstring_custom.Values['index']).AsDateTime
            );
        end
        else
          ReplaceText := TSQLQuery(ThemeUtil.AssignVar[tagstring_custom[0]]^).FieldByName(tagstring_custom.Values['index']).AsString;
      except
        ReplaceText :='----';
      end;
      ReplaceText := FilterOutput( ReplaceText, tagstring_custom.Values['filter']);
      FreeAndNil(tagstring_custom);
      Exit;
    end
    else
    begin
      ReplaceText := '';
    end;
  end;
  end;
  // check from AssignVar - end

  if tagstring_custom.Count = 0 then Begin ReplaceText := '[]'; Exit; End;
  tagname := tagstring_custom[0];
  case tagname of
    '$title' : begin
      ReplaceText := AppData.sitename;
      end;
    'title' : begin
      ReplaceText := AppData.sitename;
      end;
    '$slogan' : begin
      ReplaceText := AppData.slogan;
      end;
    'slogan' : begin
      ReplaceText := AppData.slogan;
      end;
    '$baseurl' : begin
      ReplaceText := BaseURL;
      end;
    'baseurl' : begin
      ReplaceText := BaseURL;
      end;
    '$thisurl': ReplaceText := FastPlasAppandler.URI;
    'thisurl' : ReplaceText := FastPlasAppandler.URI;
    '$theme'  : begin
      ReplaceText := ThemeName;
      end;
    'theme' : begin
      ReplaceText := ThemeName;
      end;
    '$themepath' : begin
      ReplaceText := 'themes/' + ThemeUtil.ThemeName;
      end;
    'themepath' : begin
      ReplaceText := 'themes/' + ThemeUtil.ThemeName;
      end;
    '$themefullpath' : begin
      ReplaceText := BaseURL;
      ReplaceText := ReplaceText + 'themes/' + ThemeUtil.ThemeName;
      end;
    'themefullpath' : begin
      ReplaceText := BaseURL;
      ReplaceText := ReplaceText + 'themes/' + ThemeUtil.ThemeName;
      end;
    '$version' : begin
      GetVersionInfo();
      ReplaceText := VersionInfo.FullVersion;
      end;
    'version' : begin
      GetVersionInfo();
      ReplaceText := VersionInfo.FullVersion;
      end;
    '$env' : begin
      if tagstring_custom.Values['key'] <> '' then
        ReplaceText :=Application.EnvironmentVariable[tagstring_custom.Values['key']];
      end;
    'env' : begin
      if tagstring_custom.Values['key'] <> '' then
        ReplaceText :=Application.EnvironmentVariable[tagstring_custom.Values['key']];
      end;
    'datetime' : begin
      if tagstring_custom.Values['format'] <> '' then
        ReplaceText := FormatDateTime(tagstring_custom.Values['format'], Now)
      else
        ReplaceText := FormatDateTime('dd MMM YYYY HH:nn:ss', Now);
      end;
    'date' : begin
      ReplaceText := FormatDateTime('dd MMM YYYY', Now);
      end;
    'time' : begin
      ReplaceText := FormatDateTime('HH:nn:ss', Now);
      end;
    'hit' : begin
      ReplaceText := i2s( GetHitCount(''));
      end;
    '$lang' : ReplaceText := LANG;
    'lang' : ReplaceText := LANG;

    'assign' : begin
      //s| <<-- prepare for variable type
      FTagAssign_Variable.Values[ tagstring_custom.Values['var']] := 's|'+tagstring_custom.Values['value'];
      ReplaceText :='';
    end;
    'assignadd' : begin
      s := FTagAssign_Variable.Values[ tagstring_custom.Values['var']];
      if s = ''
      then
        FTagAssign_Variable.Values[ tagstring_custom.Values['var']] := 's|'+tagstring_custom.Values['value']
      else
        FTagAssign_Variable.Values[ tagstring_custom.Values['var']] := s+tagstring_custom.Values['value'];
      ReplaceText :='';
    end;
    'value' : begin
      ReplaceText :=FTagAssign_Variable.Values[ tagstring_custom.Values['var']];
      ReplaceText :=Copy(ReplaceText,3,Length(ReplaceText)-2);
    end;
    'include' : begin
      ReplaceText := ThemeUtil.Render( @TagController, tagstring_custom.Values['file'], false, true);;
      end;
    'block' : begin
      ReplaceText := BlockController( tagstring_custom.Values['mod'], tagstring_custom.Values['func'], tagstring_custom);
      end;
    'text' : begin
      ReplaceText := BlockController( tagstring_custom.Values['mod'], tagstring_custom.Values['func'], tagstring_custom);
      end;
    'debug' : begin
      ReplaceText := getDebugInfo( tagstring_custom.Values['type']);
      end;
    'gt' : begin
      ReplaceText := __(tagstring_custom.Values['text']);
      end;
    'recaptcha' : begin
      // usage: [recaptcha key="yourkey" version="v1"]
      if tagstring_custom.Values['key'] = '' then
        ReplaceText := Format( __ERR_RECAPTCHA_INVALID_KEY, [HTMLUtil.Link( ReCaptcha_SignupUrl, ['target=_blank'])])
      else
      begin
        ReplaceText := HTMLUtil.ReCaptcha( tagstring_custom.Values['key'], tagstring_custom.Values['version']);
      end;
    end;
  end;

  {$if fpc_fullversion >= 20701}
  if ___TagCallbackMap.Contains(tagname) then begin
  {$else fpc_fullversion >= 20701}
  if ___TagCallbackMap.IndexOf(tagname) >= 0 then begin
  {$endif fpc_fullversion >= 20701}
    ReplaceText := ___TagCallbackMap[tagname](TagString,tagstring_custom);
  end;

  if FAssignVarStringMap.IndexOfName( tagname) <> -1 then
  begin
    ReplaceText :=FAssignVarStringMap.Values[ tagname];
  end else
  begin
    if FAssignVarStringMap.IndexOfName( TagString) <> -1 then
    begin
      ReplaceText :=FAssignVarStringMap.Values[ TagString];
    end;
  end;

  if ReplaceText = '' then
  begin
    // gunakan ini, jika nama variable ditampilkan saat variable tidak ditemukan
    ReplaceText := ThemeUtil.StartDelimiter +  TagString + ThemeUtil.EndDelimiter;
  end;

  if filter <> '' then
  begin
    ReplaceText := FilterOutput( ReplaceText, filter);
  end else begin

  end;

  FreeAndNil( tagstring_custom);
end;

procedure TThemeUtil.TagCleaner(Sender: TObject; const TagString: String; TagParams: TStringList; out
  ReplaceText: String);
begin
  if Length(TagString) < 3 then Exit;
  if TagString[1] = '$' then Exit;
  ReplaceText := StartDelimiter + TagString + EndDelimiter;
end;

procedure TThemeUtil.TagDefault(Sender: TObject; const TagString: String; TagParams: TStringList; out
  ReplaceText: String);
begin
  ReplaceText := StartDelimiter + TagString + EndDelimiter;
end;

function TThemeUtil.Render(TagProcessorAddress: TReplaceTagEvent; const LayoutTemplateFile: string; Cache: boolean;
  SubModule: boolean): string;
var
  templateFilename, _ext, moduleActive: string;
  templateEngine: TFPTemplate;
  response_json : TJSONObject;
begin
  if (not AppData.themeEnable) and (not Assigned(ThemeUtil)) then
  begin
    FastPlasAppandler.DieRaise(__( __ERR_THEME_NOT_ENABLED),[]);
  end;

  if Cache then
  begin
    Result := LoadCache;
    if Result <> '' then
    begin
      Result := Result+'<!-- '+getDebugInfo('time')+'-->';
      Exit;
    end;
  end;

  if not DirectoryExists('themes') then
  begin
    Result := Result + Format( __(__Err_App_Init), [
      'http://' + GetEnvironmentVariable('SERVER_NAME') + (GetEnvironmentVariable('SCRIPT_NAME'))
      + '/initialize/']);
    Exit;
  end;

  templateFilename := LayoutTemplateFile;
  if not SubModule then
    if FLayout <> '' then templateFilename := trim(FLayout);
  if templateFilename <> '' then
  begin
    templateFilename := StringReplace( templateFilename, '"', '', [rfReplaceAll]);
    templateFilename := StringReplace( templateFilename, '''', '', [rfReplaceAll]);
    _ext := FThemeExtension;
    if ExtractFileExt( templateFilename) <> '' then
      _ext := '';
    templateFilename := GetCurrentDir + DirectorySeparator + 'themes'
      + DirectorySeparator + ThemeName
      + DirectorySeparator + 'templates'
      + DirectorySeparator + templateFilename+ _ext;

    if not FileExists(templateFilename) then
    begin
      Result := EchoError( __(__Err_Theme_Not_Exists), [ templateFilename, ThemeName]);
      Exit;
    end;
  end
  else
  begin
    moduleActive := GetActiveModuleName(Application.Request);
    templateFilename := Application.Request.QueryFields.Values['act'];
    if templateFilename = '' then
      templateFilename := 'master';
    templateFilename := GetCurrentDir + DirectorySeparator + 'themes'
      + DirectorySeparator + ThemeName
      + DirectorySeparator + 'templates'
      + DirectorySeparator + 'modules'
      + DirectorySeparator + moduleActive
      + DirectorySeparator + templateFilename + FThemeExtension;
    if not FileExists(templateFilename) then
      templateFilename := GetCurrentDir + DirectorySeparator + 'themes'
        + DirectorySeparator + ThemeName
        + DirectorySeparator + 'templates'
        + DirectorySeparator + 'modules'
        + DirectorySeparator + moduleActive
        + DirectorySeparator + 'master' + FThemeExtension;
    if not FileExists(templateFilename) then
      templateFilename := GetCurrentDir + DirectorySeparator + 'themes'
          + DirectorySeparator + ThemeName
          + DirectorySeparator + 'templates'
          + DirectorySeparator + 'master' + FThemeExtension;
  end;

  try
    templateEngine := TFPTemplate.Create;
    templateEngine.FileName := templateFilename;
    templateEngine.AllowTagParams := True;
    templateEngine.StartDelimiter := FStartDelimiter;
    templateEngine.EndDelimiter := FEndDelimiter;
    templateEngine.ParamValueSeparator := FParamValueSeparator;
    if TagProcessorAddress = nil then
      templateEngine.OnReplaceTag := @TagController
    else
      templateEngine.OnReplaceTag := TagProcessorAddress;

    Result := templateEngine.GetContent;

    // clean tag
    if isCleanTag then
      Result := RenderFromContent( @TagCleaner, Result);

  except
    on e : Exception do
    begin
      FastPlasAppandler.DieRaise(e.Message,[]);
    end;
  end;

  if not SubModule then
  begin
    if FHTMLHead.JS.Count>0 then
      Result:=StringReplace(Result,'</head>',FHTMLHead.JS.Text+'</head>',[rfReplaceAll]);
    if FHTMLHead.CSS.Count>0 then
      Result:=StringReplace(Result,'</head>',FHTMLHead.CSS.Text+'</head>',[rfReplaceAll]);
    if FHTMLHead.Meta.Count>0 then
      Result:=StringReplace(Result,'</head>',FHTMLHead.Meta.Text+'</head>',[rfReplaceAll]);
    AddHit( Application.Request.URL);
  end;
  Result := AdjustLineBreaks(Result);
  if FTrimWhiteSpace then
    Result := DoTrimWhiteSpace(Result,FTrimForce);
  if Cache then
    SaveCache(Result);

  Result := Result + '<!-- '+getDebugInfo('time')+' -->';

  if FIsJSON then
  begin
    response_json := TJSONObject.Create;
    response_json.Add( 'code', 0);
    response_json.Add( 'data', Result);
    Result := response_json.AsJSON;
    FreeAndNil( response_json);
  end;
  FreeAndNil(templateEngine);
end;

function TThemeUtil.RenderFromContent(TagProcessorAddress: TReplaceTagEvent; const Content: string; TemplateFile: string
  ): string;
var
  templateEngine: TFPTemplate;
  html: TStringList;
begin
  isRenderingModule:= True;
  html := TStringList.Create;
  //TemplateFile := 'themes/' + ThemeName + '/templates/' + TemplateFile;
  if FileExists(TemplateFile) then
  begin
    html.LoadFromFile(TemplateFile);
    html.Text := StringReplace(html.Text, FStartDelimiter + 'content' +
      FEndDelimiter, Content, [rfReplaceAll]);
  end
  else
  begin
    if trim(Content) = '' then
      html.Text:= 'File "'+TemplateFile+'" ' + __( 'not found.')
    else
      html.Text := Content;
  end;

  //-- proccess conditional if
  //html.Text:= ConditionalIfProcessor( TagProcessorAddress, html.Text);

  //-- proccess foreach
  if RenderType = rtSmarty then
  begin
    html.Text:= ForeachProcessor( TagProcessorAddress, html.Text);
  end;

  templateEngine := TFPTemplate.Create;
  templateEngine.Template := html.Text;
  templateEngine.AllowTagParams := True;
  templateEngine.StartDelimiter := FStartDelimiter;
  templateEngine.EndDelimiter := FEndDelimiter;
  templateEngine.ParamValueSeparator := '=';
  if TagProcessorAddress = nil then
    templateEngine.OnReplaceTag := @TagController
  else
    templateEngine.OnReplaceTag := TagProcessorAddress;
  Result := templateEngine.GetContent;
  FreeAndNil(templateEngine);
  FreeAndNil(html);
  isRenderingModule:= False;
end;

procedure TThemeUtil.AddJS(const FileName: string);
begin
  FHTMLHead.AddJS(FileName);
end;

procedure TThemeUtil.AddCSS(const FileName: string; const Media: string);
begin
  FHTMLHead.AddCSS(FileName,Media);
end;

procedure TThemeUtil.AddMeta(const Name: string; const Content: string;
  const MetaType: string);
begin
  FHTMLHead.AddMeta(Name,Content,MetaType);
end;

function TThemeUtil.DoTrimWhiteSpace(const Content: string; ForceTrim: boolean
  ): string;
var
  html : TStringList;
  i:integer;
  skip:boolean;
  templateEngine : TFPTemplate;
begin
  html := TStringList.Create;
  html.Text:=Content;
  skip:=false;
  for i:=html.Count-1 downto 0 do
  begin
    if Pos('</script',html[i])>0 then skip:=true;
    if Pos('<script',html[i])>0 then skip:=false;
    if (not skip) or ForceTrim then
    begin
      html[i]:=trim(html[i]);
    end;
    if html[i]='' then
      html.Delete(i);
  end;

  // remove comment
  templateEngine := TFPTemplate.Create;
  templateEngine.Template := html.Text;
  templateEngine.AllowTagParams := True;
  templateEngine.StartDelimiter := '<!--';
  templateEngine.EndDelimiter := '-->';
  templateEngine.ParamValueSeparator := '=';
  templateEngine.OnReplaceTag := nil;
  Result := templateEngine.GetContent;
  FreeAndNil(templateEngine);

  FreeAndNil(html);
end;

initialization
  //___TagCallbackMap := TTagCallbackMap.Create;

finalization
  //FreeAndNil(___TagCallbackMap);

end.

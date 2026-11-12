unit theme_controller;

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  {$if fpc_fullversion >= 20701}
  //ghashmap,
  fgl,
  {$else fpc_fullversion >= 20701}
  fgl,
  {$endif fpc_fullversion >= 20701}
  {$ifdef GREYHOUND}
  ghSQL, ghSQLdbLib,
  {$endif}
  fpWeb,
  fastplaz_app, fpTemplate, fphttp, fpjson, HTTPDefs, dateutils,
  regexpr_lib, db, sqldb, StrUtils,
  common, fastplaz_handler, database_lib, datetime_lib, modvar_util, json_lib,
  Classes, SysUtils, Variants;

const
  // if use { ... }
  //__FOREACH_START = '{foreach([\.\$A-Za-z= ]+)}';
  //__FOREACH_END = '\{/foreach[\.\$A-Za-z0-9= ]+}';

  __FOREACH_START = '\[foreach([\.\$A-Za-z0-9=_ ]+)\]';
  __FOREACH_END = '\[/foreach[\.\$A-Za-z0-9=_ ]+\]';

  __CONDITIONAL_IF_START = '\[if([\.\$A-Za-z_0-9=\ "''<>]+)\]';
  //__CONDITIONAL_IF_VALUE = '([\.\$#@&A-Za-z0-9!;:=<"''\''>_\|\(\)\\\/\-\n\r \[\]]+?)(\[else\]+)?([\.\$A-Za-z0-9=_ ]+)';
  __CONDITIONAL_IF_END = '\[\/if\]';

  __HITS_FILENAME = 'hits.log';

  __ERR_THEME_NOT_ENABLED = 'Using Theme''s features but it is not enabled. Check your config.json file.';

  __ERR_RECAPTCHA_INVALID_KEY = 'Invalid recaptcha publickey.<br />Create Key from %s';
  ReCaptcha_SignupUrl = 'https://www.google.com/recaptcha/admin';

type

  // based on qtemplate
  {$if fpc_fullversion >= 20701}
    { TStringHash }
    {*
    TStringHash = class
      class function hash(s: String; n: Integer): Integer;
    end;
    generic TStringHashMap<T> = class(specialize THashMap<String,T,TStringHash>) end;
    *}
    generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
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
    foreachJsonIndex : integer;
    FIncludeDepth : integer;
    // per instance; these were unit globals shared (and freed) by every
    // TThemeUtil, so a second instance corrupted the first
    assignVarMap : TAssignVarMap;
    FAssignVarStringMap : TStringList;
    ForeachTable_Keyname,
    ForeachTable_Itemname : string;
    FTagAssign_Variable : TStringList;
    FOwnsHTMLUtil, FOwnsTagCallbackMap : boolean;
    function GetAssignVar(const TagName: String): Pointer;
    function GetBaseURL: string;
    function GetFlashMessage: string;
    function GetHitCount(const URL: String): integer;
    function GetRenderType: TRenderType;
    function GetThemeName: string;
    function GetActiveModuleName(Arequest: TRequest): string;
    function getVarValue(const VariableName: string): variant;
    procedure SetAssignVar(const TagName: String; AValue: Pointer);
    procedure SetCacheTime(AValue: integer);
    procedure SetFlashMessage(AValue: string);
    procedure SetIsJSON(AValue: boolean);
    procedure SetRenderType(AValue: TRenderType);
    procedure SetThemeName(AValue: string);
    procedure SetTrimForce(AValue: boolean);
    procedure SetTrimWhiteSpace(AValue: boolean);
    function HitKey( const URL:string): string;
    procedure AddHit( const URL:string);

    function wpautop( Content:string; BR:boolean = true):string;
    function MultiFilter( AContent:string):string;
    function FilterOutput( Content, Filter:string; ATagString: TStringList = nil):string;
    function BlockController( const ModuleName:string; const FunctionName:string; Parameter:TStrings):string;

    function GetDebugBenchmark( const DebugType:string = ''):string;
    function GetDebugGetData( const DebugType:string = ''):string;
    function GetDebugPostData( const DebugType:string = ''):string;
    function GetDebugURIString( const DebugType:string = ''):string;
    function GetDebugClassMethod( const DebugType:string = ''):string;
    function GetDebugHeadersData( const DebugType:string = ''):string;
    function GetDebugSessionData( const DebugType:string = ''):string;

    //- cache
    function getCacheFileName: string;
    function isCacheExpired: boolean;
    function LoadCache: string;
    procedure SaveCache(Content: string);

    //-- [if ] processor
    function ConditionalIfProcessor( TagProcessor: TReplaceTagEvent; Content:string):string;
    function SplitCondition( const AHeader: string): TStringList;
    function IsKnownVariable( const AName: string): boolean;
    function EvaluateCondition( const AHeader: string; out AMatched, AKnown: boolean): boolean;
    function conditionIsEqual( Value1, Value2: variant): boolean;
    function conditionIsNotEqual( Value1, Value2: variant): boolean;
    function conditionIsLessThan( Value1, Value2: variant): boolean;
    function conditionIsGreaterThan( Value1, Value2: variant): boolean;


    //-- foreach
    function ForeachProcessor( TagProcessor: TReplaceTagEvent; Content:string):string;
    function ForeachProcessor_Table( TagProcessor: TReplaceTagEvent; KeyName, Content: string):string;
    procedure ForeachProcessor_Table_TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);
    function ForeachProcessor_Dataset( TagProcessor: TReplaceTagEvent; KeyName, Content: string):string;
    procedure ForeachProcessor_Dataset_TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);
    function ForeachProcessor_Json( TagProcessor: TReplaceTagEvent; KeyName, Content: string):string;
    procedure ForeachProcessor_Json_TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);
    {$ifdef GREYHOUND}
    function ForeachProcessor_GreyhoundTable( TagProcessor: TReplaceTagEvent; KeyName, Content: string):string;
    procedure ForeachProcessor_GreyhoundTable_TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);
    {$endif GREYHOUND}
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
    function GetDebugInfo( DebugType:string):string;
    // template name taken from the request: no path separators, no "..", no #0
    class function SafeTemplateName( const AName: string): string;
    property CacheTime : integer read FCacheTime write SetCacheTime;// in hours
    property FlashMessages: string read GetFlashMessage write SetFlashMessage;

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
    function DoTrimWhiteSpace(const Content:string;ForceTrim:boolean=false):string;
    procedure AddJS( const FileName:string);
    procedure AddCSS( const FileName:string; const Media:string='all');
    procedure AddMeta( const Name:string; const Content:string; const MetaType:string = 'name');
  end;

var
  ThemeUtil: TThemeUtil;
  ___TagCallbackMap: TTagCallbackMap;

implementation

uses config_lib, logutil_lib, language_lib, versioninfo_lib, html_lib, session_controller, md5;

// Template file content, read in shared mode.
// TFPTemplate.FileName opens the file with plain fmOpenRead, which on Windows
// is an exclusive open: two requests rendering the same layout at the same
// moment fail with "Unable to open file ... being used by another process".
function ReadTemplateFile(const FileName: string): string;
var
  fs: TFileStream;
begin
  Result := '';
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, fs.Size);
    if fs.Size > 0 then
      fs.ReadBuffer(Result[1], fs.Size);
  finally
    fs.Free;
  end;
end;

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
var
  i: integer;
begin
  i := assignVarMap.IndexOf(TagName);
  if i = -1 then
    Result := nil
  else
    Result := assignVarMap.Data[i];
end;

function TThemeUtil.GetBaseURL: string;
begin
  if FBaseURL = '' then begin
    FBaseURL:= AppData.baseUrl;
  end;
  Result := FBaseURL;
end;

function TThemeUtil.GetFlashMessage: string;
begin
  Result := _SESSION[_SESSION_FLASHMESSAGE];
end;

procedure TThemeUtil.SetAssignVar(const TagName: String; AValue: Pointer);
begin
  assignVarMap[TagName] := AValue;
end;

procedure TThemeUtil.SetCacheTime(AValue: integer);
begin
  FCacheTime:=AValue;
end;

procedure TThemeUtil.SetFlashMessage(AValue: string);
begin
  if AValue = '' then
    _SESSION[_SESSION_FLASHMESSAGE] := ''
  else
    _SESSION[_SESSION_FLASHMESSAGE] := FlashMessages + trim( AValue) + '|';
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
  vartmp, assignTmp : TStrings;
  f : double;
  idx : integer;
  obj : TObject;
  jsonItem : TJSONData;
begin
  Result := '';
  vartmp := Explode( VariableName, '.');
  try
    varname := trim(vartmp[0]);
    tmpvalue := '';
    if varname = '' then
    begin
      Result := 'variable '+varname+' not found.';
      Exit;
    end;

    // is var1 numeric ?
    if TryStrToFloat( varname, f) then
    begin
      Result := f;
      Exit;
    end;

    // get direct string value
    if preg_match('(["''])(.*?)(["''])', varname) then
    begin
      tmpvalue := varname.Replace('"','').Replace('''','');
      Result := tmpvalue;
      Exit;
    end;

    // check from varstring first
    if FAssignVarStringMap.IndexOfName( varname) <> -1 then
    begin
      Result := FAssignVarStringMap.Values[ varname];
      Exit;
    end;

    // check from 'assignto' tag :
    // ex: [aItem.level assignto="level"]
    if FTagAssign_Variable.IndexOfName( varname) <> -1 then
    begin
      // stored as "<type>|<value>", ex: "s|myvalue"
      assignTmp := Explode( FTagAssign_Variable.Values[ varname], '|');
      try
        if assignTmp.Count > 1 then
          Result := assignTmp[1]
        else
          Result := FTagAssign_Variable.Values[ varname];
      finally
        FreeAndNil( assignTmp);
      end;
      Exit;
    end;

    // check from foreach-storage
    if (ForeachTable_Keyname = '') or (vartmp.Count < 2) then
      Exit;
    idx := assignVarMap.IndexOf( ForeachTable_Keyname);
    if idx = -1 then
      Exit;
    if assignVarMap.Data[idx] = nil then
      Exit;
    try
      fieldname:= vartmp[1];
      obj := TObject( assignVarMap.Data[idx]^);
      if obj is TJSONData then
      begin
        // "item.a.b" -> path "a.b" of the current row
        fieldname := Copy( VariableName, Pos('.', VariableName) + 1, Length( VariableName));
        jsonItem := TJSONData(obj).Items[foreachJsonIndex].FindPath( fieldname);
        if jsonItem <> nil then
          Result := jsonItem.AsString;
      end
      else if obj is TDataSet then
        Result := TDataSet(obj).FieldByName(fieldname).AsString;
    except
      on e: Exception do
      begin
        if AppData.debug then
          LogUtil.add( 'ThemeUtil.VarValue['+VariableName+']: ' + e.Message);
        Result := '';
      end;
    end;
  finally
    FreeAndNil( vartmp);
  end;
end;

// normalize URL into a key for hits.log; must be used by both AddHit and GetHitCount
function TThemeUtil.HitKey(const URL: string): string;
begin
  Result := ReplaceAll( URL, [ 'http://', 'https://'], '');
  Result := ReplaceAll( Result, ['?', '&', '=', '/', '\', '.'], '-');
  Result := UrlEncode( Result);
end;

procedure TThemeUtil.AddHit(const URL: string);
var
  s : string;
  i : integer;
begin
  if FHitType = htNone then Exit;
  s := HitKey( URL);
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
  html : String;
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

    Result := html;
  end else
  begin
    Result := Content;
    //todo: process if 'pre' exists
  end;

  FreeAndNil( tmp);
end;

function TThemeUtil.MultiFilter(AContent: string): string;
begin
  Result := FormatTextLikeForum(AContent);
  Result := MarkdownToHTML(Result);
end;

function TThemeUtil.GetHitCount(const URL: String): integer;
var
  s : string;
begin
  Result:=0;
  if FHitType = htNone then Exit;
  s := URL;
  if s = '' then s := Application.Request.URL;
  s := HitKey( s);

  case FHitType of
    htFile : begin
      if FileExists( IncludeTrailingPathDelimiter(AppData.tempDir)+__HITS_FILENAME) then
      begin
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

function TThemeUtil.FilterOutput(Content, Filter: string;
  ATagString: TStringList): string;
var
  i: Integer;
  s: string;
  fs: TFormatSettings;
begin
  Result := Content;
  if Filter = '' then
    Exit;
  case Filter of
    'escape', 'html', 'htmlspecialchars' : begin
      Result := HTMLEncode( Content);
    end;
    'nl2br' : begin
      Result := StringReplace( Content, #13#10, #10, [rfReplaceAll]);
      Result := StringReplace( Result, #13, #10, [rfReplaceAll]);
      Result := StringReplace( Result, #10, '<br>', [rfReplaceAll]);
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
    'dateformathuman' :
    begin
      Result := DateTimeHuman( Content);
    end;
    'date' :
    begin
      try
        i := StrToInt(Content);
        Result := FormatDateTime('dd MMM yyyy', UnixToDateTime(i, False));
      except
      end;
    end;
    'unix2date' :
    begin
      try
        i := StrToInt(Content);
        s := 'yyyy-mm-dd HH:nn:ss';
        if ATagString <> nil then
        begin
          if ATagString.Values['dateformat'] <> '' then
            s := ATagString.Values['dateformat'];
          if ATagString.Values['format'] <> '' then
            s := ATagString.Values['format'];
        end;
        Result := FormatDateTime(s, UnixToDateTime(i, False));
      except
      end;
    end;
    'number' :
    begin
      if ((Content.IsEmpty) or (Content = '0')) then
        Result := '0'
      else
      begin
        fs := DefaultFormatSettings;
        fs.DecimalSeparator := ',';
        fs.ThousandSeparator := '.';
        s := '#,###.##';
        if (ATagString <> nil) and (ATagString.Values['format'] <> '') then
          s := ATagString.Values['format'];
        Result := FormatFloat(s, s2f(Content), fs);
      end;
    end;
    'thousand' :
    begin
      if ((Content.IsEmpty) or (Content = '0')) then
      begin
        Result := '0';
        Exit;
      end;
      fs := DefaultFormatSettings;
      fs.DecimalSeparator := ',';
      fs.ThousandSeparator := '.';
      s := '#,###.##';
      Result := trunc(s2f(Content)).ToString();
      //Result := FormatFloat(s, s2f(Result), fs);
      Result := Result + '<span class="thousand">.' +FormatFloat('00#', (s2f(Content)-trunc(s2f(Content)))*1000 , fs)+ '</span>';
    end;
    'shorturl' :
    begin
      Result := HTMLUtil.Permalink( Content);
    end;
    'cleanurl' :
    begin
      Result := HTMLUtil.Permalink( Content);
    end;
    'permalink' :
    begin
      Result := HTMLUtil.Permalink( Content);
    end;
    'multifilter' :
    begin
      Result := MultiFilter( Content);
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
  useCache, ownModule : boolean;
begin
  Result := '';
  StringResult := '';
  mi := ModuleFactory.FindModule( ModuleName);
  if mi = nil then begin
    Result := EchoError( __Err_Theme_Modul_NotFond, [ModuleName]);
    Exit;
  end;

  useCache := (Parameter.Values['cache']='1') or (Parameter.Values['cache']='true');
  f := '';

  // if cache, load from cache
  if useCache then begin
    f := ExtractFileDir( Application.ExeName) + DirectorySeparator + AppData.tempDir + DirectorySeparator + 'cache'+DirectorySeparator + ModuleName + '-' + FunctionName + '.html';
    if FileExists( f) then begin

      i := HoursBetween( FileDateToDateTime( FileAge( f)), now);
      if i = 0 then begin  // cache : 1 hour
        str := TStringList.Create;
        try
          str.LoadFromFile( f);
          Result := str.Text;
        finally
          FreeAndNil( str);
        end;
        Exit;
      end;
    end;
  end;

  //i := ModuleFactory.IndexOfModule( ModuleName);
  mc := mi.ModuleClass;
  m:=FastPlasAppandler.FindModule(mc);
  ownModule := (m = nil);
  if ownModule then begin
    m:=mc.CreateNew(nil);
  end;

  try
    // OnBlockController
    if (m is TMyCustomWebModule) and (TMyCustomWebModule(m).OnBlockController <> nil) then begin
      TMyCustomWebModule(m).OnBlockController( Self, FunctionName, Parameter, StringResult);
      Result := StringResult;
    end;
  finally
    if ownModule then
      FreeAndNil( m);
  end;

  //-- if cache, save to cache
  if useCache then begin
    try
      ForceDirectories( ExtractFileDir( f));
      str := TStringList.Create;
      try
        str.Text:= StringResult;
        str.SaveToFile( f);
      finally
        FreeAndNil( str);
      end;
    except
      on E: Exception do
      begin
        if AppData.debug then
          LogUtil.add( 'BlockController cache: ' + E.Message);
      end;
    end;
  end;
end;

function TThemeUtil.GetDebugInfo(DebugType: string): string;
var
  i:integer;
  lst : TStrings;
begin
  if DebugType = '' then DebugType := 'all';
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
    'all' : begin
      Result := '<div class="fastplaz_profiler" style="padding:10px;">';
      Result := Result + GetDebugBenchmark();
      Result := Result + GetDebugGetData();
      Result := Result + GetDebugPostData();
      Result := Result + GetDebugURIString();
      //Result := Result + GetDebugClassMethod();
      Result := Result + GetDebugHeadersData();
      //Result := Result + GetDebugSessionData();
      Result := Result + '</div>';
    end;
  end;
end;

function TThemeUtil.GetDebugBenchmark(const DebugType: string): string;
var
  html : string;
begin
  StopTime:= _GetTickCount;
  ElapsedTime:= StopTime - StartTime;

  //html := '<div class="debug">';
  html := '';
  html := html + '<fieldset>';
  html := html + '<legend>Benchmark Info</legend>';
  html := html + '<table>';
  html := html + '<tr><td>Time Usage</td><td>:</td><td>' + i2s( ElapsedTime) + ' ms</td></tr>';
  //html := html + '<tr><td>Memory Usage</td><td>:</td><td>'+ f2s((GetHeapStatus.TotalAddrSpace div 1024) / 1024) +' MB</td></tr>';
  html := html + '<tr><td>Memory Usage</td><td>:</td><td>'+ f2s((SysGetHeapStatus.TotalAddrSpace div 1024) / 1024) +' MB</td></tr>';
  html := html + '</table>';
  html := html + '</fieldset>';
  //html := html + '</div>';
  Result := html;
end;

function TThemeUtil.GetDebugGetData(const DebugType: string): string;
var
  html : string;
  i : integer;
begin
  html := '';
  //html := '<div class="debug">';
  html := html + '<fieldset>';
  html := html + '<legend>Get Data</legend>';
  html  := html + '<table>';
  for i := 0 to Application.Request.QueryFields.Count -1 do
  begin
    html  := html + '<tr>';
    html  := html + '<td>'+HTMLEncode(Application.Request.QueryFields.Names[i])+'</td><td>:</td><td>' + HTMLEncode(Application.Request.QueryFields.ValueFromIndex[i]) + '</td>';
    html  := html + '</tr>';
  end;
  html  := html + '</table>';
  html := html + '</fieldset>';
  //html := html + '</div>';
  Result := html;
end;

function TThemeUtil.GetDebugPostData(const DebugType: string): string;
var
  html : string;
  i : integer;
begin
  Result := '';
  if Application.Request.Method <> 'POST' then
    Exit;
  html := '';
  //html := '<div class="debug">';
  html := html + '<fieldset>';
  html := html + '<legend>Post Data</legend>';
  html  := html + '<table>';
  for i := 0 to Application.Request.ContentFields.Count -1 do
  begin
    html  := html + '<tr>';
    html  := html + '<td>'+HTMLEncode(Application.Request.ContentFields.Names[i])+'</td><td>:</td><td>' + HTMLEncode(Application.Request.ContentFields.ValueFromIndex[i]) + '</td>';
    html  := html + '</tr>';
  end;
  html  := html + '</table>';
  html := html + '</fieldset>';
  //html := html + '</div>';
  Result := html;
end;

function TThemeUtil.GetDebugURIString(const DebugType: string): string;
begin
  Result := '';
end;

function TThemeUtil.GetDebugClassMethod(const DebugType: string): string;
begin
  Result := '';
end;

function TThemeUtil.GetDebugHeadersData(const DebugType: string): string;
var
  html : string;
  i : integer;
begin
  Result := '';
  //html := '<div class="debug">';
  html := '';
  html := html + '<fieldset>';
  html := html + '<legend>Header Data</legend>';
  html  := html + '<table>';
  html := html + '<tr><td>REDIRECT_STATUS</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['REDIRECT_STATUS']) + '</td></tr>';
  html := html + '<tr><td>HTTP_ACCEPT</td><td>:</td><td>' + HTMLEncode(Application.Request.Accept) + '</td></tr>';
  html := html + '<tr><td>HTTP_USER_AGENT</td><td>:</td><td>' + HTMLEncode(Application.Request.UserAgent) + '</td></tr>';
  html := html + '<tr><td>HTTP_CONNECTION</td><td>:</td><td>' + HTMLEncode(Application.Request. Connection) + '</td></tr>';
  html := html + '<tr><td>HTTP_REFERER</td><td>:</td><td>' + HTMLEncode(Application.Request.Referer) + '</td></tr>';
  html := html + '<tr><td>SERVER_NAME</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['SERVER_NAME']) + '</td></tr>';
  html := html + '<tr><td>SERVER_ADDR</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['SERVER_ADDR']) + '</td></tr>';
  html := html + '<tr><td>REMOTE_HOST</td><td>:</td><td>' + HTMLEncode(Application.Request.RemoteHost) + '</td></tr>';
  html := html + '<tr><td>REMOTE_ADDR</td><td>:</td><td>' + HTMLEncode(Application.Request.RemoteAddress) + '</td></tr>';
  html := html + '<tr><td>REMOTE_PORT</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['REMOTE_PORT']) + '</td></tr>';
  html := html + '<tr><td>SCRIPT_NAME</td><td>:</td><td>' + HTMLEncode(Application.Request.ScriptName) + '</td></tr>';
  html := html + '<tr><td>SCRIPT_FILENAME</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['SCRIPT_FILENAME']) + '</td></tr>';
  html := html + '<tr><td>SCRIPT_URI</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['SCRIPT_URI']) + '</td></tr>';
  html := html + '<tr><td>PATH INFO</td><td>:</td><td>' + HTMLEncode(Application.Request.PathInfo) + '</td></tr>';
  html := html + '<tr><td>PATH_TRANSLATED</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['PATH_TRANSLATED']) + '</td></tr>';
  html := html + '<tr><td>REQUEST_METHOD</td><td>:</td><td>' + HTMLEncode(Application.Request.Method) + '</td></tr>';
  html := html + '<tr><td>CONTENT_TYPE</td><td>:</td><td>' + HTMLEncode(Application.Request.ContentType) + '</td></tr>';
  html := html + '<tr><td>SERVER_PROTOCOL</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['SERVER_PROTOCOL']) + '</td></tr>';
  html := html + '<tr><td>QUERY_STRING</td><td>:</td><td>' + HTMLEncode(Application.Request.QueryString) + '</td></tr>';
  html := html + '<tr><td>HTTP_ACCEPT_ENCODING</td><td>:</td><td>' + HTMLEncode(Application.Request.AcceptEncoding) + '</td></tr>';
  html := html + '<tr><td>HTTP_ACCEPT_LANGUAGE</td><td>:</td><td>' + HTMLEncode(Application.Request.AcceptLanguage) + '</td></tr>';
  html := html + '<tr><td>HTTP_COOKIE</td><td>:</td><td>' + HTMLEncode(Application.EnvironmentVariable['HTTP_COOKIE']) + '</td></tr>';
  html := html + '<tr><td>Last Modified</td><td>:</td><td>' + HTMLEncode(Application.Request.LastModified) + '</td></tr>';
  //html := html + '<tr><td>&nbsp;</td><td>:</td><td>' + HTMLEncode(Application.Request.HTTPXRequestedWith) + '</td></tr>';
  //html := html + '<tr><td>HTTP_DNT</td><td>:</td><td>' + HTMLEncode(Application.Request.) + '</td></tr>';
  for i := 0 to Application.Request.CustomHeaders.Count - 1 do
  begin
    html  := html + '<tr>';
    html  := html + '<td>'+HTMLEncode(Application.Request.CustomHeaders.Names[i])+'</td><td>:</td><td>' + HTMLEncode(Application.Request.CustomHeaders.ValueFromIndex[i]) + '</td>';
    html  := html + '</tr>';
  end;
  html  := html + '</table>';
  html := html + '</fieldset>';
  //html := html + '</div>';
  Result := html;
end;

function TThemeUtil.GetDebugSessionData(const DebugType: string): string;
var
  html : string;
  i : integer;
  lst : TStrings;
begin
  Result := '';
  //if SessionController.IsStarted then
  //  Exit;;
  lst := TStringList.Create;
  lst.Text := SessionController.GetData;
  html := '<div class="debug">';
  html := html + '<fieldset>';
  html := html + '<legend>Session Data</legend>';
  html  := html + '<table>';
  for i := 0 to lst.Count -1 do
  begin
    html  := html + '<tr>';
    html  := html + '<td>'+HTMLEncode(lst.Names[i])+'</td><td>:</td><td>' + HTMLEncode(lst.ValueFromIndex[i]) + '</td>';
    html  := html + '</tr>';
  end;
  html  := html + '</table>';
  html := html + '</fieldset>';
  html := html + '</div>';
  lst.Free;
  Result := html;
end;

function TThemeUtil.GetVersionInfo(): boolean;
var
  vi : TVersionInfo;
begin
  Result := True;
  if VersionInfo.FullVersion <> '' then
    Exit;

  try
    vi := TVersionInfo.Create;
    try
      vi.Load( HINSTANCE);
      VersionInfo.Major        := vi.FixedInfo.FileVersion[0];
      VersionInfo.Minor        := vi.FixedInfo.FileVersion[1];
      VersionInfo.Revision     := vi.FixedInfo.FileVersion[2];
      VersionInfo.BuldNumber   := vi.FixedInfo.FileVersion[3];

      VersionInfo.Version  :=
        i2s( VersionInfo.Major) + '.' +
        i2s( VersionInfo.Minor) + '.' +
        i2s( VersionInfo.Revision);

      VersionInfo.FullVersion  :=
        i2s( VersionInfo.Major) + '.' +
        i2s( VersionInfo.Minor) + '.' +
        i2s( VersionInfo.Revision) + ' (build ' +
        i2s( VersionInfo.BuldNumber) + ')';
    finally
      vi.Free;
    end;
  except
    VersionInfo.FullVersion := '-';
    Result:= False;
  end;
end;

function TThemeUtil.getCacheFileName: string;
var
  pathinfo: string;
begin
  pathinfo := Application.Request.PathInfo;
  // strip trailing slash only when present; blindly dropping the last char
  // made "/page/1" and "/page/2" share one cache file
  if (pathinfo <> '') and (pathinfo[length(pathinfo)] = '/') then
    pathinfo := copy(pathinfo, 1, length(pathinfo) - 1);
  if Application.Request.QueryString <> '' then
    pathinfo := pathinfo + '-' + Application.Request.QueryString;
  if pathinfo = '' then
    pathinfo := 'home';
  if (pathinfo[1] = '/') or (pathinfo[1] = DirectorySeparator) then
    pathinfo := copy(pathinfo, 2, length(pathinfo) - 1);
  if pathinfo = '' then
    pathinfo := 'home';
  // a visitor with session data (logged in, a form token, ...) gets a page
  // cache of their own; the shared file is only for stateless visitors
  if SessionController.IsStarted and (SessionController.GetData <> '') then
    pathinfo := pathinfo + '-' + Copy(MD5Print(MD5String(SessionController.SessionID)), 1, 16);
  Result := ExtractFileDir(Application.ExeName) + DirectorySeparator + AppData.tempDir +
    DirectorySeparator + 'cache' + DirectorySeparator + GetActiveModuleName(Application.Request) +
    DirectorySeparator + SafeText( pathinfo) + '-' + LANG + '.html';
end;

function TThemeUtil.isCacheExpired: boolean;
begin
  Result := HoursBetween(FileDateToDateTime(FileAge(getCacheFileName)), now) >= FCacheTime;
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
      ForceDirectories(ExtractFileDir(f));
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

class function TThemeUtil.SafeTemplateName(const AName: string): string;
var
  i: integer;
begin
  Result := AName;
  i := Pos(#0, Result);
  if i > 0 then
    Result := Copy(Result, 1, i - 1);
  Result := Trim(Result);
  if (Pos('/', Result) > 0) or (Pos('\', Result) > 0) or (Pos(':', Result) > 0)
    or (Pos('..', Result) > 0) then
    Result := '';
end;

// "var op value" -> three tokens; a quoted value may contain spaces
function TThemeUtil.SplitCondition(const AHeader: string): TStringList;
var
  i: integer;
  token: string;
  quote: char;
begin
  Result := TStringList.Create;
  token := '';
  quote := #0;
  for i := 1 to Length(AHeader) do
  begin
    if quote <> #0 then
    begin
      token := token + AHeader[i];
      if AHeader[i] = quote then
        quote := #0;
    end
    else if AHeader[i] in ['"', ''''] then
    begin
      quote := AHeader[i];
      token := token + AHeader[i];
    end
    else if AHeader[i] in [' ', #9, #13, #10] then
    begin
      if token <> '' then
        Result.Add(token);
      token := '';
    end
    else
      token := token + AHeader[i];
  end;
  if token <> '' then
    Result.Add(token);
end;

// a literal, an assigned variable, an "assignto" variable or a foreach field
function TThemeUtil.IsKnownVariable(const AName: string): boolean;
var
  f: double;
begin
  Result := True;
  if AName = '' then
    Exit(False);
  if TryStrToFloat(AName, f) then
    Exit;
  if AName[1] in ['"', ''''] then
    Exit;
  if FAssignVarStringMap.IndexOfName(AName) <> -1 then
    Exit;
  if FTagAssign_Variable.IndexOfName(AName) <> -1 then
    Exit;
  if (ForeachTable_Keyname <> '') and (Pos('.', AName) > 0) then
    Exit;
  Result := False;
end;

// False when the header is not "variable operator value".
// AKnown is False when the variable does not exist: the whole block is dropped.
function TThemeUtil.EvaluateCondition(const AHeader: string; out AMatched, AKnown: boolean): boolean;
var
  parameter: TStringList;
  value1, value2: variant;
  condition: string;
begin
  Result := False;
  AMatched := False;
  AKnown := True;
  parameter := SplitCondition(AHeader);
  try
    if parameter.Count <> 3 then
      Exit;
    if not IsKnownVariable(parameter[0]) then
    begin
      AKnown := False;
      Exit(True);
    end;
    value1 := VarValue[parameter[0]];
    value2 := '';
    if not ((parameter[2] = '''''') or (parameter[2] = '""')) then
      value2 := VarValue[parameter[2]];
    condition := LowerCase(parameter[1]);
    case condition of
      '=', '==': condition := 'eq';
      '!=', '<>': condition := 'neq';
      '<': condition := 'lt';
      '>': condition := 'gt';
      '<=': condition := 'lte';
      '>=': condition := 'gte';
    end;
    case condition of
      'eq': AMatched := conditionIsEqual(value1, value2);
      'neq': AMatched := conditionIsNotEqual(value1, value2);
      'lt': AMatched := conditionIsLessThan(value1, value2);
      'gt': AMatched := conditionIsGreaterThan(value1, value2);
      'lte': AMatched := not conditionIsGreaterThan(value1, value2);
      'gte': AMatched := not conditionIsLessThan(value1, value2);
      else
        Exit;
    end;
    Result := True;
  finally
    parameter.Free;
  end;
end;

// [if var op value] ... [else] ... [/if]
// Scanned by position instead of a regular expression: the old pattern only
// matched when whitespace preceded [/if], could not take an [else] branch
// spanning lines, failed on "%", "{" or non-ASCII content (the raw tag then
// leaked into the page) and returned the tail of the true branch as the
// false branch. Blocks may be nested; an unclosed [if] is left untouched.
function TThemeUtil.ConditionalIfProcessor(TagProcessor: TReplaceTagEvent; Content: string): string;
const
  IF_OPEN = '[if ';
  ELSE_TAG = '[else]';
  IF_CLOSE = '[/if]';
var
  p, headerEnd, scan, depth, nextIf, nextElse, nextClose, nearest,
  elsePos, closePos: integer;
  header, trueBranch, falseBranch, replacement: string;
  matched, known: boolean;
begin
  Result := Content;
  p := 1;
  while True do
  begin
    p := PosEx(IF_OPEN, Result, p);
    if p = 0 then
      Break;
    headerEnd := PosEx(']', Result, p);
    if headerEnd = 0 then
      Break;
    header := Copy(Result, p + Length(IF_OPEN), headerEnd - p - Length(IF_OPEN));

    // find the [/if] of this block and its own [else]
    depth := 1;
    elsePos := 0;
    closePos := 0;
    scan := headerEnd + 1;
    while depth > 0 do
    begin
      nextIf := PosEx(IF_OPEN, Result, scan);
      nextElse := PosEx(ELSE_TAG, Result, scan);
      nextClose := PosEx(IF_CLOSE, Result, scan);
      if nextClose = 0 then
        Break;
      nearest := nextClose;
      if (nextIf > 0) and (nextIf < nearest) then
        nearest := nextIf;
      if (nextElse > 0) and (nextElse < nearest) then
        nearest := nextElse;
      if nearest = nextIf then
      begin
        Inc(depth);
        scan := nearest + Length(IF_OPEN);
      end
      else if nearest = nextElse then
      begin
        if (depth = 1) and (elsePos = 0) then
          elsePos := nearest;
        scan := nearest + Length(ELSE_TAG);
      end
      else
      begin
        Dec(depth);
        if depth = 0 then
          closePos := nearest
        else
          scan := nearest + Length(IF_CLOSE);
      end;
    end;
    if closePos = 0 then
    begin
      p := headerEnd + 1;
      Continue;
    end;

    if elsePos > 0 then
    begin
      trueBranch := Copy(Result, headerEnd + 1, elsePos - headerEnd - 1);
      falseBranch := Copy(Result, elsePos + Length(ELSE_TAG), closePos - elsePos - Length(ELSE_TAG));
    end
    else
    begin
      trueBranch := Copy(Result, headerEnd + 1, closePos - headerEnd - 1);
      falseBranch := '';
    end;

    if not EvaluateCondition(header, matched, known) then
    begin
      // not a condition we understand: keep the tag, continue inside it
      p := headerEnd + 1;
      Continue;
    end;
    if not known then
      replacement := ''
    else if matched then
      replacement := ConditionalIfProcessor(TagProcessor, trueBranch)
    else
      replacement := ConditionalIfProcessor(TagProcessor, falseBranch);
    Result := Copy(Result, 1, p - 1) + replacement + Copy(Result, closePos + Length(IF_CLOSE), MaxInt);
    p := p + Length(replacement);
  end;
end;

// Variant comparison raises EVariantError when one side is numeric and the
// other is a non-numeric string (ex: [if $count gt 0] with $count = '').
// Fall back to a plain string comparison instead of killing the render.
function TThemeUtil.conditionIsEqual(Value1, Value2: variant): boolean;
begin
  try
    Result := Value1 = Value2;
  except
    Result := VarToStr(Value1) = VarToStr(Value2);
  end;
end;

function TThemeUtil.conditionIsNotEqual(Value1, Value2: variant): boolean;
begin
  try
    Result := Value1 <> Value2;
  except
    Result := VarToStr(Value1) <> VarToStr(Value2);
  end;
end;

function TThemeUtil.conditionIsLessThan(Value1, Value2: variant): boolean;
begin
  try
    Result := Value1 < Value2;
  except
    Result := VarToStr(Value1) < VarToStr(Value2);
  end;
end;

function TThemeUtil.conditionIsGreaterThan(Value1, Value2: variant): boolean;
begin
  try
    Result := Value1 > Value2;
  except
    Result := VarToStr(Value1) > VarToStr(Value2);
  end;
end;

// Expands every [foreach ...] ... [/foreach ...] block. Blocks are matched
// by nesting depth, so a foreach inside a foreach body is left to the row
// rendering (table/dataset rows go through RenderFromContent) or to the
// next pass over the expanded output (json rows).
function TThemeUtil.ForeachProcessor(TagProcessor: TReplaceTagEvent;
  Content: string): string;
const
  OPEN_TAG = '[foreach ';
  CLOSE_TAG = '[/foreach';

  function RunForeach(const AHeader, ABody: string): string;
  var
    parameter: TStrings;
    savedKey, savedItem: string;
    savedIndex: integer;
  begin
    Result := '';
    // an inner loop (run while rendering a table/dataset row) must hand the
    // outer loop its state back
    savedKey := ForeachTable_Keyname;
    savedItem := ForeachTable_Itemname;
    savedIndex := foreachJsonIndex;
    parameter := Explode(AHeader, ' ');
    try
      ForeachTable_Keyname  := parameter.Values['from'];
      ForeachTable_Itemname := parameter.Values['item'];

      case parameter.Values['type'] of
        '' : begin
          DisplayError( 'field "type" is not define in "foreach ' + AHeader + '"');
        end;
        'table' : begin
          Result := ForeachProcessor_Table(TagProcessor, parameter.Values['from'], ABody);
        end;
        'dataset' : begin
          Result := ForeachProcessor_Dataset(TagProcessor, parameter.Values['from'], ABody);
        end;
        'json', 'jsondata' : begin
          Result := ForeachProcessor_Json(TagProcessor, parameter.Values['from'], ABody);
        end;
        {$ifdef GREYHOUND}
        'ghtable' : begin
          Result := ForeachProcessor_GreyhoundTable(TagProcessor, parameter.Values['from'], ABody);
        end;
        {$endif GREYHOUND}
        'array' : begin
          DisplayError( __( __Err_Theme_ForeachNotImplemented));
        end;
      end;
    finally
      ForeachTable_Keyname := savedKey;
      ForeachTable_Itemname := savedItem;
      foreachJsonIndex := savedIndex;
      FreeAndNil( parameter);
    end;
  end;

var
  startPos, headEnd, p, depth, closeStart, closeEnd, nextOpen, nextClose: integer;
  header, body: string;
begin
  Result := Content;
  startPos := 1;
  repeat
    startPos := PosEx(OPEN_TAG, Result, startPos);
    if startPos = 0 then
      Break;
    headEnd := PosEx(FEndDelimiter, Result, startPos);
    if headEnd = 0 then
      Break;
    header := Copy(Result, startPos + Length(OPEN_TAG), headEnd - startPos - Length(OPEN_TAG));

    // the matching close: same depth
    depth := 1;
    p := headEnd + 1;
    closeStart := 0;
    while depth > 0 do
    begin
      nextOpen := PosEx(OPEN_TAG, Result, p);
      nextClose := PosEx(CLOSE_TAG, Result, p);
      if nextClose = 0 then
        Break;
      if (nextOpen > 0) and (nextOpen < nextClose) then
      begin
        Inc(depth);
        p := nextOpen + Length(OPEN_TAG);
      end
      else
      begin
        Dec(depth);
        if depth = 0 then
          closeStart := nextClose
        else
          p := nextClose + Length(CLOSE_TAG);
      end;
    end;
    if closeStart = 0 then
    begin
      // unclosed: leave the tag as it is and look further
      startPos := headEnd + 1;
      Continue;
    end;
    closeEnd := PosEx(FEndDelimiter, Result, closeStart);
    if closeEnd = 0 then
      Break;

    body := Copy(Result, headEnd + 1, closeStart - headEnd - 1);
    Result := Copy(Result, 1, startPos - 1) + RunForeach(header, body)
      + Copy(Result, closeEnd + 1, Length(Result));
    // the next search starts inside the expanded block: a nested foreach
    // that survived the row rendering is expanded now
  until False;
end;

function TThemeUtil.ForeachProcessor_Table(TagProcessor: TReplaceTagEvent;
  KeyName, Content: string): string;
var
  html, row : string;
  templateEngine : TFPTemplate;
  i : integer;
begin
  Result := '';
  if ( AssignVar[KeyName] = nil) then
  begin
    Exit;
  end;

  html := '';
  i := 1;
  // a cursor left on a later row (or at EOF) rendered nothing
  if TSQLQuery( assignVarMap[KeyName]^).Active then
    TSQLQuery( assignVarMap[KeyName]^).First;
  while not TSQLQuery( assignVarMap[KeyName]^).EOF do
  begin
    ThemeUtil.Assign('$index', i.ToString);
    ThemeUtil.Assign('foreach_index', i2s(i mod 2));
    if (i mod 2) = 1 then
      ThemeUtil.Assign('foreach_odd', 'odd')
    else
      ThemeUtil.Assign('foreach_odd', 'even');

    templateEngine := TFPTemplate.Create;
    try
      templateEngine.Template := Content;
      templateEngine.AllowTagParams := True;
      templateEngine.StartDelimiter := FStartDelimiter;
      templateEngine.EndDelimiter := FEndDelimiter;
      templateEngine.ParamValueSeparator := '=';
      templateEngine.OnReplaceTag := @ForeachProcessor_Table_TagController;
      row := templateEngine.GetContent;
    finally
      FreeAndNil(templateEngine);
    end;

    // render only the current row; rendering the accumulated html made
    // every previous row go through the tag processor again (O(n^2))
    row := RenderFromContent(@TagController, row);

    //-- proccess conditional if
    row := ConditionalIfProcessor( TagProcessor, row);
    html := html + row;

    TSQLQuery( assignVarMap[KeyName]^).Next;
    i := i + 1;
  end;
  Result := html;
end;

function TThemeUtil.ForeachProcessor_Dataset(TagProcessor: TReplaceTagEvent;
  KeyName, Content: string): string;
var
  i: integer;
  html, row : string;
  templateEngine : TFPTemplate;
begin
  Result := '';
  if ( AssignVar[KeyName] = nil) then
  begin
    Exit;
  end;

  html := '';
  i := 1;
  if TDataSet( assignVarMap[KeyName]^).Active then
    TDataSet( assignVarMap[KeyName]^).First;
  while not TDataSet( assignVarMap[KeyName]^).EOF do
  begin
    ThemeUtil.Assign('$index', i.ToString);
    templateEngine := TFPTemplate.Create;
    try
      templateEngine.Template := Content;
      templateEngine.AllowTagParams := True;
      templateEngine.StartDelimiter := FStartDelimiter;
      templateEngine.EndDelimiter := FEndDelimiter;
      templateEngine.ParamValueSeparator := '=';
      templateEngine.OnReplaceTag := @ForeachProcessor_Dataset_TagController;
      row := templateEngine.GetContent;
    finally
      FreeAndNil(templateEngine);
    end;

    row := RenderFromContent(@TagController, row);

    //-- proccess conditional if
    row := ConditionalIfProcessor( TagProcessor, row);
    html := html + row;

    TDataSet( assignVarMap[KeyName]^).Next;
    i := i + 1;
  end;
  Result := html;
end;

function TThemeUtil.ForeachProcessor_Json(TagProcessor: TReplaceTagEvent; KeyName, Content: string): string;
var
  html, tmp : string;
  templateEngine : TFPTemplate;
  i : integer;
begin
  Result := '';
  if ForeachTable_Keyname = '' then
    Exit;
  if ( AssignVar[KeyName] = nil) then
  begin
    Exit;
  end;
  if not Assigned(TJSONData( assignVarMap[KeyName]^)) then
  begin
    Exit;
  end;

  html := '';
  foreachJsonIndex := 0;
  for i := 0 to TJSONData( assignVarMap[KeyName]^).Count - 1 do
  begin
    ThemeUtil.Assign('$index', (i+1).ToString);
    templateEngine := TFPTemplate.Create;
    templateEngine.Template := Content; // tmp
    templateEngine.AllowTagParams := True;
    templateEngine.StartDelimiter := FStartDelimiter;
    templateEngine.EndDelimiter := FEndDelimiter;
    templateEngine.ParamValueSeparator := '=';
    templateEngine.OnReplaceTag := @ForeachProcessor_Json_TagController;
    tmp := templateEngine.GetContent;
    FreeAndNil(templateEngine);

    //-- proccess conditional if while this row is still current
    tmp := ConditionalIfProcessor( TagProcessor, tmp);

    html := html + tmp;
    inc( foreachJsonIndex);
  end;

  Result := html;
end;

{$ifdef GREYHOUND}
function TThemeUtil.ForeachProcessor_GreyhoundTable(
  TagProcessor: TReplaceTagEvent; KeyName, Content: string): string;
var
  html, tmp : string;
  templateEngine : TFPTemplate;
begin
  if ( AssignVar[KeyName] = nil) then
  begin
    Exit;
  end;

  if TghSQLTable( assignVarMap[KeyName]^).IsEmpty then
  begin
    Result := '';
    Exit;
  end;

  html := '';

  TghSQLTable( assignVarMap[KeyName]^).First;
  while not TghSQLTable( assignVarMap[KeyName]^).EOF do
  begin

    //tmp := RenderFromContent(@TagController, Content);

    templateEngine := TFPTemplate.Create;
    templateEngine.Template := Content; // tmp
    templateEngine.AllowTagParams := True;
    templateEngine.StartDelimiter := FStartDelimiter;
    templateEngine.EndDelimiter := FEndDelimiter;
    templateEngine.ParamValueSeparator := '=';
    templateEngine.OnReplaceTag := @ForeachProcessor_GreyhoundTable_TagController;
    html := html + templateEngine.GetContent;
    FreeAndNil(templateEngine);

    html := RenderFromContent(@TagController, html);

    //-- proccess conditional if
    html := ConditionalIfProcessor( TagProcessor, html);

    TghSQLTable( assignVarMap[KeyName]^).Next;
  end;
  Result := html;
end;

procedure TThemeUtil.ForeachProcessor_GreyhoundTable_TagController(
  Sender: TObject; const TagString: string; TagParams: TStringList; out
  ReplaceText: string);
var
  tagstring_custom : TStringList;
  tag_with_filter, lst : TStrings;
  fieldName, filter, s : string;
  i : integer;
begin
  if ForeachTable_Keyname = '' then
    Exit;

  if Pos( '|', TagString) = 0 then
  begin
    tagstring_custom := ExplodeTags( TagString);
    filter := tagstring_custom.Values['filter'];
  end
  else begin
    tag_with_filter := Explode( TagString, '|');
    tagstring_custom := ExplodeTags( tag_with_filter[0]);
    filter := '';
    if tag_with_filter.Count > 1 then
      filter := trim( tag_with_filter[1]);
    lst := Explode( filter, ' ');
    if lst.Count > 0 then
    begin
      filter := lst[0];
      lst.Delete(0);
    end;
    tagstring_custom.Add( 'filter=' + filter);
    tagstring_custom.Add( lst.Text);
    FreeAndNil( lst);
    FreeAndNil( tag_with_filter);
  end;
  fieldName := tagstring_custom.Values['index'];

  try
    ReplaceText := TghSQLTable( assignVarMap[ForeachTable_Keyname]^)[ fieldName].AsString;
  except
    on e : Exception do
    begin
      ReplaceText:= 'ghtable' +e.Message;
    end;
  end;

  if filter <> '' then
  begin
    if filter = 'moreless' then
    begin
      s := trim( tagstring_custom.Values['count']);
      i := s2i( s);
      ReplaceText := MoreLess( ReplaceText, i);
    end
    else
      ReplaceText := FilterOutput( ReplaceText, filter);
  end;

  FreeAndNil( tagstring_custom);
end;
{$endif GREYHOUND}


procedure TThemeUtil.ForeachProcessor_Table_TagController(Sender: TObject;
  const TagString: string; TagParams: TStringList; out ReplaceText: string);
var
  tagstring_custom : TStringList;
  filter, dateAsString : string;
  sdf: ansistring;
  dateAsLongInt: LongInt;
  i : integer;
begin
  if TagString.IsEmpty then
  begin
    ReplaceText := '[]';
    Exit;
  end;
  if ForeachTable_Keyname = '' then
    Exit;
  tagstring_custom := ExplodeTags( TagString);
  ReplaceText := FStartDelimiter +  TagString + FEndDelimiter;
  filter := tagstring_custom.Values['filter'];

  if tagstring_custom[0] <> ForeachTable_Itemname then
  begin
    FreeAndNil( tagstring_custom);
    Exit;
  end;
  if tagstring_custom.Values['assignto'] <> '' then
  begin
    FTagAssign_Variable.Values[tagstring_custom.Values['assignto']]:='s|'
      + TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsString;
    FAssignVarStringMap.Values[tagstring_custom.Values['assignto']]:=
      TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsString;
    FreeAndNil( tagstring_custom);
    Exit;
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
      try
        ReplaceText := DateTimeHuman( TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsDateTime);
      except
        dateAsString := TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsString;
        ReplaceText := DateTimeHuman( dateAsString);
      end;
    end
    else
    begin
      try
        ReplaceText := FormatDateTime( tagstring_custom.Values['dateformat'],
          TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsDateTime
        );
      except
        sdf := DefaultFormatSettings.ShortDateFormat;
        DefaultFormatSettings.ShortDateFormat := 'yyyy/MM/dd';
        dateAsLongInt:= TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsInteger;
        DefaultFormatSettings.ShortDateFormat := sdf;
        ReplaceText := FormatDateTime( tagstring_custom.Values['dateformat'], UnixToDateTime(dateAsLongInt));
      end;
    end;
  end
  else
  begin
    ReplaceText := TSQLQuery( assignVarMap[ForeachTable_Keyname]^).FieldByName(tagstring_custom.Values['index']).AsString;
  end;

  if filter <> '' then
  begin
    if filter = 'moreless' then
    begin
      i := s2i( tagstring_custom.Values['count']);
      ReplaceText := MoreLess( ReplaceText, i);
    end
    else
      ReplaceText := FilterOutput( ReplaceText, filter, tagstring_custom);
  end;
  FreeAndNil( tagstring_custom);
end;

procedure TThemeUtil.ForeachProcessor_Dataset_TagController(Sender: TObject;
  const TagString: string; TagParams: TStringList; out ReplaceText: string);
var
  tagstring_custom : TStringList;
  tag_with_filter, lst : TStrings;
  fieldName, filter, s : string;
  i : integer;
begin
  if TagString.IsEmpty then
  begin
    ReplaceText := '[]';
    Exit;
  end;
  if ForeachTable_Keyname = '' then
    Exit;
  // conditionals and nested loops are evaluated after the row is rendered;
  // keep them intact
  if (Pos( 'if ', TagString) = 1) or (TagString = 'else') or (TagString = '/if')
    or (Pos( 'foreach ', TagString) = 1) or (Pos( '/foreach', TagString) = 1) then
  begin
    ReplaceText := FStartDelimiter + TagString + FEndDelimiter;
    Exit;
  end;

  if Pos( '|', TagString) = 0 then
  begin
    tagstring_custom := ExplodeTags( TagString);
    filter := tagstring_custom.Values['filter'];
  end
  else begin
    tag_with_filter := Explode( TagString, '|');
    tagstring_custom := ExplodeTags( tag_with_filter[0]);
    filter := '';
    if tag_with_filter.Count > 1 then
      filter := trim( tag_with_filter[1]);
    lst := Explode( filter, ' ');
    if lst.Count > 0 then
    begin
      filter := lst[0];
      lst.Delete(0);
    end;
    tagstring_custom.Add( 'filter=' + filter);
    tagstring_custom.Add( lst.Text);
    FreeAndNil( lst);
    FreeAndNil( tag_with_filter);
  end;
  fieldName := tagstring_custom.Values['index'];
  // a field tag of another loop's item (a nested foreach) is left for
  // that loop
  if (fieldName <> '') and (tagstring_custom.Count > 0)
    and (tagstring_custom[0] <> ForeachTable_Itemname)
    and (tagstring_custom[0] <> ForeachTable_Keyname) then
  begin
    ReplaceText := FStartDelimiter + TagString + FEndDelimiter;
    FreeAndNil( tagstring_custom);
    Exit;
  end;

  try
    ReplaceText := TDataSet( assignVarMap[ForeachTable_Keyname]^).FieldByName( fieldName).AsString;
  except
    on e : Exception do
    begin
      ReplaceText:= 'dataset' +e.Message;
    end;
  end;

  if filter <> '' then
  begin
    if filter = 'moreless' then
    begin
      s := trim( tagstring_custom.Values['count']);
      i := s2i( s);
      ReplaceText := MoreLess( ReplaceText, i);
    end
    else
      ReplaceText := FilterOutput( ReplaceText, filter, tagstring_custom);
  end;

  FreeAndNil( tagstring_custom);
end;

procedure TThemeUtil.ForeachProcessor_Json_TagController(Sender: TObject; const TagString: string;
  TagParams: TStringList; out ReplaceText: string);
var
  tagstring_custom : TStringList;
  tag_with_filter, lst : TStrings;
  fieldName, filter, s : string;
  i : integer;
  tmpFormatSettings: TFormatSettings;
begin
  if TagString.IsEmpty then
  begin
    ReplaceText := '[]';
    Exit;
  end;
  if ForeachTable_Keyname = '' then
    Exit;
  // conditionals and nested loops are evaluated after the row is rendered;
  // keep them intact
  if (Pos( 'if ', TagString) = 1) or (TagString = 'else') or (TagString = '/if')
    or (Pos( 'foreach ', TagString) = 1) or (Pos( '/foreach', TagString) = 1) then
  begin
    ReplaceText := FStartDelimiter + TagString + FEndDelimiter;
    Exit;
  end;

  if Pos( '|', TagString) = 0 then
  begin
    tagstring_custom := ExplodeTags( TagString);
    filter := tagstring_custom.Values['filter'];
  end
  else begin
    tag_with_filter := Explode( TagString, '|');
    tagstring_custom := ExplodeTags( tag_with_filter[0]);
    filter := '';
    if tag_with_filter.Count > 1 then
      filter := trim( tag_with_filter[1]);
    lst := Explode( filter, ' ');
    if lst.Count > 0 then
    begin
      filter := lst[0];
      lst.Delete(0);
    end;
    tagstring_custom.Add( 'filter=' + filter);
    tagstring_custom.Add( lst.Text);
    FreeAndNil( lst);
    FreeAndNil( tag_with_filter);
  end;
  fieldName := tagstring_custom.Values['index'];
  // a field tag of another loop's item (a nested foreach) is left for
  // that loop
  if (fieldName <> '') and (tagstring_custom.Count > 0)
    and (tagstring_custom[0] <> ForeachTable_Itemname)
    and (tagstring_custom[0] <> ForeachTable_Keyname) then
  begin
    ReplaceText := FStartDelimiter + TagString + FEndDelimiter;
    FreeAndNil( tagstring_custom);
    Exit;
  end;

  if tagstring_custom.Values['assignto'] <> '' then
  begin
    try
      FAssignVarStringMap.Values[tagstring_custom.Values['assignto']] :=
        TJSONData( assignVarMap[ForeachTable_Keyname]^).Items[ foreachJsonIndex].FindPath( fieldName).AsString;
    except
      FAssignVarStringMap.Values[tagstring_custom.Values['assignto']] := '';
    end;
    FreeAndNil( tagstring_custom);
    Exit;
  end;

  try
    ReplaceText := TJSONData( assignVarMap[ForeachTable_Keyname]^).Items[ foreachJsonIndex].FindPath( fieldName).AsString;
    if tagstring_custom.Values['dateformat'] <> '' then
    begin
      tmpFormatSettings := FormatSettings;
      tmpFormatSettings.DateSeparator := '-';
      tmpFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
      ReplaceText := FormatDateTime(tagstring_custom.Values['dateformat'], StrToDateTime(ReplaceText, tmpFormatSettings));
    end;
  except
    on e : Exception do begin
      if e.Message = 'Access violation' then
        s := ForeachTable_Keyname + ': Index "'+fieldName+'" not found'
      else
        s:= ForeachTable_Keyname + '('+fieldName+') : ' + e.Message;
      s := 'foreach json: ' + s;
      TagController( Sender, TagString, TagParams, ReplaceText);
      if ReplaceText = '' then
        ReplaceText := s;
    end;
  end;

  if filter <> '' then
  begin
    if filter = 'moreless' then
    begin
      s := trim( tagstring_custom.Values['count']);
      i := s2i( s);
      ReplaceText := MoreLess( ReplaceText, i);
    end
    else
      ReplaceText := FilterOutput( ReplaceText, filter, tagstring_custom);
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
  // do not overwrite instances that already exist (would leak and drop
  // callbacks registered before ThemeUtil was created); only free what
  // this instance created
  FOwnsHTMLUtil := not Assigned(HTMLUtil);
  if FOwnsHTMLUtil then
    HTMLUtil := THTMLUtil.Create;
  RenderType:= rtSmarty;
  FOwnsTagCallbackMap := not Assigned(___TagCallbackMap);
  if FOwnsTagCallbackMap then
    ___TagCallbackMap := TTagCallbackMap.Create;
end;

destructor TThemeUtil.Destroy;
begin
  if FOwnsTagCallbackMap then
    FreeAndNil(___TagCallbackMap);
  if FOwnsHTMLUtil then
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
  i : integer;
  s, tagname, filter, value : string;
  tagstring_custom : TStringList;
  str, tag_with_filter : TStrings;
begin
  if trim( TagString).IsEmpty then
  begin
    ReplaceText:= '[]';
    Exit;
  end;
  if Pos( '*', TagString) = 1 then
  begin
    ReplaceText := '<!--'+TagString+'-->';
    Exit;
  end;
  // tag diawali "!" diabaikan, contoh: [!include file=includes/header.html]
  if Pos( '!', TagString) = 1 then
  begin
    ReplaceText := '';
    Exit;
  end;
  if Pos( '|', TagString) = 0 then
  begin
    tagstring_custom := ExplodeTags( TagString);
    filter := tagstring_custom.Values['filter'];
  end
  else begin
    tagstring_custom := TStringList.Create;
    tag_with_filter := Explode( TagString, '|');
    tagstring_custom.Text := tag_with_filter.Text;
    FreeAndNil( tag_with_filter);
    filter := '';
    if tagstring_custom.Count > 1 then
      filter := trim( tagstring_custom[1]);
  end;
  //ReplaceText := tagstring_custom[0];
  //ReplaceText := '';
  ReplaceText := ThemeUtil.StartDelimiter +  TagString + ThemeUtil.EndDelimiter;

  // check from AssignVar
  if not FastPlasAppandler.isDisplayError then
  begin
  if tagstring_custom.Values['index']<>'' then
  begin
    if ThemeUtil.AssignVar[tagstring_custom[0]] <> Nil then
    begin
      try
        if tagstring_custom.Values['type'] = 'json' then
        begin
          s := tagstring_custom.Values['index'].Replace('.','/');
          ReplaceText := TJSONUtil(ThemeUtil.AssignVar[tagstring_custom[0]]^).Value[UnicodeString(s)];
        end;
        if tagstring_custom.Values['type'] = '' then
        begin
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
        end;

      except
        on E:Exception do
        begin
          ReplaceText :='[field not found]';
        end;
      end;
      ReplaceText := FilterOutput( ReplaceText, tagstring_custom.Values['filter'], tagstring_custom);
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

  if tagstring_custom.Count = 0 then
  begin
    ReplaceText := '[]';
    FreeAndNil( tagstring_custom);
    Exit;
  end;
  tagname := tagstring_custom[0];
  case tagname of
    'sitename' : begin
      ReplaceText := AppData.sitename;
      end;
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
      ReplaceText := AppData.baseUrl;
      end;
    'baseurl' : begin
      ReplaceText := AppData.baseUrl;
      end;
    '$thisurl': ReplaceText := FastPlasAppandler.URI;
    'thisurl' : begin
      ReplaceText := FastPlasAppandler.URI;
      if tagstring_custom.Values['type'] = 'full' then
      begin
        ReplaceText := BaseURL + ReplaceText;
        ReplaceText := StringReplace( ReplaceText, '//', '/', [rfReplaceAll]);
        // restore the scheme separator for both http:// and https://
        ReplaceText := StringReplace( ReplaceText, ':/', '://', []);
      end;
    end;
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
    'referer' : begin
        ReplaceText := Application.Request.Referer;
      end;
    'referrer' : begin
        ReplaceText := Application.Request.Referer;
      end;
    'ipaddress' : begin
        ReplaceText := GetUserIpAddress;
      end;
    'datetime' : begin
      if tagstring_custom.Values['format'] <> '' then
        ReplaceText := FormatDateTime(tagstring_custom.Values['format'], Now)
      else
        ReplaceText := FormatDateTime('dd MMM YYYY HH:nn:ss', Now);
      end;
    'date' : begin
      if tagstring_custom.Values['format'] <> '' then
        ReplaceText := FormatDateTime(tagstring_custom.Values['format'], Now)
      else
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
      if FIncludeDepth >= 32 then
        ReplaceText := EchoError( 'include too deep: %s', [tagstring_custom.Values['file']])
      else
      begin
        Inc( FIncludeDepth);
        try
          ReplaceText := ThemeUtil.Render( @TagController, tagstring_custom.Values['file'], false, true);
        finally
          Dec( FIncludeDepth);
        end;
      end;
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
    'config' : begin
      ReplaceText := Config[ tagstring_custom.Values['key']];
      end;
    'modvar' : begin
      ReplaceText:= ModVar[ tagstring_custom.Values['key']];
      end;
    'csrf-token' : begin
      HTMLUtil.ResetCSRF;
      ReplaceText := HTMLUtil.CSRF( tagstring_custom.Values['name']);
      end;
    'loadtime' : begin
      ReplaceText := GetDebugInfo( 'time');
    end;
    'flashmessages' : begin
      // usage: [flashmessages class="..."] ; [flashmessages html=1] keeps
      // markup in the messages (they are HTML encoded otherwise)
      s := FlashMessages;
      ReplaceText := '';
      if s <> '' then
      begin
        str := Explode( s, '|');
        try
          ReplaceText := '<div id="flashmessages" class="alert alert-warning alert-dismissable '+HTMLEncode(tagstring_custom.Values['class'])+'"><h4><i class="icon fa fa-ban"></i> Warning!</h4><ul>';
          for i:=0 to str.Count-1 do
          begin
            if str[i] = '' then Continue;
            if (tagstring_custom.Values['html'] = '1') or (tagstring_custom.Values['html'] = 'true') then
              ReplaceText:= ReplaceText + '<li>'+str[i]+'</li>'
            else
              ReplaceText:= ReplaceText + '<li>'+HTMLEncode(str[i])+'</li>';
          end;
          ReplaceText:= ReplaceText + '</ul></div>';
        finally
          FreeAndNil( str);
        end;
        FlashMessages:='';
      end;
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

    //-- form control
    'input' : begin
      value := tagstring_custom.Values['value'];
      //value := FAssignVarStringMap.Text;
      if pos( '$', value) = 1 then
      begin
        if FAssignVarStringMap.IndexOfName( Copy( value, 2)) <> -1 then
          value := FAssignVarStringMap.Values[ Copy( value, 2)];
      end;
      ReplaceText := HTMLUtil.AddInputLTE( tagstring_custom.Values['id'], tagstring_custom.Values['type'],
        tagstring_custom.Values['label'], value, tagstring_custom.Values['placeholder'],
        s2b( tagstring_custom.Values['required']), tagstring_custom.Values['button']);
    end;
  end;

  {$if fpc_fullversion >= 20701}
  //if ___TagCallbackMap.Contains(tagname) then begin
  if ___TagCallbackMap.IndexOf(tagname) >= 0 then begin
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

  //if ReplaceText = '['+tagstring_custom.Text.Replace(#10,' ').Trim+']' then
  if Pos('['+tagname+' ', ReplaceText)>0 then
  begin
    if filter='defaultempty' then
      ReplaceText := '';
    // gunakan ini, jika nama variable ditampilkan saat variable tidak ditemukan
    //ReplaceText := ThemeUtil.StartDelimiter +  TagString + ThemeUtil.EndDelimiter;
  end;

  if filter <> '' then
  begin
    ReplaceText := FilterOutput( ReplaceText, filter, tagstring_custom);
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
  templateFilename, _ext, moduleActive, relativeFile, layoutFile: string;
  templateEngine: TFPTemplate;
  response_json : TJSONObject;
begin
  Result := '';
  // a #0 would cut the name at the OS level and drop the extension check
  layoutFile := LayoutTemplateFile;
  if Pos(#0, layoutFile) > 0 then
    layoutFile := Copy(layoutFile, 1, Pos(#0, layoutFile) - 1);
  if not AppData.themeEnable then
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
    // set-up instructions only: the web "initialize" module is not
    // registered (it wrote files for any visitor), so no link to it
    Result := __(__Err_App_Init);
    Exit;
  end;

  templateFilename := layoutFile;
  if not SubModule then
    if FLayout <> '' then templateFilename := trim(FLayout);
  if templateFilename <> '' then
  begin
    templateFilename := StringReplace( templateFilename, '"', '', [rfReplaceAll]);
    templateFilename := StringReplace( templateFilename, '''', '', [rfReplaceAll]);
    templateFilename := trim( templateFilename);
    _ext := FThemeExtension;
    if ExtractFileExt( templateFilename) <> '' then
      _ext := '';
    templateFilename := GetCurrentDir + DirectorySeparator + 'themes'
      + DirectorySeparator + ThemeName
      + DirectorySeparator + 'templates'
      + DirectorySeparator + templateFilename+ _ext;

    if not FileExists(templateFilename) then
    begin
      // fallback: path relative to the web root (current dir).
      // A leading "/" means "from the web root", not from the drive/fs root,
      // ex: [include file="/modules/inbox/includes/thread.html"]
      relativeFile := SetDirSeparators( trim( layoutFile));
      relativeFile := StringReplace( relativeFile, '"', '', [rfReplaceAll]);
      relativeFile := StringReplace( relativeFile, '''', '', [rfReplaceAll]);
      while (relativeFile <> '') and (relativeFile[1] = DirectorySeparator) do
        Delete( relativeFile, 1, 1);
      templateFilename := IncludeTrailingPathDelimiter( GetCurrentDir) + relativeFile;
      // last resort: a real absolute path (ex: "D:\..." on Windows)
      if (not FileExists(templateFilename)) and (ExtractFileDrive( layoutFile) <> '')
        and FileExists( layoutFile) then
        templateFilename := layoutFile;
      if not FileExists(templateFilename) then
      begin
        Result := EchoError( __(__Err_Theme_Not_Exists), [ templateFilename, ThemeName]);
        Exit;
      end;
    end;
  end
  else
  begin
    // both come from the request: "act=../../x" must not leave the theme
    moduleActive := SafeTemplateName( GetActiveModuleName(Application.Request));
    templateFilename := SafeTemplateName( Application.Request.QueryFields.Values['act']);
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

  if not FileExists(templateFilename) then
  begin
    if AppData.debug then
      Result := Format(__Err_Theme_Template_NotFound, [templateFilename])
    else
      Result := Format(__Err_Theme_Template_NotFound, [ExtractFileName(templateFilename)]);
    Exit;
  end;

  try
    templateEngine := TFPTemplate.Create;
    try
      templateEngine.Template := ReadTemplateFile(templateFilename);
      templateEngine.AllowTagParams := True;
      templateEngine.StartDelimiter := FStartDelimiter;
      templateEngine.EndDelimiter := FEndDelimiter;
      templateEngine.ParamValueSeparator := FParamValueSeparator;
      if TagProcessorAddress = nil then
        templateEngine.OnReplaceTag := @TagController
      else
        templateEngine.OnReplaceTag := TagProcessorAddress;

      Result := templateEngine.GetContent;
    finally
      FreeAndNil(templateEngine);
    end;

    // clean tag
    if isCleanTag then
      Result := RenderFromContent( @TagCleaner, Result);

    //-- proccess foreach
    if RenderType = rtSmarty then
    begin
      // only from module
      //Result := ForeachProcessor( TagProcessorAddress, Result);
    end;

    //-- proccess conditional if
    Result:= ConditionalIfProcessor( TagProcessorAddress, Result);

  except
    on e : Exception do
    begin
      FastPlasAppandler.DieRaise(e.Message,[]);
    end;
  end;

  if not SubModule then
  begin
    // meta first, then stylesheets, then scripts
    if FHTMLHead.Meta.Count>0 then
      Result:=StringReplace(Result,'</head>',FHTMLHead.Meta.Text+'</head>',[rfReplaceAll]);
    if FHTMLHead.CSS.Count>0 then
      Result:=StringReplace(Result,'</head>',FHTMLHead.CSS.Text+'</head>',[rfReplaceAll]);
    if FHTMLHead.JS.Count>0 then
      Result:=StringReplace(Result,'</head>',FHTMLHead.JS.Text+'</head>',[rfReplaceAll]);
    AddHit( Application.Request.URL);
  end;
  Result := AdjustLineBreaks(Result);
  if FTrimWhiteSpace then
    Result := DoTrimWhiteSpace(Result,FTrimForce);
  if Cache then
    SaveCache(Result);

  // only on the final page: not inside every [include], not in JSON data
  if not (SubModule or FIsJSON) then
    Result := Result + '<!-- '+getDebugInfo('time')+' -->';

  if FIsJSON then
  begin
    response_json := TJSONObject.Create;
    response_json.Add( 'code', 0);
    response_json.Add( 'data', Result);
    Result := response_json.AsJSON;
    FreeAndNil( response_json);
  end;
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

  //-- proccess foreach
  if RenderType = rtSmarty then
  begin
    html.Text:= ForeachProcessor( TagProcessorAddress, html.Text);
  end;

  //-- proccess conditional if
  html.Text:= ConditionalIfProcessor( TagProcessorAddress, html.Text);

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
  // remove comments first, so the line they were on is dropped as well
  templateEngine := TFPTemplate.Create;
  templateEngine.Template := Content;
  templateEngine.AllowTagParams := True;
  templateEngine.StartDelimiter := '<!--';
  templateEngine.EndDelimiter := '-->';
  templateEngine.ParamValueSeparator := '=';
  templateEngine.OnReplaceTag := nil;
  html := TStringList.Create;
  html.Text := templateEngine.GetContent;
  FreeAndNil(templateEngine);

  // scanning backwards: a closing tag starts a block to keep as it is
  skip:=false;
  for i:=html.Count-1 downto 0 do
  begin
    if Pos('</script',html[i])>0 then skip:=true;
    if Pos('<script',html[i])>0 then skip:=false;
    if Pos('</code',html[i])>0 then skip:=true;
    if Pos('<code',html[i])>0 then skip:=false;
    if Pos('</pre',html[i])>0 then skip:=true;
    if Pos('<pre',html[i])>0 then skip:=false;
    if Pos('</textarea',html[i])>0 then skip:=true;
    if Pos('<textarea',html[i])>0 then skip:=false;
    if (not skip) or ForceTrim then
    begin
      html[i]:=trim(html[i]);
      if html[i]='' then
        html.Delete(i);
    end;
  end;
  Result := html.Text;
  FreeAndNil(html);
end;

initialization
  //___TagCallbackMap := TTagCallbackMap.Create;

finalization
  //FreeAndNil(___TagCallbackMap);

end.

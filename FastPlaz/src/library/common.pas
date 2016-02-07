unit common;

{$mode objfpc}{$H+}
{ $include define.inc}

interface

uses
  //SynExportHTML,
  fpcgi, gettext, process, Math, fpjson, jsonparser, jsonscanner, custweb, jsonConf,
  fphttpclient,
  //fphttpclient_with_ssl,
  RegExpr,
  Classes, SysUtils, fastplaz_handler, config_lib;

const
  _APP = 'FastPlaz';
  _APP_SLOGAN = 'Fast Pascal Framework for Web Development';
  _APP_URL = 'http://www.fastplaz.com';
  _SYSTEM_SITENAME = 'systems/sitename';
  _SYSTEM_SLOGAN = 'systems/slogan';
  _SYSTEM_BASEURL = 'systems/baseurl';
  _SYSTEM_WEBMASTER_EMAIL = 'systems/admin_email';
  _SYSTEM_MODULE_DEFAULT = 'systems/module_default';
  _SYSTEM_MODULE_VARIABLE = 'systems/module_variable';
  _SYSTEM_ERROR_URL = 'systems/error_url';
  _SYSTEM_ERROR_REDIRECT = 'systems/error_redirect';
  _SYSTEM_LANGUAGE_DEFAULT = 'systems/language_default';
  _SYSTEM_THEME_ENABLE = 'systems/theme_enable';
  _SYSTEM_THEME = 'systems/theme';
  _SYSTEM_DEBUG = 'systems/debug';
  _SYSTEM_DEBUGLEVEL = 'systems/debug_level';
  _SYSTEM_CACHE_TYPE = 'systems/cache';
  _SYSTEM_CACHE_TIME = 'systems/cache_time';
  _SYSTEM_CACHE_WRITE = 'systems/write';
  _SYSTEM_TEMP_DIR = 'systems/temp';
  _SYSTEM_SESSION_DIR = 'systems/session_dir';
  _SYSTEM_SESSION_TIMEOUT = 'systems/session_timeout';
  _SYSTEM_HIT_STORAGE = 'systems/hit_storage';

  _DATABASE_OPTIONS_READ = 'database/options/read';
  _DATABASE_OPTIONS_WRITE = 'database/options/write';

  _DATABASE_HOSTNAME = 'database/%s/hostname';
  _DATABASE_PORT = 'database/%s/port';
  _DATABASE_DRIVER = 'database/%s/driver';
  _DATABASE_TABLETYPE = 'database/%s/tabletype';
  _DATABASE_USERNAME = 'database/%s/username';
  _DATABASE_PASSWORD = 'database/%s/password';
  _DATABASE_DATABASENAME = 'database/%s/database_name';
  _DATABASE_TABLE_PREFIX = 'database/%s/prefix';
  _DATABASE_LIBRARY = 'database/%s/library';

  _MAIL_MAILSERVER = 'mailer/%s/hostname';
  _MAIL_USERNAME = 'mailer/%s/username';
  _MAIL_PASSWORD = 'mailer/%s/password';
  _MAIL_SMTPPORT = 'mailer/%s/smtp_port';
  _MAIL_SSL = 'mailer/%s/ssl';
  _MAIL_TLS = 'mailer/%s/tls';

  _WORDPRESS_PLUGINS_POLYLANG = 'wordpress/plugins/polylang';

  OK = 'OK';

  _ERR_DATABASE_LIBRARY_NOT_EXIST = 'Database Library "%s" not exist (%s).';
  _ERR_DATABASE_CANNOT_CONNECT = 'Cannot create database connection to "%s".';

type
  TStringArray = array of string;

  { TLocalJSONParser }

  TLocalJSONParser = class(TJSONParser)
  private
    FScanner: TJSONScanner;
  public
    property Scanner: TJSONScanner read FScanner;
  end;


function i2s(pI: integer): string;
function s2i(s: string): integer;
function f2s(n: extended): string;
function s2f(s: string): extended;
function b2s( b: boolean): string;
function s2b( s: string): boolean;
function Implode(lst: TStringList; sep: string = ';'; prefix: string = ''; suffix: string = ''): string;
function Explode(Str, Delimiter: string): TStrings;
function ExplodeTags(TagString: string): TStringList;
function isRegex( s:string): boolean;
function EchoError(const Fmt: string; const Args: array of const): string;
function _GetTickCount: DWord;

function SafeText(const SourceString: string): string;
function ReplaceAll(const Subject: string; const OldPatterns, NewPatterns: array of string;
  IgnoreCase: boolean = False): string;
function ReplaceAll(const Subject: string; const OldPatterns: array of string; NewPatterns: string;
  IgnoreCase: boolean = False): string;

function AppendPathDelim(const Path: string): string;
function DirectoryIsWritable(const DirectoryName: string): boolean;

procedure DumpJSON(J: TJSonData; DOEOLN: boolean = False);
function jsonGetString(J: TJsonData; index: string): string;
function JsonFormatter( JsonString:string):string;
function IsJsonValid( JsonString:string):boolean;
function HexToInt(HexStr: string): int64;

function RandomString(PLen: integer; PrefixString: string = ''): string;
function RandomString( MinLength, MaxLength: integer; LeadingCapital: boolean = true; UseUpper: boolean = true; UseLower: boolean = true; UseSpace: boolean = false;
  UseNumber:boolean = false; UseSpecial: boolean = false; UseSeed:boolean = false; DontUse: string = ''):string;
function EncodeQueryString( Data: array of string): string;

// php like function
procedure echo(const Message: string);
procedure echo(const Number: integer);
procedure echo(const Number: double);
procedure pr(const Message: variant);
procedure ta(const Message: variant; Width : integer = 800; Height : integer = 200);
procedure Die(const Message: string = ''); overload;
procedure Die(const Number: integer); overload;
procedure Die(const Message: TStringList); overload;

function mysql_real_escape_string(const unescaped_string: string): string;
function mysql_real_escape_string(const unescaped_strings: TStringList): string;
function CleanUrl( URL:string; Separator: string = '-'):string;
function UrlEncode(const DecodedStr: string; Pluses: boolean = True): string;
function UrlDecode(const EncodedStr: string): string;
function ucwords(const str: string): string;

function file_get_contents( TargetURL: string):string;

function preg_match( const RegexExpression:string; SourceString:string):boolean;
function preg_replace( const RegexExpression, ReplaceString, SourceString : string; UseSubstitution : boolean = True) : string;
// php like function - end

function isIPAddress( const IPAddress: string): boolean;
function FastInfo():string;

implementation

uses language_lib;

function i2s(pI: integer): string;
begin
  Result := '0';
  try
    Result := IntToStr(pI);
  except
  end;
end;

function s2i(s: string): integer;
begin
  try
    TryStrToInt(s, Result);
  except
    Result := 0;
  end;
end;

function f2s(n: extended): string;
begin
  Result := '0';
  try
    //Result := FloatToStr(n);
    //Result := FloatToStrF(n, ffCurrency, 8, 2);
    Result := Format( '%.2f', [n]);
  except
  end;
end;

function s2f(s: string): extended;
begin
  try
    Result := 0;
    TryStrToFloat(s, Result);
  except
  end;
end;

function b2s(b: boolean): string;
begin
  if b then
    Result := 'True'
  else
    Result := 'False';
end;

function s2b(s: string): boolean;
begin
  Result := StrToBool( s);
end;

function Implode(lst: TStringList; sep: string; prefix: string; suffix: string): string;
var
  i: integer;
  s: string;
begin
  i := 0;
  s := '';
  for i := 0 to lst.Count - 1 do
  begin
    s := s + sep + prefix + lst[i] + suffix;
  end;
  s := Copy(s, 2, Length(s) - 1);
  Result := s;
end;

function Explode(Str, Delimiter: string): TStrings;
var
  i: integer;
  p: string;
begin
  Result := TStringList.Create;
  while Pos(Delimiter, Str) <> 0 do
  begin
    p := '';
    for i := 1 to Pos(Delimiter, Str) - 1 do
      p := p + Str[i];
    Result.Add(p);
    //Delete(s,1,Pos(Delimiter,Str));
    Delete(Str, 1, Pos(Delimiter, Str) + Length(Delimiter) - 1);
  end;
  //result.Add(s);
  if (Length(Str) <> 0) then
    Result.Add(Str);
end;

function ExplodeTags(TagString: string): TStringList;
var
  lst: TStringList;
  i, j: integer;
  s, p: string;
begin
  lst := TStringList.Create;
  while Pos(' ', TagString) <> 0 do
  begin
    p := '';
    for i := 1 to Pos(' ', TagString) - 1 do
      p := p + TagString[i];
    lst.Add(p);
    //Delete(s,1,Pos(Delimiter,Str));
    Delete(TagString, 1, Pos(' ', TagString) + Length(' ') - 1);
  end;
  //result.Add(s);
  if (Length(TagString) <> 0) then
    lst.Add(TagString);

  for i := lst.Count - 1 downto 1 do
  begin
    if pos('=', lst[i]) = 0 then
    begin
      lst[i - 1] := lst[i - 1] + ' ' + lst[i];
      lst.Delete(i);
    end
    else
    begin
      s := lst[i];
      j := Pos('=', s);
      if (s[j + 1] = '"') and (s[Length(s)] = '"') then
      begin
        lst[i] := Copy(s, 1, j) + Copy(s, j + 2, Length(s) - j - 2);
      end;
    end;
  end;

  //-- check [.] delimiter
  if Pos('.', lst[0]) <> 0 then
  begin
    lst.Insert(0, Copy(lst[0], 1, Pos('.', lst[0]) - 1));
    lst[1] := 'index=' + Copy(lst[1], Pos('.', lst[1]) + 1, Length(lst[1]) - Pos('.', lst[1]));
  end;

  Result := lst;
end;

// maybe is regex ?
function isRegex(s: string): boolean;
begin
  Result := False;
  if Pos( '?', s) <> 0 then Result := True;
  if Pos( '^', s) <> 0 then Result := True;
  if Pos( '$', s) <> 0 then Result := True;
end;

function EchoError(const Fmt: string; const Args: array of const): string;
begin
  Result := '<div class="warning">' + Format(Fmt, Args) + '</div>';
end;


function _GetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;

function SafeText(const SourceString: string): string;
const
  NotAllowed: array[1..24] of string =
    (' ', ';', '/', '?', ':', '@', '=', '&', '#', '+', '_',
    '<', '>', '"', '%', '{', '}', '|', '\', '^', '~', '[', ']', '`'
    );
var
  s: string;
begin
  s := ReplaceAll(SourceString, NotAllowed, '-');
  Result := s;
end;

function ReplaceAll(const Subject: string; const OldPatterns, NewPatterns: array of string;
  IgnoreCase: boolean): string;
var
  ReplaceFlags: TReplaceFlags;
  NewPattern: string;
  I: integer;
begin
  ReplaceFlags := [rfReplaceAll];
  if IgnoreCase then
    Include(ReplaceFlags, rfIgnoreCase);
  Result := Subject;
  for I := Low(OldPatterns) to High(OldPatterns) do
  begin
    if I <= High(NewPatterns) then
      NewPattern := NewPatterns[I]
    else
      NewPattern := '';
    Result := StringReplace(Result, OldPatterns[I], NewPattern, ReplaceFlags);
  end;
end;

function ReplaceAll(const Subject: string; const OldPatterns: array of string; NewPatterns: string;
  IgnoreCase: boolean): string;
var
  ReplaceFlags: TReplaceFlags;
  I: integer;
begin
  ReplaceFlags := [rfReplaceAll];
  if IgnoreCase then
    Include(ReplaceFlags, rfIgnoreCase);
  Result := Subject;
  for I := Low(OldPatterns) to High(OldPatterns) do
  begin
    Result := StringReplace(Result, OldPatterns[I], NewPatterns, ReplaceFlags);
  end;
end;

procedure echo(const Message: string);
begin
  Application.Response.Contents.Text :=
    trim(Application.Response.Contents.Text) + Message;
end;

procedure echo(const Number: integer);
begin
  echo(i2s(Number));
end;

procedure echo(const Number: double);
begin
  echo(FloatToStr(Number));
end;

procedure pr(const Message: variant);
begin
  echo(#13'<pre>');
  echo(#13'');
  echo(string(Message));
  echo(#13'</pre>');
end;

procedure ta(const Message: variant; Width: integer; Height: integer);
begin
  if Width = 0 then Width := 800;
  echo( #13'<textarea style="width: '+i2s(Width)+'px !important; height: '+i2s(Height)+'px !important;">');
  echo( #13'');
  echo( string(Message));
  echo( #13'</textarea>');
end;

procedure Die(const Number: integer);
begin
  Die(i2s(Number));
end;

procedure Die(const Message: TStringList);
begin
  Die('<pre>' + Message.Text + '</pre>');
end;

function RandomString(PLen: integer; PrefixString: string): string;
var
  str: string;
begin
  Randomize;
  //string with all possible chars
  str := PrefixString + 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until (Length(Result) = PLen);
end;

function RandomString(MinLength, MaxLength: integer; LeadingCapital: boolean;
  UseUpper: boolean; UseLower: boolean; UseSpace: boolean; UseNumber: boolean;
  UseSpecial: boolean; UseSeed: boolean; DontUse: string): string;
const
  c_upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  c_lower = 'abcdefghijklmnopqrstuvwxyz';
  c_number = '0123456789';
  c_special = '~@#$%^*()_+-={}|][';
var
  rnd, chars : string;
  i, len, clen : integer;
begin
  chars:= '';
  if UseLower then
    chars:= chars + c_lower;
  if UseUpper then
    chars:= chars + c_upper;
  if UseNumber then
    chars:= chars + c_number;
  if UseSpecial then
    chars:= chars + c_special;
  if UseSpace then
  begin
    for i:=0 to (Length( chars) mod 10) do
      chars := chars + ' ';
  end;
  if DontUse <> '' then
  begin
    // .. next ...
  end;

  Randomize;
  len := RandomRange( MinLength, MaxLength);
  clen := length( chars);
  rnd := '';
  try
    for i:=1 to len do
    begin
      rnd := rnd + chars[ RandomRange( 1, clen)];
    end;
  except
  end;

  if LeadingCapital then
    rnd[1] := upCase( rnd[1]);
  Result := rnd;
end;

function EncodeQueryString(Data: array of string): string;
var
  s: string;
  i: integer;
begin
  s := '';
  if high(Data) >= 0 then
  begin
    for i := low(Data) to high(Data) do
    begin
      if s = '' then
        s := Data[i]
      else
      begin
        s := s + '&' + Data[i];
      end;
    end;
  end;
  Result := s;
end;

procedure Die(const Message: string);
begin
  //raise EFPWebError.CreateFmt( '%s %s', [Application.Response.Contents.Text, Message]);
  //raise EFPWebError.Create(Message);
  Application.Response.Contents.Add(Message);
  Application.Response.SendContent;
  Application.Terminate;
end;

function AppendPathDelim(const Path: string): string;
begin
  if (Path <> '') and not (Path[length(Path)] in AllowDirectorySeparators) then
    Result := Path + PathDelim
  else
    Result := Path;
end;

function DirectoryIsWritable(const DirectoryName: string): boolean;
var
  TempFilename: string;
  s: string;
  fHandle: THANDLE;
begin
  Result := False;
  TempFilename := SysUtils.GetTempFilename(AppendPathDelim(DirectoryName), 'ztstperm');
  fHandle := FileCreate(TempFilename);
  if (THandle(fHandle) <> feInvalidHandle) then
  begin
    s := 'WriteTest';
    if FileWrite(fHandle, S[1], Length(S)) > 0 then
      Result := True;
    FileClose(fHandle);
    DeleteFile(TempFilename);
  end;
end;

procedure DumpJSON(J: TJSonData; DOEOLN: boolean);
var
  I: integer;
begin
  // JSONType property determines kind of value.
  case J.jsontype of
    jtNull: echo('Null');
    jtBoolean: if J.AsBoolean then
        echo('True')
      else
        echo('False');
    jtNumber: {JSONNumber has extra NumberType property
                which determines kind of value (int/float).}
      case TJSONNumber(J).NumberType of
        ntInteger: echo(J.AsInteger);
        ntFloat: echo(J.AsFloat); //Write(J.AsFloat:10:2);
      end;
    jtString: echo('"' + J.AsString + '"');
    jtArray:
    begin
      echo('[ ');
      for I := 0 to J.Count - 1 do
      begin
        DumpJSON(J.Items[I], False);
        if I < J.Count - 1 then
          echo(', ');
      end;
      echo(' ]');
    end;
    jtObject:
    begin
      echo('{ ');
      for I := 0 to J.Count - 1 do
      begin
        echo('"' + TJSONObject(J).Names[i] + '" : '#13#10);
        DumpJSON(J.Items[I], False);
        if I < J.Count - 1 then
          echo(',');
      end;
      echo('}');
    end;
  end;
  if DOEOLN then
    echo(#13#10);
end;

function jsonGetString(J: TJsonData; index: string): string;
var
  i: integer;
begin
  Result := '';
  try
    i := TJSONObject(J).IndexOfName(index);
    if i <> -1 then
      Result := J.Items[i].AsString;
  except
  end;
end;

function JsonFormatter(JsonString: string): string;
// error line : VJSONParser.Scanner.CurRow;
var
  VJSONData: TJSONData = nil;
  VJSONParser: TLocalJSONParser;
begin
  Result := '';
  JsonString := trim( JsonString);
  if JsonString = '' then
    Exit;

  VJSONParser := TLocalJSONParser.Create( JsonString);
  try
    try
      VJSONParser.Strict := True;
      VJSONData := VJSONParser.Parse;
      Result := VJSONData.FormatJSON([], 2);;
      VJSONData.Free;
    except
      on E : Exception do
      begin
      end;
    end;
  finally
    VJSONParser.Free;
  end;

end;

function IsJsonValid(JsonString: string): boolean;
begin
  if JsonFormatter( JsonString) = '' then
    Result := False
  else
    Result := True;
end;

function HexToInt(HexStr: string): int64;
var
  RetVar: int64;
  i: byte;
begin
  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then
    Delete(HexStr, length(HexStr), 1);
  RetVar := 0;

  for i := 1 to length(HexStr) do
  begin
    RetVar := RetVar shl 4;
    if HexStr[i] in ['0'..'9'] then
      RetVar := RetVar + (byte(HexStr[i]) - 48)
    else
    if HexStr[i] in ['A'..'F'] then
      RetVar := RetVar + (byte(HexStr[i]) - 55)
    else
    begin
      Retvar := 0;
      break;
    end;
  end;

  Result := RetVar;
end;

function StringReplaceExt(const S: string; OldPattern, NewPattern: array of string; Flags: TReplaceFlags): string;
var
  i: integer;
begin
  Assert(Length(OldPattern) = (Length(NewPattern)));
  Result := S;
  for  i := Low(OldPattern) to High(OldPattern) do
    Result := StringReplace(Result, OldPattern[i], NewPattern[i], Flags);
end;

function mysql_real_escape_string(const unescaped_string: string): string;
begin
  Result := StringReplaceExt(unescaped_string, ['\', #39, #34, #0, #10, #13, #26],
    ['\\', '\'#39, '\'#34, '\0', '\n', '\r', '\Z'], [rfReplaceAll]);
end;

function mysql_real_escape_string(const unescaped_strings: TStringList): string;
var
  i: integer;
  lst: TStringList;
begin
  lst := TStringList.Create;
  for i := 0 to unescaped_strings.Count - 1 do
  begin
    lst.Add(mysql_real_escape_string(unescaped_strings[i]));
  end;
  Result := lst.Text;
  FreeAndNil(lst);
end;

function CleanUrl(URL: string; Separator: string): string;
begin
  Result := LowerCase(Trim(URL));
  Result := ReplaceAll(Result, [' ', ',', '?', '!', '.', '''', '+', '^',
    '"', #13, #10, '/', '\', '(', ')', '[', ']', '*', '$', '!'], Separator);
  Result := ReplaceAll(Result, ['---', '--'], '-');
end;

function UrlEncode(const DecodedStr: string; Pluses: boolean): string;
var
  I: integer;
begin
  Result := '';
  if Length(DecodedStr) > 0 then
    for I := 1 to Length(DecodedStr) do
    begin
      if not (DecodedStr[I] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ']) then
        Result := Result + '%' + IntToHex(Ord(DecodedStr[I]), 2)
      else if not (DecodedStr[I] = ' ') then
        Result := Result + DecodedStr[I]
      else
      begin
        if not Pluses then
          Result := Result + '%20'
        else
          Result := Result + '+';
      end;
    end;
end;

function UrlDecode(const EncodedStr: string): string;
var
  I: integer;
begin
  Result := '';
  if Length(EncodedStr) > 0 then
  begin
    I := 1;
    while I <= Length(EncodedStr) do
    begin
      if EncodedStr[I] = '%' then
      begin
        Result := Result + Chr(HexToInt(EncodedStr[I + 1] + EncodedStr[I + 2]));
        I := Succ(Succ(I));
      end
      else if EncodedStr[I] = '+' then
        Result := Result + ' '
      else
        Result := Result + EncodedStr[I];

      I := Succ(I);
    end;
  end;
end;

function ucwords(const str: string): string;
var
  i: integer;
  s: string;
begin
  s := ' ' + lowerCase(str);
  for i := 1 to Length(s) do
  begin
    if s[i] = ' ' then
      s[i + 1] := upcase(s[i + 1]);
  end;
  Result := trim(s);
end;

function file_get_contents(TargetURL: string): string;
var
  s : string;
begin
  Result := '';
  With TFPHTTPClient.Create(Nil) do
  begin
    try
      s := Get( TargetURL);
      Result := s;
    except
      on e : Exception do
      begin
        Result := e.Message;
      end;
    end;

    Free;
  end;
end;

function preg_match(const RegexExpression: string; SourceString: string
  ): boolean;
begin
  Result := False;
  try
    with TRegExpr.Create do
    begin
      Expression := RegexExpression;
      Result := Exec( SourceString);
      Free;
    end;
  except
  end;
end;

function preg_replace(const RegexExpression, ReplaceString, SourceString: string; UseSubstitution: boolean): string;
begin
  try
    with TRegExpr.Create do
    begin
      Expression := RegexExpression;
      Result := Replace( SourceString, ReplaceString, UseSubstitution);
      Free;
    end;
  except
    Result := SourceString;
  end;
end;

function isIPAddress(const IPAddress: string): boolean;
begin
  result:= execregexpr('[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}', IPAddress);
end;

function FastInfo: string;
var
  lst: TStringList;
  s : string;
begin
  lst := TStringList.Create;
  Application.GetEnvironmentList(lst);

  s := '<pre><b>Your Server Info:</b><br>';
//  s := #13#0'Target ' + {$i %FPCTARGET%};
//  s := s + #13#10'TargetCPU ' + {$i %FPCTARGETCPU%};
//  s := s + #13#10'Target OS ' + {$i %FPCTARGETOS%};
  s := s + #13#10'FPC Version ' + {$i %FPCVERSION%};
  s := s + #13#10'Build Date ' + {$i %DATE%};
  s := s + #13#10#13;

  lst.Text:= s + lst.Text;
  result := lst.Text;
  lst.Free;
end;

initialization
  LANG := 'en'; //GetLanguageIDs( LANG, FallbackLANG);
  AppData.debug := True;
  Config := TMyConfig.Create(nil);
  Config.ValidateFile( 'config/config.json');


finalization
  FreeAndNil(Config);

end.

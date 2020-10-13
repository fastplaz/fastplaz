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
  //netdb,
  resolve,
  zipper, strutils, dateutils, base64,
  Classes, SysUtils, fastplaz_handler, config_lib, array_helpers;

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
  _SYSTEM_SESSION_STORAGE = 'systems/session_storage';
  _SYSTEM_SESSION_TIMEOUT = 'systems/session_timeout';
  _SYSTEM_SESSION_AUTOSTART = 'systems/session_autostart';
  _SYSTEM_HIT_STORAGE = 'systems/hit_storage';
  _SYSTEM_COOKIE_PATH = 'systems/cookie_path';

  _DATABASE_OPTIONS_READ = 'database/options/read';
  _DATABASE_OPTIONS_WRITE = 'database/options/write';

  _DATABASE_HOSTNAME = 'database/%s/hostname';
  _DATABASE_PORT = 'database/%s/port';
  _DATABASE_DRIVER = 'database/%s/driver';
  _DATABASE_TABLETYPE = 'database/%s/tabletype';
  _DATABASE_USERNAME = 'database/%s/username';
  _DATABASE_PASSWORD = 'database/%s/password';
  _DATABASE_CHARSET = 'database/%s/charset';
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
  FAILED = 'FAILED';

  _ERR_DATABASE_LIBRARY_NOT_EXIST = 'Database Library "%s" not exist (%s).';
  _ERR_DATABASE_CANNOT_CONNECT = 'Cannot create database connection to "%s".';

  _CACHE_PATH = 'ztemp' + DirectorySeparator + 'cache' + DirectorySeparator;

  __URL_SEARCH = '(http|ftp|https):\/\/[\w-]+(\.[\w-]+)+([\w.,@?^=%&amp;:\/~+#-]*[\w@?^=%&amp;\/~+#-])?';
  __NORMAL_SENTENCES = '0-9a-zA-z\ \r\n\-\+*&@$#!?%''".,:;=()';
  __NORMAL_SENTENCES_WITH_SLASH = '0-9a-zA-z\ \/\r\n\-\+*&@$#!?%\[\{\}''".,:;=()<>';

type
  CharSet=Set of Char;
  //TStringArray = array of string;

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
function b2i(b: boolean): integer;
function b2is(b: boolean): string;
function b2s(b: boolean): string;
function s2b(s: string): boolean;
function StrInArray(const AValue : String;const AArrayOfString : Array of String) : Boolean;
function StreamToString(AStream: TStream): string;
function StripNonAscii(const s: string): string;
function StripCharsInSet(s:string; c:CharSet):string;
function strstr(AText, ADelimiter: String; IsBeforeNeedle: Boolean = False): String;
function StringCut(AStartString, AStopString: string; AText: string): string;
function StringHumanToNominal( StrHuman: string):string;
function StringHumanToFloat( StrHuman: string):double;
function StringHumanToDate( AStringHuman: string):TDateTime;
function StringsExists( ASubstring, AFullString:string):boolean;
function WordExists( ASubstring, AFullString:string):boolean;
function Implode(lst: TStringList; sep: string = ';'; prefix: string = '';
  suffix: string = ''): string;
function Explode(Str, Delimiter: string): TStrings;
function ExplodeTags(TagString: string): TStringList;
function isEmpty(AString: string): boolean;
function isRegex(s: string): boolean;
function EchoError(const Fmt: string; const Args: array of const): string;
function _GetTickCount: DWord;
function DateTimeToISO8601( ADateTime:TDateTime): string;
function ISO8601ToDateTime( AString:string; AOffsetHours:integer = 7): TDateTime;

function SafeText(const SourceString: string; const AReplacementString: string = '-'): string;
function ReplaceAll(const Subject: string;
  const OldPatterns, NewPatterns: array of string; IgnoreCase: boolean = False): string;
function ReplaceAll(const Subject: string; const OldPatterns: array of string;
  NewPatterns: string; IgnoreCase: boolean = False): string;

function StripNumber(const AString:string): string;
function StripNonNumber(const AString:string): string;

function AppendPathDelim(const Path: string): string;
function DirectoryIsWritable(const DirectoryName: string): boolean;
function ScanFolder(const APath: String; AWildCard:string = '*'):TStringList;
function ZipFolder(APath: string; ATarget: string): boolean;
function DownloadFile(const AURL: String;  const AFilePath: String; AUserAgent: String = ''):boolean;
function LoadFromFile(const AFileName: string): string;
function SaveToFile(const AFileName: string; const AContent: string): boolean;

procedure DumpJSON(J: TJSonData; DOEOLN: boolean = False);
function jsonGetData(AJsonData: TJsonData; APath: string): string;
function jsonGetString(J: TJsonData; index: string): string;
function JsonFormatter(JsonString: string; Const UseUTF8 : Boolean = True): string;
function IsJsonValid(JsonString: string): boolean;
function HexToInt(HexStr: string): int64;

function WordNumber( s:string): integer;
function isWord( s:string): boolean;
function RandomString(PLen: integer; PrefixString: string = ''): string;
function RandomString(MinLength, MaxLength: integer; LeadingCapital: boolean = True;
  UseUpper: boolean = True; UseLower: boolean = True; UseSpace: boolean = False;
  UseNumber: boolean = False; UseSpecial: boolean = False;
  UseSeed: boolean = False; DontUse: string = ''): string;
function EncodeQueryString(Data: array of string): string;
function StripSlash(Const DataString: UnicodeString): UnicodeString;
function StripHTML(AHTML: UnicodeString): UnicodeString;
function StripTags(AHTML: UnicodeString): UnicodeString;
function LoadCache( AName:String; AMod:String = 'general'): String;
function SaveCache( AName, AContent:String; AMod:String = 'general'): Boolean;

// php like function
procedure echo(const Message: string);
procedure echo(const Number: integer);
procedure echo(const Number: double);
procedure pr(const Message: variant);
procedure ta(const Message: variant; Width: integer = 800; Height: integer = 200);
procedure Die(const Message: string = ''; ACode: integer = 200); overload;
procedure Die(const Number: integer; ACode: integer = 200); overload;
procedure Die(const Message: TStringList; ACode: integer = 200); overload;
procedure OutputJson(const ACode: integer; AMessage: string; AForceCode: Integer = 0);

function mysql_real_escape_string(const unescaped_string: string): string;
function mysql_real_escape_string(const unescaped_strings: TStringList): string;
function isURL( const AURL: string): boolean;
function isLookLikeURL( const AURL: string): boolean;
function CleanUrl(URL: string; Separator: string = '-'): string;
function UrlEncode(const DecodedStr: string; Pluses: boolean = True): string;
function UrlDecode(const EncodedStr: string): string;
function base64_encode(const AStr: string): string;
function base64_decode(const AStr: string): string;
function ucwords(const str: string): string;
function HTMLDecode(const AStr: String): String;
function FormatTextLikeForum(const AContent: String):String;
function MarkdownToHTML(const AContent: String): String;

function file_get_contents(TargetURL: string; AShowErrorMessageAsResult: boolean = true): string;
function FileCopy(ASource, ATarget: string): boolean;

function preg_match(const RegexExpression: string; SourceString: string): boolean;
function preg_replace(const RegexExpression, ReplaceString, SourceString: string;
  UseSubstitution: boolean = True): string;
// php like function - end

function isIPAddress(const IPAddress: string): boolean;
function isEmail(const s: string): boolean;
function isDomain(const s: string): boolean;
function GetHostNameIP( HostName: string): string;
function GetUserIpAddress: string;

function Exec(const AExeName: string; const AParameter: array of string;
  var AOutput: string; AShowOptons: TShowWindowOptions): boolean;
function FastInfo(): string;

implementation

uses
  logutil_lib, language_lib;

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
    Result := Format('%.2f', [n]);
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

function b2i(b: boolean): integer;
begin
  if b then
    Result := 1
  else
    Result := 0;
end;

function b2is(b: boolean): string;
begin
  if b then
    Result := '1'
  else
    Result := '0';
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
  Result := False;
  try
    //Result := StrToBool(s);
    TryStrToBool(s, Result)
  except
  end;
  if s = 'on' then
    Result := True;
  if s = 'required' then
    Result := True;
end;

function StrInArray(const AValue: String; const AArrayOfString: array of String
  ): Boolean;
var
 Loop : String;
begin
  for Loop in AArrayOfString do
  begin
    if AValue = Loop then
    begin
       Exit(true);
    end;
  end;
  result := false;
end;

function StreamToString(AStream: TStream): string;
var
  SS: TStringStream;
begin
  if AStream <> nil then
  begin
    SS := TStringStream.Create('');
    try
      SS.CopyFrom(AStream, 0);  // No need to position at 0 nor provide size
      Result := SS.DataString;
    finally
      SS.Free;
    end;
  end
  else
  begin
    Result := '';
  end;
end;

function StripNonAscii(const s: string): string;
var
  i, Count: Integer;
begin
  SetLength(Result, Length(s));
  Count := 0;
  for i := 1 to Length(s) do
  begin
    if ((s[i] >= #32) and (s[i] <= #127)) or (s[i] in [#10, #13]) then
    begin
      inc(Count);
      Result[Count] := s[i];
    end;
  end;
  SetLength(Result, Count);
end;

// usage:
//  s := StripCharsInSet(s,[#0..#9,#11,#12,#14..#31,#127]);
function StripCharsInSet(s: string; c: CharSet): string;
var i,j:Integer;
begin
  SetLength(result,Length(s));
  j:=0;
  for i:=1 to Length(s) do
    if not (s[i] in c) then
    begin
      inc(j);
      result[j]:=s[i];
    end;
  SetLength(result,j);
end;

function strstr(AText, ADelimiter: String; IsBeforeNeedle: Boolean): String;
var
  i: Integer;
begin
  Result := '';
  i := Pos(ADelimiter, AText);
  if i = 0 then
    Exit;
  if IsBeforeNeedle then
    i := i - 1;
  Result := Copy(AText,1, i);
end;

function StringCut(AStartString, AStopString: string; AText: string): string;
var
  i: integer;
begin
  Result := '';
  i := pos(AStartString, AText);
  if i = 0 then
    Exit;
  Result := copy(AText, i + Length(AStartString));
  Result := Copy(Result, 0, pos(AStopString, Result) - 1);
end;

function StringHumanToNominal(StrHuman: string): string;
begin
  Result := StrHuman;
  Result := ReplaceAll( Result,
    ['satu', 'dua', 'tiga', 'empat', 'lima', 'enam', 'enem', 'tujuh', 'delapan', 'lapan', 'sembilan'],
    ['1', '2', '3', '4', '5', '6', '6', '7', '8', '8', '9'], False);
  Result := StringReplace( Result, 'puluh', '0', [rfReplaceAll]);
  Result := StringReplace( Result, 'ratus', '00', [rfReplaceAll]);
  Result := StringReplace( Result, 'rb', '000', [rfReplaceAll]);
  Result := StringReplace( Result, 'ribu', '000', [rfReplaceAll]);
  Result := StringReplace( Result, 'jt', '000000', [rfReplaceAll]);
  Result := StringReplace( Result, 'juta', '000000', [rfReplaceAll]);
  Result := StringReplace( Result, 'milyar', '000000000', [rfReplaceAll]);
  Result := StringReplace( Result, 'miliar', '000000000', [rfReplaceAll]);
end;

function StringHumanToFloat(StrHuman: string): double;
begin
  Result := s2f( StringHumanToNominal(StrHuman));
end;

// [x] example
//   TheDate := StringHumanToDate('17 agustus 15');
function StringHumanToDate(AStringHuman: string): TDateTime;
var
  i: integer;
  FS: TFormatSettings;

  function LongToShortMonth( AStr:string):string;
  var
    i: integer;
  begin

    Result := AStr;
    for i := 1 to 12 do
    begin
      Result := StringReplace( Result, FS.LongMonthNames[i], FS.ShortMonthNames[i], [rfReplaceAll]);
    end;

    Result := StringReplace( Result, 'januari', 'jan', [rfReplaceAll]);
    Result := StringReplace( Result, 'februari', 'feb', [rfReplaceAll]);
    Result := StringReplace( Result, 'maret', 'mar', [rfReplaceAll]);
    Result := StringReplace( Result, 'april', 'apr', [rfReplaceAll]);
    Result := StringReplace( Result, 'mei', 'may', [rfReplaceAll]);
    Result := StringReplace( Result, 'juni', 'jun', [rfReplaceAll]);
    Result := StringReplace( Result, 'juli', 'jul', [rfReplaceAll]);
    Result := StringReplace( Result, 'agustus', 'aug', [rfReplaceAll]);
    Result := StringReplace( Result, 'september', 'sep', [rfReplaceAll]);
    Result := StringReplace( Result, 'oktober', 'oct', [rfReplaceAll]);
    Result := StringReplace( Result, 'november', 'nov', [rfReplaceAll]);
    Result := StringReplace( Result, 'desember', 'dec', [rfReplaceAll]);
  end;

begin
  Result := Today;
  FS := DefaultFormatSettings;
  FS.DateSeparator := ' ';
  FS.ShortDateFormat := 'dd mmm yyyy';

  if WordNumber(AStringHuman) = 1 then
  begin
    AStringHuman := AStringHuman + ' ' + FormatDateTime('mmm yyyy',Now);
  end;
  if WordNumber(AStringHuman) = 2 then
    AStringHuman := AStringHuman + ' ' + FormatDateTime('yyyy',Now);
  if WordNumber(AStringHuman) > 2 then
  begin
    i := LastDelimiter( ' ', AStringHuman);
    if ( Length(AStringHuman) - i) = 2 then
    begin
      AStringHuman := Copy(AStringHuman,0,i) + '20' + Copy(AStringHuman,i+1);
    end;
    AStringHuman := LongToShortMonth( AStringHuman);
    try
      Result := ScanDateTime('dd mmm yyyy', AStringHuman, FS);
    except
      Result := Today;
    end;
  end;

end;

function StringsExists(ASubstring, AFullString: string): boolean;
begin
  ASubstring := StringReplace( ASubstring, ',', '|', [rfReplaceAll]);
  Result := preg_match( '('+ASubstring+')', AFullString);
end;

function WordExists(ASubstring, AFullString: string): boolean;
begin
  Result := StringsExists( ASubstring, AFullString);
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
    lst[1] := 'index=' + Copy(lst[1], Pos('.', lst[1]) + 1, Length(lst[1]) -
      Pos('.', lst[1]));
  end;

  Result := lst;
end;

function isEmpty(AString: string): boolean;
begin
  Result := False;
  if AString = '' then
    Result := True;
end;

// maybe is regex ?
function isRegex(s: string): boolean;
begin
  Result := False;
  if Pos('?', s) <> 0 then
    Result := True;
  if Pos('^', s) <> 0 then
    Result := True;
  if Pos('$', s) <> 0 then
    Result := True;
end;

function EchoError(const Fmt: string; const Args: array of const): string;
begin
  Result := '<div class="warning">' + Format(Fmt, Args) + '</div>';
end;


function _GetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;

function DateTimeToISO8601(ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-mm-dd"T"HH:nn:ss'+'+0700', ADateTime);;
end;

function ISO8601ToDateTime(AString: string; AOffsetHours: integer): TDateTime;
var
  tmpFormatSettings: TFormatSettings;
begin
  Result := now;

  AString := copy( AString,0,10) + ' ' + copy(AString,12,8);
  tmpFormatSettings := FormatSettings;
  tmpFormatSettings.DateSeparator := '-';
  tmpFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  try
    Result := StrToDateTime( AString, tmpFormatSettings);
    Result := IncHour(Result, AOffsetHours); // GMT+7
  except
  end;
end;

function SafeText(const SourceString: string; const AReplacementString: string
  ): string;
const
  NotAllowed: array[1..25] of string =
    (' ', ';', '/', '?', ':', '@', '=', '&', '#', '+', '_',
    '<', '>', '"', '%', '{', '}', '|', '\', '^', '~', '[', ']', '`', ''''
    );
begin
  Result := ReplaceAll(SourceString, NotAllowed, AReplacementString);
end;

function ReplaceAll(const Subject: string;
  const OldPatterns, NewPatterns: array of string; IgnoreCase: boolean): string;
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

function ReplaceAll(const Subject: string; const OldPatterns: array of string;
  NewPatterns: string; IgnoreCase: boolean): string;
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
  if Width = 0 then
    Width := 800;
  echo(#13'<textarea style="width: ' + i2s(Width) + 'px !important; height: ' +
    i2s(Height) + 'px !important;">');
  echo(#13'');
  echo(string(Message));
  echo(#13'</textarea>');
end;

procedure Die(const Number: integer; ACode: integer);
begin
  Die(i2s(Number), ACode);
end;

procedure Die(const Message: TStringList; ACode: integer);
begin
  Die('<pre>' + Message.Text + '</pre>', ACode);
end;

procedure OutputJson(const ACode: integer; AMessage: string; AForceCode: Integer
  );
var
  s: string;
begin
  Application.Response.ContentType := 'application/json';
  Application.Response.Content := '';
  s:= '{"code":'+i2s(ACode)+',"msg":"'+AMessage+'"}';
  if AForceCode = 0 then
    die(s, 200)
  else
    die(s, AForceCode);
end;

function WordNumber(s: string): integer;
var
  lst : TStrings;
begin
  lst := Explode( s, ' ');
  Result := lst.Count;
  lst.Free;
end;

function isWord(s: string): boolean;
begin
  Result := False;
  if WordNumber( s) = 1 then
    Result := True;
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
  rnd, chars: string;
  i, len, clen: integer;
begin
  chars := '';
  if UseLower then
    chars := chars + c_lower;
  if UseUpper then
    chars := chars + c_upper;
  if UseNumber then
    chars := chars + c_number;
  if UseSpecial then
    chars := chars + c_special;
  if UseSpace then
  begin
    for i := 0 to (Length(chars) mod 10) do
      chars := chars + ' ';
  end;
  if DontUse <> '' then
  begin
    // .. next ...
  end;

  Randomize;
  len := RandomRange(MinLength, MaxLength);
  clen := length(chars);
  rnd := '';
  try
    for i := 1 to len do
    begin
      rnd := rnd + chars[RandomRange(1, clen)];
    end;
  except
  end;

  if LeadingCapital then
    rnd[1] := upCase(rnd[1]);
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

function StripSlash(const DataString: UnicodeString): UnicodeString;
var
  L : Integer;
begin
  L:=Length(DataString);
  If (L>0) and (DataString[l]='/') then
    Result:=Copy(DataString,1,L-1)
  else
    Result:=DataString;
end;

// http://www.festra.com/eng/snip12.htm
function StripHTML(AHTML: UnicodeString): UnicodeString;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  TagBegin := Pos('<', AHTML);      // search position of first <

  while (TagBegin > 0) do
  begin  // while there is a < in S
    TagEnd := Pos('>', AHTML);              // find the matching >
    TagLength := TagEnd - TagBegin + 1;
    Delete(AHTML, TagBegin, TagLength);     // delete the tag
    TagBegin := Pos('<', AHTML);            // search for next <
  end;

  Result := AHTML;
end;

function StripTags(AHTML: UnicodeString): UnicodeString;
var
  Len: Integer;

  function ReadUntil(const ReadFrom: Integer; const C: Char): Integer;
  var
    j: Integer;
  begin
    for j := ReadFrom to Len do
      if (AHTML[j] = C) then
      begin
        Result := j;
        Exit;
      end;
    Result := Len+1;
  end;

var
  i, APos: Integer;
begin
  Len := Length(AHTML);
  i := 0;
  Result := '';
  while (i <= Len) do
  begin
    Inc(i);
    APos := ReadUntil(i, '<');
    Result := Result + Copy(AHTML, i, APos-i);
    i := ReadUntil(APos+1, '>');
  end;
end;

function LoadCache(AName: String; AMod: String): String;
var
  i: Integer;
  lst: TStringList;
begin
  Result := '';
  AName := AName.Replace('https://','');
  AName := AName.Replace('http://','');
  AName := SafeText( AName);
  AName := _CACHE_PATH + AMod + DirectorySeparator + AName + '.txt';
  if not FileExists( AName) then
    Exit;
  i := HoursBetween(FileDateToDateTime(FileAge(AName)), now);
  if i > 0 then
    Exit;

  lst := TStringList.Create;
  lst.LoadFromFile( AName);
  Result := lst.Text;

  lst.Free;
end;

function SaveCache(AName, AContent: String; AMod: String): Boolean;
var
  lst: TStringList;
begin
  Result := False;
  AName := AName.Replace('https://','');
  AName := AName.Replace('http://','');
  AName := SafeText( AName);
  AName := _CACHE_PATH + AMod + DirectorySeparator + AName + '.txt';
  lst := TStringList.Create;
  lst.Text := AContent;
  try
    lst.SaveToFile(AName);
    Result := True;
  except
  end;
  lst.Free;
end;


procedure Die(const Message: string; ACode: integer);
begin
  //raise EFPWebError.CreateFmt( '%s %s', [Application.Response.Contents.Text, Message]);
  //raise EFPWebError.Create(Message);
  StopTime:= _GetTickCount;
  ElapsedTime:= StopTime - StartTime;

  Application.Response.SetCustomHeader( 'TimeUsage', i2s( ElapsedTime));;
  Application.Response.Contents.Add(Message);
  Application.Response.Code := ACode;
  Application.Response.SendContent;
  Application.Terminate;
end;

function StripNumber(const AString: string): string;
const
  CHARS = ['0'..'9'];
var
  i : integer;
begin
  Result:='';
  for i:=0 to length(AString) do
    if not (AString[i] in CHARS) then
  Result := Result + AString[i];
end;

function StripNonNumber(const AString: string): string;
const
  CHARS = ['0'..'9'];
var
  i : integer;
begin
  Result:='';
  for i:=0 to length(AString) do
    if (AString[i] in CHARS) then
  Result := Result + AString[i];
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

function ScanFolder(const APath: String; AWildCard: string): TStringList;
var
  sPath: string;
  rec : TSearchRec;
  tmpLst: TStringList;
begin
  Result := TStringList.Create;
  sPath := IncludeTrailingPathDelimiter(APath);
  if FindFirst(sPath + AWildCard, faAnyFile, rec) = 0 then
  begin
    repeat
      // TSearchRec.Attr contain basic attributes (directory, hidden,
      // system, etc). TSearchRec only supports a subset of all possible
      // info. TSearchRec.FindData contains everything that the OS
      // reported about the item during the search, if you need to
      // access more detailed information...

      if (rec.Attr and faDirectory) <> 0 then
      begin
        // item is a directory
        if (rec.Name <> '.') and (rec.Name <> '..') then
        begin
          tmpLst := ScanFolder(sPath + rec.Name);
          if tmpLst.Count > 0 then
          begin
            Result.AddStrings( tmpLst);
          end;
        end;
      end
      else
      begin
        // item is a file
        Result.Add( IncludeTrailingPathDelimiter( APath) + rec.Name);
      end;
    until FindNext(rec) <> 0;
    FindClose(rec);
  end;end;

function ZipFolder(APath: string; ATarget: string): boolean;
var
  i: integer;
  s, szPathEntry: string;
  _Zipper: TZipper;
  _FileList: TStringList;
  _ZEntries: TZipFileEntries;

begin
  Result := False;
  if not DirectoryExists(APath) then
    Exit;

  _Zipper := TZipper.Create;
  _ZEntries := TZipFileEntries.Create(TZipFileEntry);
  _Zipper.Filename := ATarget;

  // relative directory
  szPathEntry := ExcludeTrailingPathDelimiter(APath);
  szPathEntry := IncludeTrailingPathDelimiter(ExtractFileDir( szPathEntry));

  try
    _FileList := ScanFolder(APath);
    for i := 0 to _FileList.Count - 1 do
    begin
      s := StringReplace( _FileList[i], szPathEntry, '', [rfReplaceAll]);
      // Make sure the RelativeDirectory files are not in the root of the ZipFile
      _ZEntries.AddFileEntry(_FileList[i], s);
    end;

    if (_ZEntries.Count > 0) then
    begin
      _Zipper.ZipFiles(_ZEntries);
      Result := True;
    end;
  except
    on E: Exception do
    begin
      LogUtil.Add( APath + ': ' + E.Message, 'ZIP');
    end;
  end;

  _ZEntries.Free;
  _FileList.Free;
  _Zipper.Free;
end;

function DownloadFile(const AURL: String; const AFilePath: String;
  AUserAgent: String): boolean;
begin
  Result := False;
  if (AURL='') or (AFilePath='') then
    Exit;
  if FileExists(AFilePath) then
    DeleteFile(AFilePath);
  with TFPHTTPClient.Create(nil) do
  begin
    AllowRedirect := True;
    try
      if not AUserAgent.IsEmpty then
      begin
        AddHeader('User-Agent', AUserAgent);
        AddHeader('HTTP_USER_AGENT', AUserAgent);
        AddHeader('USER_AGENT', AUserAgent);
      end;
      Get(AURL,AFilePath);
      Result := True;
    except
      on E: Exception do
      begin
        if AppData.debug then
          LogUtil.Add( E.Message, 'Download');
      end;
    end;
    Free;
  end;
end;

function LoadFromFile(const AFileName: string): string;
var
  lst: TStringList;
begin
  Result := '';
  if AFileName = '' then
    Exit;
  if not FileExists( AFileName) then
    Exit;

  lst := TStringList.Create;
  lst.LoadFromFile( AFileName);
  Result := lst.Text;

  lst.Free;
end;

function SaveToFile(const AFileName: string; const AContent: string): boolean;
var
  lst: TStringList;
begin
  Result := False;
  if AFileName = '' then
    Exit;

  lst := TStringList.Create;
  lst.Text := AContent;
  lst.SaveToFile( AFileName);
  lst.Free;
  Result := True;
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

function jsonGetData(AJsonData: TJsonData; APath: string): string;
begin
  Result := '';
  try
    APath := StringReplace( APath, '/', '.', [rfReplaceAll]);
    Result := AJsonData.GetPath(APath).AsString;
  except
  end;
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

function JsonFormatter(JsonString: string; const UseUTF8: Boolean): string;
  // error line : VJSONParser.Scanner.CurRow;
var
  VJSONData : TJSONData = nil;
  VJSONParser : TLocalJSONParser;
  setOptions : TJSONOptions;
begin
  Result := '';
  JsonString := trim(JsonString);
  if JsonString = '' then
    Exit;

  if UseUTF8 then
    VJSONParser := TLocalJSONParser.Create(JsonString, [joStrict, joUTF8])
  else
    VJSONParser := TLocalJSONParser.Create(JsonString, [joStrict]);
  try
    try
      setOptions := VJSONParser.Options;
      Include(setOptions, joStrict);

      VJSONData := VJSONParser.Parse;
      Result := VJSONData.FormatJSON([], 2);
      VJSONData.Free;
    except
      on E: Exception do
      begin
      end;
    end;
  finally
    VJSONParser.Free;
  end;

end;

function IsJsonValid(JsonString: string): boolean;
begin
  if JsonFormatter(JsonString) = '' then
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

function StringReplaceExt(const S: string; OldPattern, NewPattern: array of string;
  Flags: TReplaceFlags): string;
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

function isURL(const AURL: string): boolean;
const
  _REGEX_ISURL = '(http|https|ftp):\/\/([a-zA-Z0-9-]+)?(\/)?(.*)?$';
begin
  Result := preg_match(_REGEX_ISURL, AURL);
end;

function isLookLikeURL(const AURL: string): boolean;
const
  _REGEX_ISURL = '[a-z0-9]+([\-\.]{1}[a-z0-9]+)*\.[a-z]{2,7}';
  //_REGEX_ISURL = '[a-z0-9]+([\-\.]{1}[a-z0-9]+)*\.([a-z]+)';
begin
  Result := preg_match(_REGEX_ISURL, AURL);
end;

function CleanUrl(URL: string; Separator: string): string;
begin
  Result := LowerCase(Trim(URL));
  Result := ReplaceAll(Result, [' ', ',', '?', '!', '.', '''', '+',
    '^', '"', #13, #10, '/', '\', '(', ')', '[', ']', '*', '$', '!'], Separator);
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
      if not (DecodedStr[I] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ', '.', '-', '_']) then
      //if not (DecodedStr[I] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ']) then
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

function base64_encode(const AStr: string): string;
var
  LDecodedStream: TStringStream;
  LEncodedStream: TStringStream;
  LEncoder: TBase64EncodingStream;
begin
  LDecodedStream := TStringStream.Create(AStr);
  LEncodedStream := TStringStream.Create('');
  LEncoder       := TBase64EncodingStream.Create(LEncodedStream);
  LEncoder.CopyFrom(LDecodedStream, LDecodedStream.Size);
  Result := LEncodedStream.DataString;
  LEncoder.Free;
  LDecodedStream.Free;
  LEncodedStream.Free;
end;

function base64_decode(const AStr: string): string;
var
  LDecodedStream: TStringStream;
  LEncodedStream: TStringStream;
  LDecoder: TBase64DecodingStream;
begin
  LEncodedStream := TStringStream.Create(AStr);
  LDecodedStream := TStringStream.Create('');
  LDecoder       := TBase64DecodingStream.Create(LEncodedStream);
  LDecodedStream.CopyFrom(LDecoder, LDecoder.Size);
  Result := LDecodedStream.DataString;
  LDecodedStream.Free;
  LEncodedStream.Free;
  LDecoder.Free;
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

function HTMLDecode(const AStr: String): String;
var
  Sp, Rp, Cp, Tp: PChar;
  S: String;
  I, Code: Integer;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  Cp := Sp;
  try
    while Sp^ <> #0 do
    begin
      case Sp^ of
        '&': begin
               Cp := Sp;
               Inc(Sp);
               case Sp^ of
                 'a': if AnsiStrPos(Sp, 'amp;') = Sp then  { do not localize }
                      begin
                        Inc(Sp, 3);
                        Rp^ := '&';
                      end;
                 'l',
                 'g': if (AnsiStrPos(Sp, 'lt;') = Sp) or (AnsiStrPos(Sp, 'gt;') = Sp) then { do not localize }
                      begin
                        Cp := Sp;
                        Inc(Sp, 2);
                        while (Sp^ <> ';') and (Sp^ <> #0) do
                          Inc(Sp);
                        if Cp^ = 'l' then
                          Rp^ := '<'
                        else
                          Rp^ := '>';
                      end;
                 'n': if AnsiStrPos(Sp, 'nbsp;') = Sp then  { do not localize }
                      begin
                        Inc(Sp, 4);
                        Rp^ := ' ';
                      end;
                 'q': if AnsiStrPos(Sp, 'quot;') = Sp then  { do not localize }
                      begin
                        Inc(Sp,4);
                        Rp^ := '"';
                      end;
                 '#': begin
                        Tp := Sp;
                        Inc(Tp);
                        while (Sp^ <> ';') and (Sp^ <> #0) do
                          Inc(Sp);
                        SetString(S, Tp, Sp - Tp);
                        Val(S, I, Code);
                        Rp^ := Chr((I));
                      end;
                 else
                   Exit;
               end;
           end
      else
        Rp^ := Sp^;
      end;
      Inc(Rp);
      Inc(Sp);
    end;
  except
  end;
  SetLength(Result, Rp - PChar(Result));
end;

function FormatTextLikeForum(const AContent: String): String;
begin
  Result := (AContent);
  //Result := HtmlDecode(AContent);
  Result := preg_replace('\[size=([0-9a-z]+):([0-9a-z]+)\](['+__NORMAL_SENTENCES+']+)\[\/size:([0-9a-z]+)\]', '$3', Result, True);
  Result := preg_replace('\[b:([0-9a-z]+)\](['+__NORMAL_SENTENCES+']+)\[\/b:([0-9a-z]+)\]', '<b>$2</b>', Result, True);
  Result := preg_replace('\[b:([0-9a-z]+)\](['+__NORMAL_SENTENCES_WITH_SLASH+']+)\[\/b:([0-9a-z]+)\]', '<b>$2</b>', Result, True);
  Result := preg_replace('\[i:([0-9a-z]+)\](['+__NORMAL_SENTENCES+']+)\[\/i:([0-9a-z]+)\]', '<i>$2</i>', Result, True);
  Result := preg_replace('\[i:([0-9a-z]+)\](.+?)\[\/i:([0-9a-z]+)\]', '<i>$2</i>', Result, True);
  Result := preg_replace('\[quote:([0-9a-z]+)\](.+?)\[\/quote:([0-9a-z]+)\]', '<blockquote>$2</blockquote>', Result, True);

  Result := preg_replace('\[quote:([0-9a-z]+)\=\"([0-9a-zA-Z\ _-]+)\"\](['+__NORMAL_SENTENCES+']+)\[\/quote:([0-9a-z]+)\]', '<blockquote><b>@$2</b>: $3</blockquote>', Result, True);
  Result := preg_replace('\[quote:([0-9a-z]+)\=\"([0-9a-zA-Z\ _-]+)\"\](['+__NORMAL_SENTENCES_WITH_SLASH+']+)\[\/quote:([0-9a-z]+)\]', '<blockquote><b>@$2</b>: $3</blockquote>', Result, True);
  Result := preg_replace('\[quote\](.+?)\[\/quote\]', '<blockquote>$1</blockquote>', Result, True);

  Result := preg_replace('\[url\](.+?)\[\/url\]', '<a href="$1" class="link">$1</a>', Result, True);
  Result := preg_replace('\[url=(.+?)\](.+?)\[\/url\]', '<a href="$1" class="link">$2</a>', Result, True);

  Result := preg_replace('\[pas\]\n(.+?)\[\/pas\]', '<pre><code lang="pascal">$1</code></pre>', Result, True);
  Result := preg_replace('\[pas\](.+?)\[\/pas\]', '<pre><code lang="pascal">$1</code></pre>', Result, True);
  Result := preg_replace('\[pas:([0-9a-z]+):([0-9a-z]+)\](.+?)\[\/pas:([0-9a-z]+):([0-9a-z]+)\]', '<pre><code lang="pas">$3</code></pre>', Result, True);
  Result := preg_replace('\[code=([0-9a-z]+)\](.+?)\[\/code\]', '<pre><code lang="$1">$2</code></pre>', Result, True);
  Result := preg_replace('\[code:([0-9a-z]+):([0-9a-z]+)\](['+__NORMAL_SENTENCES+']+)\[\/code:([0-9a-z]+):([0-9a-z]+)\]', '<pre><code>$3</code></pre>', Result, True);
  Result := preg_replace('\[code:([0-9a-z]+):([0-9a-z]+)\](['+__NORMAL_SENTENCES_WITH_SLASH+']+)\[\/code:([0-9a-z]+):([0-9a-z]+)\]', '<pre><code>$3</code></pre>', Result, True);
  Result := preg_replace('\[sql:([0-9a-z]+):([0-9a-z]+)\](['+__NORMAL_SENTENCES+']+)\[\/sql:([0-9a-z]+):([0-9a-z]+)\]', '<pre><code class="sql">$3</code></pre>', Result, True);

  Result := preg_replace('\[img:([0-9a-z]+)\]', '<img src="', Result, True);
  Result := preg_replace('\[\/img:([0-9a-z]+)\]', '" onerror="this.style.display=''none''"/>', Result, True);
  Result := preg_replace('\[img\]', '<img src="', Result, True);
  Result := preg_replace('\[\/img\]', '" onerror="this.style.display=''none''"/>', Result, True);
  Result := preg_replace('\[color=([0-9a-z]+):([0-9a-z]+)\](.+?)\[\/color:([0-9a-z]+)\]', '<font style="color:$1;">$3</font>', Result, True);

  // specific pascal-id.org
  Result := preg_replace('\/dpr\/PNphpBB2-viewtopic-t-([0-9]+).pas', '/thread/unknown/$1/view-old-thread/', Result, True);
  Result := preg_replace('\/dpr\/Forum-viewtopic-p-([0-9]+).pas', '/thread/unknown/$1/view-old-thread/', Result, True);

  // force
  Result := preg_replace('\[code:([0-9a-z]+):([0-9a-z]+)\](['+__NORMAL_SENTENCES_WITH_SLASH+']+)\[\/code:([0-9a-z]+):([0-9a-z]+)\]', '<code>$3</code>', Result, True);
  Result := preg_replace('\[code\]\n(.+?)\[\/code\]', '<code>$1</code>', Result, True);
  Result := preg_replace('\[code\](.+?)\[\/code\]', '<code>$1</code>', Result, True);

  // remove unused tag
  Result := preg_replace('\[b:([0-9a-z]+)\]\[\/b:([0-9a-z]+)\]', '', Result, True);
  Result := Result.Replace('[/URL]','');
  Result := Result.Trim;
end;

function MarkdownToHTML(const AContent: String): String;
begin
  Result := AContent;

  Result := preg_replace('\!\[(.+?)\]\((.+?)\)', '<img src="$2" />', Result, True); // Image
  Result := preg_replace('\[([a-zA-Z0-9\ \-\/.,:;#]+)?\]\((.+?)\)', '<a href="$2" target="_blank" >$1</a>', Result, True); // Link
  Result := preg_replace('\*\*\*(.*?)\s\*\*\*', '<b><i>$1</i></b> ', Result, True); // Tebal Miring
  Result := preg_replace('\*\*(.*?)\*\*', '<b>$1</b> ', Result, True); // Miring
  Result := preg_replace('\*(.+?)\*', '<i>$1</i> ', Result, True); // Tebal
  //Result := preg_replace('_([^\*]*)_', '<i>$1</i> ', Result, True); // Miring
  //Result := preg_replace('_(.*?)_', '<i>$1</i> ', Result, True); // Miring

  //Result := preg_replace('> (.+?)\n', '<blockquote>$1</blockquote>'#10, Result, True); // Heading
  Result := preg_replace('### (.+?)(\n|\r)', '<h3>$1</h3>', Result, True); // Heading
  Result := preg_replace('## (.+?)(\n|\r)', '<h2>$1</h2>', Result, True); // Heading
  Result := preg_replace('# (.+?)(\n|\r)', '<h1>$1</h1>', Result, True); // Heading
  Result := preg_replace('(\ |\n|\r)#(.+?)(\ |\n|\r)', '<span class="tag">#$2</span>', Result, True); // Hashtag

  //Result := preg_replace('> (.+?)<', '<blockquote>$1</blockquote><', Result, True); // Heading
  //Result := preg_replace('### (.+?)<', '<h3>$1</h3><', Result, True); // Heading
  //Result := preg_replace('## (.+?)<', '<h2>$1</h2><', Result, True); // Heading
  //Result := preg_replace('# (.+?)<', '<h1>$1</h1><', Result, True); // Heading

  Result := preg_replace('---\n', '<hr>'#10, Result, True); // Line

  Result := preg_replace('```(.+?)```', '<pre><code>$1</code></pre>', Result, True); // Simple Code
  Result := preg_replace('`(.+?)`', '<span class="code-inline">$1</span>', Result, True); // Simple Code

  Result := preg_replace(#13#10, #10, Result, True);
  Result := preg_replace(#10#10, #10, Result, True);
end;

function file_get_contents(TargetURL: string; AShowErrorMessageAsResult: boolean
  ): string;
var
  s: string;
begin
  Result := '';
  with TFPHTTPClient.Create(nil) do
  begin
    try
      s := Get(TargetURL);
      Result := s;
    except
      on e: Exception do
      begin
        if AShowErrorMessageAsResult then
          Result := e.Message;
      end;
    end;

    Free;
  end;
end;

function FileCopy(ASource, ATarget: string): boolean;
var
  memBuffer: TMemoryStream;
begin
  Result := false;
  memBuffer := TMemoryStream.Create;
  try
    memBuffer.LoadFromFile(ASource);
    MemBuffer.SaveToFile(ATarget);
    Result := true
  except
  end;
  memBuffer.Free
end;

// ref:
// http://regexpstudio.com/en/regexp_syntax.html
function preg_match(const RegexExpression: string; SourceString: string): boolean;
begin
  Result := False;
  try
    with TRegExpr.Create do
    begin
      Expression := RegexExpression;
      Result := Exec(SourceString);
      Free;
    end;
  except
  end;
end;

function preg_replace(const RegexExpression, ReplaceString, SourceString: string;
  UseSubstitution: boolean): string;
begin
  try
    with TRegExpr.Create do
    begin
      Expression := RegexExpression;
      Result := Replace(SourceString, ReplaceString, UseSubstitution);
      Free;
    end;
  except
    Result := SourceString;
  end;
end;

function isIPAddress(const IPAddress: string): boolean;
begin
  Result := execregexpr('[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}', IPAddress);
end;

function isEmail(const s: string): boolean;
begin
  Result := execregexpr('(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$)', s);
end;

function isDomain(const s: string): boolean;
begin
  //Result := execregexpr('(^(?!\-)(?:[a-zA-Z\d\-]{0,62}[a-zA-Z\d]\.){1,126}(?!\d+)[a-zA-Z\d]{1,63}$)', s);
  Result := execregexpr('^((\w+)\.)?(([\w-]+)?)(\.[\w-]+){1,2}$', s);
end;

function GetHostNameIP(HostName: string): string;
{
var
  i: integer;
  ans: array [1..10] of THostAddr;
  lst: TStrings;
}
begin
  Result := '';
  with THostResolver.Create(nil) do
  begin
    try
      if NameLookup( HostName ) then
      begin
        Result := Trim( AddressAsString);
      end;
    except
    end;
    Free;
  end;
{
  i := ResolveName(HostName, ans);
  if i = 0 then
    Exit;
  Result := HostAddrToStr(Ans[1]);

  lst := Explode( Result, '.');
  Result := '';
  for i := 3 downto 1 do
  begin
    Result := Result + lst[i] + '.';
  end;
  Result := Result + lst[0];
  lst.Free;
}
end;

function GetUserIpAddress: string;
begin
  Result := _SERVER['HTTP_CLIENT_IP'];
  if not Result.IsEmpty then Exit;
  Result := _SERVER['HTTP_X_FORWARDED_FOR'];
  if not Result.IsEmpty then Exit;
  Result := _SERVER['REMOTE_ADDR'];
end;

// example:
//  Exec( 'executable-nya', ['parameternya'], lsS);
function Exec(const AExeName: string; const AParameter: array of string;
  var AOutput: string; AShowOptons: TShowWindowOptions): boolean;
var
  p: TProcess;
  i, exitstatus: integer;

const
  // not too small to avoid fragmentation when reading large files.
  csi_EXECCOMMAND_READ_BYTES = 65536;

  // helperfunction that does the bulk of the work.
  function internalRuncommand(p: TProcess; var outputstring: string;
  var exitstatus: integer; const AShowOptons: TShowWindowOptions = swoHIDE): integer;
  var
    numbytes, bytesread: integer;
  begin
    Result := -1;
    try
      try
        p.Options := [poUsePipes, poWaitOnExit, poStderrToOutPut];
        p.ShowWindow := AShowOptons;
        bytesread := 0;
        p.Execute;
        while p.Running do
        begin
          Setlength(outputstring, BytesRead + csi_EXECCOMMAND_READ_BYTES);
          NumBytes := p.Output.Read(outputstring[1 + bytesread],
            csi_EXECCOMMAND_READ_BYTES);
          if NumBytes > 0 then
            Inc(BytesRead, NumBytes)
          else
            Sleep(100);
        end;
        repeat
          Setlength(outputstring, BytesRead + csi_EXECCOMMAND_READ_BYTES);
          NumBytes := p.Output.Read(outputstring[1 + bytesread],
            csi_EXECCOMMAND_READ_BYTES);
          if NumBytes > 0 then
            Inc(BytesRead, NumBytes);
        until NumBytes <= 0;
        setlength(outputstring, BytesRead);
        exitstatus := p.exitstatus;
        Result := 0; // we came to here, document that.
      except
        on e: Exception do
        begin
          die( e.Message);
          Result := 1;
          setlength(outputstring, BytesRead);
        end;
      end;
    finally
      p.Free;
    end;

  end;

begin
  exitstatus:= 0;
  p := TProcess.Create(nil);
  p.Executable := AExeName;
  if high(AParameter) >= 0 then
    for i := low(AParameter) to high(AParameter) do
      p.Parameters.add(AParameter[i]);
  Result := internalruncommand(p, AOutput, exitstatus, AShowOptons) = 0;
  if exitstatus <> 0 then
    Result := False;
end;

function FastInfo(): string;
var
  lst: TStringList;
  s: string;
begin
  lst := TStringList.Create;
  Application.GetEnvironmentList(lst);

  s := '<pre><b>Your Server Info:</b><br>';
  s := s + #13#10'TargetCPU ' + {$i %FPCTARGETCPU%};
  s := s + #13#10'Target OS ' + {$i %FPCTARGETOS%};
  s := s + #13#10'FPC Version ' + {$i %FPCVERSION%};
  s := s + #13#10'Build Date ' + {$i %DATE%};
  s := s + #13#10#13;

  lst.Text := s + lst.Text;
  Result := lst.Text;
  lst.Free;
end;



initialization
  LANG := 'en'; //GetLanguageIDs( LANG, FallbackLANG);
  AppData.debug := True;
  Config := TMyConfig.Create(nil);
  Config.ValidateFile('config/config.json');


finalization
  FreeAndNil(Config);

end.

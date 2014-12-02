unit common;

{$mode objfpc}{$H+}
{ $include define.inc}

interface

uses
  //SynExportHTML,
  fpcgi, gettext, process, Math, fpjson, jsonparser, custweb, jsonConf,
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
  _SYSTEM_CACHE_TYPE = 'systems/cache';
  _SYSTEM_CACHE_TIME = 'systems/cache_time';
  _SYSTEM_CACHE_WRITE = 'systems/write';
  _SYSTEM_TEMP_DIR = 'systems/temp';
  _SYSTEM_SESSION_DIR = 'systems/session_dir';
  _SYSTEM_HIT_STORAGE = 'systems/hit_storage';

  _DATABASE_HOSTNAME = 'database/default/hostname';
  _DATABASE_DRIVER = 'database/default/driver';
  _DATABASE_TABLETYPE = 'database/default/tabletype';
  _DATABASE_USERNAME = 'database/default/username';
  _DATABASE_PASSWORD = 'database/default/password';
  _DATABASE_DATABASENAME = 'database/default/database_name';
  _DATABASE_TABLE_PREFIX = 'database/default/prefix';
  _DATABASE_LIBRARY = 'database/default/library';

  _WORDPRESS_PLUGINS_POLYLANG = 'wordpress/plugins/polylang';

type
  TStringArray = array of string;


function i2s(pI: integer): string;
function s2i(s: string): integer;
function f2s(n: extended): string;
function s2f(s: string): extended;
function Implode(lst: TStringList; sep: string = ';'; prefix: string = '';
  suffix: string = ''): string;
function Explode(Str, Delimiter: string): TStrings;
function ExplodeTags(TagString: string): TStringList;
function EchoError(const Fmt: string; const Args: array of const): string;
function _GetTickCount: DWord;

function SafeText(const SourceString: string): string;
function ReplaceAll(const Subject: string;
  const OldPatterns, NewPatterns: array of string; IgnoreCase: boolean = False): string;
function ReplaceAll(const Subject: string; const OldPatterns: array of string;
  NewPatterns: string; IgnoreCase: boolean = False): string;

function AppendPathDelim(const Path: string): string;
function DirectoryIsWritable(const DirectoryName: string): boolean;

procedure DumpJSON(J: TJSonData; DOEOLN: boolean = False);
function HexToInt(HexStr: string): int64;

function RandomString(PLen: Integer; PrefixString:string = ''): string;

procedure Die(const Message: string = ''); overload;
procedure Die(const Number: integer); overload;
procedure Die(const Message: TStringList); overload;

// php like function
function mysql_real_escape_string(const unescaped_string: string): string;
function mysql_real_escape_string(const unescaped_strings: TStringList): string;
function UrlEncode(const DecodedStr: string; Pluses: boolean = True): string;
function UrlDecode(const EncodedStr: string): string;
function ucwords(const str: string): string;

var
  Config: TMyConfig;

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
  Result := 0;
  TryStrToInt(s,Result);
end;

function f2s(n: extended): string;
begin
  Result := '0';
  try
    Result := FloatToStr(n);
    //Result := Format( '%.2f', [n]);
  except
  end;
end;

function s2f(s: string): extended;
begin
  Result := 0;
  TryStrToFloat(s,Result);
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

procedure Die(const Number: integer);
begin
  Die(i2s(Number));
end;

procedure Die(const Message: TStringList);
begin
  Die('<pre>' + Message.Text + '</pre>');
end;

function RandomString(PLen: Integer; PrefixString: string): string;
var
   str: string;
begin
   Randomize;
   //string with all possible chars
   str    := PrefixString+'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
   Result := '';
   repeat
     Result := Result + str[Random(Length(str)) + 1];
   until (Length(Result) = PLen)
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
        Result := Result + Chr(
          HexToInt(EncodedStr[I + 1] + EncodedStr[I + 2]));
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

initialization
  LANG := 'en'; //GetLanguageIDs( LANG, FallbackLANG);
  Config := TMyConfig.Create(nil);
  Config.Filename := 'config/config.json';

finalization
  FreeAndNil(Config);

end.

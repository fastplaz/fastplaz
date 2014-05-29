unit common;

{$mode objfpc}{$H+}
{ $include define.inc}

interface

uses
  //SynExportHTML,
  fpcgi, gettext, process, math,
  jsonConf,
  Classes, SysUtils, fastplaz_handler;

const
  _APP = 'FastPlaz';
  _APP_URL = 'http://www.fastplaz.com';
  _SYSTEM_SITENAME = 'systems/sitename';
  _SYSTEM_BASEURL = 'systems/baseurl';
  _SYSTEM_WEBMASTER_EMAIL = 'systems/admin_email';
  _SYSTEM_MODULE_DEFAULT = 'systems/module_default';
  _SYSTEM_MODULE_VARIABLE = 'systems/module_variable';
  _SYSTEM_ERROR_URL = 'systems/error_url';
  _SYSTEM_ERROR_REDIRECT = 'systems/error_redirect';
  _SYSTEM_LANGUAGE_DEFAULT = 'systems/language_default';
  _SYSTEM_THEME = 'systems/theme';
  _SYSTEM_DEBUG = 'systems/debug';
  _SYSTEM_CACHE_TYPE = 'systems/cache';
  _SYSTEM_TEMP_DIR = 'systems/temp';
  _SYSTEM_SESSION_DIR = 'systems/session_dir';

  _DATABASE_HOSTNAME = 'database/default/hostname';
  _DATABASE_DRIVER = 'database/default/driver';
  _DATABASE_TABLETYPE = 'database/default/tabletype';
  _DATABASE_USERNAME = 'database/default/username';
  _DATABASE_PASSWORD = 'database/default/password';
  _DATABASE_DATABASENAME = 'database/default/database_name';
  _DATABASE_TABLE_PREFIX = 'database/default/prefix';
  _DATABASE_LIBRARY = 'database/default/library';

  _WORDPRESS_PLUGINS_POLYLANG = 'wordpress/plugins/polylang';


function i2s(pI: integer): string;
function s2i(pS: string): integer;
function f2s(n: Extended):string;
function Explode(Str, Delimiter: string): TStrings;
function ExplodeTags( TagString:string): TStringList;
function EchoError (Const Fmt : String; const Args : Array of const) : String;
function _GetTickCount: DWord;

function ReplaceAll(const Subject: String;
  const OldPatterns, NewPatterns: array of String;
  IgnoreCase: Boolean = false): String;

procedure Die( const Message:string = ''); overload;
procedure Die( const Number:integer); overload;
procedure Die( const Message:TStringList); overload;

var
  Config : TJSONConfig;

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

function s2i(pS: string): integer;
begin
  Result := 0;
  try
    Result := StrToInt( pS);
  except
  end;
end;

function f2s(n: Extended): string;
begin
  Result := '0';
  try
    Result := FloatToStr( n);
    //Result := Format( '%.2f', [n]);
  except
  end;
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
  lst : TStringList;
  i : integer;
  p : string;
begin
  lst := TStringList.Create;
  while Pos(' ',  TagString) <> 0 do
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

  for i:=lst.Count-1 downto 1 do begin
    if pos( '=', lst[i]) = 0 then begin
      lst[i-1] := lst[i-1] + ' ' + lst[i];
      lst.Delete(i);
    end;
  end;

  //-- check [.] delimiter
  if Pos( '.', lst[0]) <> 0 then
  begin
    lst.Insert(0, Copy( lst[0], 1, Pos( '.', lst[0])-1));
    lst[1] := 'index='+Copy( lst[1], Pos( '.', lst[1])+1, Length(lst[1])-Pos( '.', lst[1]) );
  end;

  Result := lst;
end;

function EchoError(const Fmt: String; const Args: array of const): String;
begin
  Result := '<div class="warning">'+Format( Fmt, Args)+'</div>';
end;


function _GetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;


function ReplaceAll(const Subject: String; const OldPatterns,
  NewPatterns: array of String; IgnoreCase: Boolean): String;
var
  ReplaceFlags: TReplaceFlags;
  NewPattern: String;
  I: Integer;
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

procedure Die(const Number: integer);
begin
  Die(i2s(Number));
end;

procedure Die(const Message: TStringList);
begin
  Die( '<pre>'+Message.Text+'</pre>');
end;

procedure Die(const Message: string);
begin
  Application.Response.Contents.Add( Message);
  Application.Response.SendContent;
  Application.Terminate;
end;


initialization
  LANG := 'en'; //GetLanguageIDs( LANG, FallbackLANG);
  Config := TJSONConfig.Create(nil);
  Config.Filename:= 'config/config.json';

finalization
  FreeAndNil( Config);

end.


unit commonbase_lib;

{$mode objfpc}{$H+}
(*
  Basic helper functions without dependency to fastplaz_handler,
  so they can be used by units outside fastplaz_runtime
  (eg: json_lib in fastplaz_components).

  These functions are also available via unit common.
*)

interface

uses
  fpjson, jsonparser, jsonscanner, Classes, SysUtils;

function ReplaceAll(const Subject: string;
  const OldPatterns, NewPatterns: array of string; IgnoreCase: boolean = False): string;
function ReplaceAll(const Subject: string; const OldPatterns: array of string;
  NewPatterns: string; IgnoreCase: boolean = False): string;
function JsonFormatter(JsonString: string; Const UseUTF8 : Boolean = True): string;
function StripSlash(Const DataString: UnicodeString): UnicodeString;

implementation

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

function JsonFormatter(JsonString: string; const UseUTF8: Boolean): string;
var
  VJSONData : TJSONData = nil;
  VJSONParser : TJSONParser;
begin
  Result := '';
  JsonString := trim(JsonString);
  if JsonString = '' then
    Exit;

  if UseUTF8 then
    VJSONParser := TJSONParser.Create(JsonString, [joStrict, joUTF8])
  else
    VJSONParser := TJSONParser.Create(JsonString, [joStrict]);
  try
    try
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

end.

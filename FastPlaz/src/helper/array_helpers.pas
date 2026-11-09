unit array_helpers;
{
  USAGE:

  const
    NUMBER_LIST: array[0..5] of string = ('zero', 'one', 'two', 'three', 'four', 'five');

    if 'zero' in NUMBER_LIST then
    begin
      //
    end;

}

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  StrUtils, Classes, SysUtils;

Operator in (const AText:string;const AValues: array of string):Boolean;

type

  //TStringArray = array of string;

  { TArraySmartHelper }

  TArraySmartHelper = type helper for TStringArray
    // "a, b,c" -> ['a', 'b', 'c'] (trimmed, empty items dropped)
    class function CreateFromDelimitedString(DelimString: String; ADelimiter: Char = ','): TStringArray; static;
    function IndexOf(AString: String): integer; overload;
  public
    function Length: integer; overload; inline;
    function Count: integer; overload; inline;
    procedure Add(AString: String); overload; inline;
  end;


implementation

operator in(const AText: string; const AValues: array of string): Boolean;
begin
  Result := AnsiIndexStr(AText,AValues) <> -1;
end;

{ TArraySmartHelper }

class function TArraySmartHelper.CreateFromDelimitedString(DelimString: String;
  ADelimiter: Char): TStringArray;
var
  parts: TStringArray;
  i: integer;
begin
  Result := nil;
  parts := DelimString.Split([ADelimiter]);
  for i := 0 to system.Length(parts) - 1 do
    if Trim(parts[i]) <> '' then
      Result.Add(Trim(parts[i]));
end;

function TArraySmartHelper.IndexOf(AString: String): integer;
begin
  Result := system.Length(Self) - 1;
  while (Result >= 0) and (Self[Result] <> AString) do
    Dec(Result);
end;

function TArraySmartHelper.Length: integer;
begin
  Result := Count;
end;

function TArraySmartHelper.Count: integer;
begin
  Result := system.Length(Self);
end;

procedure TArraySmartHelper.Add(AString: String);
begin
  SetLength(Self, system.Length(Self)+1);
  Self[system.Length(Self)-1] := AString;
end;

end.


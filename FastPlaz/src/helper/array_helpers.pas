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
    class function CreateFromDelimitedString(DelimString: String): TStringArray static;
    class function IndexOf(AString: String): integer static; overload; inline;
  end;


implementation

operator in(const AText: string; const AValues: array of string): Boolean;
begin
  Result := AnsiIndexStr(AText,AValues) <> -1;
end;

{ TArraySmartHelper }

class function TArraySmartHelper.CreateFromDelimitedString(DelimString: String
  ): TStringArray;
begin

end;

class function TArraySmartHelper.IndexOf(AString: String): integer;
begin

end;

{ TArraySmartHelper }





end.


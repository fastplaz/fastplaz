{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit json_helpers;

{$mode objfpc}
{$modeswitch typehelpers}

interface

uses
  common, fpjson,
  Classes, SysUtils, string_helpers;

type

  { TJsonSmartHelper }

  TJsonSmartHelper = class helper for TJSONData
  private
    function getValue(const AKeyName: string): string;
  public
    function IndexOfName(const AIndexName: string): Integer;
    function Delete(const AIndexName: string): boolean;
    property Value[const AKeyName: string]: string read getValue;
  end;

implementation

{ TJsonSmartHelper }

function TJsonSmartHelper.getValue(const AKeyName: string): string;
var
  s: string;
begin
  s := StringReplace(AKeyName, '/', '.', [rfReplaceAll]);
  Result := '';
  try
    Result := jsonGetData(Self, s);
  except
  end;
end;

function TJsonSmartHelper.IndexOfName(const AIndexName: string): Integer;
begin
  Result := TJSONObject(Self).IndexOfName(AIndexName);
end;

function TJsonSmartHelper.Delete(const AIndexName: string): boolean;
var
  indexOfField: integer;
begin
  Result := False;
  indexOfField := TJSONObject(Self).IndexOfName(AIndexName);
  if indexOfField = -1 then
    Exit;
  TJSONObject(Self).Delete(indexOfField);
  Result := True;
end;

end.


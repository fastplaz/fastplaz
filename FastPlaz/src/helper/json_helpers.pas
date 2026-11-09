{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit json_helpers;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  common, fpjson,
  Classes, SysUtils, string_helpers;

type

  { TJsonSmartHelper }

  TJsonSmartHelper = class helper for TJSONData
  private
    function GetAsJSONFormated: TJSONStringType;
    function getValue(const AKeyName: string): string;
  public
    property AsJSONFormated: TJSONStringType read GetAsJSONFormated;
    function GetData(const APath: string): TJSONData;
    function IndexOfName(const AIndexName: string): Integer;
    function ValueOfName(const AIndexName: string): TJSONData;
    function ValueOfNameAsString(const AIndexName: string): string;
    function Delete(const AIndexName: string): boolean;
    property Value[const AKeyName: string]: string read getValue;
  end;

  { TJSONObjectSmartHelper }

  TJSONObjectSmartHelper = class helper for TJSONObject
  private
    function GetAsJSONFormated: TJSONStringType;
  public
    property AsJSONFormated: TJSONStringType read GetAsJSONFormated;

  end;

implementation

{ TJSONObjectSmartHelper }

function TJSONObjectSmartHelper.GetAsJSONFormated: TJSONStringType;
begin
  Result := JsonFormatter(Self.AsJSON, False);
end;

{ TJsonSmartHelper }

function TJsonSmartHelper.GetAsJSONFormated: TJSONStringType;
begin
  Result := JsonFormatter(Self.AsJSON, False);
end;

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

function TJsonSmartHelper.GetData(const APath: string): TJSONData;
var
  s: string;
begin
  s := StringReplace(APath, '/', '.', [rfReplaceAll]);
  Result := Self.FindPath(s);
end;

// the name based helpers only apply to an object; on an array or a value
// the unchecked cast read foreign memory
function TJsonSmartHelper.IndexOfName(const AIndexName: string): Integer;
begin
  Result := -1;
  if Self.JSONType = jtObject then
    Result := TJSONObject(Self).IndexOfName(AIndexName);
end;

function TJsonSmartHelper.ValueOfName(const AIndexName: string): TJSONData;
var
  i: Integer;
begin
  Result := Nil;
  i := IndexOfName(AIndexName);
  if i <> -1 then
    Result := TJSONObject(Self).Items[i];
end;

function TJsonSmartHelper.ValueOfNameAsString(const AIndexName: string): string;
begin
  Result := '';
  try
    Result := ValueOfName(AIndexName).AsString;
  except
  end;
end;

function TJsonSmartHelper.Delete(const AIndexName: string): boolean;
var
  indexOfField: integer;
begin
  Result := False;
  indexOfField := IndexOfName(AIndexName);
  if indexOfField = -1 then
    Exit;
  TJSONObject(Self).Delete(indexOfField);
  Result := True;
end;

end.


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
    property Value[const AKeyName: string]: string read getValue;
  end;

implementation

{ TJsonSmartHelper }

function TJsonSmartHelper.getValue(const AKeyName: string): string;
var
  s: string;
begin
  s := StringReplace(s, '/', '.', [rfReplaceAll]);
  Result := '';
  try
    Result := jsonGetData(Self, AKeyName);
  except
  end;
end;

end.


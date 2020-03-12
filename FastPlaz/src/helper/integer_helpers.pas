{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit integer_helpers;
{
  USAGE:

}

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  StrUtils, Classes, SysUtils;


type

  { TIntegerSmartHelper }

  TIntegerSmartHelper = type helper(TIntegerHelper) for Integer
  public
    function InRange(const ValueMin, ValueMax: integer): boolean; overload; inline;
  end;


implementation


{ TIntegerSmartHelper }

function TIntegerSmartHelper.InRange(const ValueMin, ValueMax: integer
  ): boolean;
begin
  Result := False;
  if (Self>=ValueMin) and (Self<=ValueMax) then
    Result := True;
end;

end.


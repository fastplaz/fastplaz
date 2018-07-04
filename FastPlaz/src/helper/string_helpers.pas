{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit string_helpers;

{$mode objfpc}
{$modeswitch typehelpers}

interface

uses
  common,
  Classes, SysUtils;

type

  { TStringSmartHelper }

  TStringSmartHelper = type helper(TStringHelper) for AnsiString
  public
    function UrlEncode: string; overload; inline;
    function UrlDecode: string; overload; inline;
    function EscapeString: string; overload; inline;
    function IsJson: boolean; overload; inline;
    function Encode64: string; overload; inline;
    function Decode64: string; overload; inline;
    function Cut( AStartText, AStopText: string):string; overload; inline;
    function SaveToFile( AFileName: string): boolean; overload; inline;
  end;

implementation

function TStringSmartHelper.UrlEncode: string;
begin
  Result := common.UrlEncode(Self);
end;

function TStringSmartHelper.UrlDecode: string;
begin
  Result := common.UrlDecode(Self);
end;

function TStringSmartHelper.EscapeString: string;
begin
  Result := mysql_real_escape_string(Self);
end;

function TStringSmartHelper.IsJson: boolean;
begin
  Result := IsJsonValid(Self);
end;

function TStringSmartHelper.Encode64: string;
begin
  Result := base64_encode(Self);
end;

function TStringSmartHelper.Decode64: string;
begin
  Result := base64_decode(Self);
end;

function TStringSmartHelper.Cut(AStartText, AStopText: string): string;
begin
  Result := StringCut(AStartText, AStopText, Self);
end;

function TStringSmartHelper.SaveToFile(AFileName: string): boolean;
var
  sText: TStringList;
begin
  Result := False;
  sText := TStringList.Create;
  sText.Text := Self;
  try
    sText.SaveToFile(AFileName);
    Result := True;
  except
  end;
  sText.Free;
end;

end.


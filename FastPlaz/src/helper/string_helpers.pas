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
    function AsDateTime: TDateTime; overload; inline;
    function UrlEncode: AnsiString; overload; inline;
    function UrlDecode: AnsiString; overload; inline;
    function EscapeString: AnsiString; overload; inline;
    function IsEmpty: boolean; overload; inline;
    function IsEqualTo( AString: string): boolean; overload; inline;
    function IsExists( AString: string): boolean; overload; inline;
    function IsJson: boolean; overload; inline;
    function IsNumeric: boolean; overload; inline;
    function Encode64: AnsiString; overload; inline;
    function Decode64: AnsiString; overload; inline;
    function Cut( AStartText, AStopText: string):AnsiString; overload; inline;
    function SaveToFile( AFileName: string): boolean; overload; inline;
    function Has( AText: string): boolean; overload; inline;
    function UcWords: AnsiString; overload; inline;

  end;

implementation

function TStringSmartHelper.AsDateTime: TDateTime;
begin
  Result := StrToDateTime( Self);
end;

function TStringSmartHelper.UrlEncode: AnsiString;
begin
  Result := common.UrlEncode(Self);
end;

function TStringSmartHelper.UrlDecode: AnsiString;
begin
  Result := common.UrlDecode(Self);
end;

function TStringSmartHelper.EscapeString: AnsiString;
begin
  Result := mysql_real_escape_string(Self);
end;

function TStringSmartHelper.IsEmpty: boolean;
begin
  Result := IsNullOrEmpty( Self);
end;

function TStringSmartHelper.IsEqualTo(AString: string): boolean;
begin
  Result := Self.Equals( AString);
end;

function TStringSmartHelper.IsExists(AString: string): boolean;
begin
  Result := False;
  if IsEmpty then
    Exit;
  if Pos( AString, Self) > 0 then
    Result := True;
end;

function TStringSmartHelper.IsJson: boolean;
begin
  Result := IsJsonValid(Self);
end;

function TStringSmartHelper.IsNumeric: boolean;
begin
  Result := False;
  try
    StrToFloat( Self);
    Result := True;
  except
  end;
end;

function TStringSmartHelper.Encode64: AnsiString;
begin
  Result := base64_encode(Self);
end;

function TStringSmartHelper.Decode64: AnsiString;
begin
  Result := base64_decode(Self);
end;

function TStringSmartHelper.Cut(AStartText, AStopText: string): AnsiString;
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

function TStringSmartHelper.Has(AText: string): boolean;
begin
  Result := False;
  if pos( AText, Self) > 0 then
    Result := True;
end;

function TStringSmartHelper.UcWords: AnsiString;
begin
  Result := common.ucwords(Self);
end;

end.


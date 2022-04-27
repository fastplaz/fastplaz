{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit string_helpers;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  common, RegExpr,
  Classes, SysUtils;

type

  { TStringSmartHelper }

  TStringSmartHelper = type helper(TStringHelper) for AnsiString
  public
    function AsDateTime: TDateTime; overload; inline;
    function AsInteger: Integer; overload; inline;
    function UrlEncode: AnsiString; overload; inline;
    function UrlDecode: AnsiString; overload; inline;
    function EscapeString: AnsiString; overload; inline;
    function RemoveSlash: AnsiString; overload; inline;
    function RemoveEmoji(const AReplaceWith: string = ''): string; overload; inline;
    function RemoveUnicode(const AReplaceWith: string = ''): string; overload; inline;
    function RemoveMarkDown(): string; overload; inline;
    function AddSlash: AnsiString; overload; inline;
    function Explode( ADelimiter:string = ','): TStrings; overload; inline;
    function IsEmpty: boolean; overload; inline;
    function IsNotEmpty: boolean; overload; inline;
    function IsEqualTo( AString: string): boolean; overload; inline;
    function IsNotEqualTo( AString: string): boolean; overload; inline;
    function IsExists( AString: string): boolean; overload; inline;
    function isVowelExists: boolean; overload; inline;
    function IsJson: boolean; overload; inline;
    function IsNumeric: boolean; overload; inline;
    function IsEmail: boolean; overload; inline;
    function IsURL: boolean; overload; inline;
    function IsDomain: boolean; overload; inline;
    function isDate(ADelimiter: string = '/'): boolean; //DD/MM/YYYY
    function Encode64: AnsiString; overload; inline;
    function Decode64: AnsiString; overload; inline;
    function Cut( AStartText, AStopText: string):AnsiString; overload; inline;
    function SaveToFile( AFileName: string): boolean; overload; inline;
    function Has( AText: string): boolean; overload; inline;
    function UcWords: AnsiString; overload; inline;
    function IsPregMatch( ARegex: string): boolean; overload; inline;
    function Split( ADelimiter: string = ','): TStrings; overload; inline;
    function StrPos( AText: string): integer; overload; inline;
    function Quoted( AQuote: char = ''''): string; overload; inline;

  end;

implementation

function TStringSmartHelper.AsDateTime: TDateTime;
var
  tmpFormatSettings: TFormatSettings;
begin
  tmpFormatSettings := FormatSettings;
  tmpFormatSettings.DateSeparator := '-';
  tmpFormatSettings.ShortDateFormat := 'yyyy-MM-dd hh:nn:ss';

  try
    Result := Now;
    Result := StrToDateTime( Self, tmpFormatSettings);
  except
  end;
end;

function TStringSmartHelper.AsInteger: Integer;
begin
  Result := s2i(Self);
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

function TStringSmartHelper.RemoveSlash: AnsiString;
begin
  Result := ExcludeTrailingBackslash(Self);
end;

function TStringSmartHelper.RemoveEmoji(const AReplaceWith: string): string;
begin
  Result := common.RemoveEmoji(Self, AReplaceWith);
end;

function TStringSmartHelper.RemoveUnicode(const AReplaceWith: string): string;
begin
  Result := common.RemoveUnicode(Self, AReplaceWith);
end;

function TStringSmartHelper.RemoveMarkDown: string;
begin
  Result := common.RemoveMarkDown(Self);
end;

function TStringSmartHelper.AddSlash: AnsiString;
begin
  Result := IncludeTrailingBackslash(Self);
end;

function TStringSmartHelper.Explode(ADelimiter: string): TStrings;
begin
  Result := common.Explode(Self, ADelimiter);
end;

function TStringSmartHelper.IsEmpty: boolean;
begin
  Result := IsNullOrEmpty( Self);
end;

function TStringSmartHelper.IsNotEmpty: boolean;
begin
  Result := not IsNullOrEmpty( Self);
end;

function TStringSmartHelper.IsEqualTo(AString: string): boolean;
begin
  Result := Self.Equals( AString);
end;

function TStringSmartHelper.IsNotEqualTo(AString: string): boolean;
begin
  Result := not Self.Equals( AString);
end;

function TStringSmartHelper.IsExists(AString: string): boolean;
begin
  Result := False;
  if IsEmpty then
    Exit;
  if Pos( AString, Self) > 0 then
    Result := True;
end;

function TStringSmartHelper.isVowelExists: boolean;
begin
  Result := common.isVowelExists(Self);
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

function TStringSmartHelper.IsEmail: boolean;
begin
  Result := False;
  if Self.IsEmpty then
    Exit;
  Result := execregexpr('(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$)', Self);
end;

function TStringSmartHelper.IsURL: boolean;
const
  _REGEX_ISURL = '(http|https|ftp):\/\/([a-zA-Z0-9-]+)?(\/)?(.*)?$';
begin
  Result := preg_match(_REGEX_ISURL, Self);
end;

function TStringSmartHelper.IsDomain: boolean;
begin
  Result := False;
  if Self.IsEmpty then
    Exit;
  Result := execregexpr('^((\w+)\.)?(([\w-]+)?)(\.[\w-]+){1,2}$', Self);
end;

function TStringSmartHelper.isDate(ADelimiter: string): boolean;
begin
  if Self.IsEmpty then
    Result := False
  else
    Result := execregexpr('^[0-9]{2}\'+ADelimiter+'[0-9]{2}\'+ADelimiter+'[0-9]{4}$', Self);
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

function TStringSmartHelper.IsPregMatch(ARegex: string): boolean;
begin
  Result := common.preg_match(ARegex, Self);
end;

function TStringSmartHelper.Split(ADelimiter: string): TStrings;
begin
  Result := Self.Explode(ADelimiter);
end;

function TStringSmartHelper.StrPos(AText: string): integer;
begin
  Result := Pos( AText, Self);
end;

function TStringSmartHelper.Quoted(AQuote: char): string;
begin
  result := AnsiQuotedStr(Self, AQuote);
end;

end.


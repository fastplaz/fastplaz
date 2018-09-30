{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit currencyibacor_integration;

{$mode objfpc}{$H+}
{
  [x] USAGE

  with TCurrencyIbacorIntegration.Create do
  begin
    Token := 'yourtoken;
    Result := Converter( 'USD', 'IDR', 1));
    Free;
  end;

}

interface

uses
  common, http_lib, fpjson, logutil_lib,
  Classes, SysUtils;

type

  { TCurrencyIbacorIntegration }

  TCurrencyIbacorIntegration = class
  private
    FDebug: boolean;
    FToken: string;
    jsonData: TJSONData;
  public
    constructor Create;
    destructor Destroy; override;

    property Token: string read FToken write FToken;
    function Converter(FromCurrency, ToCurrency: string; Value: integer): string;
  published
    property Debug: boolean read FDebug write FDebug;
  end;

implementation

const
  CURRENCY_IBACOR_URL =
    'http://ibacor.com/api/currency-converter?k=%s&view=convert&from=%s&to=%s&amount=%d';

var
  Response: IHTTPResponse;

{ TCurrencyIbacorIntegration }

constructor TCurrencyIbacorIntegration.Create;
begin
  FDebug := False;
end;

destructor TCurrencyIbacorIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
  inherited Destroy;
end;

function TCurrencyIbacorIntegration.Converter(FromCurrency, ToCurrency: string;
  Value: integer): string;
var
  _url: string;
  _http: THTTPLib;
  _nominalFloat: double;
begin
  Result := '';
  if FToken = '' then
    Exit;
  _url := Format(CURRENCY_IBACOR_URL, [FToken, FromCurrency, ToCurrency, Value]);

  _http := THTTPLib.Create(_url);
  Response := _http.Get;
  _http.Free;

  if Response.ResultCode <> 200 then
  begin
    LogUtil.Add(Response.ResultText, 'IBACOR');
    Result := '';
    Exit;
  end;

  try
    jsonData := GetJSON( Response.ResultText);
    if jsonGetData(jsonData, 'status') = 'success' then
    begin
      Result := UpperCase(FromCurrency) + ' ' + i2s(Value) + ' = ' +
        UpperCase(ToCurrency) + ' ';

      _nominalFloat:= s2f( jsonGetData(jsonData, 'data/to/amount'));
      DefaultFormatSettings.ThousandSeparator := '.';
      Result := Result + FormatFloat('###,#0', _nominalFloat);
    end;
  except
    on E: Exception do
    begin
      Result := '';
      if FDebug then
      begin
        LogUtil.Add(E.Message, 'IBACOR');
        LogUtil.Add(Response.ResultText, 'IBACOR');
      end;
    end;
  end;
end;

end.

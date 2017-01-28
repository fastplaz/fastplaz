{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit currencyibacor_integration;

{$mode objfpc}{$H+}
{
  [x] USAGE


}

interface

uses
  common, http_lib, json_lib, fpjson,
  Classes, SysUtils;

type

  { TCurrencyIbacorIntegration }

  TCurrencyIbacorIntegration = class
  private
    FToken: string;
  public
    constructor Create;
    destructor Destroy;

    property Token: string read FToken write FToken;
    function Converter(FromCurrency, ToCurrency: string; Value: integer): string;
  end;

implementation

const
  _CURRENCY_IBACOR_URL =
    'http://ibacor.com/api/currency-converter?k=%s&view=convert&from=%s&to=%s&amount=%d';

var
  Response: IHTTPResponse;

{ TCurrencyIbacorIntegration }

constructor TCurrencyIbacorIntegration.Create;
begin

end;

destructor TCurrencyIbacorIntegration.Destroy;
begin

end;

function TCurrencyIbacorIntegration.Converter(FromCurrency, ToCurrency: string;
  Value: integer): string;
var
  _url: string;
  _http: THTTPLib;
  _json: TJSONUtil;
  d : double;
begin
  if FToken = '' then
    Exit;
  _url := Format(_CURRENCY_IBACOR_URL, [FToken, FromCurrency, ToCurrency, Value]);

  _http := THTTPLib.Create(_url);
  Response := _http.Get;
  _http.Free;

  _json := TJSONUtil.Create;
  try
    _json.LoadFromJsonString(Response.ResultText);
    if _json['status'] = 'success' then
    begin
      d := s2f( _json['data/to/amount']);
      Result := UpperCase(FromCurrency) + ' ' + i2s(Value) + ' = ' +
        UpperCase(ToCurrency) + ' ';
      //ThousandSeparator := '.';
      //Result := Result + FormatFloat('###,##0', d);
      Result := Result + _json['data/to/amount'];
    end;

    //Result := FormatFloat('###,##0', d);


  except
  end;
  _json.Free;

end;

end.

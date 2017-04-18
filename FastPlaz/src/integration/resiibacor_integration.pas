{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit resiibacor_integration;

{$mode objfpc}{$H+}
{
  JNE , TIKI , POS , PANDU , JNT , SICEPAT dan WAHANA

  [x] USAGE
  with TResiIbacorController.Create do
  begin
    Token := 'YourIbacorToken';
    Vendor := 'JNE';
    AirwayBill := 'yourairwaybillnumber;
    Result := Find();
    Free;
  end;

}

interface

uses
  http_lib, json_lib, fpjson,
  Classes, SysUtils;

type

  { TResiIbacorController }

  TResiIbacorController = class
  private
    FCode: string;
    FToken: string;
    FVendor: string;
    procedure setAirwayBill(AValue: string);
  public
    constructor Create;
    destructor Destroy;

    property Token: string read FToken write FToken;
    property Vendor: string read FVendor write FVendor;
    property Code: string read FCode write FCode;
    property AirwayBill: string write setAirwayBill;

    function Find(): string;
    function Find(VendorName, CodeName: string): string;
  end;


implementation

const
  _RESI_IBACOR_API = 'http://ibacor.com/api/cek-resi?k=%s&pengirim=%s&resi=%s';

var
  Response: IHTTPResponse;

{ TResiIbacorController }

procedure TResiIbacorController.setAirwayBill(AValue: string);
begin
  FCode := AValue;
end;

constructor TResiIbacorController.Create;
begin

end;

destructor TResiIbacorController.Destroy;
begin

end;

function TResiIbacorController.Find: string;
begin
  Result := Find(FVendor, FCode);
end;

function TResiIbacorController.Find(VendorName, CodeName: string): string;
var
  s, urlTarget: string;
  httpClient: THTTPLib;
  json: TJSONUtil;
begin
  Result := '';
  if FToken = '' then
    Exit;
  FVendor := UpperCase(FVendor);
  urlTarget := Format(_RESI_IBACOR_API, [FToken, FVendor, FCode]);

  httpClient := THTTPLib.Create;
  httpClient.URL := urlTarget;
  Response := httpClient.Get;
  httpClient.Free;

  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(Response.ResultText);
    s := json['status'];
    if s = 'success' then
    begin
      Result := 'Pengiriman :';
      Result := Result + '\nStatus: ' + json['data/detail/status'];
      Result := Result + '\nService: ' + json['data/detail/service'];
      Result := Result + '\nTanggal: ' + json['data/detail/tanggal'];
      Result := Result + '\nPengirim: ' + json['data/detail/asal/nama'] + ' (' +
        json['data/detail/asal/alamat'] + ')';
      Result := Result + '\nTujuan: ' + json['data/detail/tujuan/nama'] +
        ' (' + json['data/detail/tujuan/alamat'] + ')';
    end;
  except
    Result := '';
  end;

  json.Free;
end;

end.

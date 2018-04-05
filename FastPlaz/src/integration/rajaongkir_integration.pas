{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
{
  get from API RajaOngkir
  https://rajaongkir.com

  // USAGE:

  Ongkir := TRajaOngkirIntegration.Create;
  Result := Ongkir.Find('your_airwaybill');
  Ongkir.Free;

}
unit rajaongkir_integration;

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  common,
  http_lib, fpjson,
  Classes, SysUtils;

{$ifdef RAJAONGKIR_INTEGRATION}
{$endif}

type

  TRajaOngkirAccountType = (atStarter, atBasic, atPro);


  { TRajaOngkirIntegration }

  TRajaOngkirIntegration = class(TInterfacedObject)
  private
    jsonData: TJSONData;
    FAccountType: TRajaOngkirAccountType;
    FIsSuccessfull: boolean;
    FKey: string;
    FURL: string;
    function getAccountType: TRajaOngkirAccountType;
    function getResultText: string;
    procedure setAccountType(AValue: TRajaOngkirAccountType);
    function TrackAsJson(CourierName, WayBillNumber: string): string;
  public
    constructor Create;
    destructor Destroy;
    function Track(CourierName, WayBillNumber: string): string;
  published
    property AccountType: TRajaOngkirAccountType
      read getAccountType write setAccountType;
    property URL: string read FURL write FURL;
    property Key: string read FKey write FKey;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property ResultText: string read getResultText;
  end;

implementation

const
  RAJAONGKIR_BASEURL_STARTER = 'https://api.rajaongkir.com/starter/';
  RAJAONGKIR_BASEURL_BASIC = 'https://api.rajaongkir.com/basic/';
  RAJAONGKIR_BASEURL_PRO = 'https://pro.rajaongkir.com/api/';

  PATH_STATUS_CODE = 'rajaongkir/status/code';
  PATH_STATUS_DESCRIPTION = 'rajaongkir/status/description';
  PATH_ISDELIVERED = 'rajaongkir/result/delivered';
  PATH_COURIERNAME = 'rajaongkir/result/summary/courier_name';
  PATH_SERVICECODE = 'rajaongkir/result/summary/service_code';
  PATH_WAYBILLDATE = 'rajaongkir/result/summary/waybill_date';
  PATH_SHIPPERNAME = 'rajaongkir/result/summary/shipper_name';
  PATH_ORIGIN = 'rajaongkir/result/summary/origin';
  PATH_DESTIONATION = 'rajaongkir/result/summary/destination';
  PATH_DELIVERY_STATUS = 'rajaongkir/result/delivery_status/status';
  PATH_DELIVERED_DATE = 'rajaongkir/result/delivery_status/pod_date';
  PATH_DELIVERED_TIME = 'rajaongkir/result/delivery_status/pod_time';
  PATH_RECEIVER_NAME = 'rajaongkir/result/summary/receiver_name';

var
  Response: IHTTPResponse;


{ TRajaOngkirIntegration }

function TRajaOngkirIntegration.TrackAsJson(CourierName, WayBillNumber: string): string;
var
  s, return, urlTarget: string;
  httpClient: THTTPLib;
begin
  Result := '';
  if ((FKey = '') or (CourierName = '') or (WayBillNumber = '')) then
    Exit;

  urlTarget := FURL + 'waybill';
  httpClient := THTTPLib.Create(urlTarget);
  httpClient.ContentType := 'application/x-www-form-urlencoded';
  httpClient.AddHeader('key', FKey);
  httpClient.FormData['waybill'] := WayBillNumber;
  httpClient.FormData['courier'] := CourierName;
  Response := httpClient.Post;
  httpClient.Free;

  if Response.ResultCode = 200 then
    Result := Response.ResultText;
end;

function TRajaOngkirIntegration.getAccountType: TRajaOngkirAccountType;
begin
  Result := FAccountType;
end;

function TRajaOngkirIntegration.getResultText: string;
begin
  Result := Response.ResultText;
end;

procedure TRajaOngkirIntegration.setAccountType(AValue: TRajaOngkirAccountType);
begin
  FAccountType := AValue;
  FURL := RAJAONGKIR_BASEURL_STARTER;
  if FAccountType = atBasic then
    FURL := RAJAONGKIR_BASEURL_BASIC;
  if FAccountType = atPro then
    FURL := RAJAONGKIR_BASEURL_PRO;
end;

constructor TRajaOngkirIntegration.Create;
begin
  FIsSuccessfull := False;
  AccountType := atStarter;
  FKey := '';
end;

destructor TRajaOngkirIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
  inherited Destroy;
end;

function TRajaOngkirIntegration.Track(CourierName, WayBillNumber: string): string;
begin
  Result := '';
  Result := TrackAsJson(CourierName, WayBillNumber);

  if not IsJsonValid(Result) then
    Exit;
  jsonData := GetJSON(Result);

  Result := jsonGetData(jsonData, PATH_STATUS_CODE);
  if Result <> '200' then
  begin
    Result := jsonGetData(jsonData, PATH_STATUS_DESCRIPTION);
    Exit;
  end;

  Result := 'No Resi: ' + UpperCase(WayBillNumber);
  if jsonGetData(jsonData, PATH_ISDELIVERED) = 'True' then
    Result := Result + #13'*Status: Terkirim*';
  Result := Result + #13 + jsonGetData(jsonData, PATH_COURIERNAME);
  Result := Result + #13 + 'Service: ' + jsonGetData(jsonData, PATH_SERVICECODE);
  Result := Result + #13 + 'Tanggal: ' + jsonGetData(jsonData, PATH_WAYBILLDATE);
  Result := Result + #13 + 'Pengirim: ' + jsonGetData(jsonData, PATH_SHIPPERNAME);
  Result := Result + #13 + 'Asal: ' + jsonGetData(jsonData, PATH_ORIGIN);
  Result := Result + #13 + 'Tujuan: ' + jsonGetData(jsonData, PATH_DESTIONATION);

  if jsonGetData(jsonData, PATH_DELIVERY_STATUS) = 'DELIVERED' then
  begin
    Result := Result + #13 + 'Diterima oleh: ' + jsonGetData(jsonData,
      PATH_RECEIVER_NAME);
    Result := Result + #13 + 'Tanggal: ' + jsonGetData(jsonData,
      PATH_DELIVERED_DATE) + ' ' + jsonGetData(jsonData, PATH_DELIVERED_TIME);
  end;
end;


end.

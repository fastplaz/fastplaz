{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit googlegeocoding_integration;

{$mode objfpc}{$H+}

{

  [x] Usage
  with TGoogleGeocodingIntegration.Create do
  begin
    Key := 'yourkey';
    Search('banyumanik, semarang'); // output as json

    lat := Latitude
    lng := Longitude;

    Free;
  end;

  [x] Reg
  Google Geocoding
  https://developers.google.com/maps/documentation/geocoding/intro?hl=en_US


}

interface

uses
  fpjson,
  common, http_lib, logutil_lib,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

type


  { TGoogleGeocodingIntegration }

  TGoogleGeocodingIntegration = class(TInterfacedObject)
  private
    FData: TJSONData;
    FKey: string;
    FPlaceID: string;
    FResultCode: integer;
    FResultText: string;
    function getFormatedAddress: String;
    function getLatitude: Double;
    function getLongitude: Double;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    property Key: String read FKey write FKey;
    function Search(Keyword: string): string;
    function Detail(APlaceID: string): string;
  published
    property Data: TJSONData read FData write FData;
    property ResultCode: integer read FResultCode;
    property ResultText: String read FResultText;
    property PlaceID: String read FPlaceID write FPlaceID;

    property FormatedAddress: String read getFormatedAddress;
    property Latitude: Double read getLatitude;
    property Longitude: Double read getLongitude;
  end;


implementation

const
  _GOOGLE_GEOCODING_URL =
    'https://maps.googleapis.com/maps/api/geocode/json?key=%s&address=%s';
  _GOOGLE_PLACE_DETAIL_URL =
    'https://maps.googleapis.com/maps/api/place/details/json?key=%s&placeid=%s';
  _GOOGLE_MAPS_URL = 'https://www.google.com/maps/place/%s/@%s,%s';
  _GOOGLE_MAPS_PLACEID_URL = 'https://www.google.com/maps/place/?q=place_id:';
//_GOOGLE_MAPS_DIRECTION = 'https://www.google.co.id/maps/dir//%.10f,%.10f';

var
  Response: IHTTPResponse;

{ TGoogleGeocodingIntegration }

function TGoogleGeocodingIntegration.getFormatedAddress: String;
begin
  Result := '';
  if not Assigned(FData) then
    Exit;
  Result := jsonGetData(FData, 'results[0].formatted_address');;
end;

function TGoogleGeocodingIntegration.getLatitude: Double;
begin
  Result := 0;
  try
    Result := FData.GetPath('results[0].geometry.location.lat').AsFloat;
  except
  end;
end;

function TGoogleGeocodingIntegration.getLongitude: Double;
begin
  Result := 0;
  try
    Result := FData.GetPath('results[0].geometry.location.lng').AsFloat;
  except
  end;
end;

constructor TGoogleGeocodingIntegration.Create;
begin
end;

destructor TGoogleGeocodingIntegration.Destroy;
begin
end;

function TGoogleGeocodingIntegration.Search(Keyword: string): string;
var
  _url: string;
begin
  Result := '';
  FPlaceID := '';
  if FKey = '' then
    Exit;

  _url := format(_GOOGLE_GEOCODING_URL, [FKey, UrlEncode(Keyword)]);
  with THTTPLib.Create(_url) do
  begin
    try
      //AddHeader('Cache-Control', 'no-cache');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      if Response.ResultCode <> 200 then
        Exit;
      Result := Response.ResultText;
      FData := GetJSON(Response.ResultText);
      FPlaceID := jsonGetData(FData, 'results[0].place_id');;
    except
    end;
    Free;
  end;
end;

function TGoogleGeocodingIntegration.Detail(APlaceID: string): string;
var
  _url: string;
begin
  Result := '';
  if APlaceID = '' then
    Exit;
  if FKey = '' then
    Exit;

  _url := format(_GOOGLE_PLACE_DETAIL_URL, [FKey, UrlEncode(APlaceID)]);
  with THTTPLib.Create(_url) do
  begin
    try
      //AddHeader('Cache-Control', 'no-cache');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      if Response.ResultCode <> 200 then
        Exit;

      FData := GetJSON(Response.ResultText);
      Result := Response.ResultText;
    except
    end;
    Free;
  end;

end;

end.

{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit googleplacesearch_integration;

{$mode objfpc}{$H+}

{
  Google Place Search
  https://developers.google.com/places/web-service/search?hl=id


}

interface

uses
  fpjson,
  common, http_lib, logutil_lib,
  Classes, SysUtils;

type


  { TGooglePlaceIntegration }

  TGooglePlaceIntegration = class(TInterfacedObject)
  private
    FAddress: string;
    FCount: integer;
    FData: TJSONData;
    FKey: string;
    FLatitude: double;
    FLongitude: double;
    FPlaceID: String;
    FResultCode: integer;
    FResultText: string;
    FTitle: string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    property Key: string read FKey write FKey;
    function Search(Keyword: string; ALat: double = 0; ALon: double = 0): string;
    function SearchAsText(Keyword: string; ALat: double = 0; ALon: double = 0): string;
    function Detail(APlaceID:String):String;
  published
    property Data: TJSONData read FData write FData;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Count: integer read FCount write FCount;
    property Title: string read FTitle write FTitle;
    property Address: string read FAddress write FAddress;
    property Latitude: double read FLatitude write FLatitude;
    property Longitude: double read FLongitude write FLongitude;
    property PlaceID: String read FPlaceID write FPlaceID;
  end;


implementation

const
  _GOOGLE_PLACE_TEXTSEARCH_URL =
    'https://maps.googleapis.com/maps/api/place/textsearch/json?key=%s&rankBy=distance&query=%s';
  _GOOGLE_PLACE_DETAIL_URL =
    'https://maps.googleapis.com/maps/api/place/details/json?key=%s&placeid=%s';
  _GOOGLE_MAPS_URL = 'https://www.google.com/maps/place/%s/@%s,%s';
  _GOOGLE_MAPS_PLACEID_URL = 'https://www.google.com/maps/place/?q=place_id:';
  //_GOOGLE_MAPS_DIRECTION = 'https://www.google.co.id/maps/dir//%.10f,%.10f';

var
  Response: IHTTPResponse;

{ TGooglePlaceIntegration }

constructor TGooglePlaceIntegration.Create;
begin
  FCount := 0;
end;

destructor TGooglePlaceIntegration.Destroy;
begin

end;

function TGooglePlaceIntegration.Search(Keyword: string; ALat: double;
  ALon: double): string;
var
  _url: string;
begin
  Result := '';
  FPlaceID := '';
  if FKey = '' then
    Exit;

  _url := format(_GOOGLE_PLACE_TEXTSEARCH_URL, [FKey, UrlEncode(Keyword)]);
  if (ALat <> 0) and (ALon <> 0) then
  begin
    _url := _url + '&location=' + FloatToStr(ALat) + ',' + FloatToStr(ALon);
  end;
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
    except
    end;
    Free;
  end;
end;

function TGooglePlaceIntegration.SearchAsText(Keyword: string;
  ALat: double; ALon: double): string;

var
  s, _name, _lat, _lon, _url: string;
  i, j: integer;
  _json: TJSONData;
begin
  Result := '';
  s := Search(Keyword, ALat, ALon);
  if s = '' then
    Exit;
  try
    _json := GetJSON(s);
    FCount := _json.GetPath('results').Count;
    s := '';
    if FCount > 4 then
      FCount := 4;
    for i := 0 to FCount - 1 do
    begin
      if i = 0 then
      begin
        FPlaceID := jsonGetData(_json, 'results[' + i2s(i) + '].place_id');
        FTitle := jsonGetData(_json, 'results[' + i2s(i) + '].name');
        FAddress := jsonGetData(_json, 'results[' + i2s(i) + '].formatted_address');
        FLatitude := _json.GetPath('results[' + i2s(i) +
          '].geometry.location.lat').AsFloat;
        FLongitude := _json.GetPath('results[' + i2s(i) +
          '].geometry.location.lng').AsFloat;
      end;
      _name := _json.GetPath('results[' + i2s(i) + '].name').AsString;
      _lat := Format('%.16f', [_json.GetPath('results[' + i2s(i) +
        '].geometry.location.lat').AsFloat]);
      _lon := Format('%.16f', [_json.GetPath('results[' + i2s(i) +
        '].geometry.location.lng').AsFloat]);
      s := s + '*' + _name + '*'#10;
      s := s + _json.GetPath('results[' + i2s(i) + '].formatted_address').AsString + #10;
      try
        if _json.GetPath('results[' + i2s(i) + '].opening_hours.open_now').AsBoolean then
        begin
          s := s + 'Saat ini buka.'#10;
        end;
      except
      end;
      if i = 0 then
      begin
        try
          if Detail(FPlaceID) <> '' then
          begin
            j := DayOfWeek(Now)-2;
            if j = -1 then
              j := 6;
            if (jsonGetData(Data, 'result/opening_hours/periods['+i2s(j)+']/open/time') <> '')
              and (jsonGetData(Data, 'result/opening_hours/periods['+i2s(j)+']/close/time') <> '') then
            begin
              s := s + 'Jam buka: ';
              s := s + jsonGetData(Data, 'result/opening_hours/periods['+i2s(j)+']/open/time');
              s := s + ' - ' + jsonGetData(Data, 'result/opening_hours/periods['+i2s(j)+']/close/time');
              s := s + #10;
            end;
          end;
        except
        end;
      end;

      try
        s := s + 'rating: ' + f2s(_json.GetPath('results[' + i2s(i) +
          '].rating').AsFloat) + #10;
      except
      end;

      //_url := format(_GOOGLE_MAPS_URL, [UrlEncode(_name), _lat, _lon]);
      _url := _GOOGLE_MAPS_PLACEID_URL + _json.GetPath('results[' + i2s(i) + '].place_id').AsString;
      s := s + _url + #10;

      s := s + #10;
    end;

  except
    on E: Exception do
    begin
    end;
  end;

  s := StringReplace(s, #13, '\n', [rfReplaceAll]);
  s := StringReplace(s, #10, '\n', [rfReplaceAll]);
  Result := s;
end;

function TGooglePlaceIntegration.Detail(APlaceID: String): String;
var
  _url : String;
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

      FData := GetJSON( Response.ResultText);
      Result := Response.ResultText;
    except
    end;
    Free;
  end;

end;

end.

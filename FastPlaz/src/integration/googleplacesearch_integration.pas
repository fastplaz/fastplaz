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
  common, json_lib, http_lib, logutil_lib,
  Classes, SysUtils;

type


  { TGooglePlaceIntegration }

  TGooglePlaceIntegration = class(TInterfacedObject)
  private
    FAddress: string;
    FCount: integer;
    FKey: string;
    FLatitude: double;
    FLongitude: double;
    FTitle: string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    property Key: string read FKey write FKey;
    function Search(Keyword: string; ALat: double = 0; ALon: double = 0): string;
    function SearchAsText(Keyword: string; ALat: double = 0; ALon: double = 0): string;
  published
    property Count: integer read FCount write FCount;
    property Title: string read FTitle write FTitle;
    property Address: string read FAddress write FAddress;
    property Latitude: double read FLatitude write FLatitude;
    property Longitude: double read FLongitude write FLongitude;
  end;


implementation

const
  _GOOGLE_PLACE_TEXTSEARCH_URL =
    'https://maps.googleapis.com/maps/api/place/textsearch/json?key=%s&query=%s';
  _GOOGLE_MAPS_URL = 'https://www.google.com/maps/place/%s/@%s,%s';
  _GOOGLE_MAPS_DIRECTION = 'https://www.google.co.id/maps/dir//%.10f,%.10f';

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
  if FKey = '' then
    Exit;

  _url := format(_GOOGLE_PLACE_TEXTSEARCH_URL, [FKey, UrlEncode(Keyword)]);
  if (ALat <> 0) and (ALon <> 0) then
  begin
    _url := _url + '&location=' + FloatToStr(ALat) + ',' + FloatToStr(ALon);
  end;
  LogUtil.Add(_url, 'GOOGLEPLACE');
  with THTTPLib.Create(_url) do
  begin
    try
      //AddHeader('Cache-Control', 'no-cache');
      Response := Get;
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
  i: integer;
  _json: TJSONData;
begin
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
      if FCount = 1 then
      begin
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
      s := s + 'rating: ' + f2s(_json.GetPath('results[' + i2s(i) +
        '].rating').AsFloat) + #10;

      _url := format(_GOOGLE_MAPS_URL, [UrlEncode(_name), _lat, _lon]);
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

end.

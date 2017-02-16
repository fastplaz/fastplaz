unit zomato_integration;

{


  [x] REF:
  Zomato API
  https://developers.zomato.com/documentation

}
{$mode objfpc}{$H+}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson, jsonparser,
  Classes, SysUtils;

type

  { TZomatoIntegration }

  TZomatoIntegration = class(TInterfacedObject)
  private
    FKey: string;
    FResultCode: integer;
    FResultText: string;
    jsonData: TJSONData;
    function getData(APath: string): string;
    function getDataAsFloat(APath: string): double;
  public
    constructor Create;
    destructor Destroy;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Key: string read FKey write FKey;
    function Search(AKeyword: string; ALat: double = 0; ALon: double = 0;
      ACount: integer = 4): string;
  end;

implementation

const
  ZOMATO_API_URL = 'https://developers.zomato.com/api/v2.1/';
  _GOOGLE_MAPS_URL = 'https://www.google.com/maps/place/%s/@%.10f,%.10f';
  _GOOGLE_MAPS_DIRECTION = 'https://www.google.co.id/maps/dir//%.10f,%.10f';

var
  Response: IHTTPResponse;

{ TZomatoIntegration }

function TZomatoIntegration.getData(APath: string): string;
begin
  Result := '';
  try
    Result := jsonData.GetPath(APath).AsString;
  except
  end;
end;

function TZomatoIntegration.getDataAsFloat(APath: string): double;
begin
  Result := 0;
  try
    Result := jsonData.GetPath(APath).AsFloat;
  except
  end;
end;

constructor TZomatoIntegration.Create;
begin

end;

destructor TZomatoIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TZomatoIntegration.Search(AKeyword: string; ALat: double;
  ALon: double; ACount: integer): string;
var
  i: integer;
  urlTarget, url: string;
  lat, lon: double;
begin
  Result := '';

  urlTarget := ZOMATO_API_URL + 'search?count=' + i2s(ACount) +
    '&radius=2000&q=' + UrlEncode(AKeyword);
  with THTTPLib.Create(urlTarget) do
  begin
    AddHeader('Cache-Control', 'no-cache');
    AddHeader('user-key', FKey);
    Response := Post;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      jsonData := GetJSON(FResultText);
      for i := 0 to ACount - 1 do
      begin
        lat := getDataAsFloat('restaurants[' + i2s(i) +
          '].restaurant.location.latitude');
        lon := getDataAsFloat('restaurants[' + i2s(i) +
          '].restaurant.location.longitude');
        Result := Result + '*' + getData('restaurants[' + i2s(i) +
          '].restaurant.name') + '*'#10;
        Result := Result + getData('restaurants[' + i2s(i) +
          '].restaurant.location.address') + #10;
        Result := Result + 'rating: ' + getData('restaurants[' +
          i2s(i) + '].restaurant.user_rating.aggregate_rating') + ' - ' +
          getData('restaurants[' + i2s(i) + '].restaurant.user_rating.rating_text') + #10;

        url := getData('restaurants[' + i2s(i) + '].restaurant.url');
        if (lat <> 0) and (lon <> 0) then
        begin
          try
            url := format(_GOOGLE_MAPS_DIRECTION, [lat, lon]);
          except
          end;
        end;
        Result := Result + url + #10;
        Result := Result + #10;
      end;
      Result := Trim(Result);
    end;
    Free;
  end;

end;

end.

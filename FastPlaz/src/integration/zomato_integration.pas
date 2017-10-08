unit zomato_integration;

{


  [x] REF:
  Zomato API
  https://developers.zomato.com/documentation

}
{$mode objfpc}{$H+}

interface

uses
  common, http_lib, logutil_lib,
  fpjson, jsonparser,
  Classes, SysUtils;

type

  { TZomatoIntegration }

  TZomatoIntegration = class(TInterfacedObject)
  private
    FEntityID: integer;
    FEntityType: string;
    FKey: string;
    FResultCode: integer;
    FResultText: string;
    FURL: string;
    jsonData: TJSONData;
    function getData(APath: string): string;
    function getDataAsFloat(APath: string): double;
  public
    constructor Create;
    destructor Destroy;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;
    property URL: string read FURL write FURL;

    property Key: string read FKey write FKey;
    property EntityType: string read FEntityType write FEntityType;
    property EntityID: integer read FEntityID write FEntityID;
    function ConvertJsonToTextInfo(AJson: string): string;
    function SearchAsJson(AKeyword: string; ALat: double = 0;
      ALon: double = 0; ACount: integer = 5): string;
    function Search(AKeyword: string; ALat: double = 0; ALon: double = 0;
      ACount: integer = 4): string;
  end;

implementation

const
  ZOMATO_API_URL = 'https://developers.zomato.com/api/v2.1/';
  //_GOOGLE_MAPS_URL = 'https://www.google.com/maps/place/%s/@%.10f,%.10f';
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
  FEntityType := '';
  FEntityID := 94; //Indonesia
  FURL := ZOMATO_API_URL;
end;

destructor TZomatoIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TZomatoIntegration.ConvertJsonToTextInfo(AJson: string): string;
var
  i: integer;
  s: string;
  jData: TJSONData;
begin
  Result := '';
  if AJson = '' then
    Exit;

  try
    jData := GetJSON(AJson);
    for i := 0 to jData.Count - 1 do
    begin
      Result := Result + '*' + jsonGetData(jData, '[' + i2s(i) + ']/name') + '*' + #10;
      Result := Result + 'Rating: ' + jsonGetData(jData, '[' + i2s(i) +
        ']/rating') + #10;
      Result := Result + jsonGetData(jData, '[' + i2s(i) + ']/address') + #10;

      s := jsonGetData(jData, '[' + i2s(i) + ']/url');
      if Pos('?', s) > 0 then
        s := copy(s, 0, Pos('?', s) - 1);
      if jsonGetData(jData, '[' + i2s(i) + ']/maps') <> '' then
        Result := Result + jsonGetData(jData, '[' + i2s(i) + ']/maps') + #10
      else
        Result := Result + s + #10;

      Result := Result + #10;
    end;

  except;
  end;
end;

function TZomatoIntegration.SearchAsJson(AKeyword: string; ALat: double;
  ALon: double; ACount: integer): string;
var
  i: integer;
  urlTarget: string;
  lat, lon: double;
  restaurants: TJSONArray;
  restaurant: TJSONObject;
begin
  Result := '';
  urlTarget := FURL + 'search?count=' + i2s(ACount) +
    '&radius=2000&entity_type=' + FEntityType + '&entity_id=' +
    i2s(FEntityID) + '&q=' + UrlEncode(AKeyword);

  if (ALat <> 0) and (ALon <> 0) then
  begin
    urlTarget := urlTarget + '&lat=' + FloatToStr(ALat);
    urlTarget := urlTarget + '&lon=' + FloatToStr(ALon);
  end;

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
      restaurants := TJSONArray.Create;
      i := s2i(getData('results_shown'));
      if i < ACount then
        ACount := i;
      for i := 0 to ACount - 1 do
      begin
        restaurant := TJSONObject.Create;
        restaurant.Strings['name'] :=
          getData('restaurants[' + i2s(i) + '].restaurant.name');
        restaurant.Strings['address'] :=
          getData('restaurants[' + i2s(i) + '].restaurant.location.address') +
          ', ' + getData('restaurants[' + i2s(i) + '].restaurant.location.locality');
        restaurant.Strings['url'] :=
          getData('restaurants[' + i2s(i) + '].restaurant.url');
        restaurant.Strings['rating'] :=
          getData('restaurants[' + i2s(i) +
          '].restaurant.user_rating.aggregate_rating') + ' - ' +
          getData('restaurants[' + i2s(i) + '].restaurant.user_rating.rating_text');
        restaurant.Strings['image'] :=
          getData('restaurants[' + i2s(i) + '].restaurant.featured_image');
        restaurant.Strings['thumb'] :=
          getData('restaurants[' + i2s(i) + '].restaurant.thumb');
        lat := getDataAsFloat('restaurants[' + i2s(i) +
          '].restaurant.location.latitude');
        lon := getDataAsFloat('restaurants[' + i2s(i) +
          '].restaurant.location.longitude');
        restaurant.Floats['lat'] := lat;
        restaurant.Floats['lon'] := lon;

        if (lat <> 0) and (lon <> 0) then
        begin
          try
            restaurant.Strings['maps'] := format(_GOOGLE_MAPS_DIRECTION, [lat, lon]);
          except
          end;
        end;


        restaurants.Add(restaurant);
      end;

      if ACount > 0 then
        Result := restaurants.AsJSON;
      restaurants.Free;
    end;

    Free;
  end;

end;

function TZomatoIntegration.Search(AKeyword: string; ALat: double;
  ALon: double; ACount: integer): string;
var
  i: integer;
  urlTarget: string;
  lat, lon: double;
begin
  Result := '';

  urlTarget := ZOMATO_API_URL + 'search?count=' + i2s(ACount) +
    '&radius=2000&q=' + UrlEncode(AKeyword);
  if (ALat <> 0) and (ALon <> 0) then
  begin
    urlTarget := urlTarget + '&lat=' + FloatToStr(ALat);
    urlTarget := urlTarget + '&lon=' + FloatToStr(ALon);
  end;
  LogUtil.Add(urlTarget, 'ZOMATO');
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
      i := s2i(getData('results_shown'));

      if i < ACount then
        ACount := i;
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
          i2s(i) + '].restaurant.user_rating.aggregate_rating') +
          ' - ' + getData('restaurants[' + i2s(i) +
          '].restaurant.user_rating.rating_text') + #10;

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

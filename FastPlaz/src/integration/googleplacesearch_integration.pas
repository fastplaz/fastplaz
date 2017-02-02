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
  common, json_lib, http_lib,
  Classes, SysUtils;

type


  { TGooglePlace }

  TGooglePlace = class(TInterfacedObject)
  private
    FKey: string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    property Key: string read FKey write FKey;
    function Search(Keyword: string): string;
    function SearchAsText(Keyword: string): string;
  end;


implementation

const
  _GOOGLE_PLACE_TEXTSEARCH_URL =
    'https://maps.googleapis.com/maps/api/place/textsearch/json?key=%s&query=%s';
  _GOOGLE_MAPS_URL = 'https://www.google.com/maps/place/@%s/%s,%s';

var
  Response: IHTTPResponse;

{ TGooglePlace }

constructor TGooglePlace.Create;
begin

end;

destructor TGooglePlace.Destroy;
begin

end;

function TGooglePlace.Search(Keyword: string): string;
var
  _url: string;
begin
  Result := '';
  if FKey = '' then
    Exit;

  _url := format(_GOOGLE_PLACE_TEXTSEARCH_URL, [FKey, UrlEncode(Keyword)]);
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

function TGooglePlace.SearchAsText(Keyword: string): string;
var
  s, _name, _lat, _lon, _url: string;
  i: integer;
  _json: TJSONData;
  _jArray: TJSONArray;
begin
  s := Search(Keyword);
  if s = '' then
    Exit;
  try
    _json := GetJSON(s);
    s := '';
    for i := 0 to 3 do
    begin
      _name := _json.GetPath('results[' + i2s(i) + '].name').AsString;
      _lat := Format('%.16f', [_json.GetPath('results[' + i2s(i) + '].geometry.location.lat').AsFloat]);
      _lon := Format('%.16f', [_json.GetPath('results[' + i2s(i) + '].geometry.location.lng').AsFloat]);
      s := s + '*' + _name + '*'#10;
      s := s + _json.GetPath('results[' + i2s(i) + '].formatted_address').AsString + #10;
      s := s + 'rating: ' + f2s(_json.GetPath('results[' + i2s(i) +
        '].rating').AsFloat) + #10;

      _url := format( _GOOGLE_MAPS_URL, [_name, _lat, _lon]);
      s := s + _url + #10;

      s := s + #10;
    end;

  except
    on E:Exception do
    begin
      s := s + E.Message;
    end;
  end;

  Result := s;
end;

end.

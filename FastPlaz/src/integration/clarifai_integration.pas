{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit clarifai_integration;
{
  [x] USAGE

  _img := TClarifai.Create;
  _img.Token:= 'yourtokenkey';
  _img.ImageURL:= 'http://imageurl';
  Result := _img.GetTagsAsString;


  _img.Free;


}
{$mode objfpc}{$H+}

interface

uses
  fpjson,
  common, json_lib, http_lib,
  Classes, SysUtils;

type

  { TClarifai }

  TClarifai = class(TInterfacedObject)
  private
    FCommand: string;
    FImageURL: string;
    FToken: string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    property Command: string read FCommand write FCommand;
    property ImageURL: string read FImageURL write FImageURL;
    property Token: string read FToken write FToken;

    function GetTags: string;
    function GetTagsAsString: string;
  end;

implementation

const
  _CLARIFAI_URL = 'https://api.clarifai.com/v1/';

var
  Response: IHTTPResponse;

{ TClarifai }

constructor TClarifai.Create;
begin

end;

destructor TClarifai.Destroy;
begin

end;

function TClarifai.GetTags: string;
var
  _url: string;
begin
  Result := '';
  if (FImageURL = '') or (FToken = '') then
    Exit;

  _url := _CLARIFAI_URL + 'tag/';
  with THTTPLib.Create(_url) do
  begin
    try
      //ContentType := 'application/x-www-form-urlencoded';
      //AddHeader('Cache-Control', 'no-cache');
      AddHeader('Authorization', 'Bearer ' + FToken);
      FormData['url'] := trim(FImageURL);
      FormData['t'] := '-';

      Response := Post;
      Result := Response.ResultText;
    except
    end;

    Free;
  end;

end;

function TClarifai.GetTagsAsString: string;
var
  s: string;
  _json: TJSONUtil;
  _jsonData: TJSONData;
  _jArray: TJSONArray;
begin
  Result := '';
  if FToken = '' then
    Exit;
  s := GetTags;
  if s = '' then
    Exit;

  {
  _json := TJSONUtil.Create;
  _json.LoadFromJsonString(s);
  if _json['status_code'] = 'OK' then
  begin
    Result := _json['results/result/tag/classes'];
  end;
  _json.Free;
  }

  _jsonData := GetJSON(s);
  try
    s := _jsonData.GetPath('status_code').AsString;
    if s = 'OK' then
    begin
      s := _jsonData.GetPath('results[0].result.tag.classes').AsJSON;
      Result := ReplaceAll(s, ['[', ']', '"'], '');
    end;

  except
  end;
  _jsonData.Free;
end;

end.
{
CURL Manual:

curl "https://api.clarifai.com/v1/tag/"   -X POST --data-urlencode "url=https://samples.clarifai.com/metro-north.jpg"   -H "Authorization: Bearer tokenkey"
}







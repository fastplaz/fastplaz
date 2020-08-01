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
  common, http_lib, logutil_lib,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

type

  { TClarifai }

  TClarifai = class(TInterfacedObject)
  private
    FClientID: string;
    FClientSecret: string;
    FCommand: string;
    FImageURL: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
    procedure SetImageURL(AValue: string);
  public
    constructor Create;
    destructor Destroy;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Command: string read FCommand write FCommand;
    property ImageURL: string read FImageURL write SetImageURL;
    property Token: string read FToken write FToken;

    function GetTags(ADownloadFile: boolean = False): string;
    function GetTagsAsString(ADownloadFile: boolean = False): string;
    function RequestTokenAsJson: string;
    function RequestToken: string;
  published
    property ClientID: string read FClientID write FClientID;
    property ClientSecret: string read FClientSecret write FClientSecret;
  end;

implementation

const
  _CLARIFAI_URL = 'https://api.clarifai.com/v1/';

var
  Response: IHTTPResponse;

{ TClarifai }

procedure TClarifai.SetImageURL(AValue: string);
begin
  if FImageURL = AValue then
    Exit;
  FImageURL := Trim(AValue);
end;

constructor TClarifai.Create;
begin
  FToken := '';
  FClientID := '';
  FClientSecret := '';
end;

destructor TClarifai.Destroy;
begin

end;

function TClarifai.GetTags(ADownloadFile: boolean): string;
var
  _url, tmpFile: string;
begin
  Result := '';
  if (FImageURL = '') or (FToken = '') then
    Exit;

  if ADownloadFile then
  begin
    tmpFile := _CACHE_PATH + UrlEncode(FImageURL) + '.jpg';
    if not DownloadFile(FImageURL, tmpFile) then
    begin
      Exit;
    end;
  end;

  _url := _CLARIFAI_URL + 'tag/';
  with THTTPLib.Create(_url) do
  begin
    try
      //ContentType := 'application/x-www-form-urlencoded';
      //AddHeader('Cache-Control', 'no-cache');
      AddHeader('Authorization', 'Bearer ' + FToken);
      if ADownloadFile then
        AddFile(tmpFile, 'encoded_data')
      else
        FormData['url'] := trim(FImageURL);
      FormData['t'] := '-';

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      Result := Response.ResultText;
    except
      on E: Exception do
      begin
        LogUtil.Add(E.Message, 'clarifai');
      end;
    end;

    Free;
  end;

  if FileExists(tmpFile) then
    DeleteFile(tmpFile);
end;

function TClarifai.GetTagsAsString(ADownloadFile: boolean): string;
var
  s: string;
  _jsonData: TJSONData;
begin
  Result := '';
  if FToken = '' then
    Exit;
  s := GetTags(ADownloadFile);
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
    end
    else
    begin
      LogUtil.Add(s, 'CLARIFAI');
    end;
  except
    on E: Exception do
    begin
      LogUtil.Add(E.Message, 'CLARIFAI');
    end;
  end;
  _jsonData.Free;
end;

{
curl -X POST "https://api.clarifai.com/v1/token/" \
    -d "client_id={client_id}" \
    -d "client_secret={client_secret}" \
    -d "grant_type=client_credentials"
}
function TClarifai.RequestTokenAsJson: string;
var
  urlTarget: string;
begin
  Result := '';
  if (FClientID = '') or (FClientSecret = '') then
    Exit;

  urlTarget := _CLARIFAI_URL + 'token/';
  with THTTPLib.Create(urlTarget) do
  begin
    try
      //AddHeader('Authorization', 'Bearer ' + FToken);
      FormData['client_id'] := FClientID;
      FormData['client_secret'] := FClientSecret;
      FormData['grant_type'] := 'client_credentials';
      FormData['client_type'] := 'confidential';
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      Result := Response.ResultText;
    except
      on E: Exception do
      begin
        LogUtil.Add(E.Message, 'clarifai');
      end;
    end;

    Free;
  end;
end;

function TClarifai.RequestToken: string;
var
  s: string;
  _jsonData: TJSONData;
begin
  Result := '';
  s := RequestTokenAsJson;
  if s = '' then
    Exit;
  try
    _jsonData := GetJSON(s);
    Result := _jsonData.GetPath('access_token').AsString;
  finally
  end;
end;

end.
{
CURL Manual:

curl "https://api.clarifai.com/v1/tag/"   -X POST --data-urlencode "url=https://samples.clarifai.com/metro-north.jpg"   -H "Authorization: Bearer tokenkey"
}






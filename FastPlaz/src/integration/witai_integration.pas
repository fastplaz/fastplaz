{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit witai_integration;

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson,
  Classes, SysUtils;

type

  { TWitAiIntegration }

  TWitAiIntegration = class(TInterfacedObject)
  private
    FContentType: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Token: string read FToken write FToken;
    property ContentType: string read FContentType write FContentType;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    function Speech(FileName: string): string;
    function SpeechToText(FileName: string): string;
  end;

implementation

const
  _WITAI_SPEECH_URL = 'https://api.wit.ai/speech';
  _WITAI_CONTENTTYPE_MPEG = 'audio/mpeg';

var
  Response: IHTTPResponse;

{ TWitAI }

constructor TWitAiIntegration.Create;
begin
  FContentType := _WITAI_CONTENTTYPE_MPEG;
end;

destructor TWitAiIntegration.Destroy;
begin
end;

function TWitAiIntegration.Speech(FileName: string): string;
begin
  Result := '';
  if FToken = '' then
    Exit;

  with THTTPLib.Create(_WITAI_SPEECH_URL) do
  begin
    //ContentType := 'application/x-www-form-urlencoded';
    //AddHeader('Cache-Control', 'no-cache');
    AddHeader('Authorization', 'Bearer ' + FToken);
    AddHeader('Content-Type', FContentType);

    try
      RequestBody := TFileStream.Create(FileName, fmOpenRead);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      Result := FResultText;
    except
      on E: Exception do
      begin
        LogUtil.Add(E.Message, 'WITAI');
      end;
    end;

    Free;
  end;
end;

function TWitAiIntegration.SpeechToText(FileName: string): string;
var
  json: TJSONUtil;
begin
  Result := Speech(FileName);
  if Result = '' then
    Exit;

  json := TJSONUtil.Create;
  json.LoadFromJsonString(Result);
  Result := json['_text'];
  if Result = 'null' then
    Result := '';
  json.Free;
end;

end.


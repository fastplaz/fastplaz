unit yandextranslate_integration;

{ DEPRECATED: Yandex Translate API v1.5 discontinued. The unit stays for existing callers; every request to the
  old endpoint fails. }

{

[x] USAGE

  // TRANSLATE
  with TYandexTranslateIntegration.Create do
  begin
    Key := 'youryandexkey';
    Result := Translate('en-id', 'how are you?'); // translate from English to Indonesia
    Free;
  end;

  // DETECT LANGUAGE
  with TYandexTranslateIntegration.Create do
  begin
    Key := 'youryandexkey;
    Hint := 'en,id'; // <--- optional
    Result := Detect('kalimat yang akan dideteksi');
    Free;
  end;


}

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, logutil_lib,
  fpjson,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

type

  { TYandexTranslateIntegration }

  TYandexTranslateIntegration = class(TInterfacedObject)
  private
    FCache: boolean;
    FCallback: string;
    FFormat: string;
    FHint: string;
    FKey: string;
    FResultCode: integer;
    FResultText: string;
    jsonData: TJSONData;
    function generateQuery: string;
    function getData(APath: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Key: string read FKey write FKey;
    property Hint: string read FHint write FHint;
    property Format: string read FFormat write FFormat;
    property Callback: string read FCallback write FCallback;

    function Detect(AText: string): string;
    function Translate(ALang, AText: string): string;
  published
    property Cache: boolean read FCache write FCache;
  end deprecated 'Yandex Translate API v1.5 discontinued';

implementation

const
  _YANDEX_DETECT_URL = 'https://translate.yandex.net/api/v1.5/tr.json/detect?key=';
  _YANDEX_TRANSLATE_URL = 'https://translate.yandex.net/api/v1.5/tr.json/translate?key=';

  TRANSLATE_CACHE_PATH = 'ztemp/cache/translate/';
  TRANSLATE_CACHE_EXTENSION = '.txt';


{ TYandexTranslateIntegration }

function TYandexTranslateIntegration.generateQuery: string;
begin
  Result := '';
end;

function TYandexTranslateIntegration.getData(APath: string): string;
begin
  Result := '';
  try
    Result := jsonData.GetPath(APath).AsString;
  except
  end;
end;

constructor TYandexTranslateIntegration.Create;
begin
  FKey := '';
  FHint := '';
  FCallback := '';
  FFormat := 'plain';
  FCache := False;
end;

destructor TYandexTranslateIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
  inherited Destroy;
end;

function TYandexTranslateIntegration.Detect(AText: string): string;
var
  Response: IHTTPResponse;
  url: string;
begin
  Result := '';
  if FKey = '' then
    Exit;
  if AText = '' then
    Exit;

  url := _YANDEX_DETECT_URL + FKey;
  if FHint <> '' then
    url := url + '&hint=' + FHint;
  if FCallback <> '' then
    url := url + '&callback=' + FCallback;
  url := url + '&text=' + UrlEncode(AText);

  with THTTPLib.Create(url) do
  try
    Response := Get;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if Response.ResultCode = 200 then
    begin
      FreeAndNil(jsonData);
      jsonData := GetJSON(Response.ResultText);
      Result := getData('lang');
    end;
  finally
    Free;
  end;

end;

function TYandexTranslateIntegration.Translate(ALang, AText: string): string;
var
  Response: IHTTPResponse;
  filename, url: string;
begin
  Result := '';
  if FKey = '' then
    Exit;
  if AText = '' then
    Exit;

  filename := TRANSLATE_CACHE_PATH + Trim(SafeText(StripNonAscii(AText))) +
    '-' + ALang + TRANSLATE_CACHE_EXTENSION;

  if FCache and FileExists(filename) then
  begin
    Result := LoadFromFile(filename);
    Exit;
  end;

  url := _YANDEX_TRANSLATE_URL + FKey;
  if FCallback <> '' then
    url := url + '&callback=' + FCallback;
  url := url + '&format=' + FFormat;
  url := url + '&lang=' + ALang;
  url := url + '&text=' + UrlEncode(AText);

  with THTTPLib.Create(url) do
  try
    Response := Get;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if Response.ResultCode = 200 then
    begin
      FreeAndNil(jsonData);
      jsonData := GetJSON(Response.ResultText);
      Result := getData('text[0]');

      if FCache then
        SaveToFile(filename, Result);
    end;
  finally
    Free;
  end;

end;

end.



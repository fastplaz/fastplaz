unit cognitiveocr_integration;

{
  Cognitive Computer Vision OCR
  https://www.microsoft.com/cognitive-services/en-us/computer-vision-api

  [x] USAGE
  with TCognitiveOCR.Create do
  begin
    Token:= 'yourcognitivetoken';
    Result := Scan( 'https://pbs.twimg.com/media/A_8C876CMAAzG7L.jpg');
    Free;
  end;


}

{$mode objfpc}{$H+}

interface

uses
  fpjson,
  common, http_lib, logutil_lib,
  Classes, SysUtils;

type

  { TCognitiveOCR }

  TCognitiveOCR = class(TInterfacedObject)
  private
    FDetectOrientation: boolean;
    FImageURL: string;
    FLanguage: string;
    FToken: string;
    jsonData: TJSONData;
    function generateURL: string;
    function getData(APath: string): string;

    function getWord(ARegion: integer; ALine: integer; AIndex: integer): string;
    function getWordsAtLine(ARegion: integer; ALine: integer): string;
    function getAllWords: string;
  public
    constructor Create;
    destructor Destroy;

    property Token: string read FToken write FToken;
    property Language: string read FLanguage write FLanguage;
    property ImageURL: string read FImageURL write FImageURL;
    property DetectOrientation: boolean read FDetectOrientation write FDetectOrientation;

    function Scan: string;
    function Scan(AImageURL: string): string;
    function ScanAsJSON(AImageURL: string): string;
  end;

implementation

const
  _COGNITIVE_OCR_URL =
    'https://westus.api.cognitive.microsoft.com/vision/v1.0/ocr?language=%s&detectOrientation=%s';

var
  Response: IHTTPResponse;


{ TCognitiveOCR }

function TCognitiveOCR.generateURL: string;
begin
  Result := 'false';
  if DetectOrientation then
    Result := 'true';
  Result := Format(_COGNITIVE_OCR_URL, [FLanguage, Result]);
end;

function TCognitiveOCR.getData(APath: string): string;
begin
  Result := jsonGetData(jsonData, APath);
end;

function TCognitiveOCR.getWord(ARegion: integer; ALine: integer;
  AIndex: integer): string;
begin
  Result := getData('regions[' + i2s(ARegion) + '].lines[' + i2s(ALine) +
    '].words[' + i2s(AIndex) + '].text');
  Result := SafeText(Result);
  Result := ReplaceAll(Result, ['»', ','], '', True);
  Result := Trim(Result);
end;

function TCognitiveOCR.getWordsAtLine(ARegion: integer; ALine: integer): string;
var
  i: integer;
  s: string;
begin
  Result := '';
  s := '';
  i := 0;
  repeat
    s := getWord(ARegion, ALine, i);
    Result := Result + ' ' + s;
    i := i + 1;
  until s = '';
  Result := Trim(Result);
end;

function TCognitiveOCR.getAllWords: string;
var
  i: integer;
  s: string;
begin
  Result := '';
  s := '';
  i := 0;
  repeat
    s := trim(getWordsAtLine(0, i));
    Result := Result + #10 + s;
    i := i + 1;
  until s = '';
  Result := Trim(Result);
end;

constructor TCognitiveOCR.Create;
begin
  FLanguage := 'en';
  FDetectOrientation := True;
end;

destructor TCognitiveOCR.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TCognitiveOCR.Scan: string;
begin
  Result := Scan(FImageURL);
end;

function TCognitiveOCR.Scan(AImageURL: string): string;
begin
  Result := ScanAsJSON(AImageURL);
  if Result = '' then
    Exit;

  jsonData := GetJSON(Result);
  Result := getAllWords;
end;

function TCognitiveOCR.ScanAsJSON(AImageURL: string): string;
var
  _body: string;
begin
  Result := '';
  if FToken = '' then
    Exit;
  if AImageURL = '' then
    Exit;

  with THTTPLib.Create(generateURL) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Ocp-Apim-Subscription-Key', FToken);
      _body := '{"url":"' + AImageURL + '"}';
      RequestBody := TStringStream.Create(_body);
      Response := Post;
      if Response.ResultCode = 200 then
      begin
        Result := Response.ResultText;
      end;
    except
      on E: Exception do
        LogUtil.Add(E.Message, 'OCR');
    end;
    Free;
  end;

end;

end.

unit cognitivedomainspecific_integration;
{
  Cognitive Computer Vision Domain Specific

  [x] USAGE
  with TCognitiveDomainSpecific.Create do
  begin
    Token := 'yourcognitivetoken;
    Model := 'celebrities';
    Result := Scan('imageURL');
    Free;
  end;

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

  { TCognitiveDomainSpecific }

  TCognitiveDomainSpecific = class(TInterfacedObject)
  private
    FDetails: string;
    FEndPoint: String;
    FFeatures: string;
    FImageURL: string;
    FResultText: String;
    FToken: string;
    jsonData: TJSONData;
    function generateURL: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Token: string read FToken write FToken;
    property ImageURL: string read FImageURL write FImageURL;

    function Scan: string;
    function Scan(AImageURL: string): string;
    function ScanAsJSON(AImageURL: string): string;
  published
    property ResultText: String read FResultText;
    property Features: string read FFeatures write FFeatures;
    property Details: string read FDetails write FDetails;
    property EndPoint: String read FEndPoint write FEndPoint;
  end;

implementation

const
  _COGNITIVE_OCR_URL =
    'https://%s.api.cognitive.microsoft.com/vision/v1.0/analyze?visualFeatures=%s&details=%s&language=en';

var
  Response: IHTTPResponse;

{ TCognitiveDomainSpecific }

function TCognitiveDomainSpecific.generateURL: string;
begin
  Result := Format(_COGNITIVE_OCR_URL, [FEndPoint, FFeatures, FDetails]);
end;

constructor TCognitiveDomainSpecific.Create;
begin
  FFeatures := 'Description,Faces,Categories,Tags,Adult';
  FDetails := 'celebrities';
  FEndPoint := 'southeastasia';
end;

destructor TCognitiveDomainSpecific.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TCognitiveDomainSpecific.Scan: string;
begin
  Result := Scan(FImageURL);
end;

function TCognitiveDomainSpecific.Scan(AImageURL: string): string;
begin
  Result := ScanAsJSON(AImageURL);
  if Result = '' then
    Exit;

  jsonData := GetJSON(Result);
  Result := jsonGetData(jsonData, 'result.celebrities[0].name') + ' ' + jsonGetData(jsonData, 'result.celebrities[1].name');
  Result := Trim( Result);
end;

function TCognitiveDomainSpecific.ScanAsJSON(AImageURL: string): string;
var
  _body: string;
begin
  Result := '';
  FResultText := '';
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
      FResultText := Response.ResultText;
      if Response.ResultCode = 200 then
      begin
        Result := Response.ResultText;
      end;
    except
      on E: Exception do
        LogUtil.Add(E.Message, 'OCRDOM');
    end;
    Free;
  end;

end;

end.


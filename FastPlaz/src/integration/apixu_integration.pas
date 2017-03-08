unit apixu_integration;

{
  Weather API and Geo
  https://www.apixu.com/

  [x] USAGE
  with TApixuIntegration.Create do
  begin
    Key := 'yourkey';
    Result := WeatherAsJson('jakarta, indonesia');

    Free;
  end;

}

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson, jsonparser, variants,
  Classes, SysUtils;

type

  { TApixuIntegration }

  TApixuIntegration = class(TInterfacedObject)
  private
    FKey: string;
    FResultCode: integer;
    FResultText: string;

    jsonData: TJSONData;
    function getValue(AVariable: string): string;
  public
    constructor Create;
    destructor Destroy;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Key: string read FKey write FKey;
    property Data[AVariable: string]: string read getValue;

    function GetDataFloat(AVariable: string): double;
    function WeatherAsJson(ACityName: string): string;
  end;


implementation

const
  _APIXU_API_URL = 'https://api.apixu.com/v1/current.json?key=%s&q=%s';

var
  Response: IHTTPResponse;

{ TApixuIntegration }

function TApixuIntegration.getValue(AVariable: string): string;
begin
  Result := '';
  try
    Result := jsonData.GetPath(AVariable).AsString;
  except
  end;
end;

constructor TApixuIntegration.Create;
begin
  FKey := '';

end;

destructor TApixuIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TApixuIntegration.GetDataFloat(AVariable: string): double;
begin
  Result := 0;
  try
    Result := jsonData.GetPath(AVariable).AsFloat;
  except
  end;
end;

function TApixuIntegration.WeatherAsJson(ACityName: string): string;
var
  urlTarget: string;
begin
  Result := '';
  if FKey = '' then
    Exit;

  urlTarget := Format(_APIXU_API_URL, [FKey, UrlEncode(ACityName)]);
  with THTTPLib.Create(urlTarget) do
  begin
    //AddHeader('Cache-Control', 'no-cache');
    Response := Get;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      jsonData := GetJSON(FResultText);
      Result := FResultText;
    end;
    Free;
  end;
end;

end.

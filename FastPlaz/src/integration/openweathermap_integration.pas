unit openweathermap_integration;

{
  OPEN WEATHER MAP
  http://openweathermap.org/

  [x] USAGE
  with TOpenWeatherMapIntegration.Create do
  begin
    Result := WeatherAsJson('jakarta,id');

    Free;
  end;

}
{$mode objfpc}{$H+}

interface

uses
  common, http_lib, logutil_lib,
  fpjson, jsonparser, variants,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

type

  { TOpenWeatherMapIntegration }

  TOpenWeatherMapIntegration = class(TInterfacedObject)
  private
    FKey: string;
    FLanguage: string;
    FMode: string;
    FResultCode: integer;
    FResultText: string;
    FUnits: string;

    jsonData: TJSONData;
    function getValue(AVariable: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Key: string read FKey write FKey;
    property Language: string read FLanguage write FLanguage;
    property Units: string read FUnits write FUnits;
    property Mode: string read FMode write FMode;
    property Data[AVariable: string]: string read getValue;

    function GetData(AVariable: string): variant;
    function GetDataFloat(AVariable: string): double;
    function WeatherAsJson(ACityName: string): string;
  end;

implementation

const
  //_OPENWEATHERMAP_API = 'http://api.openweathermap.org/data/2.5/';
  _OPENWEATHERMAP_API_WEATHER =
    'http://api.openweathermap.org/data/2.5/weather?appid=%s&lang=%s&units=%s&mode=%s&q=%s';



{ TOpenWeatherMapIntegration }

function TOpenWeatherMapIntegration.getValue(AVariable: string): string;
begin
  Result := '';
  try
    Result := jsonData.GetPath(AVariable).AsString;
  except
  end;
end;

constructor TOpenWeatherMapIntegration.Create;
begin
  FLanguage := 'id';
  FUnits := 'metric';
  FMode := 'json';
end;

destructor TOpenWeatherMapIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
  inherited Destroy;
end;

// the value at AVariable as a variant (string, number or boolean); empty
// when the path does not exist
function TOpenWeatherMapIntegration.GetData(AVariable: string): variant;
var
  v: TJSONData;
begin
  Result := '';
  if not Assigned(jsonData) then
    Exit;
  try
    v := jsonData.FindPath(AVariable);
    if Assigned(v) and not (v.JSONType in [jtNull, jtObject, jtArray]) then
      Result := v.Value;
  except
  end;
end;

function TOpenWeatherMapIntegration.GetDataFloat(AVariable: string): double;
begin
  Result := 0;
  try
    Result := jsonData.GetPath(AVariable).AsFloat;
  except
  end;
end;

function TOpenWeatherMapIntegration.WeatherAsJson(ACityName: string): string;
var
  Response: IHTTPResponse;
  urlTarget: string;
begin
  Result := '';
  if FKey = '' then
    Exit;
  urlTarget := Format(_OPENWEATHERMAP_API_WEATHER, [FKey, FLanguage,
    FUnits, FMode, UrlEncode(ACityName)]);
  with THTTPLib.Create(urlTarget) do
  try
    //AddHeader('Cache-Control', 'no-cache');
    Response := Get;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      if FMode = 'json' then
        FreeAndNil(jsonData);
        jsonData := GetJSON(FResultText, False);
      Result := FResultText;
    end;
  finally
    Free;
  end;
end;

end.

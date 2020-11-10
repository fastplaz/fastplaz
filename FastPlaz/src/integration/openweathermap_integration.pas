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
    destructor Destroy;

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

var
  Response: IHTTPResponse;


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
  FUnits := 'metrics';
  FMode := 'json';
end;

destructor TOpenWeatherMapIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TOpenWeatherMapIntegration.GetData(AVariable: string): variant;
begin
  die(VarType(AVariable));
  try
    case VarType(AVariable) of
      varstring:
      begin
        Result := '';
        Result := jsonData.GetPath(AVariable).AsString;
      end;
      vardecimal,
      vardouble:
      begin
        Result := jsonData.GetPath(AVariable).AsFloat;
      end;
    end;
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
  urlTarget: string;
begin
  Result := '';
  if FKey = '' then
    Exit;
  urlTarget := Format(_OPENWEATHERMAP_API_WEATHER, [FKey, FLanguage,
    FUnits, FMode, ACityName]);
  with THTTPLib.Create(urlTarget) do
  begin
    //AddHeader('Cache-Control', 'no-cache');
    Response := Get;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      if FMode = 'json' then
        jsonData := GetJSON(FResultText, False);
      Result := FResultText;
    end;
    Free;
  end;
end;

end.

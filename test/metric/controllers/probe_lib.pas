unit probe_lib;

{ Shared helpers of the metric probe controllers: the common JSON fields
  (process id, run mode, request number), JSON output and parameter parsing. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, HTTPDefs;

// pid, mode (cgi/http) and request_count of this process
function ProbeInfo: TJSONObject;
// sends AJson (and frees it) with status ACode
procedure SendJson(AResponse: TResponse; AJson: TJSONObject; ACode: integer = 200);
procedure SendText(AResponse: TResponse; const AText: string; ACode: integer = 200);
// unix time in milliseconds (UTC)
function NowMs: int64;
// integer query value limited to AMin..AMax, ADefault when missing or invalid
function ParamInt(const AValue: string; ADefault, AMin, AMax: integer): integer;

var
  // when this process started (uptime in /probe/stats)
  ProcessStartMs: int64;
  // requests seen by the probe controllers in this process; survives between
  // requests on purpose (CGI: always 1, standalone: grows)
  ProbeHits: integer = 0;

implementation

uses
  dateutils, fastplaz_app;

function ProbeInfo: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('pid', integer(GetProcessID));
  if IsStandalone then
    Result.Add('mode', 'http')
  else
    Result.Add('mode', 'cgi');
  Result.Add('request_count', RequestCount);
end;

procedure SetStatus(AResponse: TResponse; ACode: integer);
begin
  AResponse.Code := ACode;
  case ACode of
    200: AResponse.CodeText := 'OK';
    400: AResponse.CodeText := 'Bad Request';
    404: AResponse.CodeText := 'Not Found';
    500: AResponse.CodeText := 'Internal Server Error';
  end;
end;

procedure SendJson(AResponse: TResponse; AJson: TJSONObject; ACode: integer);
begin
  try
    SetStatus(AResponse, ACode);
    AResponse.ContentType := 'application/json';
    AResponse.Content := AJson.AsJSON;
  finally
    AJson.Free;
  end;
end;

procedure SendText(AResponse: TResponse; const AText: string; ACode: integer);
begin
  SetStatus(AResponse, ACode);
  AResponse.ContentType := 'text/plain; charset=utf-8';
  AResponse.Content := AText;
end;

function NowMs: int64;
begin
  Result := MilliSecondsBetween(LocalTimeToUniversal(Now), UnixEpoch);
end;

function ParamInt(const AValue: string; ADefault, AMin, AMax: integer): integer;
begin
  Result := StrToIntDef(Trim(AValue), ADefault);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

initialization
  ProcessStartMs := NowMs;

end.

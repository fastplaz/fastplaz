unit probe_sleep_controller;

{ GET /probe/sleep?ms=N: holds the request for N ms (0..30000, default 1000).
  Reports start and end time, so a client running requests in parallel can
  see whether they were served side by side or one after the other
  (head-of-line blocking), and how shutdown treats a running request. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeSleepController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  fpjson, probe_lib;

procedure TProbeSleepController.Get;
var
  json: TJSONObject;
  ms: integer;
  started: int64;
begin
  ms := ParamInt(_GET['ms'], 1000, 0, 30000);
  started := NowMs;
  Sleep(ms);
  json := ProbeInfo;
  json.Add('ms', ms);
  json.Add('started_ms', started);
  json.Add('finished_ms', NowMs);
  SendJson(Response, json);
end;

end.

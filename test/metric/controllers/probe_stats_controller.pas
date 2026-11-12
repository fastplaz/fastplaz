unit probe_stats_controller;

{ GET /probe/stats: process id, request count, uptime and heap usage.
  Polled during a long load test it shows memory growth per request
  (a leak) and confirms systems/server/max_requests restarts the process. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeStatsController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  fpjson, fastplaz_app, probe_lib;

procedure TProbeStatsController.Get;
var
  json: TJSONObject;
  heap: TFPCHeapStatus;
begin
  heap := GetFPCHeapStatus;
  json := ProbeInfo;
  json.Add('max_requests', MaxRequests);
  json.Add('uptime_ms', NowMs - ProcessStartMs);
  json.Add('heap_used', int64(heap.CurrHeapUsed));
  json.Add('heap_size', int64(heap.CurrHeapSize));
  json.Add('heap_free', int64(heap.CurrHeapFree));
  json.Add('probe_hits', ProbeHits);
  SendJson(Response, json);
end;

end.

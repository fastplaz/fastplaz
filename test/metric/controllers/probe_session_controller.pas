unit probe_session_controller;

{ GET /probe/session?op=inc|get|reset: a counter kept in the session.
  Run many "inc" requests in parallel with one session cookie: the final
  counter shows lost updates between processes (CGI, several standalone
  processes) or between requests of one process. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeSessionController = class(TMyCustomController)
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    procedure Get; override;
  end;

implementation

uses
  fpjson, probe_lib;

const
  COUNTER_KEY = 'probe_counter';

constructor TProbeSessionController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  CreateSession := True;
end;

procedure TProbeSessionController.Get;
var
  json: TJSONObject;
  op: string;
  counter: integer;
begin
  op := LowerCase(_GET['op']);
  if op = '' then
    op := 'inc';
  counter := StrToIntDef(Session[COUNTER_KEY], 0);
  if op = 'inc' then
  begin
    Inc(counter);
    Session[COUNTER_KEY] := IntToStr(counter);
  end
  else if op = 'reset' then
  begin
    counter := 0;
    Session[COUNTER_KEY] := '0';
  end
  else if op <> 'get' then
  begin
    json := ProbeInfo;
    json.Add('msg', 'unknown op, use inc, get or reset');
    SendJson(Response, json, 400);
    Exit;
  end;

  json := ProbeInfo;
  json.Add('op', op);
  json.Add('session_id', SessionID);
  json.Add('counter', counter);
  SendJson(Response, json);
end;

end.

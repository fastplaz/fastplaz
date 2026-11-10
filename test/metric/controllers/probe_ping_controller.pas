unit probe_ping_controller;

{ GET /probe/ping: plain "ok", no theme, no session, no database.
  The cheapest request the framework can serve (baseline for load tests). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbePingController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  probe_lib;

procedure TProbePingController.Get;
begin
  SendText(Response, 'ok');
end;

end.

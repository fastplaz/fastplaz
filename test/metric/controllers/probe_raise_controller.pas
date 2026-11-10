unit probe_raise_controller;

{ GET /probe/raise: an unhandled exception inside a module.
  Expected: an error response (500) and a server that keeps serving;
  the next request must be clean (check with /probe/state). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeRaiseController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  theme_controller;

procedure TProbeRaiseController.Get;
begin
  // left behind on purpose: the next request must not see it
  ThemeUtil.Assign('probe_mark', 'from-raise');
  Response.SetCustomHeader('X-Probe-Mark', 'from-raise');
  raise Exception.Create('probe: exception raised on purpose');
end;

end.

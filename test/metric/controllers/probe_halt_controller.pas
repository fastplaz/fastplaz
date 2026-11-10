unit probe_halt_controller;

{ GET /probe/halt: Die() in the middle of a handler.
  Die sends the response and stops the request (ends the process in CGI
  mode only); the code after it still runs. Expected body: "halted" only,
  never "after-die", and the next request must be clean. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeHaltController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  common, theme_controller;

procedure TProbeHaltController.Get;
begin
  ThemeUtil.Assign('probe_mark', 'from-halt');
  Response.ContentType := 'text/plain; charset=utf-8';
  Die('halted');
  // must not reach the client
  Response.Content := Response.Content + 'after-die';
end;

end.

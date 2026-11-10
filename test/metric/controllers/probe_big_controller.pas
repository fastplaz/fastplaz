unit probe_big_controller;

{ GET /probe/big?kb=N: a plain text body of N KB (1..10240, default 1024).
  For output buffering, transfer time and clients that disconnect early.
  The X-Probe-Size header carries the expected size in bytes. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeBigController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  probe_lib;

procedure TProbeBigController.Get;
const
  LINE = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+/'; // 64 bytes
var
  body: string;
  kb, i: integer;
begin
  kb := ParamInt(_GET['kb'], 1024, 1, 10240);
  SetLength(body, kb * 1024);
  for i := 0 to kb * 16 - 1 do
    Move(LINE[1], body[i * 64 + 1], 64);
  Response.SetCustomHeader('X-Probe-Size', IntToStr(Length(body)));
  SendText(Response, body);
end;

end.

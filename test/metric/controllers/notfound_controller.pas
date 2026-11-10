unit notfound_controller;

{ Every address no other route knows: a real 404.
  Registered last in routes.pas with the pattern ^/.+ ; JSON for API style
  callers (Accept: application/json or a non-GET method), text otherwise. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TNotFoundController = class(TMyCustomController)
  private
    procedure SendNotFound;
  public
    procedure Get; override;
    procedure Post; override;
    procedure Put; override;
    procedure Patch; override;
    procedure Delete; override;
  end;

implementation

uses
  fpjson, probe_lib;

procedure TNotFoundController.SendNotFound;
var
  json: TJSONObject;
begin
  if (not isGet) or (Pos('application/json', LowerCase(Request.Accept)) > 0) then
  begin
    json := TJSONObject.Create;
    json.Add('code', 404);
    json.Add('msg', 'not found');
    json.Add('path_info', Request.PathInfo);
    SendJson(Response, json, 404);
  end
  else
    SendText(Response, 'not found: ' + Request.PathInfo, 404);
end;

procedure TNotFoundController.Get;
begin
  SendNotFound;
end;

procedure TNotFoundController.Post;
begin
  SendNotFound;
end;

procedure TNotFoundController.Put;
begin
  SendNotFound;
end;

procedure TNotFoundController.Patch;
begin
  SendNotFound;
end;

procedure TNotFoundController.Delete;
begin
  SendNotFound;
end;

end.

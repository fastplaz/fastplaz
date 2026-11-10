unit probe_echo_controller;

{ /probe/echo (GET, POST, PUT, PATCH, DELETE): what the dispatcher handed to
  the module - method, path, query, a few headers and the body (length, MD5
  and the first 200 bytes), so a client can check large bodies arrive intact. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeEchoController = class(TMyCustomController)
  private
    procedure Echo;
  public
    procedure Get; override;
    procedure Post; override;
    procedure Put; override;
    procedure Patch; override;
    procedure Delete; override;
  end;

implementation

uses
  fpjson, md5, probe_lib;

procedure TProbeEchoController.Echo;
var
  json, query, headers: TJSONObject;
  body: string;
  i: integer;
begin
  json := ProbeInfo;
  json.Add('method', Request.Method);
  json.Add('path_info', Request.PathInfo);
  json.Add('query_string', Request.QueryString);

  query := TJSONObject.Create;
  for i := 0 to Request.QueryFields.Count - 1 do
    query.Add(Request.QueryFields.Names[i], Request.QueryFields.ValueFromIndex[i]);
  json.Add('query', query);

  headers := TJSONObject.Create;
  headers.Add('content-type', Request.ContentType);
  headers.Add('user-agent', Request.UserAgent);
  headers.Add('x-probe', Header['X-Probe']);
  json.Add('headers', headers);

  body := Request.Content;
  json.Add('body_length', Length(body));
  json.Add('body_md5', MD5Print(MD5String(body)));
  json.Add('body_head', Copy(body, 1, 200));
  SendJson(Response, json);
end;

procedure TProbeEchoController.Get;
begin
  Echo;
end;

procedure TProbeEchoController.Post;
begin
  Echo;
end;

procedure TProbeEchoController.Put;
begin
  Echo;
end;

procedure TProbeEchoController.Patch;
begin
  Echo;
end;

procedure TProbeEchoController.Delete;
begin
  Echo;
end;

end.

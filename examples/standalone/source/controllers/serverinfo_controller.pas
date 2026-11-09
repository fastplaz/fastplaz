unit serverinfo_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_app, HTTPDefs, fastplaz_handler;

type

  { TServerInfoController }

  // /info : the CGI variables as FastPlaz sees them, in either mode
  TServerInfoController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses common;

const
  VARIABLES: array[0..11] of string = (
    'REQUEST_METHOD', 'REQUEST_URI', 'PATH_INFO', 'QUERY_STRING',
    'SCRIPT_NAME', 'SERVER_NAME', 'SERVER_PORT', 'SERVER_PROTOCOL',
    'REMOTE_ADDR', 'HTTP_HOST', 'HTTP_USER_AGENT', 'HTTPS');

procedure TServerInfoController.Get;
var
  json, vars: TJSONObject;
  s: string;
begin
  json := TJSONObject.Create;
  try
    if IsStandalone then
      json.Add('mode', 'http')
    else
      json.Add('mode', 'cgi');
    json.Add('pid', GetProcessID);
    vars := TJSONObject.Create;
    for s in VARIABLES do
      vars.Add(s, _SERVER[s]);
    json.Add('server', vars);
    json.Add('get', _GET['q']);
    Response.ContentType := 'application/json';
    Response.Content := json.FormatJSON;
  finally
    json.Free;
  end;
end;

end.

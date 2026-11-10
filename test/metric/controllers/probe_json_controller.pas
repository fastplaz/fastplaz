unit probe_json_controller;

{ GET /probe/json: a small JSON object built with fpjson (JSON overhead). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeJsonController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  fpjson, probe_lib;

procedure TProbeJsonController.Get;
var
  json: TJSONObject;
  items: TJSONArray;
  i: integer;
begin
  json := ProbeInfo;
  json.Add('code', 0);
  json.Add('msg', 'ok');
  items := TJSONArray.Create;
  for i := 1 to 10 do
    items.Add(TJSONObject.Create(['id', i, 'name', 'item ' + IntToStr(i)]));
  json.Add('items', items);
  SendJson(Response, json);
end;

end.

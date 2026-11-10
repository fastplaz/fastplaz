unit probe_state_controller;

{ GET /probe/state?mark=X: request isolation check.

  First reports what the per-request state looks like on arrival ("before"),
  then stores the mark in that state. A fresh request must never see the
  mark of an earlier one: every "before" value has to be empty/false.
  "probe_hits" is process-wide on purpose (shows the process is reused). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeStateController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  fpjson, common, theme_controller, probe_lib;

const
  MARK_VAR = 'probe_mark';

// True when the template variable is assigned, whatever its value: the
// [if] processor drops the whole block for an unknown variable
function ThemeVarAssigned: boolean;
begin
  Result := Trim(ThemeUtil.RenderFromContent(nil,
    '[if ' + MARK_VAR + ' neq "~"]1[else]1[/if]')) = '1';
end;

procedure TProbeStateController.Get;
var
  json, before: TJSONObject;
  mark: string;
begin
  Inc(ProbeHits);
  mark := _GET['mark'];

  before := TJSONObject.Create;
  // template variable assigned by an earlier request
  before.Add('theme_var', ThemeVarAssigned);
  // database flag set by an earlier /probe/db request
  before.Add('use_database', AppData.useDatabase);
  // query value of an earlier request
  before.Add('get_leak', _GET['leak']);
  // response header set by an earlier request
  before.Add('response_header', Response.GetCustomHeader('X-Probe-Mark'));

  // store the mark everywhere it could leak from
  ThemeUtil.Assign(MARK_VAR, mark);
  Response.SetCustomHeader('X-Probe-Mark', mark);

  json := ProbeInfo;
  json.Add('mark', mark);
  json.Add('probe_hits', ProbeHits);
  json.Add('before', before);
  // proves the template variable check works: must be true
  json.Add('theme_var_after', ThemeVarAssigned);
  SendJson(Response, json);
end;

end.

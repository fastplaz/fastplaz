unit probe_db_controller;

{ GET /probe/db?op=insert|count|reset[&mark=X]: SQLite through database_lib.

  The table is created when missing. Parallel "insert" requests from several
  processes show lock handling (SQLITE_BUSY); "count" afterwards must equal
  the number of successful inserts. In standalone mode the connection is
  closed after every request unless systems/server/db_persistent is on. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

type
  TProbeDbController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

implementation

uses
  fpjson, database_lib, probe_lib;

const
  TABLE_NAME = 'probe_hits';
  CREATE_SQL = 'CREATE TABLE IF NOT EXISTS probe_hits ('
    + 'id INTEGER PRIMARY KEY AUTOINCREMENT, '
    + 'mark VARCHAR(100), pid INTEGER, created_ms INTEGER)';

procedure TProbeDbController.Get;
var
  json: TJSONObject;
  model: TSimpleModel;
  op: string;
  ok: boolean;
begin
  op := LowerCase(_GET['op']);
  if op = '' then
    op := 'insert';
  if (op <> 'insert') and (op <> 'count') and (op <> 'reset') then
  begin
    json := ProbeInfo;
    json.Add('msg', 'unknown op, use insert, count or reset');
    SendJson(Response, json, 400);
    Exit;
  end;

  DataBaseInit;
  json := ProbeInfo;
  json.Add('op', op);
  model := TSimpleModel.Create(TABLE_NAME, 'id');
  try
    ok := QueryExec(CREATE_SQL);
    if ok and (op = 'insert') then
    begin
      model.New;
      model['mark'] := _GET['mark'];
      model['pid'] := integer(GetProcessID);
      model['created_ms'] := NowMs;
      ok := model.Save;
      if ok then
        json.Add('id', model.LastInsertID)
      else
        json.Add('msg', model.Message);
    end
    else if ok and (op = 'reset') then
      ok := QueryExec('DELETE FROM ' + TABLE_NAME);

    if ok then
    begin
      ok := model.Select('COUNT(*) AS total').Open;
      if ok then
        json.Add('total', integer(model['total']));
    end;
  finally
    model.Free;
  end;

  if ok then
    SendJson(Response, json)
  else
    SendJson(Response, json, 500);
end;

end.

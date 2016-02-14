unit session_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

const
  SESSION_TABLENAME = 'session_info';
  SESSION_KEY = 'sessid';
  SESSION_FIELD_VARS = 'vars';
  SESSION_FIELD_UID = 'uid';
  SESSION_FIELD_IPADDR = 'ipaddr';
  SESSION_FIELD_LASTUSED = 'lastused';

type

  { TSessionModel }

  TSessionModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');

  end;

implementation

uses
  common;

constructor TSessionModel.Create(const DefaultTableName: string = '');
begin
  inherited Create( SESSION_TABLENAME); // table name = session_info
  primaryKey := SESSION_KEY;
end;

end.

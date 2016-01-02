unit grouppermission_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

type

  { TGroupPermissionModel }

  TGroupPermissionModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

uses
  common, fastplaz_handler;

const
  PERMISSION_TABLENAME = 'group_perms';


constructor TGroupPermissionModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(PERMISSION_TABLENAME); // table name = users
end;

end.


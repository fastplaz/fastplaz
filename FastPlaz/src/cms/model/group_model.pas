unit group_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

const
  GROUP_TABLE_NAME = 'groups';
  GROUP_DEFAULT_ID = 1;
  GROUP_DEFAULTNAME = 'Users';
  {$include '../../../define_cms.inc'}

type

  { TGroupModel }

  TGroupModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');

    function GetID(const GroupName: string): integer;
    function AddUserToGroup(UserID: integer;
      GroupID: integer = GROUP_DEFAULT_ID): boolean;
    function AddUserToGroup(UserID: integer;
      GroupName: string = GROUP_DEFAULTNAME): boolean;

    function SafeDelete(const GroupID: integer; DeleteBy: integer = 0): boolean;
  end;

implementation

uses
  common, fastplaz_handler, groupmembership_model;

constructor TGroupModel.Create(const DefaultTableName: string = '');
begin
  // an empty name made TSimpleModel derive "groups_util" from the class name
  if DefaultTableName = '' then
    inherited Create(GROUP_TABLE_NAME)
  else
    inherited Create(DefaultTableName);
  primaryKey := GROUP_FIELD_ID;
end;

function TGroupModel.GetID(const GroupName: string): integer;
begin
  Result := 0;
  if FindFirst([GROUP_FIELD_NAME + '=' + QuoteValue(GroupName)]) then
  begin
    Result := Value[GROUP_FIELD_ID];
  end;
end;

function TGroupModel.AddUserToGroup(UserID: integer; GroupID: integer): boolean;
begin
  Result := False;
  with TGroupMembershipModel.Create() do
  begin
    Result := Add(UserID, GroupID);
    // the counter only moves for a new membership
    if Result then
      QueryExec('UPDATE groups SET nbuser=(nbuser+1) WHERE gid = ' + i2s(GroupID));
    Free;
  end;
end;

function TGroupModel.AddUserToGroup(UserID: integer; GroupName: string): boolean;
var
  i: integer;
begin
  Result := False;
  i := GetID(GroupName);
  if i > 0 then
    Result := AddUserToGroup(UserID, i);
end;

function TGroupModel.SafeDelete(const GroupID: integer; DeleteBy: integer): boolean;

var
  sqlText: string;
begin
  sqlText := 'UPDATE groups SET deleted_date=CURRENT_TIMESTAMP, deleted_by=' + i2s(
    DeleteBy) + ' WHERE gid = ' + i2s(GroupID);
  Result := QueryExec(sqlText);
end;

end.

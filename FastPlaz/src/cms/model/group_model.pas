unit group_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

const
  GROUP_DEFAULT_ID = 1;
  GROUP_DEFAULTNAME = 'Users';
  {$include define_cms.inc}

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
  end;

implementation

uses
  common, fastplaz_handler, groupmembership_model;


constructor TGroupModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(DefaultTableName); // table name = users
end;

function TGroupModel.GetID(const GroupName: string): integer;
begin
  Result := 0;
  if FindFirst([GROUP_FIELD_NAME + '="' + GroupName + '"']) then
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
    QueryExec( 'UPDATE groups SET nbuser=(nbuser+1) WHERE gid = ' + i2s(GroupID));
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

end.

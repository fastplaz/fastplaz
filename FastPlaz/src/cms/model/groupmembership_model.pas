unit groupmembership_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

type

  { TGroupMembershipModel }

  TGroupMembershipModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
    function Add(UserID: integer; GroupID: integer = 1): boolean;
  end;

implementation

uses
  common, fastplaz_handler;

const
  MEMBERSHIP_TABLENAME = 'group_membership';
  MEMBERSHIP_FIELD_USERID = 'uid';
  MEMBERSHIP_FIELD_GROUPID = 'gid';

{ TGroupMembershipModel }

constructor TGroupMembershipModel.Create(const DefaultTableName: string);
begin
  inherited Create(MEMBERSHIP_TABLENAME); // table name = users
end;

function TGroupMembershipModel.Add(UserID: integer; GroupID: integer): boolean;
begin
  Result := False;
  if not FindFirst([MEMBERSHIP_FIELD_USERID + '=' + i2s(UserID),
    MEMBERSHIP_FIELD_GROUPID + '=' + i2s(GroupID)]) then
  begin
    New;
    SetFieldValue(MEMBERSHIP_FIELD_USERID, UserID);
    SetFieldValue(MEMBERSHIP_FIELD_GROUPID, GroupID);
    Result := Save();
  end;
end;


end.

unit grouppermission_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

const
  {$include '../../../define_cms.inc'}

type

  { TGroupPermissionModel }

  TGroupPermissionModel = class(TSimpleModel)
  private
    function fixSecurityString(Security: string): string;
  public
    constructor Create(const DefaultTableName: string = '');

    function Add(GroupID: integer; Compnent, Instance: string;
      Level: integer = 0): boolean;

    function getAuthInfo(UserID: integer): boolean;
    function getSecurityLevel(UserID: integer; Component: string;
      Instance: string): integer;
  end;

implementation

uses
  common, fastplaz_handler;

const
  PERMISSION_TABLENAME = 'group_perms';
  PERMISSION_FIELD_ID = 'pid';
  PERMISSION_FIELD_GROUPID = 'gid';
  PERMISSION_FIELD_SEQUENCE = 'sequence';
  PERMISSION_FIELD_REALM = 'realm';
  PERMISSION_FIELD_COMPONENT = 'component';
  PERMISSION_FIELD_INSTANCE = 'instance';
  PERMISSION_FIELD_LEVEL = 'level';
  PERMISSION_FIELD_BOND = 'bond';


function TGroupPermissionModel.fixSecurityString(Security: string): string;
begin
  if Security = '' then
    Security := '.*';
  if Pos(Security, ':') = 1 then
    Security := '.*' + Security;
  Security := StringReplace(Security, '::', ':.*:', [rfReplaceAll]);
  if (pos(Security, ':') = (Length(Security) - 1)) then
    Security := '.*';
  Result := Security;
end;

constructor TGroupPermissionModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(PERMISSION_TABLENAME); // table name = users
  primaryKey := PERMISSION_FIELD_ID;
end;

function TGroupPermissionModel.Add(GroupID: integer; Compnent, Instance: string;
  Level: integer): boolean;
var
  sequence: integer;
begin
  Result := False;
  sequence := 0;
  if Data.Active then
    Data.Close;
  Data.SQL.Text := 'SELECT MAX( sequence)+1 AS sequence FROM ' + PERMISSION_TABLENAME;
  Data.Open;
  if RecordCount > 0 then
    sequence := Value[PERMISSION_FIELD_SEQUENCE];

  New;
  SetFieldValue(PERMISSION_FIELD_GROUPID, GroupID);
  SetFieldValue(PERMISSION_FIELD_SEQUENCE, sequence);
  SetFieldValue(PERMISSION_FIELD_REALM, 0);
  SetFieldValue(PERMISSION_FIELD_COMPONENT, Compnent);
  SetFieldValue(PERMISSION_FIELD_INSTANCE, Instance);
  SetFieldValue(PERMISSION_FIELD_LEVEL, Level);
  SetFieldValue(PERMISSION_FIELD_BOND, 0);
  if Save() then
    Result := True;
end;

{
  get auth information list of the user
}
function TGroupPermissionModel.getAuthInfo(UserID: integer): boolean;
var
  s: string;
begin
  Result := False;
  if Data.Active then
    Data.Close;
  Data.SQL.Text :=
    'select m.gid, p.sequence, p.component, p.instance, p.level, g.name,state from group_membership m '
    + #13'join group_perms p on p.gid=m.gid ' + #13'join groups g on m.gid=g.gid ' +
    #13'where m.uid=%d ' + #13'order by sequence';
  Data.SQL.Text := Format(Data.SQL.Text, [UserId]);
  Data.Open;
  if RecordCount > 0 then
  begin
    Result := True;
  end;
end;

{
example:
  userID := 2;
  secLevel := getSecurityLevel( userID, 'modulname', 'list');
}
function TGroupPermissionModel.getSecurityLevel(UserID: integer;
  Component: string; Instance: string): integer;
var
  i, level: integer;
  s, c: string;
begin
  Result := ACCESS_INVALID;

  // if "::" --> ""
  if Component = StringOfChar(':', Length(Component)) then
    Component := '';
  if Instance = StringOfChar(':', Length(Instance)) then
    Instance := '';

  if not getAuthInfo(UserID) then
    Exit;

  // generic permission
  if (Component = '') and (Instance = '') then
  begin
    // Looking for best permission
    level := 0;
    while not EOF do
    begin
      if Value[PERMISSION_FIELD_LEVEL] > level then
        level := Value[PERMISSION_FIELD_LEVEL];
      Next;
    end;
    Result := level;
    Exit;
  end;

  if UpperCase(Instance) = 'ANY' then
  begin
    First;
    level := 0;
    while not EOF do
    begin
      c := Value[PERMISSION_FIELD_COMPONENT];
      c := '^' + fixSecurityString(c) + '$';
      if not preg_match(c, Component) then
      begin
        Next;
        Continue;
      end;

      i := Value[PERMISSION_FIELD_LEVEL];
      if i > level then
        level := i;

      c := Value[PERMISSION_FIELD_INSTANCE];
      c := '^' + fixSecurityString(c) + '$';
      if (preg_match(c, '::') and preg_match(c, '')) then
      begin
        break;
      end;
    end;//while not EOF
    Result := level;
    Exit;
  end;// if UpperCase( Instance) = 'ANY'

  if Instance = '' then
  begin
    First;
    while not EOF do
    begin
      c := Value[PERMISSION_FIELD_COMPONENT];
      c := '^' + fixSecurityString(c) + '$';
      if not preg_match(c, Component) then
      begin
        Next;
        Continue;
      end;

      c := Value[PERMISSION_FIELD_INSTANCE];
      c := '^' + fixSecurityString(c) + '$';
      if not (preg_match(c, '::') and preg_match(c, '')) then
      begin
        Next;
        Continue;
      end;

      level := Value[PERMISSION_FIELD_LEVEL];
      Result := level;
      break;
    end; // while not EOF
    Exit;
  end;// if Instance = ''

  //normal permission check
  First;
  while not EOF do
  begin
    c := Value[PERMISSION_FIELD_COMPONENT];
    c := '^' + fixSecurityString(c) + '$';
    if ((Component <> '') and (not preg_match(c, Component))) then
    begin
      Next;
      Continue;
    end;

    c := Value[PERMISSION_FIELD_INSTANCE];
    c := '^' + fixSecurityString(c) + '$';
    if (not preg_match(c, Instance)) then
    begin
      Next;
      Continue;
    end;

    level := Value[PERMISSION_FIELD_LEVEL];
    Result := level;
    break;
  end;// while not EOF

end;

end.



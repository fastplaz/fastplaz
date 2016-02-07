unit user_model;

{$mode objfpc}{$H+}

interface

uses
  common, security_util, database_lib, logutil_lib,
  Math, Classes, SysUtils;

const
  USER_GROUP_DEFAULT_ID = 1;
  USER_GROUP_DEFAULT_NAME = 'Users';

  USER_TABLE_NAME = 'users';
  USER_PRIMARY_KEY = 'uid';
  USER_FIELDNAME_ID = 'uid';
  USER_FIELDNAME_NAME = 'name';
  USER_FIELDNAME_USERNAME = 'uname';
  USER_FIELDNAME_PASSWORD = 'pass';
  USER_FIELDNAME_EMAIL = 'email';
  USER_FIELDNAME_ACTIVATED = 'activated';
  USER_FIELDNAME_REGDATE = 'user_regdate';
  USER_FIELDNAME_LASTLOGIN = 'lastlogin';

  {$include define_cms.inc}

type

  { TUserModel }

  TUserModel = class(TSimpleModel)
  private
    FSecUtil: TSecurityUtil;
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;

    function Add(const UserName, Email: string; Password: string = '';
      Params: TStrings = nil): integer;
    function Add(const Email: string; Password: string = '';
      Params: TStrings = nil): integer;
    function ChangePassword(const UserID: integer; const NewPassword: string): boolean;
    function AssignToGroup(const UserID: integer;
      const GroupID: integer = USER_GROUP_DEFAULT_ID): boolean;
    function AssignToGroup(const UserID: integer;
      const GroupName: string = USER_GROUP_DEFAULT_NAME): boolean;

    function Activate(const UserID: integer): boolean;
    function DeActivate(const UserID: integer): boolean;
    function Suspend(const UserID: integer): boolean;
    function SafeDelete(const UserID: integer; DeleteBy: integer = 0): boolean;

    function isActive(const UserID: integer): boolean;
    function isUserNameExists(const UserName: string): boolean;
    function isEmailExists(const EmailAddress: string): boolean;

    function GenerateUsername(const EmailAddress: string): string;
    function GenerateHashedPassword(const UnhashedPassword: string): string;
  end;

implementation

uses
  fastplaz_handler, group_util, permission_util;

constructor TUserModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(USER_TABLE_NAME); // table name = users
  FSecUtil := TSecurityUtil.Create;
  primaryKey := USER_PRIMARY_KEY;
end;

destructor TUserModel.Destroy;
begin
  FSecUtil.Free;
  inherited Destroy;
end;

function TUserModel.Add(const UserName, Email: string; Password: string;
  Params: TStrings): integer;
var
  saltedHash: string;
  i: integer;
begin
  Result := 0;
  if (UserName = '') or (Email = '') then
    Exit;

  //if user exists
  if FindFirst([USER_FIELDNAME_USERNAME + '="' + UserName + '" OR ' +
    USER_FIELDNAME_EMAIL + '="' + Email + '"'], USER_FIELDNAME_ID + ' desc') then
  begin
    Exit;
  end;

  if Password = '' then
    saltedHash := FSecUtil.GenerateSaltedHash(FSecUtil.GeneratePassword)
  else
    saltedHash := FSecUtil.GenerateSaltedHash(Password);

  New;
  SetFieldValue(USER_FIELDNAME_NAME, UserName);
  SetFieldValue(USER_FIELDNAME_USERNAME, UserName);
  SetFieldValue(USER_FIELDNAME_EMAIL, Email);
  SetFieldValue(USER_FIELDNAME_PASSWORD, saltedHash);
  SetFieldValue(USER_FIELDNAME_REGDATE, Now);

  //-- save params
  if Params <> nil then
  begin
    for i := 0 to Params.Count - 1 do
    begin
      SetFieldValue(Params.Names[i], Params.ValueFromIndex[i]);
    end;
  end;

  if Save() then
  begin
    Result := LastInsertID;
    //AssignToGroup(Result, USER_GROUP_DEFAULT_ID);
  end;

end;

function TUserModel.Add(const Email: string; Password: string;
  Params: TStrings): integer;
begin
  Result := Add(GenerateUsername(Email), Email, Password, Params);
end;

function TUserModel.ChangePassword(const UserID: integer;
  const NewPassword: string): boolean;
var
  saltedHash: string;
begin
  Result := False;
  saltedHash := FSecUtil.GenerateSaltedHash(NewPassword);
  if FindFirst([USER_FIELDNAME_ID + '="' + i2s(UserID) + '"'],
    USER_FIELDNAME_ID + ' desc') then
  begin
    New;
    SetFieldValue(USER_FIELDNAME_PASSWORD, saltedHash);
    if Save(USER_FIELDNAME_ID + '=' + i2s(UserID)) then
    begin
      Result := True;
    end;

  end;
end;

{
  example:
  UsersUtil.AssignToGroup( 24, 1);
  UsersUtil.AssignToGroup( 24, 'Users');
}
function TUserModel.AssignToGroup(const UserID: integer;
  const GroupID: integer): boolean;
begin
  Result := False;
  with TGroupsUtil.Create() do
  begin
    Result := AddUserToGroup(UserID, GroupID);
    Free;
  end;
end;

function TUserModel.AssignToGroup(const UserID: integer;
  const GroupName: string): boolean;
begin
  Result := False;
  with TGroupsUtil.Create() do
  begin
    Result := AddUserToGroup(UserID, GroupName);
    Free;
  end;
end;

function TUserModel.Activate(const UserID: integer): boolean;
begin
  Result := False;
  New;
  SetFieldValue(USER_FIELDNAME_ACTIVATED, 1);
  if Save(USER_FIELDNAME_ID + '=' + i2s(UserID)) then
  begin
    AssignToGroup(UserID, USER_GROUP_DEFAULT_NAME);
    Result := True;
  end;
end;

function TUserModel.DeActivate(const UserID: integer): boolean;
begin
  Result := False;
  New;
  SetFieldValue(USER_FIELDNAME_ACTIVATED, 0);
  if Save(USER_FIELDNAME_ID + '=' + i2s(UserID)) then
  begin
    Result := True;
  end;
end;

function TUserModel.Suspend(const UserID: integer): boolean;
begin
  Result := False;
  New;
  SetFieldValue(USER_FIELDNAME_ACTIVATED, 2);
  if Save(USER_FIELDNAME_ID + '=' + i2s(UserID)) then
  begin
    Result := True;
  end;
end;

function TUserModel.SafeDelete(const UserID: integer; DeleteBy: integer): boolean;
var
  sql : string;
begin
  sql := 'UPDATE users SET deleted_date=now(), deleted_by='+i2s(DeleteBy)+' WHERE uid = ' + i2s(UserID);
  Result := QueryExec( sql);
end;

function TUserModel.isActive(const UserID: integer): boolean;
begin
  Result := False;
  if UserID = 0 then
    Exit;
  Clear;
  if Find([USER_FIELDNAME_ID + '=' + i2s(UserID), USER_FIELDNAME_ACTIVATED + '=1']) then
    Result := True;
end;

function TUserModel.isUserNameExists(const UserName: string): boolean;
begin
  Result := False;
  if UserName = '' then
    Exit;
  Clear;
  if Find([USER_FIELDNAME_USERNAME + '=''' + UserName + '''']) then
    Result := True;
end;

function TUserModel.isEmailExists(const EmailAddress: string): boolean;
begin
  Result := False;
  if EmailAddress = '' then
    Exit;
  Clear;
  if Find([USER_FIELDNAME_EMAIL + '=''' + EmailAddress + '''']) then
    Result := True;
end;

function TUserModel.GenerateUsername(const EmailAddress: string): string;
var
  uname: string;
begin
  Result := '';
  if EmailAddress = '' then
    Exit;
  uname := Copy(EmailAddress, 1, Pos('@', EmailAddress) - 1);
  if Find([USER_FIELDNAME_USERNAME + ' LIKE ''' + uname + '%''']) then
  begin
    uname := uname + i2s(RecordCount + 1);
  end;

  Result := uname;
end;

function TUserModel.GenerateHashedPassword(const UnhashedPassword: string): string;
begin
  with TSecurityUtil.Create do
  begin
    Result := GenerateSaltedHash(UnhashedPassword);
    Free;
  end;
end;

end.

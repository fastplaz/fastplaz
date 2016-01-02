unit user_util;

{$mode objfpc}{$H+}

interface

uses
  security_util, user_model,
  fpcgi, common, Math, Classes, SysUtils;

const
  PASSWORD_LENGTH_MIN = 5;
  USER_GROUP_DEFAULT_ID = 1;
  USER_GROUP_DEFAULT_NAME = 'Users';

type

  { TUsersUtil }

  TUsersUtil = class(TUserModel)
  private
    FSecUtil: TSecurityUtil;
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;

    function Add(const UserName, Email: string; Password: string = '';
      Params: TStrings = nil): integer;
    function Delete(const UserID: integer): boolean; overload;
    function ChangePassword(const UserID: integer; const NewPassword: string): boolean;
    function AssignToGroup( const UserID:integer; const GroupID: integer = USER_GROUP_DEFAULT_ID):boolean;
    function AssignToGroup( const UserID:integer; const GroupName: string = USER_GROUP_DEFAULT_NAME):boolean;

    function GeneratePassword: string;
    function GenerateHashedPassword(const UnhashedPassword: string): string;

    function isLoggedIn: boolean;
    function Login(const Username: string; const Password: string;
      RememberMe: boolean = False): boolean;
    function Logout: boolean;
  end;

implementation

uses
  fastplaz_handler, group_util;

const
  USER_PRIMARY_KEY = 'uid';
  USER_FIELDNAME_ID = 'uid';
  USER_FIELDNAME_USERNAME = 'uname';
  USER_FIELDNAME_PASSWORD = 'pass';
  USER_FIELDNAME_EMAIL = 'email';
  USER_FIELDNAME_REGDATE = 'user_regdate';

{ TUsersUtil }

constructor TUsersUtil.Create(const DefaultTableName: string);
begin
  inherited Create;
  FSecUtil := TSecurityUtil.Create;
  primaryKey := USER_PRIMARY_KEY;
end;

destructor TUsersUtil.Destroy;
begin
  FSecUtil.Free;
  inherited Destroy;
end;

function TUsersUtil.Add(const UserName, Email: string; Password: string;
  Params: TStrings): integer;
var
  saltedHash: string;
  i: integer;
begin
  Result := 0;
  if (UserName = '') or (Email = '') then
    Exit;

  saltedHash := Password;
  if saltedHash = '' then
    saltedHash := GeneratePassword;
  saltedHash := FSecUtil.GenerateSaltedHash(saltedHash);

  New;
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
    AssignToGroup( Result, USER_GROUP_DEFAULT_ID);
  end;
end;

function TUsersUtil.Delete(const UserID: integer): boolean;
begin
  Result := Delete( USER_FIELDNAME_ID + '=' + i2s( UserID));
end;

function TUsersUtil.ChangePassword(const UserID: integer;
  const NewPassword: string): boolean;
var
  saltedHash: string;
begin
  Result := False;
  saltedHash := FSecUtil.GenerateSaltedHash(NewPassword);
  if FindFirst([USER_FIELDNAME_ID + '="' + i2s(UserID) + '"'],
    USER_FIELDNAME_ID + ' desc') then
  begin
    SetFieldValue(USER_FIELDNAME_PASSWORD, saltedHash);
    New;
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
function TUsersUtil.AssignToGroup(const UserID: integer; const GroupID: integer
  ): boolean;
begin
  with TGroupsUtil.Create() do
  begin
    Result := AddUserToGroup( UserID, GroupID);
    Free;
  end;
end;

function TUsersUtil.AssignToGroup(const UserID: integer; const GroupName: string
  ): boolean;
begin
  Result := False;
  with TGroupsUtil.Create() do
  begin
    Result := AddUserToGroup( UserID, GroupName);
    Free;
  end;
end;


function TUsersUtil.GeneratePassword: string;
var
  min, max: integer;
begin
  Randomize;
  min := RandomRange(PASSWORD_LENGTH_MIN, 15);
  max := RandomRange(PASSWORD_LENGTH_MIN + 3, 15);
  Result := RandomString(min, max, False, True, True, False, True, False, False, '');
end;

function TUsersUtil.GenerateHashedPassword(const UnhashedPassword: string): string;
begin
  with TSecurityUtil.Create do
  begin
    Result := GenerateSaltedHash(UnhashedPassword);
    Free;
  end;
end;

function TUsersUtil.isLoggedIn: boolean;
var
  uid: string;
begin
  Result := False;
  if SessionController.IsExpired then
  begin
    Logout;
    Exit;
  end;

  uid := _SESSION['uid'];
  if uid <> '' then  //-- simple check
    Result := True;
end;

function TUsersUtil.Login(const Username: string; const Password: string;
  RememberMe: boolean): boolean;
var
  hashedData: string;
begin
  Result := False;
  if FindFirst([USER_FIELDNAME_USERNAME + '="' + Username + '"'],
    USER_FIELDNAME_ID + ' desc') then
  begin
    hashedData := Data[USER_FIELDNAME_PASSWORD];
    if FSecUtil.CheckSaltedHash(Password, hashedData) then
    begin
      // save session
      _SESSION['uid'] := Data[USER_FIELDNAME_ID];
      _SESSION['uname'] := Data[USER_FIELDNAME_USERNAME];
      _SESSION['rememberme'] := RememberMe;

      Result := True;
    end;
  end
  else
    SessionController.EndSession;
end;

function TUsersUtil.Logout: boolean;
begin
  try
    SessionController.EndSession(False);
  except
  end;
  Result := True;
end;

end.

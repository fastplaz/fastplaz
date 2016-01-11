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
  {$include define_cms.inc}

type

  { TUsersUtil }

  TUsersUtil = class(TUserModel)
  private
    FLoginAttempsMax: integer;
    function getLoggedInUserID: longint;
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;
    property UserIdLoggedIn: longint read getLoggedInUserID;
    property LoginAttempsMax : integer read FLoginAttempsMax write FLoginAttempsMax;

    function isLoggedIn: boolean;
    function Login(const UserEmail: string; const Password: string;
      RememberMe: boolean = False): boolean;
    function Logout: boolean;

    function checkPermission(Component: string = ''; Instance: string = '';
      Level: integer = ACCESS_NONE): boolean;

  end;

implementation

uses
  fastplaz_handler, group_util, permission_util;

{ TUsersUtil }

function TUsersUtil.getLoggedInUserID: longint;
var
  uid: string;
begin
  Result := 0;
  if SessionController.IsTerminated then
    Exit;
  if SessionController.IsExpired then
  begin
    Logout;
    Exit;
  end;

  uid := _SESSION['uid'];
  if uid <> '' then  //-- simple check
    Result := s2i(uid);
end;

constructor TUsersUtil.Create(const DefaultTableName: string);
begin
  inherited Create;
  FLoginAttempsMax := 0;
end;

destructor TUsersUtil.Destroy;
begin
  inherited Destroy;
end;

function TUsersUtil.isLoggedIn: boolean;
var
  uid: string;
begin
  Result := False;
  if getLoggedInUserID > 0 then
    Result := True;
end;

function TUsersUtil.Login(const UserEmail: string; const Password: string;
  RememberMe: boolean): boolean;
var
  hashedData: string;
  i: integer;
begin
  Result := False;
  i := s2i(_SESSION['failedlogin']);
  SessionController.Clear;
  _SESSION['failedlogin'] := i + 1;
  if FLoginAttempsMax > 0 then
  begin
    if i > FLoginAttempsMax then
      Exit;
  end;
  if FindFirst([USER_FIELDNAME_EMAIL + '="' + UserEmail + '"'],
    USER_FIELDNAME_ID + ' desc') then
  begin
    hashedData := Data[USER_FIELDNAME_PASSWORD];
    with TSecurityUtil.Create do
    begin
      if CheckSaltedHash(Password, hashedData) then
      begin
        // save session
        _SESSION['uid'] := Data[USER_FIELDNAME_ID];
        _SESSION['uname'] := Data[USER_FIELDNAME_USERNAME];
        _SESSION['email'] := Data[USER_FIELDNAME_EMAIL];
        _SESSION['rememberme'] := RememberMe;
        SessionController.DeleteKey('failedlogin');
        Result := True;
      end;
      Free;
    end;
  end;//--- findFirst
end;

function TUsersUtil.Logout: boolean;
begin
  try
    SessionController.EndSession(True);
  except
  end;
  Result := True;
end;

function TUsersUtil.checkPermission(Component: string; Instance: string;
  Level: integer): boolean;
begin
  Result := False;

  if UserIdLoggedIn = 0 then
  begin
    Exit;
  end;

  with TPermissionUtil.Create() do
  begin
    Result := checkPermission(Component, Instance, Level);
    Free;
  end;

end;

end.

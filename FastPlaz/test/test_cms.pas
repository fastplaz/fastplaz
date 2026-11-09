unit test_cms;

{
  src/cms and src/app: the password/CSRF helpers, the permission pattern
  matcher and the user/group/permission models against a temporary SQLite
  database (ignored when the SQLite library cannot be loaded, see
  SQLITE3_LIBRARY). The controllers are listed in uses only so that they
  keep compiling: they had not compiled since 2018.
}

{$mode objfpc}{$H+}

interface

uses
  security_util, grouppermission_model, group_model, groupmembership_model,
  user_model, user_util, group_util, permission_util,
  user_controller, admin_controller, useradmin_controller, groupadmin_controller,
  permissionadmin_controller, search_controller, systeminfo_controller,
  notification_controller, template_controller, about_controller,
  example_controller,
  database_lib, session_controller, fastplaz_handler, common, sqldb, sqlite3dyn,
  Classes, SysUtils, fpcunit, testregistry;

type

  { TSecurityUtilTest }

  TSecurityUtilTest = class(TTestCase)
  published
    procedure TestSaltedHashRoundTrip;
    procedure TestSaltedHashRejects;
    procedure TestPasswordAndEmailRules;
    procedure TestGeneratePassword;
    procedure TestTimingSafeEquals;
    procedure TestGenerateCSRF;
  end;

  { TPermissionPatternTest }

  TPermissionPatternTest = class(TTestCase)
  published
    procedure TestFixSecurityString;
  end;

  { TCmsSQLiteTest }

  TCmsSQLiteTest = class(TTestCase)
  private
    FDir, FDbFile, FSkipReason: string;
    FUser: TUserUtil;
    function Scalar(const ASQL: string): string;
    function AddActiveUser(const AEmail, APassword: string): integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddUserAndLookup;
    procedure TestQuotesAreNotInjected;
    procedure TestChangePassword;
    procedure TestActivateSuspendDelete;
    procedure TestGroupsAndMembership;
    procedure TestPermissionLevels;
    procedure TestLoginFlow;
  end;

implementation

const
  CMS_SCHEMA: array[0..3] of string = (
    'CREATE TABLE users(uid INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, uname TEXT, '
    + 'email TEXT, pass TEXT, activated INTEGER, user_regdate TEXT, lastlogin TEXT, '
    + 'deleted_by INTEGER, deleted_date TEXT)',
    'CREATE TABLE groups(gid INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, gtype INTEGER, '
    + 'description TEXT, prefix TEXT, state INTEGER, nbuser INTEGER, nbumax INTEGER, '
    + 'link INTEGER, uidmaster INTEGER, deleted_by INTEGER, deleted_date TEXT)',
    'CREATE TABLE group_membership(uid INTEGER, gid INTEGER)',
    'CREATE TABLE group_perms(pid INTEGER PRIMARY KEY AUTOINCREMENT, gid INTEGER, '
    + 'sequence INTEGER, realm INTEGER, component TEXT, instance TEXT, level INTEGER, bond INTEGER)');

{ TSecurityUtilTest }

procedure TSecurityUtilTest.TestSaltedHashRoundTrip;
var
  sec: TSecurityUtil;
  h1, h2: string;
  parts: TStrings;
begin
  sec := TSecurityUtil.Create;
  try
    h1 := sec.GenerateSaltedHash('a@x.id' + 'rahasia');
    h2 := sec.GenerateSaltedHash('a@x.id' + 'rahasia');
    // prefix $ salt $ sha256
    parts := Explode(h1, SALT_DELIM);
    try
      AssertEquals(3, parts.Count);
      AssertEquals(SALT_PREFIX, parts[0]);
      AssertEquals(5, Length(parts[1]));
      AssertEquals(64, Length(parts[2]));
    finally
      parts.Free;
    end;
    // a fresh salt every time, both verify
    AssertTrue(h1 <> h2);
    AssertTrue(sec.CheckSaltedHash('a@x.id' + 'rahasia', h1));
    AssertTrue(sec.CheckSaltedHash('a@x.id' + 'rahasia', h2));
    // the stored format of existing databases still verifies
    AssertTrue(sec.CheckSaltedHash('data', sec.GenerateSaltedHash('data', 'abcde')));
  finally
    sec.Free;
  end;
end;

procedure TSecurityUtilTest.TestSaltedHashRejects;
var
  sec: TSecurityUtil;
  h: string;
begin
  sec := TSecurityUtil.Create;
  try
    h := sec.GenerateSaltedHash('a@x.id' + 'rahasia');
    AssertFalse(sec.CheckSaltedHash('a@x.id' + 'Rahasia', h));
    AssertFalse(sec.CheckSaltedHash('', h));
    AssertFalse(sec.CheckSaltedHash('a@x.id' + 'rahasia', ''));
    AssertFalse(sec.CheckSaltedHash('a@x.id' + 'rahasia', 'nodelimiter'));
    AssertFalse(sec.CheckSaltedHash('a@x.id' + 'rahasia', '8$onlysalt'));
    AssertFalse(sec.CheckSaltedHash('a@x.id' + 'rahasia', h + 'x'));
  finally
    sec.Free;
  end;
end;

procedure TSecurityUtilTest.TestPasswordAndEmailRules;
begin
  AssertFalse(TSecurityUtil.IsPasswordAcceptable(''));
  AssertFalse(TSecurityUtil.IsPasswordAcceptable('abcd'));
  AssertTrue(TSecurityUtil.IsPasswordAcceptable('abcde'));
  AssertTrue(TSecurityUtil.IsValidEmail('luri@example.co.id'));
  AssertFalse(TSecurityUtil.IsValidEmail(''));
  AssertFalse(TSecurityUtil.IsValidEmail('luri'));
  AssertFalse(TSecurityUtil.IsValidEmail('luri@'));
  AssertFalse(TSecurityUtil.IsValidEmail('a b@example.com'));
  AssertFalse(TSecurityUtil.IsValidEmail('<script>@example.com'));
  AssertFalse(TSecurityUtil.IsValidEmail(StringOfChar('a', 250) + '@x.id'));
end;

procedure TSecurityUtilTest.TestGeneratePassword;
var
  sec: TSecurityUtil;
  i: integer;
begin
  sec := TSecurityUtil.Create;
  try
    for i := 1 to 20 do
      AssertTrue(Length(sec.GeneratePassword) >= PASSWORD_LENGTH_MIN);
    AssertTrue(sec.GeneratePassword <> sec.GeneratePassword);
  finally
    sec.Free;
  end;
end;

procedure TSecurityUtilTest.TestTimingSafeEquals;
begin
  AssertTrue(TimingSafeEquals('', ''));
  AssertTrue(TimingSafeEquals('abc', 'abc'));
  AssertFalse(TimingSafeEquals('abc', 'abd'));
  AssertFalse(TimingSafeEquals('abc', 'ab'));
  AssertFalse(TimingSafeEquals('ab', 'abc'));
  AssertFalse(TimingSafeEquals('', 'a'));
end;

procedure TSecurityUtilTest.TestGenerateCSRF;
var
  sec: TSecurityUtil;
  t: string;
begin
  sec := TSecurityUtil.Create;
  try
    t := sec.GenerateCSRF('form');
    AssertEquals('form', Copy(t, 1, 4));
    AssertEquals(4 + CSRFTOKEN_LENGTH, Length(t));
    AssertTrue(t <> sec.GenerateCSRF('form'));
  finally
    sec.Free;
  end;
end;

{ TPermissionPatternTest }

procedure TPermissionPatternTest.TestFixSecurityString;
begin
  AssertEquals('.*', TGroupPermissionModel.FixSecurityString(''));
  AssertEquals('user', TGroupPermissionModel.FixSecurityString('user'));
  AssertEquals('.*', TGroupPermissionModel.FixSecurityString('.*'));
  // the leading ':' case never worked (Pos arguments were reversed)
  AssertEquals('.*:list', TGroupPermissionModel.FixSecurityString(':list'));
  AssertEquals('a:.*:b', TGroupPermissionModel.FixSecurityString('a::b'));
  AssertEquals('a:.*', TGroupPermissionModel.FixSecurityString('a:'));
  AssertEquals('.*:.*:b', TGroupPermissionModel.FixSecurityString('::b'));
end;

{ TCmsSQLiteTest }

function TCmsSQLiteTest.Scalar(const ASQL: string): string;
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  try
    q.DataBase := FUser.Connector;
    q.Transaction := FUser.Connector.Transaction;
    q.SQL.Text := ASQL;
    q.Open;
    Result := q.Fields[0].AsString;
    q.Close;
  finally
    q.Free;
  end;
end;

function TCmsSQLiteTest.AddActiveUser(const AEmail, APassword: string): integer;
var
  params: TStringList;
begin
  params := TStringList.Create;
  try
    params.Add(USER_FIELDNAME_ACTIVATED + '=1');
    Result := FUser.Add(AEmail, APassword, params);
  finally
    params.Free;
  end;
end;

procedure TCmsSQLiteTest.SetUp;
var
  lib: string;
begin
  FSkipReason := '';
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_cms_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FDbFile := FDir + 'cms.sqlite';
  if FileExists(FDbFile) then
    DeleteFile(FDbFile);
  AppData.databaseRead := 'default';
  AppData.databaseWrite := 'default';
  AppData.tablePrefix := '';
  AppData.debug := False;
  lib := GetEnvironmentVariable('SQLITE3_LIBRARY');
  if lib <> '' then
    SQLiteDefaultLibrary := lib;
  FUser := TUserUtil.Create;
  with FUser.Connector do
  begin
    if Connected then
      Close;
    ConnectorType := 'SQLite3';
    DatabaseName := FDbFile;
    HostName := '';
    UserName := '';
    Password := '';
    try
      Open;
    except
      on E: Exception do
        FSkipReason := 'SQLite client library not available (' + E.Message + ')';
    end;
  end;
  if FSkipReason <> '' then
  begin
    FreeAndNil(FUser);
    Ignore(FSkipReason);
  end;
  // sqlite runs one statement per ExecuteDirect
  for lib in CMS_SCHEMA do
    if Trim(lib) <> '' then
      FUser.Connector.ExecuteDirect(lib);
  FUser.Connector.Transaction.Commit;
end;

procedure TCmsSQLiteTest.TearDown;
begin
  if Assigned(FUser) then
  begin
    try
      if FUser.Connector.Connected then
      begin
        if FUser.Connector.Transaction.Active then
          FUser.Connector.Transaction.Rollback;
        FUser.Connector.Close;
      end;
    except
    end;
    FreeAndNil(FUser);
  end;
  if FileExists(FDbFile) then
    DeleteFile(FDbFile);
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
end;

procedure TCmsSQLiteTest.TestAddUserAndLookup;
var
  uid: integer;
begin
  uid := FUser.Add('luri@example.id', 'rahasia1', nil);
  AssertTrue(uid > 0);
  AssertTrue(FUser.isEmailExists('luri@example.id'));
  AssertFalse(FUser.isEmailExists('other@example.id'));
  // the user name is the local part of the address
  AssertTrue(FUser.isUserNameExists('luri'));
  AssertEquals('luri2', FUser.GenerateUsername('luri@other.id'));
  // duplicates, bad addresses and short passwords are refused
  AssertEquals(0, FUser.Add('luri@example.id', 'rahasia1', nil));
  AssertEquals(0, FUser.Add('not-an-email', 'rahasia1', nil));
  AssertEquals(0, FUser.Add('b@example.id', 'abc', nil));
  // without a password a random one is generated
  AssertTrue(FUser.Add('c@example.id') > uid);
  AssertEquals('2', Scalar('SELECT COUNT(*) FROM users'));
  // a new user is not active: pending for the admin
  AssertEquals(2, FUser.PendingCount);
  AssertFalse(FUser.isActive(uid));
end;

procedure TCmsSQLiteTest.TestQuotesAreNotInjected;
var
  uid: integer;
begin
  uid := AddActiveUser('luri@example.id', 'rahasia1');
  AssertTrue(uid > 0);
  // the old code pasted the strings into the WHERE clause
  AssertFalse(FUser.isEmailExists('x'' OR 1=1 --'));
  AssertFalse(FUser.isEmailExists('x" OR 1=1 --'));
  AssertFalse(FUser.isUserNameExists('x'' OR ''''='''));
  AssertEquals(0, FUser.Add('o''brien@example.id', 'rahasia1', nil));
  // a quote in the local part reaches the LIKE as data
  AssertEquals('luri''x', FUser.GenerateUsername('luri''x@example.id'));
end;

procedure TCmsSQLiteTest.TestChangePassword;
var
  uid: integer;
  sec: TSecurityUtil;
begin
  uid := AddActiveUser('luri@example.id', 'rahasia1');
  AssertFalse(FUser.ChangePassword(uid, 'abc'));
  AssertTrue(FUser.ChangePassword(uid, 'barubaru'));
  AssertFalse(FUser.ChangePassword(uid + 100, 'barubaru'));
  AssertTrue(FUser.FindFirst([USER_FIELDNAME_ID + '=' + IntToStr(uid)]));
  sec := TSecurityUtil.Create;
  try
    AssertTrue(sec.CheckSaltedHash('luri@example.id' + 'barubaru', FUser[USER_FIELDNAME_PASSWORD]));
    AssertFalse(sec.CheckSaltedHash('luri@example.id' + 'rahasia1', FUser[USER_FIELDNAME_PASSWORD]));
  finally
    sec.Free;
  end;
end;

procedure TCmsSQLiteTest.TestActivateSuspendDelete;
var
  uid: integer;
  groups: TGroupsUtil;
begin
  groups := TGroupsUtil.Create;
  try
    AssertTrue(groups.Add(USER_GROUP_DEFAULT_NAME, 'default') > 0);
  finally
    groups.Free;
  end;
  uid := FUser.Add('luri@example.id', 'rahasia1', nil);
  AssertFalse(FUser.isActive(uid));
  AssertTrue(FUser.Activate(uid));
  AssertTrue(FUser.isActive(uid));
  // activation joins the default group once
  AssertEquals('1', Scalar('SELECT COUNT(*) FROM group_membership WHERE uid=' + IntToStr(uid)));
  AssertTrue(FUser.Activate(uid));
  AssertEquals('1', Scalar('SELECT COUNT(*) FROM group_membership WHERE uid=' + IntToStr(uid)));
  AssertEquals('1', Scalar('SELECT nbuser FROM groups'));
  AssertTrue(FUser.DeActivate(uid));
  AssertFalse(FUser.isActive(uid));
  AssertTrue(FUser.Suspend(uid));
  AssertEquals('2', Scalar('SELECT activated FROM users WHERE uid=' + IntToStr(uid)));
  // CURRENT_TIMESTAMP instead of MySQL's now()
  AssertTrue(FUser.SafeDelete(uid, 1));
  AssertEquals('1', Scalar('SELECT deleted_by FROM users WHERE uid=' + IntToStr(uid)));
  AssertTrue(Scalar('SELECT deleted_date FROM users WHERE uid=' + IntToStr(uid)) <> '');
end;

procedure TCmsSQLiteTest.TestGroupsAndMembership;
var
  groups: TGroupsUtil;
  gid, uid: integer;
begin
  uid := AddActiveUser('luri@example.id', 'rahasia1');
  groups := TGroupsUtil.Create;
  try
    gid := groups.Add('Editors', 'can edit');
    AssertTrue(gid > 0);
    AssertEquals(gid, groups.GetID('Editors'));
    AssertEquals(0, groups.GetID('Nobody'));
    AssertEquals(0, groups.GetID('x'' OR 1=1 --'));
    AssertTrue(groups.AddUserToGroup(uid, gid));
    // the second time is a no-op and the counter stays at one
    AssertFalse(groups.AddUserToGroup(uid, gid));
    AssertFalse(groups.AddUserToGroup(uid, 'Editors'));
    AssertEquals('1', Scalar('SELECT nbuser FROM groups WHERE gid=' + IntToStr(gid)));
    AssertFalse(groups.AddUserToGroup(uid, 'Nobody'));
    AssertTrue(groups.SafeDelete(gid, uid));
    AssertEquals(IntToStr(uid), Scalar('SELECT deleted_by FROM groups WHERE gid=' + IntToStr(gid)));
  finally
    groups.Free;
  end;
end;

procedure TCmsSQLiteTest.TestPermissionLevels;
var
  groups: TGroupsUtil;
  perms: TGroupPermissionModel;
  util: TPermissionUtil;
  gid, uid: integer;
begin
  uid := AddActiveUser('luri@example.id', 'rahasia1');
  groups := TGroupsUtil.Create;
  perms := TGroupPermissionModel.Create;
  util := TPermissionUtil.Create;
  try
    gid := groups.Add('Editors', 'can edit');
    AssertTrue(groups.AddUserToGroup(uid, gid));
    // the first row on an empty table used to fail on a NULL sequence
    AssertTrue(perms.Add(gid, 'user', '.*', ACCESS_ADD));
    AssertTrue(perms.Add(gid, 'admin', 'list', ACCESS_READ));
    AssertEquals('1', Scalar('SELECT MIN(sequence) FROM group_perms'));
    AssertEquals('2', Scalar('SELECT MAX(sequence) FROM group_perms'));

    AssertTrue(util.checkPermission('user', 'user', ACCESS_ADD, uid));
    AssertTrue(util.checkPermission('user', 'anything', ACCESS_READ, uid));
    AssertFalse(util.checkPermission('user', 'user', ACCESS_ADMIN, uid));
    AssertTrue(util.checkPermission('admin', 'list', ACCESS_READ, uid));
    AssertFalse(util.checkPermission('admin', 'edit', ACCESS_READ, uid));
    // 'ANY' looked for the best level and used to loop forever
    AssertTrue(util.checkPermission('admin', 'ANY', ACCESS_READ, uid));
    AssertFalse(util.checkPermission('admin', 'ANY', ACCESS_EDIT, uid));
    AssertTrue(util.checkPermission('user', 'ANY', ACCESS_ADD, uid));
    // generic: the highest level of the user
    AssertTrue(util.checkPermission('', '', ACCESS_ADD, uid));
    AssertFalse(util.checkPermission('', '', ACCESS_DELETE, uid));
    // unknown component, unknown user
    AssertFalse(util.checkPermission('nothing', 'x', ACCESS_NONE, uid));
    AssertFalse(util.checkPermission('user', 'user', ACCESS_READ, uid + 100));
    AssertEquals(ACCESS_INVALID, perms.getSecurityLevel(uid + 100, 'user', 'user'));
  finally
    util.Free;
    perms.Free;
    groups.Free;
  end;
end;

// TUserUtil.Login works on the global session; it is started on the
// test's own session directory
procedure TCmsSQLiteTest.TestLoginFlow;
var
  uid: integer;
  oldDir: string;
begin
  uid := AddActiveUser('luri@example.id', 'rahasia1');
  oldDir := SessionController.SessionDir;
  _SetEnv('HTTP_COOKIE', '');
  _SetEnv('HTTP_USER_AGENT', 'fastplaz-cms-test');
  _SetEnv('REMOTE_ADDR', '127.0.0.1');
  SessionController.SessionDir := FDir;
  try
    AssertTrue(SessionController.StartSession);
    AssertFalse(FUser.isLoggedIn);

    // a quote in the name is data, not SQL
    AssertFalse(FUser.Login('x'' OR 1=1 --', 'rahasia1'));
    AssertFalse(FUser.Login('luri@example.id', 'salah'));
    AssertEquals(2, FUser.FailedLoginCount);
    AssertFalse(FUser.isLoggedIn);

    AssertTrue(FUser.Login('luri@example.id', 'rahasia1'));
    AssertTrue(FUser.isLoggedIn);
    AssertEquals(uid, FUser.UserIdLoggedIn);
    AssertEquals('luri@example.id', string(FUser.UserInfo['email']));
    AssertEquals('luri', FUser.Username);
    AssertEquals(0, FUser.FailedLoginCount);
    AssertTrue(Scalar('SELECT lastlogin FROM users WHERE uid=' + IntToStr(uid)) <> '');

    AssertTrue(FUser.Logout);
    AssertFalse(FUser.isLoggedIn);

    // an inactive user cannot log in
    FUser.Suspend(uid);
    AssertFalse(FUser.Login('luri@example.id', 'rahasia1'));
  finally
    try
      SessionController.EndSession(True);
    except
    end;
    SessionController.SessionDir := oldDir;
    _SetEnv('HTTP_COOKIE', '');
  end;
end;

initialization
  RegisterTest(TSecurityUtilTest);
  RegisterTest(TPermissionPatternTest);
  RegisterTest(TCmsSQLiteTest);

end.

unit test_session_controller;

{
  TSessionController with the file storage (temporary directory) and, when
  the SQLite client library can be loaded, the database storage.
  The CGI environment is faked through environment variables.
}

{$mode objfpc}{$H+}

interface

uses
  session_controller, common, database_lib, md5, IniFiles, sqldb, sqlite3dyn, DateUtils,
  Classes, SysUtils, fpcunit, testregistry;

type

  { TSessionFileTest }

  TSessionFileTest = class(TTestCase)
  private
    FDir: string;
    FCookie: string;
    function NewSession(const ACookie: string = ''): TSessionController;
    procedure SetLastAccess(const ASessionFile: string; AValue: TDateTime);
    procedure RemoveDirContent;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultSessionDirFromTemp;
    procedure TestEncodeDecodeValue;
    procedure TestNewCookieIsRandom;
    procedure TestCookieReused;
    procedure TestSessionIDFromCookieAgentAndIP;
    procedure TestPrefixSuffixAndForceUniqueID;
    procedure TestStartSessionResult;
    procedure TestValuesBeforeStartIgnored;
    procedure TestValueRoundTrip;
    procedure TestValueWithLineBreaks;
    procedure TestReadIntegerAndDateTime;
    procedure TestDeleteKeyPersisted;
    procedure TestClearKeepsSessionAlive;
    procedure TestTimeoutIsInMinutes;
    procedure TestExpiredSessionWipedOnce;
    procedure TestZeroTimeoutNeverExpires;
    procedure TestEndSessionSoftPersisted;
    procedure TestEndSessionForceRemovesFile;
    procedure TestRestartSession;
    procedure TestRegenerateIDMovesData;
    procedure TestGetData;
    procedure TestStartAgainAfterForcedEnd;
  end;

  { TSessionDatabaseTest }

  TSessionDatabaseTest = class(TTestCase)
  private
    FDir: string;
    FDbFile: string;
    FSkipReason: string;
    FSchema: TSimpleModel;
    function NewSession(const ACookie: string): TSessionController;
    function Query(const ASQL: string): string;
    function RowCount: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStartCreatesRow;
    procedure TestValueRoundTrip;
    procedure TestQuotesAreNotSqlInjected;
    procedure TestForwardedForNotTrustedByDefault;
    procedure TestDeleteKeyAndClear;
    procedure TestExpiredSessionWiped;
    procedure TestRegenerateIDReplacesRow;
    procedure TestEndSessionForceDeletesRow;
  end;

implementation


const
  AGENT = 'fastplaz-test-agent';
  IPADDR = '127.0.0.1';

procedure SetRequestEnv(const ACookie: string);
begin
  _SetEnv('HTTP_COOKIE', ACookie);
  _SetEnv('HTTP_USER_AGENT', AGENT);
  _SetEnv('REMOTE_ADDR', IPADDR);
  _SetEnv('HTTP_X_FORWARDED_FOR', '');
  _SetEnv('HTTPS', '');
end;

{ TSessionFileTest }

function TSessionFileTest.NewSession(const ACookie: string): TSessionController;
begin
  if ACookie = '' then
    SetRequestEnv(FCookie)
  else
    SetRequestEnv(ACookie);
  Result := TSessionController.Create;
  Result.SessionDir := FDir;
end;

procedure TSessionFileTest.SetLastAccess(const ASessionFile: string; AValue: TDateTime);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(ASessionFile);
  try
    ini.WriteDateTime(_SESSION_SESSION, _SESSION_KEYLAST, AValue);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TSessionFileTest.RemoveDirContent;
var
  sr: TSearchRec;
begin
  if FindFirst(FDir + '*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr and faDirectory) = 0 then
        DeleteFile(FDir + sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TSessionFileTest.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_session_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  RemoveDirContent;
  FCookie := '_=cookie123; token=tok456';
  AppData.trustProxy := False;
end;

procedure TSessionFileTest.TearDown;
begin
  RemoveDirContent;
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
  SetRequestEnv('');
end;

procedure TSessionFileTest.TestDefaultSessionDirFromTemp;
var
  s: TSessionController;
  savedTemp: string;
begin
  savedTemp := GetEnvironmentVariable('TEMP');
  try
    SetRequestEnv('');
    _SetEnv('TEMP', ExcludeTrailingPathDelimiter(FDir));
    s := TSessionController.Create;
    try
      AssertEquals(FDir + 'sessions' + PathDelim, s.SessionDir);
      AssertTrue(s.StartSession);
      AssertTrue(DirectoryExists(FDir + 'sessions'));
      AssertTrue(FileExists(FDir + 'sessions' + PathDelim + s.SessionID + '.ses'));
      s.Terminate;
    finally
      s.Free;
    end;
    RemoveDir(FDir + 'sessions');
    // no TEMP at all: relative fallback under the application
    _SetEnv('TEMP', '');
    s := TSessionController.Create;
    try
      AssertEquals(IncludeTrailingPathDelimiter(_SESSION_DIR_DEFAULT), s.SessionDir);
    finally
      s.Free;
    end;
  finally
    _SetEnv('TEMP', savedTemp);
  end;
end;

procedure TSessionFileTest.TestEncodeDecodeValue;
const
  raw = 'line1'#13#10'line2'#10'back\slash\n';
begin
  AssertEquals('line1\r\nline2\nback\\slash\\n', EncodeSessionValue(raw));
  AssertEquals(raw, DecodeSessionValue(EncodeSessionValue(raw)));
  AssertEquals('', DecodeSessionValue(''));
  AssertEquals('plain', DecodeSessionValue('plain'));
  // unknown escape and a trailing backslash are kept as they are
  AssertEquals('\x', DecodeSessionValue('\x'));
  AssertEquals('a\', DecodeSessionValue('a\'));
  AssertTrue(Pos(#10, EncodeSessionValue(raw)) = 0);
end;

procedure TSessionFileTest.TestNewCookieIsRandom;
var
  s1, s2: TSessionController;
begin
  s1 := NewSession(' ');
  s2 := NewSession(' ');
  try
    AssertTrue(s1.IsNewSession);
    AssertEquals('fastplaz', Copy(s1.CookieID, 1, 8));
    AssertEquals(8 + 41, Length(s1.CookieID));
    AssertTrue(s1.CookieID <> s2.CookieID);
    AssertTrue(s1.SessionID <> s2.SessionID);
    AssertEquals('', s1.Token);
  finally
    s1.Free;
    s2.Free;
  end;
end;

procedure TSessionFileTest.TestCookieReused;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    AssertFalse(s.IsNewSession);
    AssertEquals('cookie123', s.CookieID);
    AssertEquals('tok456', s.Token);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestSessionIDFromCookieAgentAndIP;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    AssertEquals(MD5Print(MD5String('cookie123-' + AGENT + '-' + IPADDR)), s.SessionID);
  finally
    s.Free;
  end;
  // a different client (user agent) does not get the same session
  SetRequestEnv(FCookie);
  _SetEnv('HTTP_USER_AGENT', 'other');
  s := TSessionController.Create;
  try
    AssertTrue(s.SessionID <> MD5Print(MD5String('cookie123-' + AGENT + '-' + IPADDR)));
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestPrefixSuffixAndForceUniqueID;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    s.SessionPrefix := 'app';
    s.SessionSuffix := 'x';
    s.ForceUniqueID := 'forced';
    AssertTrue(s.StartSession);
    AssertEquals('app-' + MD5Print(MD5String('forced-' + AGENT + '-' + IPADDR)) + '-x', s.SessionID);
    AssertTrue(FileExists(FDir + s.SessionID + '.ses'));
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestStartSessionResult;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    AssertFalse(s.IsStarted);
    AssertTrue('first start', s.StartSession);
    AssertTrue(s.IsStarted);
    AssertFalse(s.IsTerminated);
    AssertFalse(s.IsExpired);
    AssertTrue('second start is a no-op', s.StartSession);
    AssertTrue(FileExists(FDir + s.SessionID + '.ses'));
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestValuesBeforeStartIgnored;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    s['name'] := 'Budi';
    AssertEquals('', s['name']);
    AssertEquals(0, s.ReadInteger('name'));
    AssertFalse(FileExists(FDir + s.SessionID + '.ses'));
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestValueRoundTrip;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    s.StartSession;
    s['name'] := 'Budi';
    s['city'] := 'Jakarta';
    AssertEquals('Budi', s['name']);
  finally
    s.Free;
  end;
  // next request, same cookie
  s := NewSession;
  try
    s.StartSession;
    AssertEquals('Budi', s['name']);
    AssertEquals('Jakarta', s['city']);
    AssertEquals('', s['missing']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestValueWithLineBreaks;
const
  raw = 'first'#13#10'[session]'#10'active=0'#10'back\slash';
var
  s: TSessionController;
begin
  s := NewSession;
  try
    s.StartSession;
    s['note'] := raw;
    s['after'] := 'ok';
    AssertEquals(raw, s['note']);
  finally
    s.Free;
  end;
  s := NewSession;
  try
    s.StartSession;
    AssertEquals(raw, s['note']);
    AssertEquals('ok', s['after']);
    AssertFalse('injected line must not become a key', s.GetData.Contains(#10'active=0'));
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestReadIntegerAndDateTime;
var
  s: TSessionController;
  d: TDateTime;
begin
  d := EncodeDateTime(2026, 11, 1, 10, 30, 0, 0);
  s := NewSession;
  try
    s.StartSession;
    s['count'] := '42';
    s['when'] := DateTimeToStr(d);
    s['text'] := 'abc';
    AssertEquals(42, s.ReadInteger('count'));
    AssertEquals(0, s.ReadInteger('text'));
    AssertEquals(0, s.ReadInteger('missing'));
    AssertTrue(Abs(s.ReadDateTime('when') - d) < 1 / SecsPerDay);
    AssertEquals(0, s.ReadDateTime('text'));
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestDeleteKeyPersisted;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    s.StartSession;
    s['a'] := '1';
    s['b'] := '2';
    s.DeleteKey('a');
    AssertEquals('', s['a']);
  finally
    s.Free;
  end;
  s := NewSession;
  try
    s.StartSession;
    AssertEquals('', s['a']);
    AssertEquals('2', s['b']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestClearKeepsSessionAlive;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    s.StartSession;
    s['a'] := '1';
    s.Clear;
    AssertEquals('', s['a']);
    AssertTrue(s.IsStarted);
    s['b'] := '2';
    AssertEquals('2', s['b']);
  finally
    s.Free;
  end;
  s := NewSession;
  try
    s.StartSession;
    AssertEquals('', s['a']);
    AssertEquals('2', s['b']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestTimeoutIsInMinutes;
var
  s: TSessionController;
  f: string;
begin
  s := NewSession;
  try
    s.StartSession;
    s['a'] := '1';
    f := FDir + s.SessionID + '.ses';
  finally
    s.Free;
  end;
  // last seen 30 minutes ago
  SetLastAccess(f, IncMinute(Now, -30));
  s := NewSession;
  try
    s.TimeOut := 60;
    s.StartSession;
    AssertFalse('30 min < 60 min', s.IsExpired);
    AssertEquals('1', s['a']);
    AssertTrue(s.Interval > 29 * 60);
    AssertTrue(s.Interval < 31 * 60);
  finally
    s.Free;
  end;
  SetLastAccess(f, IncMinute(Now, -30));
  s := NewSession;
  try
    s.TimeOut := 10;
    s.StartSession;
    AssertTrue('30 min > 10 min', s.IsExpired);
    AssertEquals('', s['a']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestExpiredSessionWipedOnce;
var
  s: TSessionController;
  f: string;
begin
  s := NewSession;
  try
    s.StartSession;
    s['a'] := '1';
    f := FDir + s.SessionID + '.ses';
  finally
    s.Free;
  end;
  SetLastAccess(f, IncDay(Now, -1));
  s := NewSession;
  try
    s.TimeOut := 60;
    AssertTrue(s.StartSession);
    AssertTrue(s.IsExpired);
    AssertEquals('', s['a']);
    // data written after the wipe must survive further IsExpired reads
    s['b'] := '2';
    AssertTrue(s.IsExpired);
    AssertTrue(s.IsExpired);
    AssertEquals('2', s['b']);
    AssertTrue(s.Interval < 60);
  finally
    s.Free;
  end;
  s := NewSession;
  try
    s.TimeOut := 60;
    s.StartSession;
    AssertFalse(s.IsExpired);
    AssertEquals('2', s['b']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestZeroTimeoutNeverExpires;
var
  s: TSessionController;
  f: string;
begin
  s := NewSession;
  try
    s.StartSession;
    s['a'] := '1';
    f := FDir + s.SessionID + '.ses';
  finally
    s.Free;
  end;
  SetLastAccess(f, IncYear(Now, -1));
  s := NewSession;
  try
    s.TimeOut := 0;
    s.StartSession;
    AssertFalse(s.IsExpired);
    AssertEquals('1', s['a']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestEndSessionSoftPersisted;
var
  s: TSessionController;
  f: string;
begin
  s := NewSession;
  try
    s.StartSession;
    s['a'] := '1';
    f := FDir + s.SessionID + '.ses';
    s.EndSession(False);
    AssertFalse(s.IsTerminated);
    AssertTrue(FileExists(f));
  finally
    s.Free;
  end;
  s := NewSession;
  try
    s.StartSession;
    AssertEquals('', s['a']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestEndSessionForceRemovesFile;
var
  s: TSessionController;
  f: string;
begin
  s := NewSession;
  try
    s.StartSession;
    s['a'] := '1';
    f := FDir + s.SessionID + '.ses';
    s.Terminate;
    AssertTrue(s.IsTerminated);
    AssertFalse(FileExists(f));
    AssertEquals('', s['a']);
    s['b'] := '2'; // ignored, terminated
    AssertEquals('', s['b']);
    AssertFalse(FileExists(f));
    s.ForceUpdate;
    s.DeleteKey('a');
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestRestartSession;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    s.StartSession;
    s['a'] := '1';
    AssertTrue(s.RestartSession);
    AssertTrue(s.IsStarted);
    AssertEquals('', s['a']);
    s['b'] := '2';
    AssertEquals('2', s['b']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestRegenerateIDMovesData;
var
  s: TSessionController;
  oldID, oldCookie, newCookie: string;
begin
  s := NewSession;
  try
    AssertFalse('not started yet', s.RegenerateID);
    s.StartSession;
    s['uid'] := '7';
    oldID := s.SessionID;
    oldCookie := s.CookieID;
    AssertTrue(s.RegenerateID);
    newCookie := s.CookieID;
    AssertTrue(newCookie <> oldCookie);
    AssertTrue(s.SessionID <> oldID);
    AssertTrue(s.IsNewSession);
    AssertFalse('old file removed', FileExists(FDir + oldID + '.ses'));
    AssertTrue('new file written', FileExists(FDir + s.SessionID + '.ses'));
    AssertEquals('7', s['uid']);
    s['after'] := 'x';
  finally
    s.Free;
  end;
  // the browser comes back with the new cookie
  s := NewSession('_=' + newCookie);
  try
    s.StartSession;
    AssertEquals('7', s['uid']);
    AssertEquals('x', s['after']);
  finally
    s.Free;
  end;
  // the old cookie starts an empty session
  s := NewSession('_=' + oldCookie);
  try
    s.StartSession;
    AssertEquals('', s['uid']);
  finally
    s.Free;
  end;
end;

procedure TSessionFileTest.TestGetData;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    AssertEquals('', s.GetData);
    s.StartSession;
    s['a'] := '1';
    AssertTrue(s.GetData.Contains('a=1'));
    AssertTrue(s.GetData.Contains('[' + _SESSION_SESSION + ']'));
  finally
    s.Free;
  end;
end;

// EndSession(True) left the controller "started and terminated": in a
// daemon the next request could never open a session again
procedure TSessionFileTest.TestStartAgainAfterForcedEnd;
var
  s: TSessionController;
begin
  s := NewSession;
  try
    AssertTrue(s.StartSession);
    s['a'] := '1';
    s.EndSession(True);
    AssertTrue(s.IsTerminated);
    AssertFalse(s.IsStarted);
    AssertTrue(s.StartSession);
    AssertFalse(s.IsTerminated);
    AssertTrue(s.IsStarted);
    s['b'] := '2';
    AssertEquals('2', s['b']);
    AssertEquals('', s['a']);
  finally
    s.Free;
  end;
end;

{ TSessionDatabaseTest }

const
  SESSION_SCHEMA =
    'CREATE TABLE session_info (' +
    'sessid VARCHAR(222) NOT NULL PRIMARY KEY, ' +
    'ipaddr VARCHAR(32), ' +
    'lastused BIGINT DEFAULT 0, ' +
    'uid BIGINT DEFAULT 0, ' +
    'remember INTEGER DEFAULT 0, ' +
    'vars TEXT)';

function TSessionDatabaseTest.NewSession(const ACookie: string): TSessionController;
begin
  SetRequestEnv(ACookie);
  Result := TSessionController.Create;
  Result.Storage := _SESSION_STORAGE_DATABASE;
end;

function TSessionDatabaseTest.Query(const ASQL: string): string;
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  try
    q.DataBase := FSchema.Connector;
    q.SQL.Text := ASQL;
    q.Open;
    if q.EOF then
      Result := ''
    else
      Result := q.Fields[0].AsString;
    q.Close;
  finally
    q.Free;
  end;
end;

function TSessionDatabaseTest.RowCount: integer;
begin
  Result := StrToIntDef(Query('SELECT COUNT(*) FROM session_info'), -1);
end;

procedure TSessionDatabaseTest.SetUp;
var
  lib: string;
begin
  FSkipReason := '';
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_session_db_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FDbFile := FDir + 'session.sqlite';
  if FileExists(FDbFile) then
    DeleteFile(FDbFile);
  AppData.databaseRead := 'default';
  AppData.databaseWrite := 'default';
  AppData.tablePrefix := '';
  AppData.debug := False;
  AppData.trustProxy := False;
  lib := GetEnvironmentVariable('SQLITE3_LIBRARY');
  if lib <> '' then
    SQLiteDefaultLibrary := lib;
  FSchema := TSimpleModel.Create('session_info', 'sessid');
  with FSchema.Connector do
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
    FreeAndNil(FSchema);
    Ignore(FSkipReason);
  end;
  FSchema.Connector.ExecuteDirect(SESSION_SCHEMA);
  FSchema.Connector.Transaction.Commit;
end;

procedure TSessionDatabaseTest.TearDown;
begin
  if Assigned(FSchema) then
  begin
    try
      if FSchema.Connector.Connected then
      begin
        if FSchema.Connector.Transaction.Active then
          FSchema.Connector.Transaction.Rollback;
        FSchema.Connector.Close;
      end;
    except
    end;
    FreeAndNil(FSchema);
  end;
  if FileExists(FDbFile) then
    DeleteFile(FDbFile);
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
  SetRequestEnv('');
end;

procedure TSessionDatabaseTest.TestStartCreatesRow;
var
  s: TSessionController;
begin
  s := NewSession('_=dbcookie');
  try
    AssertEquals(0, RowCount);
    AssertTrue(s.StartSession);
    AssertEquals(1, RowCount);
    AssertEquals(s.SessionID, Query('SELECT sessid FROM session_info'));
    AssertEquals(IPADDR, Query('SELECT ipaddr FROM session_info'));
    AssertTrue(StrToInt64Def(Query('SELECT lastused FROM session_info'), 0) > 0);
  finally
    s.Free;
  end;
end;

procedure TSessionDatabaseTest.TestValueRoundTrip;
var
  s: TSessionController;
begin
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    s['uid'] := '12';
    s['name'] := 'multi'#10'line';
    AssertEquals('multi'#10'line', s['name']);
    AssertEquals('12', Query('SELECT uid FROM session_info'));
  finally
    s.Free;
  end;
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    AssertEquals(1, RowCount);
    AssertEquals('12', s['uid']);
    AssertEquals(12, s.ReadInteger('uid'));
    AssertEquals('multi'#10'line', s['name']);
  finally
    s.Free;
  end;
end;

procedure TSessionDatabaseTest.TestQuotesAreNotSqlInjected;
const
  evil = 'O''Brien"; DROP TABLE session_info; --';
var
  s: TSessionController;
begin
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    s['name'] := evil;
    AssertEquals(evil, s['name']);
  finally
    s.Free;
  end;
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    AssertEquals(1, RowCount);
    AssertEquals(evil, s['name']);
  finally
    s.Free;
  end;
end;

procedure TSessionDatabaseTest.TestForwardedForNotTrustedByDefault;
var
  s: TSessionController;
begin
  SetRequestEnv('_=dbcookie');
  _SetEnv('HTTP_X_FORWARDED_FOR', '1.2.3.4'', 0, 0, ''''); DROP TABLE session_info; --');
  s := TSessionController.Create;
  s.Storage := _SESSION_STORAGE_DATABASE;
  try
    AssertTrue(s.StartSession);
    AssertEquals(1, RowCount);
    AssertEquals(IPADDR, Query('SELECT ipaddr FROM session_info'));
  finally
    s.Free;
  end;
end;

procedure TSessionDatabaseTest.TestDeleteKeyAndClear;
var
  s: TSessionController;
begin
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    s['a'] := '1';
    s['b'] := '2';
    s.DeleteKey('a');
    AssertEquals('', s['a']);
    AssertEquals('2', s['b']);
  finally
    s.Free;
  end;
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    AssertEquals('', s['a']);
    AssertEquals('2', s['b']);
    s.Clear;
    AssertEquals('', s['b']);
    AssertTrue(s.IsStarted);
  finally
    s.Free;
  end;
  AssertEquals(1, RowCount);
  AssertEquals('', Query('SELECT vars FROM session_info'));
end;

procedure TSessionDatabaseTest.TestExpiredSessionWiped;
var
  s: TSessionController;
begin
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    s['a'] := '1';
  finally
    s.Free;
  end;
  FSchema.Connector.ExecuteDirect('UPDATE session_info SET lastused = ' +
    IntToStr(DateTimeToUnix(IncDay(Now, -1), False)));
  FSchema.Connector.Transaction.Commit;
  s := NewSession('_=dbcookie');
  try
    s.TimeOut := 60;
    AssertTrue(s.StartSession);
    AssertTrue(s.IsExpired);
    AssertEquals('', s['a']);
    s['b'] := '2';
    AssertTrue(s.IsExpired);
    AssertEquals('2', s['b']);
  finally
    s.Free;
  end;
end;

procedure TSessionDatabaseTest.TestRegenerateIDReplacesRow;
var
  s: TSessionController;
  oldID, newCookie: string;
begin
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    s['uid'] := '5';
    oldID := s.SessionID;
    AssertTrue(s.RegenerateID);
    newCookie := s.CookieID;
    AssertTrue(s.SessionID <> oldID);
    AssertEquals(1, RowCount);
    AssertEquals(s.SessionID, Query('SELECT sessid FROM session_info'));
    AssertEquals('5', s['uid']);
  finally
    s.Free;
  end;
  s := NewSession('_=' + newCookie);
  try
    s.StartSession;
    AssertEquals('5', s['uid']);
  finally
    s.Free;
  end;
end;

procedure TSessionDatabaseTest.TestEndSessionForceDeletesRow;
var
  s: TSessionController;
begin
  s := NewSession('_=dbcookie');
  try
    s.StartSession;
    s['a'] := '1';
    s.EndSession(True);
    AssertTrue(s.IsTerminated);
    AssertEquals(0, RowCount);
    AssertEquals('', s['a']);
  finally
    s.Free;
  end;
end;

initialization
  RegisterTest(TSessionFileTest);
  RegisterTest(TSessionDatabaseTest);

end.

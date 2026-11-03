unit test_database_lib;

{
  TSimpleModel against a temporary SQLite database, so no server is needed.
  Requires the SQLite client library (sqlite3.dll / libsqlite3.so). When it
  cannot be loaded the whole suite is ignored; point SQLITE3_LIBRARY at the
  library file to use a specific one:

    set SQLITE3_LIBRARY=C:\path\to\sqlite3.dll
    run_tests.bat --suite=TSimpleModelSQLiteTest
}

{$mode objfpc}{$H+}

interface

uses
  database_lib, common, sqldb, sqlite3dyn, fpjson, Variants,
  Classes, SysUtils, fpcunit, testregistry;

type

  { TSimpleModelSQLiteTest }

  TSimpleModelSQLiteTest = class(TTestCase)
  private
    FDir: string;
    FDbFile: string;
    FModel: TSimpleModel;
    FSkipReason: string;
    procedure Connect;
    procedure Disconnect;
    procedure Exec(const ASQL: string);
    function CountRows: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRequestFinishedRollsBackAndCloses;
    procedure TestPersistentConnectionStaysOpen;
    procedure TestIdleConnectionIsClosed;
    procedure TestReservedTableNameIsQuoted;
    procedure TestSaveAndDeleteFailuresReturnFalse;
    procedure TestConnected;
    procedure TestFieldListFromSqlite;
    procedure TestSaveInsertAndLastInsertID;
    procedure TestSaveTwiceSameFieldNoDuplicate;
    procedure TestSaveWithoutFields;
    procedure TestFindByIdAndUpdate;
    procedure TestUpdateWithoutRecordIsNoop;
    procedure TestFindWhereOrderLimit;
    procedure TestFindNoMatch;
    procedure TestFindStringKeyIsQuoted;
    procedure TestAllAndRecordCount;
    procedure TestAsJsonArray;
    procedure TestDeleteWhereAndById;
    procedure TestSoftDelete;
    procedure TestSelectWhereOrderByLimitOpen;
    procedure TestWhereWithFormat;
    procedure TestRecordsTotalFiltered;
    procedure TestQueryExecAndRowsAffected;
    procedure TestQueryOpenToJson;
    procedure TestValueOnClosedDataset;
    procedure TestExecScriptRunsEveryStatement;
    procedure TestExecScriptWithTrigger;
    procedure TestExecScriptFailureRollsBack;
    procedure TestQueryExecScript;
  end;

  { TSplitSQLStatementsTest: no database needed }

  TSplitSQLStatementsTest = class(TTestCase)
  published
    procedure TestEmptyAndBlank;
    procedure TestSimpleStatements;
    procedure TestSemicolonInsideQuotes;
    procedure TestComments;
    procedure TestMySQLHintKept;
    procedure TestDollarQuotedBody;
    procedure TestTriggerBody;
    procedure TestTriggerWithCase;
    procedure TestPostgresTriggerWithoutBody;
    procedure TestBeginTransactionIsAStatement;
  end;

implementation

const
  SCHEMA =
    'CREATE TABLE persons (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'name VARCHAR(50), ' +
    'age INTEGER, ' +
    'city VARCHAR(50), ' +
    'is_deleted INTEGER DEFAULT 0)';

procedure TSimpleModelSQLiteTest.Connect;
var
  lib: string;
begin
  lib := GetEnvironmentVariable('SQLITE3_LIBRARY');
  if lib <> '' then
    SQLiteDefaultLibrary := lib;
  FModel := TSimpleModel.Create('persons', 'id');
  with FModel.Connector do
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
end;

procedure TSimpleModelSQLiteTest.Disconnect;
begin
  if Assigned(FModel) then
  begin
    try
      if FModel.Connector.Connected then
      begin
        if FModel.Connector.Transaction.Active then
          FModel.Connector.Transaction.Rollback;
        FModel.Connector.Close;
      end;
    except
    end;
    FreeAndNil(FModel);
  end;
end;

procedure TSimpleModelSQLiteTest.Exec(const ASQL: string);
begin
  FModel.Connector.ExecuteDirect(ASQL);
  FModel.Connector.Transaction.Commit;
end;

function TSimpleModelSQLiteTest.CountRows: integer;
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  try
    q.DataBase := FModel.Connector;
    q.SQL.Text := 'SELECT COUNT(*) AS n FROM persons';
    q.Open;
    Result := q.Fields[0].AsInteger;
    q.Close;
  finally
    q.Free;
  end;
end;

procedure TSimpleModelSQLiteTest.SetUp;
begin
  FSkipReason := '';
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_db_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FDbFile := FDir + 'test.sqlite';
  if FileExists(FDbFile) then
    DeleteFile(FDbFile);
  // the model reads these; keep read and write on the same connection
  AppData.databaseRead := 'default';
  AppData.databaseWrite := 'default';
  AppData.tablePrefix := '';
  AppData.debug := False;
  Connect;
  if FSkipReason <> '' then
  begin
    Disconnect;
    Ignore(FSkipReason);
  end;
  Exec(SCHEMA);
  Exec('INSERT INTO persons (name, age, city) VALUES (''Budi'', 30, ''Jakarta'')');
  Exec('INSERT INTO persons (name, age, city) VALUES (''Sari'', 25, ''Bandung'')');
  Exec('INSERT INTO persons (name, age, city) VALUES (''Tono'', 41, ''Jakarta'')');
end;

procedure TSimpleModelSQLiteTest.TearDown;
begin
  Disconnect;
  if FileExists(FDbFile) then
    DeleteFile(FDbFile);
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
end;

// standalone mode: what a request leaves uncommitted is rolled back, as the
// end of a CGI process does, and the connection is closed (reopened on use)
procedure TSimpleModelSQLiteTest.TestRequestFinishedRollsBackAndCloses;
begin
  DataBasePersistent := False;
  FModel.Connector.ExecuteDirect('INSERT INTO persons (name, age, city) VALUES (''Uncommitted'', 1, ''X'')');
  AssertTrue('transaction open', FModel.Connector.Transaction.Active);
  DataBaseRequestFinished;
  AssertFalse('closed after the request', FModel.Connector.Connected);
  FModel.Connector.Open;
  AssertEquals('insert rolled back', 3, CountRows);
end;

procedure TSimpleModelSQLiteTest.TestPersistentConnectionStaysOpen;
begin
  DataBasePersistent := True;
  try
    FModel.Connector.ExecuteDirect('INSERT INTO persons (name, age, city) VALUES (''Uncommitted'', 1, ''X'')');
    DataBaseRequestFinished;
    AssertTrue('kept open', FModel.Connector.Connected);
    AssertFalse('transaction ended', FModel.Connector.Transaction.Active);
    AssertEquals('insert rolled back', 3, CountRows);
    // a fresh request within the idle timeout keeps it open
    DataBaseIdleTimeout := 300;
    DataBaseRequestStarted;
    AssertTrue('still open', FModel.Connector.Connected);
  finally
    DataBasePersistent := False;
  end;
end;

procedure TSimpleModelSQLiteTest.TestIdleConnectionIsClosed;
begin
  DataBasePersistent := True;
  DataBaseIdleTimeout := 1;
  try
    DataBaseRequestFinished;
    AssertTrue(FModel.Connector.Connected);
    Sleep(1100);
    DataBaseRequestStarted;
    AssertFalse('closed after being idle', FModel.Connector.Connected);
    // the next query opens it again
    AssertEquals(3, CountRows);
    AssertTrue(FModel.Connector.Connected);
  finally
    DataBasePersistent := False;
    DataBaseIdleTimeout := 300;
  end;
end;

// generated SQL quotes the table and its alias: "order" (and "groups" on
// MySQL 8) are reserved words
procedure TSimpleModelSQLiteTest.TestReservedTableNameIsQuoted;
var
  m: TSimpleModel;
begin
  Exec('CREATE TABLE "order"(id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT)');
  m := TSimpleModel.Create('order', 'id');
  try
    AssertEquals('`order`', m.QuotedTableName);
    AssertEquals('`order`', m.QuotedAliasName);
    AssertEquals('`db`.`t`', m.QuoteIdentifier('db.t'));
    AssertEquals('t u', m.QuoteIdentifier('t u'));
    AssertEquals('`t`', m.QuoteIdentifier('`t`'));
    AssertEquals('', m.QuoteIdentifier(''));
    m.New;
    m['name'] := 'x';
    AssertTrue(m.Save);
    AssertTrue(m.Find([]));
    AssertEquals(1, m.RecordCount);
    AssertEquals('x', string(m['name']));
    AssertTrue(m.SQL.Text, Pos('FROM `order` `order`', m.SQL.Text) > 0);
    AssertTrue(m.SQL.Text, Pos('`order`.name', m.SQL.Text) > 0);
    AssertTrue(m.Find(['name=''x''']));
    AssertTrue(m.GetAll);
    AssertEquals(1, m.RecordCount);
    AssertTrue(m.Delete('id=1'));
    AssertFalse(m.Find([]));
  finally
    m.Free;
  end;
end;

// a failed INSERT/UPDATE/DELETE used to end the request through
// DisplayError; now the call returns False and Message says why, and the
// model stays usable
procedure TSimpleModelSQLiteTest.TestSaveAndDeleteFailuresReturnFalse;
var
  m: TSimpleModel;
begin
  Exec('CREATE TABLE uniq(id INTEGER PRIMARY KEY AUTOINCREMENT, code TEXT UNIQUE)');
  m := TSimpleModel.Create('uniq', 'id');
  try
    m.New;
    m['code'] := 'a';
    AssertTrue(m.Save);
    AssertEquals('', m.Message);
    m.New;
    m['code'] := 'a';
    AssertFalse(m.Save);
    AssertTrue(m.Message, Pos('UNIQUE', UpperCase(m.Message)) > 0);
    // an unknown column
    m.New;
    m['nope'] := 'x';
    AssertFalse(m.Save);
    AssertTrue(m.Message <> '');
    // still usable afterwards
    m.New;
    m['code'] := 'b';
    AssertTrue(m.Save);
    AssertEquals('', m.Message);
    AssertTrue(m.Find([]));
    AssertEquals(2, m.RecordCount);
    // a bad DELETE
    AssertFalse(m.Delete('nope=1'));
    AssertTrue(m.Message <> '');
    AssertTrue(m.Delete('code=''b'''));
    AssertEquals('', m.Message);
    AssertTrue(m.Find([]));
    AssertEquals(1, m.RecordCount);
  finally
    m.Free;
  end;
end;

procedure TSimpleModelSQLiteTest.TestConnected;
begin
  AssertTrue(FModel.Connector.Connected);
  AssertEquals('persons', FModel.TableName);
  AssertEquals(3, CountRows);
end;

procedure TSimpleModelSQLiteTest.TestFieldListFromSqlite;
begin
  AssertEquals(5, FModel.FieldLists.Count);
  AssertEquals('persons.id', FModel.FieldLists[0]);
  AssertEquals('persons.name', FModel.FieldLists[1]);
end;

procedure TSimpleModelSQLiteTest.TestSaveInsertAndLastInsertID;
begin
  FModel.New;
  FModel['name'] := 'Dewi';
  FModel['age'] := 33;
  FModel['city'] := 'Surabaya';
  AssertTrue(FModel.Message, FModel.Save);
  AssertEquals(4, FModel.LastInsertID);
  AssertEquals(4, CountRows);
  AssertTrue(FModel.Find(4));
  AssertEquals('Dewi', string(FModel['name']));
  AssertEquals(33, Integer(FModel['age']));
end;

procedure TSimpleModelSQLiteTest.TestSaveTwiceSameFieldNoDuplicate;
begin
  FModel.New;
  FModel['name'] := 'first';
  FModel['name'] := 'second';
  FModel['age'] := 1;
  AssertTrue(FModel.Message, FModel.Save);
  AssertTrue(FModel.Find(FModel.LastInsertID));
  AssertEquals('second', string(FModel['name']));
end;

procedure TSimpleModelSQLiteTest.TestSaveWithoutFields;
begin
  FModel.New;
  AssertFalse(FModel.Save);
  AssertEquals('no field value to save', FModel.Message);
  AssertEquals(3, CountRows);
end;

procedure TSimpleModelSQLiteTest.TestFindByIdAndUpdate;
begin
  AssertTrue(FModel.Find(2));
  AssertEquals('Sari', string(FModel['name']));
  AssertEquals(1, FModel.RecordCount);
  FModel['age'] := 26;
  AssertTrue(FModel.Message, FModel.Update);
  AssertTrue(FModel.Find('2'));
  AssertEquals(26, Integer(FModel['age']));
  AssertEquals('other rows untouched', 3, CountRows);
end;

procedure TSimpleModelSQLiteTest.TestUpdateWithoutRecordIsNoop;
begin
  FModel.New;
  FModel['age'] := 99;
  // no Find before Update: must not run "UPDATE ... WHERE id="
  AssertFalse(FModel.Update);
  AssertTrue(FModel.Find(1));
  AssertEquals(30, Integer(FModel['age']));
end;

procedure TSimpleModelSQLiteTest.TestFindWhereOrderLimit;
begin
  AssertTrue(FModel.Find(['city=''Jakarta'''], 'age DESC'));
  AssertEquals(2, FModel.RecordCount);
  AssertEquals('Tono', string(FModel['name']));
  FModel.Next;
  AssertEquals('Budi', string(FModel['name']));
  FModel.Next;
  AssertTrue(FModel.EOF);

  AssertTrue(FModel.Find(['city=''Jakarta'''], 'age ASC', 1));
  AssertEquals(1, FModel.RecordCount);
  AssertEquals('Budi', string(FModel['name']));
end;

procedure TSimpleModelSQLiteTest.TestFindNoMatch;
begin
  AssertFalse(FModel.Find(['city=''Nowhere''']));
  AssertFalse(FModel.Find(999));
end;

// Find(string) used to concatenate the key into the WHERE clause
procedure TSimpleModelSQLiteTest.TestFindStringKeyIsQuoted;
var
  m: TSimpleModel;
begin
  Exec('INSERT INTO persons (name, age, city) VALUES (''O''''Brien'', 50, ''Dublin'')');
  // an injection attempt must not match every row
  AssertFalse(FModel.Find('1'' OR ''1''=''1'));
  AssertTrue(FModel.Find('3'));
  AssertEquals('Tono', string(FModel['name']));

  m := TSimpleModel.Create('persons', 'name');
  try
    AssertTrue(m.Find('O''Brien'));
    AssertEquals(1, m.RecordCount);
    AssertEquals('Dublin', string(m['city']));
    AssertFalse(m.Find('x'' OR name<>''x'));
  finally
    m.Free;
  end;
end;

procedure TSimpleModelSQLiteTest.TestAllAndRecordCount;
begin
  AssertTrue(FModel.All);
  AssertEquals(3, FModel.RecordCount);
  AssertTrue(FModel.GetAll(2));
  AssertEquals(2, FModel.RecordCount);
  AssertTrue(FModel.GetAll(2, 2));
  AssertEquals(1, FModel.RecordCount);
end;

procedure TSimpleModelSQLiteTest.TestAsJsonArray;
var
  a: TJSONArray;
begin
  AssertTrue(FModel.Find(['age > 26'], 'id'));
  a := FModel.AsJsonArray;
  try
    AssertEquals(2, a.Count);
    AssertEquals(2, FModel.RecordCountFromArray);
    AssertEquals('Budi', a.Objects[0].Strings['name']);
    AssertEquals(41, a.Objects[1].Integers['age']);
  finally
    a.Free;
  end;
  FModel.Close;
  a := FModel.AsJsonArray;
  try
    AssertEquals('closed dataset gives an empty array', 0, a.Count);
  finally
    a.Free;
  end;
end;

procedure TSimpleModelSQLiteTest.TestDeleteWhereAndById;
begin
  AssertTrue(FModel.Delete('city=''Bandung'''));
  AssertEquals(2, CountRows);
  AssertTrue(FModel.Delete(1));
  AssertEquals(1, CountRows);
  AssertTrue(FModel.Find(3));
end;

procedure TSimpleModelSQLiteTest.TestSoftDelete;
begin
  AssertTrue(FModel.SoftDelete(1));
  AssertEquals('row still there', 3, CountRows);
  AssertTrue(FModel.Find(1));
  AssertEquals(1, Integer(FModel['is_deleted']));
  FModel.UseSoftDelete := True;
  AssertTrue(FModel.All);
  AssertEquals('soft deleted rows hidden', 2, FModel.RecordCount);
end;

procedure TSimpleModelSQLiteTest.TestSelectWhereOrderByLimitOpen;
begin
  AssertTrue(FModel.Select('name, age').Where('age >= 25').OrderBy('age DESC').Limit(2).Open);
  AssertEquals(2, FModel.RecordCount);
  AssertEquals('Tono', string(FModel['name']));
  AssertTrue(Pos('ORDER BY age DESC', FModel.SQL.Text) > 0);
end;

procedure TSimpleModelSQLiteTest.TestWhereWithFormat;
begin
  AssertTrue(FModel.Where('city=%s', [QuotedStr('Bandung')]).Open);
  AssertEquals(1, FModel.RecordCount);
  AssertEquals('Sari', string(FModel['name']));
end;

procedure TSimpleModelSQLiteTest.TestRecordsTotalFiltered;
begin
  AssertEquals(2, FModel.RecordsTotalFiltered('SELECT COUNT(*) FROM persons WHERE city=''Jakarta'''));
  AssertEquals(0, FModel.RecordsTotalFiltered(''));
end;

procedure TSimpleModelSQLiteTest.TestQueryExecAndRowsAffected;
begin
  AssertTrue(QueryExec('UPDATE persons SET age = age + 1 WHERE city=''Jakarta'''));
  AssertEquals(2, QueryExecRowAffected);
  AssertFalse(QueryExec('UPDATE no_such_table SET x = 1'));
  AssertTrue(FModel.Find(1));
  AssertEquals(31, Integer(FModel['age']));
end;

procedure TSimpleModelSQLiteTest.TestQueryOpenToJson;
var
  o: TJSONObject;
  a: TJSONArray;
begin
  o := TJSONObject.Create;
  AssertTrue(QueryOpenToJson('SELECT name FROM persons ORDER BY id', o, False));
  try
    AssertEquals(3, o.Integers['count']);
    AssertEquals(3, o.Arrays['data'].Count);
    AssertEquals('Budi', o.Arrays['data'].Objects[0].Strings['name']);
  finally
    o.Free;
  end;
  a := TJSONArray.Create;
  AssertTrue(QueryOpenToJson('SELECT name FROM persons WHERE id=2', a, False));
  try
    AssertEquals(1, a.Count);
    AssertEquals('Sari', a.Objects[0].Strings['name']);
  finally
    a.Free;
  end;
end;

procedure TSimpleModelSQLiteTest.TestValueOnClosedDataset;
begin
  FModel.Close;
  AssertTrue(VarIsNull(FModel['name']) or VarIsEmpty(FModel['name']) or
    (string(FModel['name']) = ''));
  AssertEquals(-1, FModel.RecordCount);
  AssertTrue(FModel.EOF);
end;

procedure TSimpleModelSQLiteTest.TestExecScriptRunsEveryStatement;
begin
  AssertTrue(FModel.Message, FModel.ExecScript(
    '-- two inserts and an update' + LineEnding +
    'INSERT INTO persons (name, age, city) VALUES (''A;B'', 1, ''X'');' + LineEnding +
    'INSERT INTO persons (name, age, city) VALUES (''C'', 2, ''X'');' + LineEnding +
    'UPDATE persons SET age = 9 WHERE city = ''X'';'));
  AssertEquals('', FModel.Message);
  AssertEquals(5, CountRows);
  AssertTrue(FModel.Find(['name=''A;B''']));
  AssertEquals(9, Integer(FModel['age']));
end;

procedure TSimpleModelSQLiteTest.TestExecScriptWithTrigger;
begin
  AssertTrue(FModel.Message, FModel.ExecScript(
    'CREATE TABLE audit (person_id INTEGER, note TEXT);' + LineEnding +
    'CREATE TRIGGER persons_ai AFTER INSERT ON persons' + LineEnding +
    'BEGIN' + LineEnding +
    '  INSERT INTO audit VALUES (NEW.id, ''added'');' + LineEnding +
    '  UPDATE persons SET city = CASE WHEN NEW.city IS NULL THEN ''-'' ELSE NEW.city END' + LineEnding +
    '    WHERE id = NEW.id;' + LineEnding +
    'END;' + LineEnding +
    'INSERT INTO persons (name, age) VALUES (''D'', 5);'));
  AssertTrue(FModel.Find(['name=''D''']));
  AssertEquals('-', string(FModel['city']));
  AssertTrue(QueryExec('DELETE FROM audit WHERE note = ''added'''));
  AssertEquals(1, QueryExecRowAffected);
end;

procedure TSimpleModelSQLiteTest.TestExecScriptFailureRollsBack;
begin
  AssertFalse(FModel.ExecScript(
    'INSERT INTO persons (name) VALUES (''E'');' +
    'INSERT INTO no_such_table VALUES (1);' +
    'INSERT INTO persons (name) VALUES (''F'');'));
  AssertTrue(FModel.Message, Pos('statement 2:', FModel.Message) = 1);
  AssertEquals(3, CountRows);
  // the connection stays usable
  AssertTrue(FModel.Message, FModel.ExecScript('INSERT INTO persons (name) VALUES (''G'')'));
  AssertEquals(4, CountRows);
end;

procedure TSimpleModelSQLiteTest.TestQueryExecScript;
begin
  AssertTrue(QueryExecScript('DELETE FROM persons WHERE id = 1; DELETE FROM persons WHERE id = 2'));
  AssertEquals(1, CountRows);
  AssertFalse(QueryExecScript('DELETE FROM persons; SELECT * FROM no_such_table'));
  AssertEquals(1, CountRows);
end;

{ TSplitSQLStatementsTest }

procedure AssertStatements(const AExpected: array of string; const AActual: TStringArray);
var
  i: integer;
begin
  TAssert.AssertEquals('statement count', Length(AExpected), Length(AActual));
  for i := 0 to High(AExpected) do
    TAssert.AssertEquals('statement ' + IntToStr(i + 1), AExpected[i], AActual[i]);
end;

procedure TSplitSQLStatementsTest.TestEmptyAndBlank;
begin
  AssertEquals(0, Length(SplitSQLStatements('')));
  AssertEquals(0, Length(SplitSQLStatements(' ; ;' + LineEnding + ';')));
  AssertEquals(0, Length(SplitSQLStatements('-- only a comment' + LineEnding + '/* and another */;')));
end;

procedure TSplitSQLStatementsTest.TestSimpleStatements;
begin
  AssertStatements(['SELECT 1', 'SELECT 2', 'SELECT 3'],
    SplitSQLStatements('SELECT 1; SELECT 2;' + LineEnding + 'SELECT 3'));
end;

procedure TSplitSQLStatementsTest.TestSemicolonInsideQuotes;
begin
  AssertStatements([
    'INSERT INTO t VALUES (''a;b'', ''it''''s;'')',
    'SELECT "x;y" FROM `t;1`'],
    SplitSQLStatements(
      'INSERT INTO t VALUES (''a;b'', ''it''''s;'');SELECT "x;y" FROM `t;1`;'));
end;

procedure TSplitSQLStatementsTest.TestComments;
begin
  // a dropped comment leaves one space behind
  AssertStatements(['SELECT 1', 'SELECT   2','SELECT ''--not a comment'''],
    SplitSQLStatements(
      '-- first; still comment' + LineEnding +
      'SELECT 1; /* a; b */' + LineEnding +
      'SELECT /* inline; */ 2;' + LineEnding +
      'SELECT ''--not a comment'';'));
end;

procedure TSplitSQLStatementsTest.TestMySQLHintKept;
begin
  AssertStatements(['/*!40101 SET NAMES utf8 */', 'SELECT 1'],
    SplitSQLStatements('/*!40101 SET NAMES utf8 */; SELECT 1;'));
end;

procedure TSplitSQLStatementsTest.TestDollarQuotedBody;
const
  FN = 'CREATE FUNCTION f() RETURNS int AS $body$ BEGIN RETURN 1; END; $body$ LANGUAGE plpgsql';
begin
  AssertStatements([FN, 'SELECT $$a;b$$', 'SELECT $1'],
    SplitSQLStatements(FN + '; SELECT $$a;b$$; SELECT $1;'));
end;

procedure TSplitSQLStatementsTest.TestTriggerBody;
const
  TRG = 'CREATE TRIGGER t1 AFTER INSERT ON a BEGIN INSERT INTO b VALUES (1); DELETE FROM c; END';
begin
  AssertStatements(['CREATE TABLE a (x)', TRG, 'SELECT 1'],
    SplitSQLStatements('CREATE TABLE a (x); ' + TRG + '; SELECT 1;'));
end;

procedure TSplitSQLStatementsTest.TestTriggerWithCase;
const
  TRG = 'create trigger t2 after update on a begin update a set x = case when 1 then 2 end; end';
begin
  AssertStatements([TRG, 'SELECT 1'], SplitSQLStatements(TRG + ';SELECT 1'));
end;

procedure TSplitSQLStatementsTest.TestPostgresTriggerWithoutBody;
const
  TRG = 'CREATE TRIGGER t3 BEFORE UPDATE ON a FOR EACH ROW EXECUTE FUNCTION f()';
begin
  AssertStatements([TRG, 'SELECT 1'], SplitSQLStatements(TRG + '; SELECT 1;'));
end;

procedure TSplitSQLStatementsTest.TestBeginTransactionIsAStatement;
begin
  AssertStatements(['BEGIN', 'INSERT INTO a VALUES (1)', 'COMMIT'],
    SplitSQLStatements('BEGIN; INSERT INTO a VALUES (1); COMMIT;'));
end;

initialization
  RegisterTest(TSimpleModelSQLiteTest);
  RegisterTest(TSplitSQLStatementsTest);

end.

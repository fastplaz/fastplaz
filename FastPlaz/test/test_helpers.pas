unit test_helpers;

{
  The type helpers in src/helper and the module variable model/util
  (the latter against a temporary SQLite database, ignored when the
  SQLite client library cannot be loaded).
}

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  modvar_model, modvar_util, database_lib, common,
  fpjson, jsonparser, sqldb, sqlite3dyn, DateUtils, Variants,
  Classes, SysUtils, fpcunit, testregistry,
  // the helper units must come after SysUtils: only the last helper in
  // scope applies to a type, and SysUtils has its own string/integer ones
  string_helpers, datetime_helpers, json_helpers, array_helpers, integer_helpers;

type

  { TStringHelperTest }

  TStringHelperTest = class(TTestCase)
  published
    procedure TestAsDateTime;
    procedure TestIsNumeric;
    procedure TestSaveToFileExactBytes;
    procedure TestIsDate;
    procedure TestIsEmailAndUrl;
  end;

  { TDateTimeHelperTest }

  TDateTimeHelperTest = class(TTestCase)
  published
    procedure TestDiffNotTruncated;
    procedure TestFromStringKeepsTime;
    procedure TestFromStringInvalid;
    procedure TestUnixRoundTrip;
    procedure TestIntegerAsDateTime;
  end;

  { TJsonHelperTest }

  TJsonHelperTest = class(TTestCase)
  published
    procedure TestLongValuesNotTruncated;
    procedure TestNameHelpersOnArray;
    procedure TestGetDataAndDelete;
  end;

  { TArrayHelperTest }

  TArrayHelperTest = class(TTestCase)
  published
    procedure TestCreateFromDelimitedString;
    procedure TestIndexOfAndIn;
  end;

  { TModvarSQLiteTest }

  TModvarSQLiteTest = class(TTestCase)
  private
    FDir, FDbFile: string;
    FModel: TModvarUtil;
    FSkipReason: string;
    function CountRows: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetAndGet;
    procedure TestQuoteInNames;
    procedure TestNullValue;
    procedure TestQuoteValue;
  end;

implementation

{ TStringHelperTest }

procedure TStringHelperTest.TestAsDateTime;
var
  s: string;
begin
  s := '2026-11-01 23:30:15';
  AssertEquals('2026-11-01 23:30:15', FormatDateTime('yyyy-mm-dd hh:nn:ss', s.AsDateTime));
  s := '2026-11-01';
  AssertEquals('2026-11-01 00:00:00', FormatDateTime('yyyy-mm-dd hh:nn:ss', s.AsDateTime));
  s := 'not a date';
  AssertTrue('falls back to Now', Abs(s.AsDateTime - Now) < 1 / 24);
end;

procedure TStringHelperTest.TestIsNumeric;
var
  s: string;
begin
  s := '12';
  AssertTrue(s.IsNumeric);
  s := '-1.5';
  AssertTrue(s.IsNumeric);
  s := 'abc';
  AssertFalse(s.IsNumeric);
  s := '';
  AssertFalse(s.IsNumeric);
end;

procedure TStringHelperTest.TestSaveToFileExactBytes;
var
  s, f: string;
  fs: TFileStream;
  back: string;
begin
  f := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_helper_' + IntToStr(GetProcessID) + '.txt';
  s := 'line1'#10'line2';
  AssertTrue(s.SaveToFile(f));
  fs := TFileStream.Create(f, fmOpenRead);
  try
    SetLength(back, fs.Size);
    if fs.Size > 0 then
      fs.ReadBuffer(back[1], fs.Size);
  finally
    fs.Free;
  end;
  DeleteFile(f);
  // no line ending appended, no CRLF conversion
  AssertEquals(s, back);
end;

procedure TStringHelperTest.TestIsDate;
var
  s: string;
begin
  s := '31/12/2026';
  AssertTrue(s.isDate);
  s := '31-12-2026';
  AssertTrue(s.isDate('-'));
  s := '32/12/2026';
  AssertFalse(s.isDate);
  s := '2026/12/31';
  AssertFalse(s.isDate);
end;

procedure TStringHelperTest.TestIsEmailAndUrl;
var
  s: string;
begin
  s := 'user.name+tag@example.co.id';
  AssertTrue(s.IsEmail);
  s := 'not an email';
  AssertFalse(s.IsEmail);
  s := 'https://example.com/path?x=1';
  AssertTrue(s.IsURL);
  s := 'example.com';
  AssertFalse(s.IsURL);
  AssertTrue(s.IsDomain);
end;

{ TDateTimeHelperTest }

procedure TDateTimeHelperTest.TestDiffNotTruncated;
var
  d, d2: TDateTime;
begin
  d := EncodeDate(2026, 1, 1);
  d2 := EncodeDate(2026, 1, 3);
  // 2 days = 172800 s, more than a Word can hold
  AssertEquals(172800, d.SecondsDiff(d2));
  AssertEquals(2880, d.MinutesDiff(d2));
  AssertEquals(48, d.HoursDiff(d2));
  AssertEquals(2, d.DaysDiff(d2));
  AssertEquals(0, d.WeeksDiff(d2));
  AssertEquals(0, d.MonthsDiff(d2));
  AssertEquals(0, d.YearsDiff(d2));
end;

procedure TDateTimeHelperTest.TestFromStringKeepsTime;
var
  d: TDateTime;
begin
  d := 0;
  d := d.FromString('2026-11-01 23:30:15');
  AssertEquals('2026-11-01 23:30:15', d.Format);
  d := d.FromString('01/11/2026 08:05', 'dd/mm/yyyy hh:nn');
  AssertEquals('2026-11-01 08:05:00', d.Format);
  d := d.FromString('01/11/2026', 'dd/mm/yyyy');
  AssertEquals('2026-11-01 00:00:00', d.Format);
end;

procedure TDateTimeHelperTest.TestFromStringInvalid;
var
  d: TDateTime;
begin
  d := 0;
  d := d.FromString('nope');
  AssertTrue(Abs(d - Now) < 1 / 24);
end;

procedure TDateTimeHelperTest.TestUnixRoundTrip;
var
  d, back: TDateTime;
begin
  d := EncodeDateTime(2026, 11, 1, 12, 0, 0, 0);
  back := 0;
  back := back.FromUnixTime(d.AsUnixTime);
  AssertTrue('local round trip', Abs(back - d) < 1 / SecsPerDay);
  back := back.FromUnixTime(d.AsUnixTime(True), True);
  AssertTrue('utc round trip', Abs(back - d) < 1 / SecsPerDay);
end;

procedure TDateTimeHelperTest.TestIntegerAsDateTime;
var
  d: TDateTime;
  i: integer;
begin
  d := EncodeDateTime(2026, 11, 1, 12, 0, 0, 0);
  i := d.AsUnixTime;
  AssertTrue(Abs(i.AsDateTime - d) < 1 / SecsPerDay);
  AssertTrue(i.InRange(0, MaxInt));
  AssertFalse(i.InRange(0, 10));
end;

{ TJsonHelperTest }

procedure TJsonHelperTest.TestLongValuesNotTruncated;
var
  j: TJSONData;
  long: string;
begin
  long := StringOfChar('x', 300);
  j := GetJSON('{"a":{"b":"' + long + '"}}');
  try
    AssertEquals(300, Length(j.Value['a/b']));
    AssertEquals(300, Length(j.Value['a.b']));
    AssertEquals(300, Length(j.GetData('a').ValueOfNameAsString('b')));
  finally
    j.Free;
  end;
end;

procedure TJsonHelperTest.TestNameHelpersOnArray;
var
  j: TJSONData;
begin
  j := GetJSON('[1,2]');
  try
    AssertEquals(-1, j.IndexOfName('x'));
    AssertNull(j.ValueOfName('x'));
    AssertEquals('', j.ValueOfNameAsString('x'));
    AssertFalse(j.Delete('x'));
    AssertEquals(2, j.Count);
  finally
    j.Free;
  end;
  j := GetJSON('"text"');
  try
    AssertEquals(-1, j.IndexOfName('x'));
  finally
    j.Free;
  end;
end;

procedure TJsonHelperTest.TestGetDataAndDelete;
var
  j: TJSONData;
begin
  j := GetJSON('{"a":{"b":1},"c":2}');
  try
    AssertNotNull(j.GetData('a/b'));
    AssertEquals(1, j.GetData('a/b').AsInteger);
    AssertNull(j.GetData('a/x'));
    AssertEquals(0, j.IndexOfName('a'));
    AssertTrue(j.Delete('c'));
    AssertFalse(j.Delete('c'));
    AssertEquals(-1, j.IndexOfName('c'));
  finally
    j.Free;
  end;
end;

{ TArrayHelperTest }

procedure TArrayHelperTest.TestCreateFromDelimitedString;
var
  a: TStringArray;
begin
  a := TStringArray.CreateFromDelimitedString('a, b,,c ');
  AssertEquals(3, a.Count);
  AssertEquals('a', a[0]);
  AssertEquals('b', a[1]);
  AssertEquals('c', a[2]);
  a := TStringArray.CreateFromDelimitedString('x|y', '|');
  AssertEquals(2, a.Length);
  a := TStringArray.CreateFromDelimitedString('');
  AssertEquals(0, a.Count);
end;

procedure TArrayHelperTest.TestIndexOfAndIn;
var
  a: TStringArray;
begin
  a := nil;
  AssertEquals(-1, a.IndexOf('x'));
  a.Add('one');
  a.Add('two');
  AssertEquals(1, a.IndexOf('two'));
  AssertEquals(-1, a.IndexOf('TWO'));
  AssertTrue('one' in a);
  AssertFalse('three' in a);
end;

{ TModvarSQLiteTest }

const
  MODVAR_SCHEMA =
    'CREATE TABLE module_vars (' +
    'mvid INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'modname VARCHAR(50), ' +
    'name VARCHAR(50), ' +
    'value TEXT)';

function TModvarSQLiteTest.CountRows: integer;
var
  q: TSQLQuery;
begin
  q := TSQLQuery.Create(nil);
  try
    q.DataBase := FModel.Connector;
    q.SQL.Text := 'SELECT COUNT(*) FROM module_vars';
    q.Open;
    Result := q.Fields[0].AsInteger;
    q.Close;
  finally
    q.Free;
  end;
end;

procedure TModvarSQLiteTest.SetUp;
var
  lib: string;
begin
  FSkipReason := '';
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_modvar_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FDbFile := FDir + 'modvar.sqlite';
  if FileExists(FDbFile) then
    DeleteFile(FDbFile);
  AppData.databaseRead := 'default';
  AppData.databaseWrite := 'default';
  AppData.tablePrefix := '';
  AppData.debug := False;
  lib := GetEnvironmentVariable('SQLITE3_LIBRARY');
  if lib <> '' then
    SQLiteDefaultLibrary := lib;
  FModel := TModvarUtil.Create;
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
  if FSkipReason <> '' then
  begin
    FreeAndNil(FModel);
    Ignore(FSkipReason);
  end;
  FModel.Connector.ExecuteDirect(MODVAR_SCHEMA);
  FModel.Connector.Transaction.Commit;
end;

procedure TModvarSQLiteTest.TearDown;
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
  if FileExists(FDbFile) then
    DeleteFile(FDbFile);
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
end;

procedure TModvarSQLiteTest.TestSetAndGet;
begin
  AssertEquals('', VarToStr(FModel['mymod/color']));
  FModel['mymod/color'] := 'blue';
  AssertEquals(1, CountRows);
  AssertEquals('blue', VarToStr(FModel['mymod/color']));
  FModel['mymod/color'] := 'red';
  AssertEquals('updated in place', 1, CountRows);
  AssertEquals('red', VarToStr(FModel['mymod/color']));
  FModel['size'] := 42; // module "system"
  AssertEquals(2, CountRows);
  AssertEquals(42, Integer(FModel['system/size']));
  AssertEquals('', VarToStr(FModel['other/color']));
end;

procedure TModvarSQLiteTest.TestQuoteInNames;
begin
  FModel['o''brien/it''s'] := 'v1';
  AssertEquals(1, CountRows);
  AssertEquals('v1', VarToStr(FModel['o''brien/it''s']));
  FModel['o''brien/it''s'] := 'v2';
  AssertEquals(1, CountRows);
  AssertEquals('v2', VarToStr(FModel['o''brien/it''s']));
end;

procedure TModvarSQLiteTest.TestNullValue;
begin
  FModel.Connector.ExecuteDirect('INSERT INTO module_vars (modname, name, value) VALUES (''m'', ''n'', NULL)');
  FModel.Connector.Transaction.Commit;
  AssertEquals('', VarToStr(FModel['m/n']));
  AssertEquals('dflt', VarToStr(FModel.GetCustom('m', 'n', 'dflt')));
end;

procedure TModvarSQLiteTest.TestQuoteValue;
begin
  AssertEquals('''plain''', FModel.QuoteValue('plain'));
  AssertEquals('''it''''s''', FModel.QuoteValue('it''s'));
  // not MySQL: backslashes are literal
  AssertEquals('''a\b''', FModel.QuoteValue('a\b'));
end;

initialization
  RegisterTest(TStringHelperTest);
  RegisterTest(TDateTimeHelperTest);
  RegisterTest(TJsonHelperTest);
  RegisterTest(TArrayHelperTest);
  RegisterTest(TModvarSQLiteTest);

end.

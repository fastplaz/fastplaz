unit test_logutil_lib;

{$mode objfpc}{$H+}

interface

uses
  logutil_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { TLogUtilTest }

  TLogUtilTest = class(TTestCase)
  private
    FDir: string;
    FLog: string;
    function ReadLog: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddCreatesFile;
    procedure TestAddAppends;
    procedure TestAddWithModName;
    procedure TestAddSkip;
    procedure TestAddSingleLinePerEntry;
    procedure TestAddUnwritablePathDoesNotRaise;
    procedure TestPrefixChangesFileName;
    procedure TestDefaultFileNameHasDate;
  end;

implementation

function TLogUtilTest.ReadLog: TStringList;
begin
  Result := TStringList.Create;
  if FileExists(FLog) then
    Result.LoadFromFile(FLog);
end;

procedure TLogUtilTest.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_log_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FLog := FDir + 'custom.log';
end;

procedure TLogUtilTest.TearDown;
var
  rec: TSearchRec;
begin
  if FindFirst(FDir + '*', faAnyFile, rec) = 0 then
  begin
    repeat
      if (rec.Attr and faDirectory) = 0 then
        DeleteFile(FDir + rec.Name);
    until FindNext(rec) <> 0;
    FindClose(rec);
  end;
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
end;

procedure TLogUtilTest.TestAddCreatesFile;
var
  lines: TStringList;
begin
  LogUtil.Add('hello', '', False, FLog);
  lines := ReadLog;
  try
    AssertEquals(1, lines.Count);
    AssertTrue('timestamp prefix', Pos(' | hello', lines[0]) = 20);
  finally
    lines.Free;
  end;
end;

procedure TLogUtilTest.TestAddAppends;
var
  lines: TStringList;
begin
  LogUtil.Add('one', '', False, FLog);
  LogUtil.Add('two', '', False, FLog);
  lines := ReadLog;
  try
    AssertEquals(2, lines.Count);
    AssertTrue(Pos('| one', lines[0]) > 0);
    AssertTrue(Pos('| two', lines[1]) > 0);
  finally
    lines.Free;
  end;
end;

procedure TLogUtilTest.TestAddWithModName;
var
  lines: TStringList;
begin
  LogUtil.Add('boom', 'MOD', False, FLog);
  lines := ReadLog;
  try
    AssertEquals(1, lines.Count);
    AssertTrue(Pos('| MOD: boom', lines[0]) > 0);
  finally
    lines.Free;
  end;
end;

procedure TLogUtilTest.TestAddSkip;
begin
  LogUtil.Add('skipped', '', True, FLog);
  AssertFalse(FileExists(FLog));
end;

procedure TLogUtilTest.TestAddSingleLinePerEntry;
var
  lines: TStringList;
begin
  LogUtil.Add('line1' + #13#10 + '2024-01-01 00:00:00 | forged', '', False, FLog);
  lines := ReadLog;
  try
    AssertEquals('CR/LF collapsed into one entry', 1, lines.Count);
    AssertTrue(Pos('line1 2024-01-01 00:00:00 | forged', lines[0]) > 0);
  finally
    lines.Free;
  end;
end;

procedure TLogUtilTest.TestAddUnwritablePathDoesNotRaise;
begin
  LogUtil.Add('x', '', False, FDir + 'no' + PathDelim + 'such' + PathDelim + 'dir.log');
  AssertTrue(True);
end;

procedure TLogUtilTest.TestPrefixChangesFileName;
var
  old: UnicodeString;
begin
  old := LogUtil.FullName;
  try
    LogUtil.Prefix := 'unit-';
    AssertTrue(Pos(UnicodeString('/unit-app-'), LogUtil.FullName) > 0);
    AssertTrue(LogUtil.FullName <> old);
  finally
    LogUtil.Prefix := '';
  end;
  AssertEquals(string(old), string(LogUtil.FullName));
end;

procedure TLogUtilTest.TestDefaultFileNameHasDate;
begin
  AssertEquals('app-' + FormatDateTime('YYYYMMDD', Now) + '.log',
    string(LogUtil.FileName));
  AssertTrue(Pos(LogUtil.FileName, LogUtil.FullName) > 0);
end;

initialization
  RegisterTest(TLogUtilTest);

end.

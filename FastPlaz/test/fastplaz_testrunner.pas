{ FastPlaz console test runner with live progress.

  TFastPlazTestRunner extends the FPCUnit TTestRunner: every suite and every
  test is printed the moment it starts, followed by its result and duration,
  so a long run shows what is going on instead of staying silent until the
  end. A short summary with the list of failures closes the run.

  Options added to the standard runner:
    --no-live    plain FPCUnit behaviour (report written when the run ends)
    --no-color   never emit ANSI colors (NO_COLOR in the environment does the same)

  The live output goes to stdout. The regular FPCUnit report is still written
  when --file is given or when the format is not plain (xml, latex), so
  `--format=xml --file=result.xml` keeps working for CI.
}
unit fastplaz_testrunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, fpcunitreport, consoletestrunner;

type

  { TLiveTestListener }

  TLiveTestListener = class(TNoRefCountObject, ITestListener)
  private
    FTotal: Integer;
    FDone: Integer;
    FPassed: Integer;
    FFailed: Integer;
    FErrors: Integer;
    FIgnored: Integer;
    FLevel: Integer;
    FUseColor: Boolean;
    FRunStart: QWord;
    FTestStart: QWord;
    FTestFailure: TTestFailure;
    FTestIsError: Boolean;
    FProblems: TStringList;
    function Indent: string;
    function Paint(const AText, AColor: string): string;
    procedure WriteOut(const AText: string);
    procedure WriteLineOut(const AText: string);
  public
    constructor Create(ATotal: Integer; AUseColor: Boolean);
    destructor Destroy; override;
    function GetExitCode: Integer;
    procedure WriteSummary;
    { ITestListener }
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
    property Total: Integer read FTotal;
    property Passed: Integer read FPassed;
    property Failed: Integer read FFailed;
    property Errors: Integer read FErrors;
    property Ignored: Integer read FIgnored;
  end;

  { TFastPlazTestRunner }

  TFastPlazTestRunner = class(TTestRunner)
  private
    FLive: Boolean;
    FColor: Boolean;
  protected
    procedure AppendLongOpts; override;
    procedure WriteCustomHelp; override;
    function ParseOptions: Boolean; override;
    procedure DoTestRun(ATest: TTest); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

function StdOutIsTerminal: Boolean;
function EnableConsoleColors: Boolean;

implementation

uses
  {$IFDEF WINDOWS}Windows{$ELSE}termio{$ENDIF};

const
  NAME_WIDTH = 48;
  C_RESET = #27'[0m';
  C_BOLD = #27'[1m';
  C_DIM = #27'[2m';
  C_RED = #27'[31m';
  C_GREEN = #27'[32m';
  C_YELLOW = #27'[33m';
  C_CYAN = #27'[36m';

function StdOutIsTerminal: Boolean;
{$IFDEF WINDOWS}
var
  Mode: DWORD;
begin
  Result := GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), Mode);
end;
{$ELSE}
begin
  Result := IsATTY(Output) <> 0;
end;
{$ENDIF}

function EnableConsoleColors: Boolean;
{$IFDEF WINDOWS}
var
  H: THandle;
  Mode: DWORD;
begin
  Result := False;
  H := GetStdHandle(STD_OUTPUT_HANDLE);
  if not GetConsoleMode(H, Mode) then
    Exit;
  if (Mode and ENABLE_VIRTUAL_TERMINAL_PROCESSING) <> 0 then
    Exit(True);
  Result := SetConsoleMode(H, Mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING);
end;
{$ELSE}
begin
  Result := StdOutIsTerminal;
end;
{$ENDIF}

function FormatDuration(AMilliseconds: QWord): string;
begin
  if AMilliseconds < 1000 then
    Result := IntToStr(AMilliseconds) + ' ms'
  else if AMilliseconds < 60000 then
    Result := IntToStr(AMilliseconds div 1000) + '.' +
      IntToStr((AMilliseconds mod 1000) div 100) + ' s'
  else
    Result := IntToStr(AMilliseconds div 60000) + ' min ' +
      IntToStr((AMilliseconds mod 60000) div 1000) + ' s';
end;

function FirstLine(const AText: string): string;
var
  P: Integer;
begin
  Result := AText;
  P := Pos(#10, Result);
  if P > 0 then
    SetLength(Result, P - 1);
  Result := TrimRight(Result);
end;

{ TLiveTestListener }

constructor TLiveTestListener.Create(ATotal: Integer; AUseColor: Boolean);
begin
  inherited Create;
  FTotal := ATotal;
  FUseColor := AUseColor;
  FProblems := TStringList.Create;
  FRunStart := GetTickCount64;
end;

destructor TLiveTestListener.Destroy;
begin
  FProblems.Free;
  inherited Destroy;
end;

function TLiveTestListener.Indent: string;
begin
  Result := StringOfChar(' ', FLevel * 2);
end;

function TLiveTestListener.Paint(const AText, AColor: string): string;
begin
  if FUseColor then
    Result := AColor + AText + C_RESET
  else
    Result := AText;
end;

procedure TLiveTestListener.WriteOut(const AText: string);
begin
  Write(AText);
  Flush(Output);
end;

procedure TLiveTestListener.WriteLineOut(const AText: string);
begin
  WriteLn(AText);
  Flush(Output);
end;

function TLiveTestListener.GetExitCode: Integer;
begin
  // same convention as the FPCUnit console runner
  Result := Ord(FFailed <> 0);
  if FErrors <> 0 then
    Result := Result or 2;
end;

procedure TLiveTestListener.StartTestSuite(ATestSuite: TTestSuite);
begin
  if ATestSuite.TestName <> '' then
  begin
    WriteLineOut(Indent + Paint(ATestSuite.TestName, C_BOLD + C_CYAN) +
      Paint(' (' + IntToStr(ATestSuite.CountTestCases) + ' tests)', C_DIM));
    Inc(FLevel);
  end;
end;

procedure TLiveTestListener.EndTestSuite(ATestSuite: TTestSuite);
begin
  if ATestSuite.TestName <> '' then
    Dec(FLevel);
end;

procedure TLiveTestListener.StartTest(ATest: TTest);
var
  Counter, Name: string;
begin
  Inc(FDone);
  FTestFailure := nil;
  FTestIsError := False;
  Counter := Format('[%*d/%d]', [Length(IntToStr(FTotal)), FDone, FTotal]);
  Name := ATest.TestName;
  if Length(Name) < NAME_WIDTH then
    Name := Name + ' ' + StringOfChar('.', NAME_WIDTH - Length(Name) - 1)
  else
    Name := Name + ' ..';
  WriteOut(Indent + Paint(Counter, C_DIM) + ' ' + Name + ' ');
  FTestStart := GetTickCount64;
end;

procedure TLiveTestListener.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  FTestFailure := AFailure;
  FTestIsError := False;
end;

procedure TLiveTestListener.AddError(ATest: TTest; AError: TTestFailure);
begin
  FTestFailure := AError;
  FTestIsError := True;
end;

procedure TLiveTestListener.EndTest(ATest: TTest);
var
  Elapsed, Status, Detail, FullName: string;
begin
  Elapsed := Paint('(' + FormatDuration(GetTickCount64 - FTestStart) + ')', C_DIM);
  FullName := ATest.TestSuiteName;
  if FullName <> '' then
    FullName := FullName + '.';
  FullName := FullName + ATest.TestName;

  if FTestFailure = nil then
  begin
    Inc(FPassed);
    WriteLineOut(Paint('OK', C_GREEN) + ' ' + Elapsed);
  end
  else if FTestIsError then
  begin
    Inc(FErrors);
    Status := 'ERROR';
    Detail := FTestFailure.ExceptionClassName + ': ' + FTestFailure.ExceptionMessage;
    WriteLineOut(Paint(Status, C_BOLD + C_RED) + ' ' + Elapsed);
    WriteLineOut(Indent + '      ' + Paint(FirstLine(Detail), C_RED));
    FProblems.Add(Status + '  ' + FullName + sLineBreak + '    ' + Detail +
      sLineBreak + '    at ' + FTestFailure.LocationInfo);
  end
  else if FTestFailure.IsIgnoredTest then
  begin
    Inc(FIgnored);
    WriteLineOut(Paint('IGNORED', C_YELLOW) + ' ' + Elapsed);
    if FTestFailure.ExceptionMessage <> '' then
      WriteLineOut(Indent + '      ' + Paint(FirstLine(FTestFailure.ExceptionMessage), C_DIM));
  end
  else
  begin
    Inc(FFailed);
    Status := 'FAILED';
    Detail := FTestFailure.ExceptionMessage;
    WriteLineOut(Paint(Status, C_BOLD + C_RED) + ' ' + Elapsed);
    WriteLineOut(Indent + '      ' + Paint(FirstLine(Detail), C_RED));
    FProblems.Add(Status + '  ' + FullName + sLineBreak + '    ' + Detail +
      sLineBreak + '    at ' + FTestFailure.LocationInfo);
  end;
  FTestFailure := nil;
end;

procedure TLiveTestListener.WriteSummary;
var
  i: Integer;
  Line, Verdict: string;
begin
  WriteLineOut('');
  WriteLineOut(Paint(StringOfChar('-', 72), C_DIM));
  if (FFailed = 0) and (FErrors = 0) then
    Verdict := Paint('PASSED', C_BOLD + C_GREEN)
  else
    Verdict := Paint('FAILED', C_BOLD + C_RED);
  Line := Verdict + '  tests: ' + IntToStr(FDone) +
    '  passed: ' + Paint(IntToStr(FPassed), C_GREEN);
  if FFailed > 0 then
    Line := Line + '  failed: ' + Paint(IntToStr(FFailed), C_RED)
  else
    Line := Line + '  failed: 0';
  if FErrors > 0 then
    Line := Line + '  errors: ' + Paint(IntToStr(FErrors), C_RED)
  else
    Line := Line + '  errors: 0';
  if FIgnored > 0 then
    Line := Line + '  ignored: ' + Paint(IntToStr(FIgnored), C_YELLOW)
  else
    Line := Line + '  ignored: 0';
  Line := Line + '  time: ' + FormatDuration(GetTickCount64 - FRunStart);
  WriteLineOut(Line);
  if FProblems.Count > 0 then
  begin
    WriteLineOut('');
    WriteLineOut(Paint('Problems:', C_BOLD));
    for i := 0 to FProblems.Count - 1 do
      WriteLineOut('  ' + FProblems[i]);
  end;
  WriteLineOut('');
end;

{ TFastPlazTestRunner }

constructor TFastPlazTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLive := True;
  FColor := True;
end;

procedure TFastPlazTestRunner.AppendLongOpts;
begin
  inherited AppendLongOpts;
  LongOpts.Add('no-live');
  LongOpts.Add('no-color');
end;

procedure TFastPlazTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  writeln('  --no-live                 do not print each test as it runs (plain FPCUnit output)');
  writeln('  --no-color                do not use ANSI colors in the live output');
end;

function TFastPlazTestRunner.ParseOptions: Boolean;
begin
  Result := inherited ParseOptions;
  if not Result then
    Exit;
  if HasOption('no-live') then
    FLive := False;
  if HasOption('no-color') or (SysUtils.GetEnvironmentVariable('NO_COLOR') <> '') then
    FColor := False;
  // a report written to stdout in a machine readable format must stay clean
  if (FileName = '') and not (FormatParam in [fPlain, fPlainNoTiming]) then
    FLive := False;
end;

procedure TFastPlazTestRunner.DoTestRun(ATest: TTest);
var
  Live: TLiveTestListener;
  ResultsWriter: TCustomResultsWriter;
  TestResult: TTestResult;
  WriteReport, UseColor: Boolean;
begin
  if not FLive then
  begin
    inherited DoTestRun(ATest);
    Exit;
  end;

  // the plain report duplicates the live output, keep it only for files
  WriteReport := FileName <> '';
  UseColor := FColor and StdOutIsTerminal and EnableConsoleColors;

  Live := nil;
  ResultsWriter := nil;
  TestResult := TTestResult.Create;
  try
    Live := TLiveTestListener.Create(ATest.CountTestCases, UseColor);
    TestResult.AddListener(Live);
    if WriteReport then
    begin
      ResultsWriter := GetResultsWriter;
      ResultsWriter.Filename := FileName;
      TestResult.AddListener(ResultsWriter);
    end;
    writeln(Title);
    writeln;
    ATest.Run(TestResult);
    Live.WriteSummary;
    if WriteReport then
    begin
      ResultsWriter.WriteResult(TestResult);
      writeln('Report written to ', FileName);
    end;
  finally
    if Assigned(Live) then
      ExitCode := Live.GetExitCode;
    TestResult.Free;
    ResultsWriter.Free;
    Live.Free;
  end;
end;

end.

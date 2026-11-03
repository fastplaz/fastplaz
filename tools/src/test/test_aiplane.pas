unit test_aiplane;

{
  Agent Plane core tests.

  TACPAdapterTest drives the real process layer against mock_acp_agent, which
  run_tests.sh/.bat compile first (or point AIPLANE_MOCK_AGENT at a build).
  TClaudeStreamTest feeds captured Claude Code stream-json lines straight into
  the parser, so it needs no claude binary.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  aiplane_types, aiplane_process, aiplane_acp, aiplane_claude, aiplane_pi, aiplane_presets;

type

  { TAIEventRecorder }

  TAIEventRecorder = class
  public
    Events: TStringList;
    Text: string;
    Thinking: string;
    StopReason: string;
    Done: boolean;
    Errors: TStringList;
    Tools: TStringList;
    Sent: TStringList;
    Permissions: array of TAIPermissionRequest;
    AnswerPermission: string; // '' = leave pending, 'reject' = first reject option
    BufferPath, BufferText: string;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure OnEvent(Sender: TObject; const AEvent: TAIEvent);
    procedure OnPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
    function OnReadFile(Sender: TObject; const APath: string; out AContent: string): boolean;
  end;

  { TAIPlaneHelpersTest }

  TAIPlaneHelpersTest = class(TTestCase)
  published
    procedure TestFindExecutableOnPath;
    procedure TestFindExecutableMissing;
    procedure TestToolStatusFromString;
    procedure TestPresetCreatesAdapter;
    procedure TestPresetOverridesCommand;
    procedure TestCopilotPreset;
    procedure TestUnknownPresetRaises;
    procedure TestRuntimeExecutable;
    procedure TestClaudeEffortArg;
    procedure TestSplitProseOnly;
    procedure TestSplitCodeBlock;
    procedure TestSplitUnclosedFence;
    procedure TestSplitEmpty;
    procedure TestFilterModels;
    procedure TestParseQuestions;
    procedure TestParseQuestionsInvalid;
    procedure TestQuestionsText;
  end;

  { TACPAdapterTest }

  TACPAdapterTest = class(TTestCase)
  private
    FAdapter: TACPAdapter;
    FRec: TAIEventRecorder;
    FTempFile: string;
    procedure WaitDone(ATimeoutMs: integer = 10000);
    procedure WaitFor(const AText: string; ATimeoutMs: integer = 10000);
    procedure PromptAndWait(const AText: string);
    procedure BusyPrompt;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestQueuedPromptStartsRuntime;
    procedure TestModelsFromConfigOptions;
    procedure TestModelAvailable;
    procedure TestSecondTurnReusesProcess;
    procedure TestThinkingChunk;
    procedure TestPlanUpdate;
    procedure TestReadFileWithLineAndLimit;
    procedure TestReadFileFromEditorBuffer;
    procedure TestReadMissingFileFails;
    procedure TestPermissionAllow;
    procedure TestPermissionAnsweredLater;
    procedure TestPermissionWithoutHandlerRejects;
    procedure TestCancelStopsTurn;
    procedure TestCancelAnswersPendingPermission;
    procedure TestPromptWhileBusyRaises;
    procedure TestPromptError;
    procedure TestRuntimeCrashThenRestart;
    procedure TestModelAndEffortBeforeStart;
    procedure TestModelChangeWhileReady;
    procedure TestUnknownEffortIgnored;
    procedure TestNonJSONBannerIgnored;
    procedure TestUnicodeText;
    procedure TestNewSession;
    procedure TestMissingExecutable;
    procedure TestStopThenPromptRestarts;
    procedure TestMcpServerSentWhenAgentTakesHttp;
    procedure TestMcpServerSkippedWithoutHttp;
    procedure TestNoMcpServerWithoutUrl;
  end;

  { TClaudeStreamTest }

  TClaudeStreamTest = class(TTestCase)
  private
    FAdapter: TClaudeStreamAdapter;
    FRec: TAIEventRecorder;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitSetsSession;
    procedure TestTextDeltas;
    procedure TestAssistantTextNotDoubled;
    procedure TestAssistantTextWithoutDeltas;
    procedure TestThinkingDelta;
    procedure TestToolUseAndResult;
    procedure TestToolResultError;
    procedure TestResultSuccess;
    procedure TestResultError;
    procedure TestPermissionAllowResponse;
    procedure TestPermissionDenyResponse;
    procedure TestQuestionRequest;
    procedure TestQuestionAnswerResponse;
    procedure TestQuestionDeclined;
    procedure TestQuestionBaseAdapterDeclines;
    procedure TestIgnoresGarbage;
    procedure TestArguments;
    procedure TestMcpConfigArguments;
    procedure TestStaticModels;
    procedure TestModelAvailable;
  end;

  { TPiRpcTest }

  TPiRpcTest = class(TTestCase)
  private
    FAdapter: TPiRpcAdapter;
    FRec: TAIEventRecorder;
    procedure Busy;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestArguments;
    procedure TestSplitModel;
    procedure TestThinkingLevel;
    procedure TestTextAndThinking;
    procedure TestToolLifecycle;
    procedure TestToolError;
    procedure TestModelsAndState;
    procedure TestModelAvailable;
    procedure TestPromptRejected;
    procedure TestSettledFinishesTurn;
    procedure TestSettledIgnoredWhenIdle;
    procedure TestMessageError;
    procedure TestAborted;
    procedure TestUsage;
    procedure TestConfirmDialog;
    procedure TestConfirmRejected;
    procedure TestSelectDialog;
    procedure TestInputDialogCancelled;
    procedure TestPresetUsesRpc;
    procedure TestMcpExtensionArguments;
  end;

implementation

type
  TPiProbe = class(TPiRpcAdapter)
  public
    procedure Probe(AArgs: TStrings);
    procedure MakeBusy;
  end;

procedure TPiProbe.Probe(AArgs: TStrings);
begin
  BuildArguments(AArgs);
end;

procedure TPiProbe.MakeBusy;
begin
  SetState(aasBusy);
end;

type
  TClaudeArgsProbe = class(TClaudeStreamAdapter)
  public
    procedure Probe(AArgs: TStrings);
  end;

procedure TClaudeArgsProbe.Probe(AArgs: TStrings);
begin
  BuildArguments(AArgs);
end;

function MockAgentPath: string;
begin
  Result := GetEnvironmentVariable('AIPLANE_MOCK_AGENT');
  if Result = '' then
    Result := ExpandFileName('mock_acp_agent' + ExtractFileExt(ParamStr(0)));
end;

{ TAIEventRecorder }

constructor TAIEventRecorder.Create;
begin
  Events := TStringList.Create;
  Errors := TStringList.Create;
  Tools := TStringList.Create;
  Sent := TStringList.Create;
  AnswerPermission := 'allow';
end;

destructor TAIEventRecorder.Destroy;
begin
  Sent.Free;
  Tools.Free;
  Errors.Free;
  Events.Free;
  inherited Destroy;
end;

procedure TAIEventRecorder.Reset;
begin
  Events.Clear;
  Errors.Clear;
  Tools.Clear;
  Sent.Clear;
  Text := '';
  Thinking := '';
  StopReason := '';
  Done := False;
  SetLength(Permissions, 0);
end;

procedure TAIEventRecorder.OnEvent(Sender: TObject; const AEvent: TAIEvent);
const
  STATUS: array[TAIToolStatus] of string = ('pending', 'running', 'completed', 'failed');
begin
  case AEvent.Kind of
    aekText: Text := Text + AEvent.Text;
    aekThinking: Thinking := Thinking + AEvent.Text;
    aekError: Errors.Add(AEvent.Text);
    aekToolCall, aekToolUpdate:
      Tools.Add(Format('%s %s %s %s|%s', [AEvent.ToolCallId, STATUS[AEvent.ToolStatus],
        AEvent.ToolTitle, AEvent.ToolPath, Trim(AEvent.Text)]));
    aekDone:
    begin
      Done := True;
      StopReason := AEvent.StopReason;
    end;
    aekLog:
      if Copy(AEvent.Text, 1, 3) = '>> ' then
        Sent.Add(Copy(AEvent.Text, 4, MaxInt));
  end;
  if AEvent.Kind <> aekLog then
    Events.Add(IntToStr(Ord(AEvent.Kind)) + ':' + AEvent.Text);
end;

procedure TAIEventRecorder.OnPermission(Sender: TObject;
  const ARequest: TAIPermissionRequest);
var
  i: integer;
begin
  SetLength(Permissions, Length(Permissions) + 1);
  Permissions[High(Permissions)] := ARequest;
  if AnswerPermission = '' then
    Exit;
  for i := 0 to High(ARequest.Options) do
    if Pos(AnswerPermission, ARequest.Options[i].Kind) = 1 then
    begin
      TAIAdapter(Sender).RespondPermission(ARequest.Key, ARequest.Options[i].Id);
      Exit;
    end;
end;

function TAIEventRecorder.OnReadFile(Sender: TObject; const APath: string;
  out AContent: string): boolean;
begin
  Result := (BufferPath <> '') and SameFileName(APath, BufferPath);
  if Result then
    AContent := BufferText;
end;

{ TAIPlaneHelpersTest }

procedure TAIPlaneHelpersTest.TestFindExecutableOnPath;
begin
  {$IFDEF WINDOWS}
  AssertTrue(AIFindExecutable('cmd') <> '');
  {$ELSE}
  AssertTrue(AIFindExecutable('sh') <> '');
  {$ENDIF}
end;

procedure TAIPlaneHelpersTest.TestFindExecutableMissing;
begin
  AssertEquals('', AIFindExecutable('no_such_runtime_fastplaz'));
  AssertEquals('', AIFindExecutable(''));
  AssertEquals('', AIFindExecutable(ExpandFileName('no_such_dir' + PathDelim + 'x')));
end;

procedure TAIPlaneHelpersTest.TestToolStatusFromString;
begin
  AssertTrue(AIToolStatusFromString('pending') = atsPending);
  AssertTrue(AIToolStatusFromString('in_progress') = atsRunning);
  AssertTrue(AIToolStatusFromString('completed') = atsCompleted);
  AssertTrue(AIToolStatusFromString('failed') = atsFailed);
  AssertTrue(AIToolStatusFromString('???') = atsPending);
end;

procedure TAIPlaneHelpersTest.TestPresetCreatesAdapter;
var
  a: TAIAdapter;
begin
  a := AICreateAdapter(AI_PRESET_OPENCODE);
  try
    AssertTrue(a is TACPAdapter);
    AssertEquals('opencode', a.Command);
    AssertEquals(1, a.Arguments.Count);
    AssertEquals('acp', a.Arguments[0]);
  finally
    a.Free;
  end;
  a := AICreateAdapter(AI_PRESET_CLAUDE);
  try
    AssertTrue(a is TClaudeStreamAdapter);
    AssertEquals('claude', a.Command);
    AssertEquals(0, a.Arguments.Count);
  finally
    a.Free;
  end;
  AssertTrue(AIPresetCount >= 3);
end;

procedure TAIPlaneHelpersTest.TestCopilotPreset;
var
  a: TAIAdapter;
begin
  a := AICreateAdapter(AI_PRESET_COPILOT);
  try
    AssertTrue('Copilot speaks ACP', a is TACPAdapter);
    AssertEquals('copilot', a.Command);
    AssertEquals('--acp', a.Arguments[0]);
    AssertEquals('IDE tools without a second prompt', '--allow-tool=fastplaz',
      a.Arguments[1]);
  finally
    a.Free;
  end;
end;

procedure TAIPlaneHelpersTest.TestPresetOverridesCommand;
var
  a: TAIAdapter;
begin
  a := AICreateAdapter(AI_PRESET_PI, 'C:\tools\my agent.exe', '--x --y');
  try
    AssertEquals('C:\tools\my agent.exe', a.Command);
    AssertEquals(2, a.Arguments.Count);
    AssertEquals('--y', a.Arguments[1]);
  finally
    a.Free;
  end;
end;

procedure TAIPlaneHelpersTest.TestUnknownPresetRaises;
var
  raised: boolean;
begin
  raised := False;
  try
    AICreateAdapter('nope').Free;
  except
    on EAIAdapter do
      raised := True;
  end;
  AssertTrue(raised);
end;

procedure TAIPlaneHelpersTest.TestRuntimeExecutable;
begin
  {$IFDEF WINDOWS}
  AssertTrue('explicit command on PATH', AIRuntimeExecutable(AI_PRESET_PI, 'cmd') <> '');
  {$ELSE}
  AssertTrue('explicit command on PATH', AIRuntimeExecutable(AI_PRESET_PI, 'sh') <> '');
  {$ENDIF}
  AssertEquals('missing command', '',
    AIRuntimeExecutable(AI_PRESET_PI, 'no_such_runtime_fastplaz'));
  AssertEquals('unknown preset', '', AIRuntimeExecutable('nope'));
  AssertTrue(Pos('"no_such_runtime_fastplaz" was not found',
    AIRuntimeMissingMessage(AI_PRESET_PI, 'no_such_runtime_fastplaz')) > 0);
  AssertTrue(Pos('Pi is not available', AIRuntimeMissingMessage(AI_PRESET_PI, 'x_y_z')) > 0);
end;

procedure TAIPlaneHelpersTest.TestClaudeEffortArg;
begin
  AssertEquals('', ClaudeEffortArg(''));
  AssertEquals('', ClaudeEffortArg('auto'));
  AssertEquals('low', ClaudeEffortArg('off'));
  AssertEquals('low', ClaudeEffortArg('minimal'));
  AssertEquals('medium', ClaudeEffortArg('Medium'));
  AssertEquals('xhigh', ClaudeEffortArg('xhigh'));
end;

procedure TAIPlaneHelpersTest.TestSplitProseOnly;
var
  parts: TStringList;
begin
  parts := AISplitCodeBlocks('  Hello' + LineEnding + 'world  ');
  try
    AssertEquals(1, parts.Count);
    AssertEquals('Hello' + LineEnding + 'world', parts[0]);
    AssertNull(parts.Objects[0]);
  finally
    parts.Free;
  end;
end;

procedure TAIPlaneHelpersTest.TestSplitCodeBlock;
var
  parts: TStringList;
begin
  parts := AISplitCodeBlocks('Before' + LineEnding + '```pascal' + LineEnding +
    'begin' + LineEnding + '  WriteLn;' + LineEnding + 'end.' + LineEnding + '```' +
    LineEnding + 'After');
  try
    AssertEquals(3, parts.Count);
    AssertEquals('Before', parts[0]);
    AssertNotNull('code block', parts.Objects[1]);
    AssertEquals('indentation kept', 'begin' + LineEnding + '  WriteLn;' + LineEnding + 'end.', parts[1]);
    AssertEquals('After', parts[2]);
    AssertNull(parts.Objects[2]);
  finally
    parts.Free;
  end;
end;

procedure TAIPlaneHelpersTest.TestSplitUnclosedFence;
var
  parts: TStringList;
begin
  parts := AISplitCodeBlocks('Code:' + LineEnding + '```' + LineEnding + 'x := 1;');
  try
    AssertEquals(2, parts.Count);
    AssertNotNull('still streaming, shown as code', parts.Objects[1]);
    AssertEquals('x := 1;', parts[1]);
  finally
    parts.Free;
  end;
end;

procedure TAIPlaneHelpersTest.TestSplitEmpty;
var
  parts: TStringList;
begin
  parts := AISplitCodeBlocks('');
  try
    AssertEquals(0, parts.Count);
  finally
    parts.Free;
  end;
end;

procedure TAIPlaneHelpersTest.TestParseQuestions;
var
  q: TAIQuestions;
begin
  AssertTrue(AIParseQuestions('{"questions":[{"question":"Which?","header":"Kind",' +
    '"options":[{"label":"A","description":"first"},{"label":"B"},"C",{"description":"no label"}],' +
    '"multiSelect":true},{"question":"","options":[]},{"question":"Name?"}]}', q));
  AssertEquals(2, Length(q));
  AssertEquals('Which?', q[0].Question);
  AssertEquals('Kind', q[0].Header);
  AssertTrue(q[0].MultiSelect);
  AssertEquals(3, Length(q[0].Options));
  AssertEquals('A', q[0].Options[0].Caption);
  AssertEquals('first', q[0].Options[0].Description);
  AssertEquals('B', q[0].Options[1].Caption);
  AssertEquals('C', q[0].Options[2].Caption);
  AssertEquals('Name?', q[1].Question);
  AssertFalse(q[1].MultiSelect);
  AssertEquals(0, Length(q[1].Options));
end;

procedure TAIPlaneHelpersTest.TestParseQuestionsInvalid;
var
  q: TAIQuestions;
begin
  AssertFalse(AIParseQuestions('', q));
  AssertFalse(AIParseQuestions('not json', q));
  AssertFalse(AIParseQuestions('[1]', q));
  AssertFalse(AIParseQuestions('{"command":"ls"}', q));
  AssertFalse(AIParseQuestions('{"questions":{}}', q));
  AssertFalse(AIParseQuestions('{"questions":[1,{"header":"x"}]}', q));
  AssertEquals(0, Length(q));
end;

procedure TAIPlaneHelpersTest.TestQuestionsText;
var
  q: TAIQuestions;
  s: string;
begin
  AIParseQuestions('{"questions":[{"question":"Which?","header":"Kind","multiSelect":true,' +
    '"options":[{"label":"A","description":"first"},{"label":"B"}]}]}', q);
  s := AIQuestionsText(q);
  AssertTrue(s, Pos('[Kind] Which? (choose any)', s) = 1);
  AssertTrue(s, Pos('  - A: first', s) > 0);
  AssertTrue(s, Pos('  - B', s) > 0);
end;

procedure TAIPlaneHelpersTest.TestFilterModels;
var
  all, found: TStringList;
begin
  all := TStringList.Create;
  found := TStringList.Create;
  try
    all.Add('anthropic/claude-sonnet-4');
    all.Add('anthropic/claude-opus-4');
    all.Add('openai/gpt-5');
    all.Add('opencode/deepseek-v4.1-flash-free');
    AIFilterModels(all, '', found);
    AssertEquals('empty filter keeps all', 4, found.Count);
    AIFilterModels(all, 'CLAUDE', found);
    AssertEquals('case-insensitive', 2, found.Count);
    AIFilterModels(all, 'sonnet claude', found);
    AssertEquals('every word, any order', 1, found.Count);
    AssertEquals('anthropic/claude-sonnet-4', found[0]);
    AIFilterModels(all, '  flash  free ', found);
    AssertEquals('extra spaces', 1, found.Count);
    AIFilterModels(all, 'llama', found);
    AssertEquals(0, found.Count);
  finally
    found.Free;
    all.Free;
  end;
end;

{ TACPAdapterTest }

procedure TACPAdapterTest.SetUp;
begin
  if not FileExists(MockAgentPath) then
    Ignore('mock_acp_agent not built: ' + MockAgentPath);
  FRec := TAIEventRecorder.Create;
  FAdapter := TACPAdapter.Create;
  FAdapter.Command := MockAgentPath;
  FAdapter.OnEvent := @FRec.OnEvent;
  FAdapter.OnPermission := @FRec.OnPermission;
  FAdapter.OnReadFile := @FRec.OnReadFile;
  FTempFile := '';
end;

procedure TACPAdapterTest.TearDown;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FRec);
  if (FTempFile <> '') and FileExists(FTempFile) then
    DeleteFile(FTempFile);
end;

procedure TACPAdapterTest.WaitDone(ATimeoutMs: integer);
var
  t: QWord;
begin
  t := GetTickCount64;
  while not FRec.Done do
  begin
    if GetTickCount64 - t > QWord(ATimeoutMs) then
      Fail('Timed out waiting for the turn to finish. Events: ' + FRec.Events.CommaText);
    if not FAdapter.Poll then
      Sleep(5);
  end;
end;

procedure TACPAdapterTest.WaitFor(const AText: string; ATimeoutMs: integer);
var
  t: QWord;
begin
  t := GetTickCount64;
  while Pos(AText, FRec.Text) = 0 do
  begin
    if GetTickCount64 - t > QWord(ATimeoutMs) then
      Fail('Timed out waiting for "' + AText + '". Text: ' + FRec.Text);
    if not FAdapter.Poll then
      Sleep(5);
  end;
end;

procedure TACPAdapterTest.PromptAndWait(const AText: string);
begin
  FRec.Reset;
  FAdapter.Prompt(AText);
  WaitDone;
end;

procedure TACPAdapterTest.BusyPrompt;
begin
  FAdapter.Prompt('hello');
end;

procedure TACPAdapterTest.TestQueuedPromptStartsRuntime;
begin
  AssertTrue(FAdapter.State = aasIdle);
  FAdapter.Prompt('hello');
  AssertTrue('starting while the handshake runs', FAdapter.State = aasStarting);
  WaitDone;
  AssertEquals('Hello from mock', FRec.Text);
  AssertEquals('end_turn', FRec.StopReason);
  AssertTrue(FAdapter.State = aasReady);
  AssertEquals('mock_session_1', FAdapter.SessionId);
  AssertEquals('MockAgent', FAdapter.AgentName);
  AssertEquals(0, FRec.Errors.Count);
end;

procedure TACPAdapterTest.TestModelsFromConfigOptions;
begin
  PromptAndWait('hello');
  AssertEquals(2, Length(FAdapter.Models));
  AssertEquals('mock/fast', FAdapter.Models[0].Id);
  AssertEquals('Mock Fast', FAdapter.Models[0].Name);
  AssertEquals('grouped options are flattened', 'mock/smart', FAdapter.Models[1].Id);
end;

procedure TACPAdapterTest.TestModelAvailable;
begin
  AssertTrue('no list yet: nothing to check against', FAdapter.ModelAvailable('mock/other'));
  PromptAndWait('hello');
  AssertTrue('runtime default', FAdapter.ModelAvailable(''));
  AssertTrue(FAdapter.ModelAvailable('mock/fast'));
  AssertTrue('case does not matter', FAdapter.ModelAvailable('Mock/Smart'));
  AssertFalse(FAdapter.ModelAvailable('mock/other'));
end;

procedure TACPAdapterTest.TestSecondTurnReusesProcess;
begin
  PromptAndWait('hello');
  PromptAndWait('second');
  AssertEquals('echo: second', FRec.Text);
  AssertEquals('mock_session_1', FAdapter.SessionId);
  AssertEquals('no second handshake', -1, FRec.Sent.Text.IndexOf('"initialize"'));
end;

procedure TACPAdapterTest.TestThinkingChunk;
begin
  PromptAndWait('think');
  AssertEquals('Let me think.', FRec.Thinking);
  AssertEquals('Done thinking.', FRec.Text);
end;

procedure TACPAdapterTest.TestPlanUpdate;
var
  i: integer;
  found: boolean;
begin
  PromptAndWait('plan');
  found := False;
  for i := 0 to FRec.Events.Count - 1 do
    if Pos(IntToStr(Ord(aekPlan)) + ':[completed] Read the unit', FRec.Events[i]) = 1 then
      found := Pos('[pending] Write the test', FRec.Events[i]) > 0;
  AssertTrue('plan event with both entries', found);
end;

procedure TACPAdapterTest.TestReadFileWithLineAndLimit;
var
  list: TStringList;
begin
  FTempFile := GetTempFileName('', 'aiplane');
  list := TStringList.Create;
  try
    list.Add('first');
    list.Add('second');
    list.Add('third');
    list.SaveToFile(FTempFile);
  finally
    list.Free;
  end;
  PromptAndWait('read ' + FTempFile);
  AssertEquals('line 2: second', FRec.Text);
  AssertEquals(2, FRec.Tools.Count);
  AssertEquals('call_1 pending Read ' + ExtractFileName(FTempFile) + ' ' + FTempFile + '|',
    FRec.Tools[0]);
  AssertEquals('call_1 completed  |second', FRec.Tools[1]);
end;

procedure TACPAdapterTest.TestReadFileFromEditorBuffer;
begin
  FTempFile := GetTempFileName('', 'aiplane');
  FRec.BufferPath := FTempFile;
  FRec.BufferText := 'one' + LineEnding + 'unsaved' + LineEnding + 'three';
  PromptAndWait('read ' + FTempFile);
  AssertEquals('the editor buffer wins over the missing file', 'line 2: unsaved', FRec.Text);
end;

procedure TACPAdapterTest.TestReadMissingFileFails;
begin
  PromptAndWait('read ' + ExpandFileName('no_such_file_aiplane.txt'));
  AssertEquals('read failed', FRec.Text);
  AssertTrue(Pos('"error"', FRec.Sent.Text) > 0);
end;

procedure TACPAdapterTest.TestPermissionAllow;
begin
  PromptAndWait('permission');
  AssertEquals(1, Length(FRec.Permissions));
  AssertEquals('Delete temp.txt', FRec.Permissions[0].ToolTitle);
  AssertEquals('delete', FRec.Permissions[0].ToolKind);
  AssertEquals(2, Length(FRec.Permissions[0].Options));
  AssertTrue(Pos('temp.txt', FRec.Permissions[0].Detail) > 0);
  AssertEquals('permission: yes', FRec.Text);
end;

procedure TACPAdapterTest.TestPermissionAnsweredLater;
var
  t: QWord;
begin
  FRec.AnswerPermission := '';
  FRec.Reset;
  FAdapter.Prompt('permission');
  t := GetTickCount64;
  while Length(FRec.Permissions) = 0 do
  begin
    if GetTickCount64 - t > 10000 then
      Fail('no permission request');
    if not FAdapter.Poll then
      Sleep(5);
  end;
  // The agent waits while the user decides.
  t := GetTickCount64;
  while GetTickCount64 - t < 200 do
    FAdapter.Poll;
  AssertFalse(FRec.Done);
  FAdapter.RespondPermission(FRec.Permissions[0].Key, 'no');
  WaitDone;
  AssertEquals('permission: no', FRec.Text);
  // A second answer for the same key is ignored.
  FAdapter.RespondPermission(FRec.Permissions[0].Key, 'yes');
end;

procedure TACPAdapterTest.TestPermissionWithoutHandlerRejects;
begin
  FAdapter.OnPermission := nil;
  PromptAndWait('permission');
  AssertEquals('permission: no', FRec.Text);
end;

procedure TACPAdapterTest.TestCancelStopsTurn;
begin
  FRec.Reset;
  FAdapter.Prompt('slow');
  WaitFor('1 ');
  FAdapter.Cancel;
  WaitDone;
  AssertEquals('cancelled', FRec.StopReason);
  AssertTrue('stopped early: ' + FRec.Text, Pos('20 ', FRec.Text) = 0);
  AssertTrue(FAdapter.State = aasReady);
end;

procedure TACPAdapterTest.TestCancelAnswersPendingPermission;
var
  t: QWord;
begin
  FRec.AnswerPermission := '';
  FRec.Reset;
  FAdapter.Prompt('permission');
  t := GetTickCount64;
  while Length(FRec.Permissions) = 0 do
  begin
    if GetTickCount64 - t > 10000 then
      Fail('no permission request');
    if not FAdapter.Poll then
      Sleep(5);
  end;
  FAdapter.Cancel;
  WaitDone;
  AssertEquals('permission: cancelled', FRec.Text);
  AssertEquals('cancelled', FRec.StopReason);
end;

procedure TACPAdapterTest.TestPromptWhileBusyRaises;
begin
  FRec.Reset;
  FAdapter.Prompt('slow');
  WaitFor('1 ');
  AssertException(EAIAdapter, @BusyPrompt);
  FAdapter.Cancel;
  WaitDone;
end;

procedure TACPAdapterTest.TestPromptError;
begin
  PromptAndWait('fail');
  AssertEquals('error', FRec.StopReason);
  AssertEquals(1, FRec.Errors.Count);
  AssertTrue(Pos('mock failure', FRec.Errors[0]) > 0);
  AssertTrue('the session is still usable', FAdapter.State = aasReady);
  PromptAndWait('hello');
  AssertEquals('Hello from mock', FRec.Text);
end;

procedure TACPAdapterTest.TestRuntimeCrashThenRestart;
begin
  PromptAndWait('crash');
  AssertEquals('about to crash', FRec.Text);
  AssertEquals('error', FRec.StopReason);
  AssertTrue(FRec.Errors.Count > 0);
  AssertTrue(Pos('code 3', FRec.Errors[0]) > 0);
  AssertTrue(FAdapter.State = aasFailed);
  PromptAndWait('hello');
  AssertEquals('Hello from mock', FRec.Text);
  AssertTrue(FAdapter.State = aasReady);
end;

procedure TACPAdapterTest.TestModelAndEffortBeforeStart;
begin
  FAdapter.Model := 'mock/smart';
  FAdapter.Effort := 'high';
  PromptAndWait('model?');
  AssertEquals('model=mock/smart effort=high', FRec.Text);
end;

procedure TACPAdapterTest.TestModelChangeWhileReady;
begin
  PromptAndWait('hello');
  FAdapter.Model := 'mock/smart';
  PromptAndWait('model?');
  AssertEquals('model=mock/smart effort=medium', FRec.Text);
end;

procedure TACPAdapterTest.TestUnknownEffortIgnored;
begin
  FAdapter.Effort := 'xhigh';
  PromptAndWait('model?');
  AssertEquals('model=mock/fast effort=medium', FRec.Text);
  AssertEquals(0, FRec.Errors.Count);
end;

procedure TACPAdapterTest.TestNonJSONBannerIgnored;
begin
  FAdapter.Arguments.Add('--banner');
  PromptAndWait('hello');
  AssertEquals('Hello from mock', FRec.Text);
end;

procedure TACPAdapterTest.TestUnicodeText;
begin
  PromptAndWait('unicode');
  AssertEquals('Halo dunia ' + #$E2#$9C#$93 + ' caf' + #$C3#$A9, FRec.Text);
end;

procedure TACPAdapterTest.TestNewSession;
var
  t: QWord;
begin
  PromptAndWait('hello');
  FAdapter.NewSession;
  t := GetTickCount64;
  while FAdapter.State <> aasReady do
  begin
    if GetTickCount64 - t > 10000 then
      Fail('new session not ready');
    if not FAdapter.Poll then
      Sleep(5);
  end;
  AssertEquals('mock_session_2', FAdapter.SessionId);
  PromptAndWait('hello');
  AssertEquals('Hello from mock', FRec.Text);
end;

procedure TACPAdapterTest.TestMissingExecutable;
begin
  FAdapter.Command := 'no_such_runtime_fastplaz';
  PromptAndWait('hello');
  AssertEquals('error', FRec.StopReason);
  AssertTrue(Pos('not found', FRec.Errors.Text) > 0);
  AssertTrue(FAdapter.State = aasFailed);
end;

procedure TACPAdapterTest.TestStopThenPromptRestarts;
begin
  PromptAndWait('hello');
  FAdapter.Stop;
  AssertTrue(FAdapter.State = aasStopped);
  AssertEquals('', FAdapter.SessionId);
  PromptAndWait('again');
  AssertEquals('echo: again', FRec.Text);
end;

procedure TACPAdapterTest.TestMcpServerSentWhenAgentTakesHttp;
begin
  FAdapter.Arguments.Add('--mcp-http');
  FAdapter.McpUrl := 'http://127.0.0.1:17845/mcp';
  FAdapter.McpToken := 'tok123';
  PromptAndWait('mcp?');
  AssertTrue('agent capability read', FAdapter.McpHttp);
  AssertTrue(FRec.Text, Pos('"type" : "http"', FRec.Text) > 0);
  AssertTrue(FRec.Text, Pos('"name" : "fastplaz"', FRec.Text) > 0);
  AssertTrue(FRec.Text, Pos('"url" : "http://127.0.0.1:17845/mcp"', FRec.Text) > 0);
  AssertTrue(FRec.Text, Pos('"value" : "Bearer tok123"', FRec.Text) > 0);
end;

procedure TACPAdapterTest.TestMcpServerSkippedWithoutHttp;
begin
  FAdapter.McpUrl := 'http://127.0.0.1:17845/mcp';
  FAdapter.McpToken := 'tok123';
  PromptAndWait('mcp?');
  AssertFalse(FAdapter.McpHttp);
  AssertEquals('[]', FRec.Text);
end;

procedure TACPAdapterTest.TestNoMcpServerWithoutUrl;
begin
  FAdapter.Arguments.Add('--mcp-http');
  PromptAndWait('mcp?');
  AssertEquals('[]', FRec.Text);
end;

const
  QUESTION_LINE = '{"type":"control_request","request_id":"req_q","request":{"subtype":"can_use_tool","tool_name":"AskUserQuestion","tool_use_id":"toolu_q","input":{"questions":[{"question":"Controller jenis apa?","header":"Jenis","options":[{"label":"Simple Controller","description":"Halaman web"},{"label":"API Controller (JSON)","description":"REST"}],"multiSelect":false},{"question":"Nama = modul?","header":"Nama","options":[{"label":"report","description":"TReportController"}],"multiSelect":true}]}}}';

{ TClaudeStreamTest }

procedure TClaudeStreamTest.SetUp;
begin
  FRec := TAIEventRecorder.Create;
  FAdapter := TClaudeStreamAdapter.Create;
  FAdapter.OnEvent := @FRec.OnEvent;
  FAdapter.OnPermission := @FRec.OnPermission;
end;

procedure TClaudeStreamTest.TearDown;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FRec);
end;

procedure TClaudeStreamTest.TestInitSetsSession;
begin
  FAdapter.HandleLine('{"type":"system","subtype":"init","cwd":"D:\\x","session_id":"abc-123","model":"claude-haiku-5-5","tools":["Read"]}');
  AssertEquals('abc-123', FAdapter.SessionId);
  AssertTrue(Pos('model: claude-haiku-5-5', FRec.Events.Text) > 0);
end;

procedure TClaudeStreamTest.TestTextDeltas;
begin
  FAdapter.HandleLine('{"type":"stream_event","event":{"type":"message_start","message":{}}}');
  FAdapter.HandleLine('{"type":"stream_event","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hero "}}}');
  FAdapter.HandleLine('{"type":"stream_event","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"v0.12"}}}');
  AssertEquals('Hero v0.12', FRec.Text);
end;

procedure TClaudeStreamTest.TestAssistantTextNotDoubled;
begin
  FAdapter.HandleLine('{"type":"stream_event","event":{"type":"message_start","message":{}}}');
  FAdapter.HandleLine('{"type":"stream_event","event":{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"pong"}}}');
  FAdapter.HandleLine('{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"pong"}]}}');
  AssertEquals('pong', FRec.Text);
end;

procedure TClaudeStreamTest.TestAssistantTextWithoutDeltas;
begin
  FAdapter.HandleLine('{"type":"stream_event","event":{"type":"message_start","message":{}}}');
  FAdapter.HandleLine('{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"whole"}]}}');
  AssertEquals('whole', FRec.Text);
end;

procedure TClaudeStreamTest.TestThinkingDelta;
begin
  FAdapter.HandleLine('{"type":"stream_event","event":{"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"hmm"}}}');
  AssertEquals('hmm', FRec.Thinking);
  AssertEquals('', FRec.Text);
end;

procedure TClaudeStreamTest.TestToolUseAndResult;
begin
  FAdapter.HandleLine('{"type":"stream_event","event":{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"toolu_1","name":"Read","input":{}}}}');
  FAdapter.HandleLine('{"type":"assistant","message":{"content":[{"type":"tool_use","id":"toolu_1","name":"Read","input":{"file_path":"D:\\a\\version.txt"}}]}}');
  FAdapter.HandleLine('{"type":"user","message":{"role":"user","content":[{"tool_use_id":"toolu_1","type":"tool_result","content":[{"type":"text","text":"1\tHero v0.12.06"}]}]}}');
  AssertEquals(3, FRec.Tools.Count);
  AssertEquals('toolu_1 pending Read |', FRec.Tools[0]);
  AssertEquals('toolu_1 running Read D:\a\version.txt|{ "file_path" : "D:\\a\\version.txt" }', FRec.Tools[1]);
  AssertEquals('toolu_1 completed  |1'#9'Hero v0.12.06', FRec.Tools[2]);
end;

procedure TClaudeStreamTest.TestToolResultError;
begin
  FAdapter.HandleLine('{"type":"user","message":{"content":[{"tool_use_id":"toolu_2","type":"tool_result","is_error":true,"content":"File does not exist."}]}}');
  AssertEquals('toolu_2 failed  |File does not exist.', FRec.Tools[0]);
end;

procedure TClaudeStreamTest.TestResultSuccess;
begin
  FAdapter.HandleLine('{"type":"result","subtype":"success","is_error":false,"result":"pong","stop_reason":"end_turn","session_id":"s1","total_cost_usd":0.0027,"usage":{"input_tokens":4,"output_tokens":87}}');
  AssertTrue(FRec.Done);
  AssertEquals('end_turn', FRec.StopReason);
  AssertEquals('s1', FAdapter.SessionId);
  AssertEquals(0, FRec.Errors.Count);
  AssertTrue(FRec.Events.Text, Pos('in 4, out 87, cost $0.0027', FRec.Events.Text) > 0);
end;

procedure TClaudeStreamTest.TestResultError;
begin
  FAdapter.HandleLine('{"type":"result","subtype":"error_during_execution","is_error":true,"result":"API Error: overloaded","session_id":"s1"}');
  AssertTrue(FRec.Done);
  AssertEquals('error', FRec.StopReason);
  AssertEquals('API Error: overloaded', FRec.Errors[0]);
end;

procedure TClaudeStreamTest.TestPermissionAllowResponse;
var
  sent: string;
begin
  FAdapter.HandleLine('{"type":"control_request","request_id":"req_7","request":{"subtype":"can_use_tool","tool_name":"Bash","tool_use_id":"toolu_9","input":{"command":"echo hi"}}}');
  AssertEquals(1, Length(FRec.Permissions));
  AssertEquals('Bash', FRec.Permissions[0].ToolTitle);
  AssertEquals('allow', FRec.Permissions[0].Options[0].Id);
  AssertEquals(1, FRec.Sent.Count);
  sent := FRec.Sent[0];
  AssertTrue(sent, Pos('"type" : "control_response"', sent) > 0);
  AssertTrue(sent, Pos('"request_id" : "req_7"', sent) > 0);
  AssertTrue(sent, Pos('"behavior" : "allow"', sent) > 0);
  AssertTrue(sent, Pos('"updatedInput" : { "command" : "echo hi" }', sent) > 0);
end;

procedure TClaudeStreamTest.TestPermissionDenyResponse;
begin
  FRec.AnswerPermission := 'reject';
  FAdapter.HandleLine('{"type":"control_request","request_id":"req_8","request":{"subtype":"can_use_tool","tool_name":"Write","input":{"file_path":"x"}}}');
  AssertEquals(1, FRec.Sent.Count);
  AssertTrue(Pos('"behavior" : "deny"', FRec.Sent[0]) > 0);
end;

procedure TClaudeStreamTest.TestQuestionRequest;
begin
  FRec.AnswerPermission := '';
  FAdapter.HandleLine(QUESTION_LINE);
  AssertEquals(1, Length(FRec.Permissions));
  AssertEquals('AskUserQuestion', FRec.Permissions[0].ToolTitle);
  AssertEquals(2, Length(FRec.Permissions[0].Questions));
  AssertEquals('Controller jenis apa?', FRec.Permissions[0].Questions[0].Question);
  AssertEquals(2, Length(FRec.Permissions[0].Questions[0].Options));
  AssertTrue(FRec.Permissions[0].Questions[1].MultiSelect);
  AssertEquals(0, FRec.Sent.Count);
end;

procedure TClaudeStreamTest.TestQuestionAnswerResponse;
var
  sent: string;
begin
  FRec.AnswerPermission := '';
  FAdapter.HandleLine(QUESTION_LINE);
  FAdapter.RespondQuestion(FRec.Permissions[0].Key, ['API Controller (JSON)', 'report, invoice']);
  AssertEquals(1, FRec.Sent.Count);
  sent := FRec.Sent[0];
  AssertTrue(sent, Pos('"request_id" : "req_q"', sent) > 0);
  AssertTrue(sent, Pos('"behavior" : "allow"', sent) > 0);
  // the questions go back with the answers keyed by question text
  AssertTrue(sent, Pos('"questions" : [', sent) > 0);
  AssertTrue(sent, Pos('"answers" : { "Controller jenis apa?" : "API Controller (JSON)", ' +
    '"Nama = modul?" : "report, invoice" }', sent) > 0);
  // answered once only
  FAdapter.RespondQuestion(FRec.Permissions[0].Key, ['again']);
  AssertEquals(1, FRec.Sent.Count);
end;

procedure TClaudeStreamTest.TestQuestionDeclined;
begin
  FRec.AnswerPermission := '';
  FAdapter.HandleLine(QUESTION_LINE);
  FAdapter.RespondQuestion(FRec.Permissions[0].Key, []);
  AssertEquals(1, FRec.Sent.Count);
  AssertTrue(FRec.Sent[0], Pos('"behavior" : "deny"', FRec.Sent[0]) > 0);
  AssertTrue(FRec.Sent[0], Pos('"request_id" : "req_q"', FRec.Sent[0]) > 0);
end;

procedure TClaudeStreamTest.TestQuestionBaseAdapterDeclines;
var
  pi: TPiRpcAdapter;
begin
  // runtimes without question support decline through RespondPermission
  pi := TPiRpcAdapter.Create;
  try
    pi.RespondQuestion('unknown', ['x']);
  finally
    pi.Free;
  end;
end;

procedure TClaudeStreamTest.TestIgnoresGarbage;
begin
  FAdapter.HandleLine('not json');
  FAdapter.HandleLine('[1,2]');
  FAdapter.HandleLine('{"type":"rate_limit_event"}');
  AssertEquals(0, FRec.Events.Count);
end;

procedure TClaudeStreamTest.TestArguments;
var
  probe: TClaudeArgsProbe;
  args: TStringList;
begin
  probe := TClaudeArgsProbe.Create;
  args := TStringList.Create;
  try
    probe.Probe(args);
    AssertTrue(args.IndexOf('--model') < 0);
    AssertTrue(args.IndexOf('--effort') < 0);
    AssertTrue(args.IndexOf('stream-json') > 0);
    AssertTrue(args.IndexOf('stdio') > 0);
    args.Clear;
    probe.Model := 'sonnet';
    probe.Effort := 'minimal';
    probe.HandleLine('{"type":"system","subtype":"init","session_id":"sid-1"}');
    probe.Arguments.Add('--extra');
    probe.Probe(args);
    AssertEquals('sonnet', args[args.IndexOf('--model') + 1]);
    AssertEquals('low', args[args.IndexOf('--effort') + 1]);
    AssertEquals('sid-1', args[args.IndexOf('--resume') + 1]);
    AssertEquals('--extra', args[args.Count - 1]);
  finally
    args.Free;
    probe.Free;
  end;
end;

procedure TClaudeStreamTest.TestMcpConfigArguments;
var
  probe: TClaudeArgsProbe;
  args, config: TStringList;
  fileName: string;
begin
  probe := TClaudeArgsProbe.Create;
  args := TStringList.Create;
  config := TStringList.Create;
  try
    probe.Probe(args);
    AssertTrue('no server, no --mcp-config', args.IndexOf('--mcp-config') < 0);
    args.Clear;
    probe.McpUrl := 'http://127.0.0.1:17845/mcp';
    probe.McpToken := 'tok123';
    probe.Probe(args);
    AssertTrue(args.IndexOf('--mcp-config') >= 0);
    fileName := args[args.IndexOf('--mcp-config') + 1];
    AssertTrue('config file written', FileExists(fileName));
    config.LoadFromFile(fileName);
    AssertTrue(Pos('"fastplaz"', config.Text) > 0);
    AssertTrue(Pos('"type" : "http"', config.Text) > 0);
    AssertTrue(Pos('http://127.0.0.1:17845/mcp', config.Text) > 0);
    AssertTrue(Pos('Bearer tok123', config.Text) > 0);
    AssertEquals('mcp__fastplaz', args[args.IndexOf('--allowedTools') + 1]);
  finally
    config.Free;
    args.Free;
    probe.Free;
  end;
  AssertFalse('config file removed with the adapter', FileExists(fileName));
end;

procedure TClaudeStreamTest.TestStaticModels;
begin
  AssertEquals(3, Length(FAdapter.Models));
  AssertEquals('opus', FAdapter.Models[0].Id);
end;

procedure TClaudeStreamTest.TestModelAvailable;
begin
  AssertTrue(FAdapter.ModelAvailable(''));
  AssertTrue(FAdapter.ModelAvailable('sonnet'));
  AssertTrue('full model id', FAdapter.ModelAvailable('claude-sonnet-4-5'));
  AssertFalse(FAdapter.ModelAvailable('gpt-5'));
end;

{ TPiRpcTest }

procedure TPiRpcTest.SetUp;
begin
  FRec := TAIEventRecorder.Create;
  FAdapter := TPiProbe.Create;
  FAdapter.OnEvent := @FRec.OnEvent;
  FAdapter.OnPermission := @FRec.OnPermission;
end;

procedure TPiRpcTest.TearDown;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FRec);
end;

procedure TPiRpcTest.Busy;
begin
  TPiProbe(FAdapter).MakeBusy;
  FRec.Reset;
end;

procedure TPiRpcTest.TestArguments;
var
  args: TStringList;
begin
  args := TStringList.Create;
  try
    FAdapter.Arguments.Add('--no-session');
    TPiProbe(FAdapter).Probe(args);
    AssertEquals('--mode', args[0]);
    AssertEquals('rpc', args[1]);
    AssertEquals('--no-session', args[2]);
    AssertEquals('pi', FAdapter.Command);
  finally
    args.Free;
  end;
end;

procedure TPiRpcTest.TestMcpExtensionArguments;
var
  probe: TPiProbe;
  args, ext: TStringList;
  fileName: string;
begin
  probe := TPiProbe.Create;
  args := TStringList.Create;
  ext := TStringList.Create;
  try
    probe.Probe(args);
    AssertTrue('no server, no extension', args.IndexOf('-e') < 0);
    args.Clear;
    probe.McpUrl := 'http://127.0.0.1:17845/mcp';
    probe.McpToken := 'tok123';
    probe.Probe(args);
    AssertTrue(args.IndexOf('-e') >= 0);
    fileName := args[args.IndexOf('-e') + 1];
    AssertTrue('extension written', FileExists(fileName));
    ext.LoadFromFile(fileName);
    AssertTrue(Pos('pi.registerMcpServer("fastplaz"', ext.Text) > 0);
    AssertTrue(Pos('http:\/\/127.0.0.1:17845\/mcp', ext.Text) +
      Pos('http://127.0.0.1:17845/mcp', ext.Text) > 0);
    AssertTrue(Pos('Bearer tok123', ext.Text) > 0);
    AssertTrue(Pos('"direct"', ext.Text) > 0);
  finally
    ext.Free;
    args.Free;
    probe.Free;
  end;
  AssertFalse('extension removed with the adapter', FileExists(fileName));
end;

procedure TPiRpcTest.TestSplitModel;
var
  provider, id: string;
begin
  PiSplitModel('anthropic/claude-sonnet-4', provider, id);
  AssertEquals('anthropic', provider);
  AssertEquals('claude-sonnet-4', id);
  PiSplitModel('openrouter/meta/llama-4', provider, id);
  AssertEquals('openrouter', provider);
  AssertEquals('ids may contain slashes', 'meta/llama-4', id);
  PiSplitModel('gpt-5', provider, id);
  AssertEquals('', provider);
  AssertEquals('gpt-5', id);
end;

procedure TPiRpcTest.TestThinkingLevel;
begin
  AssertEquals('', PiThinkingLevel('auto'));
  AssertEquals('', PiThinkingLevel(''));
  AssertEquals('off', PiThinkingLevel('off'));
  AssertEquals('xhigh', PiThinkingLevel('XHigh'));
  AssertEquals('max', PiThinkingLevel('max'));
end;

procedure TPiRpcTest.TestTextAndThinking;
begin
  FAdapter.HandleLine('{"type":"message_update","assistantMessageEvent":{"type":"thinking_delta","contentIndex":0,"delta":"hmm"}}');
  FAdapter.HandleLine('{"type":"message_update","usage":{"input":1},"assistantMessageEvent":{"type":"text_delta","contentIndex":1,"delta":"Hel"}}');
  FAdapter.HandleLine('{"type":"message_update","assistantMessageEvent":{"type":"text_delta","contentIndex":1,"delta":"lo"}}');
  FAdapter.HandleLine('{"type":"message_update","assistantMessageEvent":{"type":"text_end","contentIndex":1,"content":"Hello"}}');
  AssertEquals('hmm', FRec.Thinking);
  AssertEquals('text_end does not repeat the text', 'Hello', FRec.Text);
end;

procedure TPiRpcTest.TestToolLifecycle;
begin
  FAdapter.HandleLine('{"type":"message_update","assistantMessageEvent":{"type":"toolcall_start","contentIndex":0,"id":"call_1","toolName":"read"}}');
  FAdapter.HandleLine('{"type":"tool_execution_start","toolCallId":"call_1","toolName":"read","args":{"path":"src/a.pas"}}');
  FAdapter.HandleLine('{"type":"tool_execution_end","toolCallId":"call_1","toolName":"read","result":{"content":[{"type":"text","text":"unit a;"}],"details":{}},"isError":false,"durationMs":3}');
  AssertEquals(3, FRec.Tools.Count);
  AssertEquals('call_1 pending read |', FRec.Tools[0]);
  AssertEquals('call_1 running read src/a.pas|{ "path" : "src/a.pas" }', FRec.Tools[1]);
  AssertEquals('call_1 completed read |unit a;', FRec.Tools[2]);
end;

procedure TPiRpcTest.TestToolError;
begin
  FAdapter.HandleLine('{"type":"tool_execution_end","toolCallId":"c2","toolName":"bash","result":{"content":[{"type":"text","text":"not found"}]},"isError":true}');
  AssertEquals('c2 failed bash |not found', FRec.Tools[0]);
end;

procedure TPiRpcTest.TestModelsAndState;
begin
  FAdapter.HandleLine('{"id":"fp1","type":"response","command":"get_available_models","success":true,"data":{"models":[' +
    '{"id":"claude-sonnet-4","name":"Claude Sonnet 4","provider":"anthropic"},' +
    '{"id":"gpt-5","name":"GPT-5","provider":"openai"}]}}');
  AssertEquals(2, Length(FAdapter.Models));
  AssertEquals('anthropic/claude-sonnet-4', FAdapter.Models[0].Id);
  AssertEquals('Claude Sonnet 4', FAdapter.Models[0].Name);
  FAdapter.HandleLine('{"id":"fp2","type":"response","command":"get_state","success":true,"data":{' +
    '"model":{"id":"gpt-5","provider":"openai"},"thinkingLevel":"medium","sessionId":"abc123"}}');
  AssertEquals('abc123', FAdapter.SessionId);
  AssertTrue(Pos('model: openai/gpt-5', FRec.Events.Text) > 0);
end;

procedure TPiRpcTest.TestModelAvailable;
begin
  FAdapter.HandleLine('{"id":"fp1","type":"response","command":"get_available_models","success":true,"data":{"models":[' +
    '{"id":"claude-sonnet-4","name":"Claude Sonnet 4","provider":"anthropic"}]}}');
  AssertTrue(FAdapter.ModelAvailable('anthropic/claude-sonnet-4'));
  AssertTrue('bare id of a listed model', FAdapter.ModelAvailable('claude-sonnet-4'));
  AssertFalse('other provider', FAdapter.ModelAvailable('openai/claude-sonnet-4'));
  AssertFalse(FAdapter.ModelAvailable('gpt-5'));
end;

procedure TPiRpcTest.TestPromptRejected;
begin
  Busy;
  FAdapter.HandleLine('{"id":"fp3","type":"response","command":"prompt","success":false,"error":"No API key found for the selected model."}');
  AssertTrue(FRec.Done);
  AssertEquals('error', FRec.StopReason);
  AssertEquals('No API key found for the selected model.', FRec.Errors[0]);
  AssertTrue(FAdapter.State = aasReady);
end;

procedure TPiRpcTest.TestSettledFinishesTurn;
begin
  Busy;
  FAdapter.HandleLine('{"id":"fp4","type":"response","command":"prompt","success":true,"data":{"disposition":"started"}}');
  AssertFalse('accepted is not finished', FRec.Done);
  FAdapter.HandleLine('{"type":"agent_end","messages":[],"willRetry":false}');
  AssertFalse('agent_end is not the end yet', FRec.Done);
  FAdapter.HandleLine('{"type":"agent_settled","aborted":false}');
  AssertTrue(FRec.Done);
  AssertEquals('end_turn', FRec.StopReason);
  AssertTrue(FAdapter.State = aasReady);
end;

procedure TPiRpcTest.TestSettledIgnoredWhenIdle;
begin
  FAdapter.HandleLine('{"type":"agent_settled","aborted":false}');
  AssertFalse(FRec.Done);
end;

procedure TPiRpcTest.TestMessageError;
begin
  Busy;
  FAdapter.HandleLine('{"type":"message_end","message":{"role":"assistant","content":[],"stopReason":"error","errorMessage":"rate limited","usage":{"input":0,"output":0}}}');
  FAdapter.HandleLine('{"type":"agent_settled","aborted":false}');
  AssertEquals('rate limited', FRec.Errors[0]);
  AssertEquals('error', FRec.StopReason);
end;

procedure TPiRpcTest.TestAborted;
begin
  Busy;
  FAdapter.HandleLine('{"type":"agent_settled","aborted":true}');
  AssertEquals('cancelled', FRec.StopReason);
end;

procedure TPiRpcTest.TestUsage;
begin
  FAdapter.HandleLine('{"type":"message_end","message":{"role":"assistant","content":[],"stopReason":"stop",' +
    '"usage":{"input":100,"output":5,"cacheRead":0,"cacheWrite":0,"totalTokens":105,"cost":{"input":0.001,"output":0.0002,"total":0.0012}}}}');
  AssertTrue(FRec.Events.Text, Pos('in 100, out 5, cost $0.0012', FRec.Events.Text) > 0);
  // user messages are not assistant output
  FRec.Reset;
  FAdapter.HandleLine('{"type":"message_end","message":{"role":"user","content":"hi"}}');
  AssertEquals(0, FRec.Events.Count);
end;

procedure TPiRpcTest.TestConfirmDialog;
var
  sent: string;
begin
  FAdapter.HandleLine('{"type":"extension_ui_request","id":"uuid-2","method":"confirm","title":"Run rm -rf build?","message":"This deletes the folder.","timeout":5000}');
  AssertEquals(1, Length(FRec.Permissions));
  AssertEquals('Run rm -rf build?', FRec.Permissions[0].ToolTitle);
  AssertEquals('This deletes the folder.', FRec.Permissions[0].Detail);
  AssertEquals(2, Length(FRec.Permissions[0].Options));
  sent := FRec.Sent[FRec.Sent.Count - 1];
  AssertTrue(sent, Pos('"type" : "extension_ui_response"', sent) > 0);
  AssertTrue(sent, Pos('"id" : "uuid-2"', sent) > 0);
  AssertTrue(sent, Pos('"confirmed" : true', sent) > 0);
end;

procedure TPiRpcTest.TestConfirmRejected;
begin
  FRec.AnswerPermission := 'reject';
  FAdapter.HandleLine('{"type":"extension_ui_request","id":"u3","method":"confirm","title":"Sure?"}');
  AssertTrue(Pos('"confirmed" : false', FRec.Sent[FRec.Sent.Count - 1]) > 0);
end;

procedure TPiRpcTest.TestSelectDialog;
var
  sent: string;
begin
  FAdapter.HandleLine('{"type":"extension_ui_request","id":"uuid-1","method":"select","title":"Allow dangerous command?","options":["Allow","Block"]}');
  AssertEquals(2, Length(FRec.Permissions[0].Options));
  AssertEquals('Block', FRec.Permissions[0].Options[1].Name);
  sent := FRec.Sent[FRec.Sent.Count - 1];
  AssertTrue(sent, Pos('"value" : "Allow"', sent) > 0);
end;

procedure TPiRpcTest.TestInputDialogCancelled;
var
  sent: string;
begin
  FAdapter.HandleLine('{"type":"extension_ui_request","id":"u9","method":"input","title":"Name?"}');
  AssertEquals(0, Length(FRec.Permissions));
  sent := FRec.Sent[FRec.Sent.Count - 1];
  AssertTrue(sent, Pos('"id" : "u9"', sent) > 0);
  AssertTrue(sent, Pos('"cancelled" : true', sent) > 0);
end;

procedure TPiRpcTest.TestPresetUsesRpc;
var
  a: TAIAdapter;
begin
  a := AICreateAdapter(AI_PRESET_PI);
  try
    AssertTrue(a is TPiRpcAdapter);
    AssertEquals('pi', a.Command);
  finally
    a.Free;
  end;
end;

initialization
  RegisterTest(TAIPlaneHelpersTest);
  RegisterTest(TACPAdapterTest);
  RegisterTest(TClaudeStreamTest);
  RegisterTest(TPiRpcTest);

end.

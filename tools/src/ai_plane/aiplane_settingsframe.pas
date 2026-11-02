unit aiplane_settingsframe;

{
  Agent Plane default settings: runtime, command, arguments, model and effort,
  with model detection and a "Run Test" that sends one prompt.

  With IDE tools (MCP) on, Run Test also checks the MCP path: it serves a
  temporary MCP server with one tool, connection_check, hands it to the
  runtime like the chat does, and asks the agent to call it. The result says
  whether the runtime answered and whether the agent reached the tool.

  Used by the AI Harness tab of FastPlaz/Configuration and by the tester app;
  the host loads and stores the values (Runtime, Command, ... properties).

  Detection starts the runtime and waits for its session (ACP agents report
  their models there); Claude Code has a fixed list of aliases. Everything is
  polled from a timer, no threads.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Spin,
  aiplane_types, aiplane_presets, aiplane_mcpserver;

type

  { TAIPlaneSettingsFrame }

  TAIPlaneSettingsFrame = class(TFrame)
    btnDetect: TButton;
    btnTest: TButton;
    cbbEffort: TComboBox;
    cbbModel: TComboBox;
    cbbRuntime: TComboBox;
    chkMcpEnabled: TCheckBox;
    edt_Arguments: TEdit;
    edt_Command: TEdit;
    lblArguments: TLabel;
    lblCommand: TLabel;
    lblEffort: TLabel;
    lblMcpPort: TLabel;
    lblModel: TLabel;
    lblNote: TLabel;
    lblRuntime: TLabel;
    lblStatus: TLabel;
    mem_Output: TMemo;
    spnMcpPort: TSpinEdit;
    tmrProbe: TTimer;
    procedure McpEnabledChange(Sender: TObject);
    procedure btnDetectClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure cbbModelChange(Sender: TObject);
    procedure cbbRuntimeSelect(Sender: TObject);
    procedure tmrProbeTimer(Sender: TObject);
    procedure ValueChange(Sender: TObject);
  private
    FProbe: TAIAdapter;
    FProbeTesting: boolean;
    FProbeStarted: QWord;
    FProbeText: string;
    FProbeError: string;
    FWorkingDir: string;
    FOnChange: TNotifyEvent;
    FAllModels: TStringList;
    FFiltering: boolean;
    FProbeMcp: TAIMcpServer;
    FCheckWord: string;
    FToolCalled: boolean;
    function StartProbeMcp: boolean;
    procedure StopProbeMcp;
    procedure ProbeMcpLog(Sender: TObject; const AText: string);
    procedure ToolConnectionCheck(ACall: TAIMcpToolCall);
    procedure SetModelList(const AModels: TAIModelList);
    procedure SetupControls;
    procedure UpdateNote;
    procedure StartProbe(ATest: boolean);
    procedure FinishProbe(const AMessage: string);
    procedure ProbeEvent(Sender: TObject; const AEvent: TAIEvent);
    procedure ProbePermission(Sender: TObject; const ARequest: TAIPermissionRequest);
    function GetRuntime: string;
    procedure SetRuntime(const AValue: string);
    function GetCommand: string;
    procedure SetCommand(const AValue: string);
    function GetArguments: string;
    procedure SetArguments(const AValue: string);
    function GetModel: string;
    procedure SetModel(const AValue: string);
    function GetEffort: string;
    procedure SetEffort(const AValue: string);
    function GetMcpEnabled: boolean;
    procedure SetMcpEnabled(AValue: boolean);
    function GetMcpPort: integer;
    procedure SetMcpPort(AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Fills the model list for the selected runtime. }
    procedure DetectModels;
    { Starts the runtime and sends AIPLANE_TEST_PROMPT, or with IDE tools
      on AIPLANE_TEST_MCP_PROMPT against a temporary MCP server. }
    procedure RunTest;
    function Busy: boolean;
    property Runtime: string read GetRuntime write SetRuntime;
    property Command: string read GetCommand write SetCommand;
    property Arguments: string read GetArguments write SetArguments;
    property Model: string read GetModel write SetModel;
    property Effort: string read GetEffort write SetEffort;
    { IDE tools through the MCP server (on by default) and its port. }
    property McpEnabled: boolean read GetMcpEnabled write SetMcpEnabled;
    property McpPort: integer read GetMcpPort write SetMcpPort;
    { Where the test runs the runtime ('' = current directory). }
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

const
  AIPLANE_TEST_PROMPT = 'Reply with exactly one word: OK';
  AIPLANE_TEST_TOOL = 'connection_check';
  AIPLANE_TEST_MCP_PROMPT = 'Call the ' + AI_MCP_SERVER_ID + ' tool ' +
    AIPLANE_TEST_TOOL + ' and reply with only the word it returns.';
  AIPLANE_PROBE_TIMEOUT_MS = 90000;

implementation

{$R *.lfm}

{ TAIPlaneSettingsFrame }

constructor TAIPlaneSettingsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllModels := TStringList.Create;
  SetupControls;
  SetRuntime(AI_DEFAULT_PRESET);
end;

destructor TAIPlaneSettingsFrame.Destroy;
begin
  // nil when loading the .lfm failed
  if Assigned(tmrProbe) then
    tmrProbe.Enabled := False;
  if Assigned(FProbe) then
  begin
    FProbe.OnEvent := nil;
    FProbe.Stop;
    FreeAndNil(FProbe);
  end;
  StopProbeMcp;
  FAllModels.Free;
  inherited Destroy;
end;

// Controls are in the .lfm; the lists come from code.
procedure TAIPlaneSettingsFrame.SetupControls;
var
  i: integer;
begin
  cbbRuntime.Items.Clear;
  for i := 0 to AIPresetCount - 1 do
    cbbRuntime.Items.Add(AIPreset(i).Title);
  cbbEffort.Items.Clear;
  for i := 0 to High(AI_EFFORT_LEVELS) do
    cbbEffort.Items.Add(AI_EFFORT_LEVELS[i]);
  cbbEffort.ItemIndex := 0;
  btnTest.Hint := 'Start the runtime and send: ' + AIPLANE_TEST_PROMPT;
  cbbModel.TextHint := 'runtime default';
end;

procedure TAIPlaneSettingsFrame.UpdateNote;
var
  preset: TAIRuntimePreset;
begin
  if AIFindPreset(GetRuntime, preset) then
  begin
    edt_Command.TextHint := preset.Command;
    edt_Arguments.TextHint := preset.Arguments;
    lblNote.Caption := preset.AdapterClass.AdapterName + ' - ' + preset.Note;
  end
  else
    lblNote.Caption := '';
end;

function TAIPlaneSettingsFrame.Busy: boolean;
begin
  Result := Assigned(FProbe);
end;

procedure TAIPlaneSettingsFrame.StartProbe(ATest: boolean);
var
  i: integer;
begin
  if Busy then
    Exit;
  mem_Output.Clear;
  FProbeText := '';
  FProbeError := '';
  FProbeTesting := ATest;
  try
    FProbe := AICreateAdapter(GetRuntime, GetCommand, GetArguments);
  except
    on E: Exception do
    begin
      lblStatus.Caption := E.Message;
      Exit;
    end;
  end;
  FProbe.OnEvent := @ProbeEvent;
  FProbe.OnPermission := @ProbePermission;
  FProbe.WorkingDir := FWorkingDir;
  FProbe.Model := GetModel;
  FProbe.Effort := GetEffort;
  FToolCalled := False;
  if ATest and GetMcpEnabled and StartProbeMcp then
  begin
    FProbe.McpUrl := FProbeMcp.Url;
    FProbe.McpToken := FProbeMcp.Token;
  end;
  btnDetect.Enabled := False;
  btnTest.Enabled := False;
  lblStatus.Font.Color := clWindowText;
  lblStatus.Caption := 'Starting ' + FProbe.Command + ' ...';
  FProbeStarted := GetTickCount64;
  try
    if ATest and Assigned(FProbeMcp) then
      FProbe.Prompt(AIPLANE_TEST_MCP_PROMPT)
    else if ATest then
      FProbe.Prompt(AIPLANE_TEST_PROMPT)
    else if Length(FProbe.Models) > 0 then
    begin
      // Fixed list, nothing to start.
      SetModelList(FProbe.Models);
      FinishProbe(Format('%d model(s)', [Length(FProbe.Models)]));
      Exit;
    end
    else
      FProbe.Start;
  except
    on E: Exception do
    begin
      FProbeError := E.Message;
      FinishProbe('');
      Exit;
    end;
  end;
  tmrProbe.Enabled := True;
end;

procedure TAIPlaneSettingsFrame.FinishProbe(const AMessage: string);
var
  probe: TAIAdapter;
begin
  tmrProbe.Enabled := False;
  probe := FProbe;
  FProbe := nil;
  if Assigned(probe) then
  begin
    probe.OnEvent := nil;
    probe.OnPermission := nil;
    probe.Stop;
    probe.Free;
  end;
  StopProbeMcp;
  btnDetect.Enabled := True;
  btnTest.Enabled := True;
  if FProbeError <> '' then
  begin
    lblStatus.Font.Color := clRed;
    lblStatus.Caption := 'Failed: ' + FProbeError;
  end
  else
  begin
    // a runtime that answers but does not reach the IDE tools is a warning
    if Pos('IDE tools not reached', AMessage) > 0 then
      lblStatus.Font.Color := $0060A0  // dark orange
    else
      lblStatus.Font.Color := clGreen;
    lblStatus.Caption := AMessage;
  end;
end;

// The test's own MCP server: the configured port, or the next free one when
// the IDE's server holds it.
function TAIPlaneSettingsFrame.StartProbeMcp: boolean;
begin
  FCheckWord := 'plaz-' + Copy(AIMcpNewToken, 1, 6);
  FProbeMcp := TAIMcpServer.Create;
  FProbeMcp.OnLog := @ProbeMcpLog;
  FProbeMcp.Instructions := 'Agent Plane connection test of the IDE tools.';
  FProbeMcp.RegisterTool(AIPLANE_TEST_TOOL, 'Connection check',
    'Returns the check word of this Agent Plane connection test.',
    '{"type":"object","properties":{}}', mtpAuto, @ToolConnectionCheck);
  Result := FProbeMcp.Start(GetMcpPort);
  if not Result then
  begin
    mem_Output.Lines.Add('[mcp] not started: ' + FProbeMcp.LastError);
    FreeAndNil(FProbeMcp);
  end;
end;

procedure TAIPlaneSettingsFrame.StopProbeMcp;
begin
  if not Assigned(FProbeMcp) then
    Exit;
  FProbeMcp.OnLog := nil;
  FProbeMcp.Stop;
  FreeAndNil(FProbeMcp);
end;

procedure TAIPlaneSettingsFrame.ProbeMcpLog(Sender: TObject; const AText: string);
begin
  mem_Output.Lines.Add('[mcp] ' + Copy(AText, 1, 300));
end;

procedure TAIPlaneSettingsFrame.ToolConnectionCheck(ACall: TAIMcpToolCall);
begin
  FToolCalled := True;
  ACall.Finish('The check word is: ' + FCheckWord);
end;

procedure TAIPlaneSettingsFrame.ProbeEvent(Sender: TObject; const AEvent: TAIEvent);
begin
  case AEvent.Kind of
    aekText:
      FProbeText := FProbeText + AEvent.Text;
    aekStatus:
      mem_Output.Lines.Add(AEvent.Text);
    aekError:
    begin
      mem_Output.Lines.Add('error: ' + AEvent.Text);
      if FProbeError = '' then
        FProbeError := AEvent.Text;
    end;
    aekLog:
      if (Copy(AEvent.Text, 1, 7) = 'stderr:') or
        (Copy(AEvent.Text, 1, 14) = 'IDE tools off:') then
        mem_Output.Lines.Add(AEvent.Text);
    aekToolCall:
      mem_Output.Lines.Add('tool: ' + AEvent.ToolTitle);
  end;
end;

procedure TAIPlaneSettingsFrame.ProbePermission(Sender: TObject;
  const ARequest: TAIPermissionRequest);
var
  i: integer;
begin
  // The test's own tool may run; refuse anything else it asks for.
  if Assigned(FProbeMcp) and (Pos(AIPLANE_TEST_TOOL,
    LowerCase(ARequest.ToolTitle + ' ' + ARequest.Detail)) > 0) then
    for i := 0 to High(ARequest.Options) do
      if ARequest.Options[i].Kind = 'allow_once' then
      begin
        TAIAdapter(Sender).RespondPermission(ARequest.Key, ARequest.Options[i].Id);
        Exit;
      end;
  TAIAdapter(Sender).RespondPermission(ARequest.Key, '');
end;

procedure TAIPlaneSettingsFrame.tmrProbeTimer(Sender: TObject);
var
  i: integer;
  keep: string;
  elapsed: double;
begin
  if not Assigned(FProbe) then
  begin
    tmrProbe.Enabled := False;
    Exit;
  end;
  while FProbe.Poll do ;
  if not Assigned(FProbe) then
    Exit;
  if Assigned(FProbeMcp) then
    while FProbeMcp.Poll do ;

  if GetTickCount64 - FProbeStarted > AIPLANE_PROBE_TIMEOUT_MS then
  begin
    FProbeError := 'no answer within ' + IntToStr(AIPLANE_PROBE_TIMEOUT_MS div 1000) + ' s';
    FinishProbe('');
    Exit;
  end;

  if FProbe.State = aasFailed then
  begin
    if FProbeError = '' then
      FProbeError := 'runtime stopped';
    FinishProbe('');
    Exit;
  end;

  if Length(FProbe.Models) > 0 then
    SetModelList(FProbe.Models);

  if FProbeTesting then
  begin
    if FProbe.State = aasReady then
    begin
      mem_Output.Lines.Add('');
      mem_Output.Lines.Add('answer: ' + Trim(FProbeText));
      elapsed := (GetTickCount64 - FProbeStarted) / 1000;
      if FProbeError <> '' then
        FinishProbe('')
      else if not Assigned(FProbeMcp) then
        FinishProbe(Format('OK in %.1f s: %s', [elapsed, Copy(Trim(FProbeText), 1, 60)]))
      else if FToolCalled and (Pos(FCheckWord, StringReplace(FProbeText,
        LineEnding, '', [rfReplaceAll])) > 0) then
        FinishProbe(Format('OK in %.1f s: runtime answered, IDE tools (MCP) reached',
          [elapsed]))
      else if FToolCalled then
        FinishProbe(Format('OK in %.1f s: IDE tools (MCP) called, but the answer ' +
          'did not repeat the check word', [elapsed]))
      else
        FinishProbe(Format('Runtime OK in %.1f s, IDE tools not reached: the ' +
          'agent did not call %s (see the log below)', [elapsed, AIPLANE_TEST_TOOL]));
    end;
  end
  else if FProbe.State = aasReady then
    FinishProbe(Format('%d model(s) found', [Length(FProbe.Models)]));
end;

// The full model list; the combo box shows the part that matches what is
// typed in it.
procedure TAIPlaneSettingsFrame.SetModelList(const AModels: TAIModelList);
var
  i: integer;
  keep: string;
begin
  FAllModels.Clear;
  for i := 0 to High(AModels) do
    FAllModels.Add(AModels[i].Id);
  FFiltering := True;
  try
    keep := cbbModel.Text;
    cbbModel.Items.Assign(FAllModels);
    cbbModel.Text := keep;
  finally
    FFiltering := False;
  end;
  if FAllModels.Count > 0 then
  begin
    cbbModel.TextHint := Format('runtime default - type to search %d models', [FAllModels.Count]);
    cbbModel.Hint := Format('%d model(s); type part of a name to filter, every word must match',
      [FAllModels.Count]);
  end
  else
  begin
    cbbModel.TextHint := 'runtime default';
    cbbModel.Hint := 'Model (empty = runtime default)';
  end;
end;

procedure TAIPlaneSettingsFrame.cbbModelChange(Sender: TObject);
var
  typed: string;
  caret: integer;
begin
  if FFiltering then
    Exit;
  ValueChange(Sender);
  if FAllModels.Count = 0 then
    Exit;
  typed := cbbModel.Text;
  caret := cbbModel.SelStart;
  FFiltering := True;
  try
    // a picked model (exact name) shows the whole list again next time
    if (typed = '') or (FAllModels.IndexOf(typed) >= 0) then
      cbbModel.Items.Assign(FAllModels)
    else
      AIFilterModels(FAllModels, typed, cbbModel.Items);
    // changing Items resets the edit text on some widgetsets
    if cbbModel.Text <> typed then
      cbbModel.Text := typed;
    if cbbModel.Focused and (typed <> '') and (FAllModels.IndexOf(typed) < 0) and
      (cbbModel.Items.Count > 0) and not cbbModel.DroppedDown then
    begin
      cbbModel.DroppedDown := True;
      // dropping down selects the whole text: keep typing where we were
      if cbbModel.Text <> typed then
        cbbModel.Text := typed;
    end;
    cbbModel.SelStart := caret;
    cbbModel.SelLength := 0;
  finally
    FFiltering := False;
  end;
end;

procedure TAIPlaneSettingsFrame.DetectModels;
begin
  StartProbe(False);
end;

procedure TAIPlaneSettingsFrame.RunTest;
begin
  StartProbe(True);
end;

procedure TAIPlaneSettingsFrame.cbbRuntimeSelect(Sender: TObject);
begin
  edt_Command.Text := '';
  edt_Arguments.Text := '';
  SetModelList(nil);
  cbbModel.Text := '';
  UpdateNote;
  ValueChange(Sender);
  if AIRuntimeExecutable(GetRuntime) = '' then
  begin
    // Still selectable: the user may type the command path next.
    lblStatus.Font.Color := clRed;
    lblStatus.Caption := 'Not found: ' + AIRuntimeMissingMessage(GetRuntime);
    mem_Output.Lines.Text := AIRuntimeMissingMessage(GetRuntime);
    Exit;
  end;
  DetectModels;
end;

procedure TAIPlaneSettingsFrame.btnDetectClick(Sender: TObject);
begin
  DetectModels;
end;

procedure TAIPlaneSettingsFrame.btnTestClick(Sender: TObject);
begin
  RunTest;
end;

procedure TAIPlaneSettingsFrame.ValueChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TAIPlaneSettingsFrame.GetRuntime: string;
begin
  if cbbRuntime.ItemIndex < 0 then
    Result := ''
  else
    Result := AIPreset(cbbRuntime.ItemIndex).Id;
end;

procedure TAIPlaneSettingsFrame.SetRuntime(const AValue: string);
var
  i: integer;
begin
  for i := 0 to AIPresetCount - 1 do
    if SameText(AIPreset(i).Id, AValue) then
      cbbRuntime.ItemIndex := i;
  UpdateNote;
end;

function TAIPlaneSettingsFrame.GetCommand: string;
begin
  Result := Trim(edt_Command.Text);
end;

procedure TAIPlaneSettingsFrame.SetCommand(const AValue: string);
begin
  edt_Command.Text := AValue;
end;

function TAIPlaneSettingsFrame.GetArguments: string;
begin
  Result := Trim(edt_Arguments.Text);
end;

procedure TAIPlaneSettingsFrame.SetArguments(const AValue: string);
begin
  edt_Arguments.Text := AValue;
end;

function TAIPlaneSettingsFrame.GetModel: string;
begin
  Result := Trim(cbbModel.Text);
end;

procedure TAIPlaneSettingsFrame.SetModel(const AValue: string);
begin
  cbbModel.Text := AValue;
end;

function TAIPlaneSettingsFrame.GetEffort: string;
begin
  if cbbEffort.ItemIndex < 0 then
    Result := 'auto'
  else
    Result := cbbEffort.Items[cbbEffort.ItemIndex];
end;

procedure TAIPlaneSettingsFrame.SetEffort(const AValue: string);
var
  i: integer;
begin
  i := cbbEffort.Items.IndexOf(LowerCase(AValue));
  if i < 0 then
    i := 0;
  cbbEffort.ItemIndex := i;
end;

procedure TAIPlaneSettingsFrame.McpEnabledChange(Sender: TObject);
begin
  spnMcpPort.Enabled := chkMcpEnabled.Checked;
  lblMcpPort.Enabled := chkMcpEnabled.Checked;
  ValueChange(Sender);
end;

function TAIPlaneSettingsFrame.GetMcpEnabled: boolean;
begin
  Result := chkMcpEnabled.Checked;
end;

procedure TAIPlaneSettingsFrame.SetMcpEnabled(AValue: boolean);
begin
  chkMcpEnabled.Checked := AValue;
  spnMcpPort.Enabled := AValue;
  lblMcpPort.Enabled := AValue;
end;

function TAIPlaneSettingsFrame.GetMcpPort: integer;
begin
  Result := spnMcpPort.Value;
end;

procedure TAIPlaneSettingsFrame.SetMcpPort(AValue: integer);
begin
  spnMcpPort.Value := AValue;
end;

end.

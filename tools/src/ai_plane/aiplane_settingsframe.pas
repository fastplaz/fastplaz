unit aiplane_settingsframe;

{
  AI Plane default settings: runtime, command, arguments, model and effort,
  with model detection and a "Run Test" that sends one prompt.

  Used by the AI Plane tab of Fast Code Configuration and by the tester app;
  the host loads and stores the values (Runtime, Command, ... properties).

  Detection starts the runtime and waits for its session (ACP agents report
  their models there); Claude Code has a fixed list of aliases. Everything is
  polled from a timer, no threads.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  aiplane_types, aiplane_presets;

type

  { TAIPlaneSettingsFrame }

  TAIPlaneSettingsFrame = class(TFrame)
    btnDetect: TButton;
    btnTest: TButton;
    cbbEffort: TComboBox;
    cbbModel: TComboBox;
    cbbRuntime: TComboBox;
    edt_Arguments: TEdit;
    edt_Command: TEdit;
    lblArguments: TLabel;
    lblCommand: TLabel;
    lblEffort: TLabel;
    lblModel: TLabel;
    lblNote: TLabel;
    lblRuntime: TLabel;
    lblStatus: TLabel;
    mem_Output: TMemo;
    tmrProbe: TTimer;
    procedure btnDetectClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Fills the model list for the selected runtime. }
    procedure DetectModels;
    { Starts the runtime and sends AIPLANE_TEST_PROMPT. }
    procedure RunTest;
    function Busy: boolean;
    property Runtime: string read GetRuntime write SetRuntime;
    property Command: string read GetCommand write SetCommand;
    property Arguments: string read GetArguments write SetArguments;
    property Model: string read GetModel write SetModel;
    property Effort: string read GetEffort write SetEffort;
    { Where the test runs the runtime ('' = current directory). }
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

const
  AIPLANE_TEST_PROMPT = 'Reply with exactly one word: OK';
  AIPLANE_PROBE_TIMEOUT_MS = 90000;

implementation

{$R *.lfm}

{ TAIPlaneSettingsFrame }

constructor TAIPlaneSettingsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  btnDetect.Enabled := False;
  btnTest.Enabled := False;
  lblStatus.Font.Color := clWindowText;
  lblStatus.Caption := 'Starting ' + FProbe.Command + ' ...';
  FProbeStarted := GetTickCount64;
  try
    if ATest then
      FProbe.Prompt(AIPLANE_TEST_PROMPT)
    else if Length(FProbe.Models) > 0 then
    begin
      // Fixed list, nothing to start.
      cbbModel.Items.Clear;
      for i := 0 to High(FProbe.Models) do
        cbbModel.Items.Add(FProbe.Models[i].Id);
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
  btnDetect.Enabled := True;
  btnTest.Enabled := True;
  if FProbeError <> '' then
  begin
    lblStatus.Font.Color := clRed;
    lblStatus.Caption := 'Failed: ' + FProbeError;
  end
  else
  begin
    lblStatus.Font.Color := clGreen;
    lblStatus.Caption := AMessage;
  end;
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
      if Copy(AEvent.Text, 1, 7) = 'stderr:' then
        mem_Output.Lines.Add(AEvent.Text);
  end;
end;

procedure TAIPlaneSettingsFrame.ProbePermission(Sender: TObject;
  const ARequest: TAIPermissionRequest);
begin
  // The test prompt needs no tools; refuse anything it asks for.
  TAIAdapter(Sender).RespondPermission(ARequest.Key, '');
end;

procedure TAIPlaneSettingsFrame.tmrProbeTimer(Sender: TObject);
var
  i: integer;
  keep: string;
begin
  if not Assigned(FProbe) then
  begin
    tmrProbe.Enabled := False;
    Exit;
  end;
  while FProbe.Poll do ;
  if not Assigned(FProbe) then
    Exit;

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
  begin
    keep := cbbModel.Text;
    cbbModel.Items.Clear;
    for i := 0 to High(FProbe.Models) do
      cbbModel.Items.Add(FProbe.Models[i].Id);
    cbbModel.Text := keep;
  end;

  if FProbeTesting then
  begin
    if FProbe.State = aasReady then
    begin
      mem_Output.Lines.Add('');
      mem_Output.Lines.Add('answer: ' + Trim(FProbeText));
      if FProbeError = '' then
        FinishProbe(Format('OK in %.1f s: %s', [(GetTickCount64 - FProbeStarted) / 1000,
          Copy(Trim(FProbeText), 1, 60)]))
      else
        FinishProbe('');
    end;
  end
  else if FProbe.State = aasReady then
    FinishProbe(Format('%d model(s) found', [Length(FProbe.Models)]));
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
  cbbModel.Items.Clear;
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

end.

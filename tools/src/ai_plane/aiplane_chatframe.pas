unit aiplane_chatframe;

{
  The Agent Plane chat as a frame, so the IDE window and the tester app show the
  same thing:

    [Runtime v] [Model v] [Effort v]           [New] [Log]
    +----------------------------------------------------+
    | conversation (TAIChatView)                          |
    +----------------------------------------------------+
    | log (hidden by default)                             |
    +----------------------------------------------------+
    | input                                     [Send]   |
    status: agent - model - session - usage

  Enter sends, Shift+Enter adds a line, Esc stops the running turn. The
  adapter is polled from a timer, so all events arrive in the main thread.

  The host sets the runtime (SetRuntime), the working directory and,
  optionally, OnReadFile to serve unsaved editor buffers. OnSettingsChanged
  tells it to persist runtime/model/effort.

  The model is remembered per runtime (RuntimeModels, "preset=model"): picking
  another runtime brings back the model last used with it, and starts the
  runtime so its model list fills the model box.

  The runtime starts as soon as it is chosen. Send stays disabled until it
  answered and while the chosen model is not one it offers
  (TAIAdapter.ModelAvailable); the reason is in the button hint and the
  status line. New conversation retries a runtime that failed to start.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Buttons, LCLType,
  LCLIntf, Menus,
  aiplane_types, aiplane_presets, aiplane_chatview;

type
  { Answer to a permission card shown with ShowToolPermission. }
  TAIToolPermissionAnswer = procedure(Sender: TObject;
    const AKey, AOptionId: string) of object;

  { A line for the host's own log (the IDE Messages window): the runtime and
    model became active, or the agent reported an error. }
  TAIChatNotice = procedure(Sender: TObject; const AText: string;
    AIsError: boolean) of object;

  { TAIChatFrame }

  TAIChatFrame = class(TFrame)
    btnLog: TSpeedButton;
    btnNew: TSpeedButton;
    btnSend: TButton;
    cbbEffort: TComboBox;
    cbbModel: TComboBox;
    cbbRuntime: TComboBox;
    lblEffort: TLabel;
    lblModel: TLabel;
    lblRuntime: TLabel;
    lblStatus: TLabel;
    pnlStatus: TPanel;
    mem_Input: TMemo;
    mem_Log: TMemo;
    pnlInput: TPanel;
    pnlTop: TPanel;
    pnlView: TPanel;
    splLog: TSplitter;
    tmrPoll: TTimer;
    procedure cbbRuntimeSelect(Sender: TObject);
    procedure lblEffortClick(Sender: TObject);
    procedure lblModelClick(Sender: TObject);
    procedure lblRuntimeClick(Sender: TObject);
    procedure StatusLabelMouseEnter(Sender: TObject);
    procedure StatusLabelMouseLeave(Sender: TObject);
    procedure cbbModelChange(Sender: TObject);
    procedure cbbEffortSelect(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnLogClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure mem_InputKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure tmrPollTimer(Sender: TObject);
  private
    FAdapter: TAIAdapter;
    FView: TAIChatView;
    FInputHint: TStaticText;
    FStatusMenu: TPopupMenu;
    FPresetId: string;
    FCommand: string;
    FArguments: string;
    FWorkingDir: string;
    FAgentText, FModelText, FSessionText, FUsageText: string;
    FCurrentText: TAIChatItem;
    FCurrentThinking: TAIChatItem;
    FOnReadFile: TAIReadFileHandler;
    FOnSettingsChanged: TNotifyEvent;
    FFillingModels: boolean;
    FModelCount: integer;
    FMcpUrl: string;
    FMcpToken: string;
    FMcpStatus: string;
    FOnToolPermission: TAIToolPermissionAnswer;
    FRuntimeModels: TStringList;
    FStartPending: boolean;
    FHoldStart: boolean;
    FWasReady: boolean;
    FTurnQueued: boolean;
    FActiveNotice: string;
    FOnNotice: TAIChatNotice;
    procedure ShowError(const AText: string);
    procedure DoNotice(const AText: string; AIsError: boolean);
    function RuntimeTitle: string;
    procedure RememberModel;
    function TurnRunning: boolean;
    function SendBlockReason: string;
    procedure SetupControls;
    procedure ShowStatusMenu(ALabel: TLabel);
    function AddStatusMenuItem(const ACaption: string; ATag: integer;
      AChecked, AEnabled: boolean; AHandler: TNotifyEvent): TMenuItem;
    procedure RuntimeMenuClick(Sender: TObject);
    procedure ModelMenuClick(Sender: TObject);
    procedure EffortMenuClick(Sender: TObject);
    function CurrentModel: string;
    procedure InputChange(Sender: TObject);
    procedure InputHintClick(Sender: TObject);
    procedure FitComboWidths;
    procedure CreateAdapter;
    procedure FreeAdapter;
    procedure UpdateControls;
    procedure UpdateStatus;
    procedure FillModels;
    procedure Send;
    procedure AdapterEvent(Sender: TObject; const AEvent: TAIEvent);
    procedure AdapterPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
    function AdapterReadFile(Sender: TObject; const APath: string; out AContent: string): boolean;
    procedure ViewButtonClick(Sender: TObject; AItem: TAIChatItem; const AButtonId: string);
    function GetEffort: string;
    function GetModel: string;
    procedure SetEffort(const AValue: string);
    procedure SetModel(const AValue: string);
    procedure SetWorkingDir(const AValue: string);
    procedure SetHoldStart(AValue: boolean);
    procedure DoSettingsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Switches runtime. ACommand/AArguments override the preset when not
      empty. Stops the current runtime. }
    procedure SetRuntime(const APresetId: string; const ACommand: string = '';
      const AArguments: string = '');
    procedure SendPrompt(const AText: string);
    { True when Send is enabled: the runtime answered and the chosen model is
      one it offers. }
    function ReadyToSend: boolean;
    procedure NewChat;
    procedure FocusInput;
    { The IDE's MCP server for the next runtime start (empty URL: no IDE
      tools). Restarts the runtime when it changed. AStatus is shown in the
      status line ("MCP", an error, empty when off). }
    procedure SetMcpServer(const AUrl, AToken, AStatus: string);
    { Shows a permission card for one of the IDE's own tools; the answer goes
      to OnToolPermission, not to the runtime. }
    procedure ShowToolPermission(const ARequest: TAIPermissionRequest);
    { Adds a line to the protocol log (shown with the Log button). }
    procedure AddLog(const AText: string);
    property Adapter: TAIAdapter read FAdapter;
    property View: TAIChatView read FView;
    property PresetId: string read FPresetId;
    property Command: string read FCommand;
    property Arguments: string read FArguments;
    property Model: string read GetModel write SetModel;
    property Effort: string read GetEffort write SetEffort;
    { The model last used with each runtime, "preset=model". Updated by the
      frame; the host loads and saves it. }
    property RuntimeModels: TStringList read FRuntimeModels;
    property WorkingDir: string read FWorkingDir write SetWorkingDir;
    { True keeps the runtime from starting on its own (the host does not know
      the working directory yet); False starts it if it is due. Sending a
      prompt still starts it. }
    property HoldStart: boolean read FHoldStart write SetHoldStart;
    property OnReadFile: TAIReadFileHandler read FOnReadFile write FOnReadFile;
    property OnSettingsChanged: TNotifyEvent read FOnSettingsChanged write FOnSettingsChanged;
    property OnToolPermission: TAIToolPermissionAnswer read FOnToolPermission
      write FOnToolPermission;
    property OnNotice: TAIChatNotice read FOnNotice write FOnNotice;
  end;

implementation

{$R *.lfm}

const
  MAX_LOG_LINES = 2000;
  MIN_COMBO_WIDTH = 60;
  MAX_COMBO_WIDTH = 260;

type
  TToolIcon = (tiNew, tiLog);

{ Toolbar glyphs drawn in code: no image files, they follow the theme's text
  colour and the screen scaling, in the IDE and in the tester alike. }
function MakeToolIcon(AIcon: TToolIcon; ASize: integer): TBitmap;
var
  c: TCanvas;
  m, x1, x2, i, y: integer;
begin
  Result := TBitmap.Create;
  Result.SetSize(ASize, ASize);
  c := Result.Canvas;
  c.Brush.Color := clFuchsia;
  c.FillRect(0, 0, ASize, ASize);
  c.Pen.Color := ColorToRGB(clBtnText);
  c.Pen.Width := ASize div 8;
  if c.Pen.Width < 1 then
    c.Pen.Width := 1;
  m := ASize div 2;
  case AIcon of
    tiNew:
    begin
      // plus sign
      c.Line(m, ASize div 5, m, ASize - ASize div 5);
      c.Line(ASize div 5, m, ASize - ASize div 5, m);
    end;
    tiLog:
    begin
      // lines of text
      x1 := ASize div 6;
      for i := 0 to 3 do
      begin
        y := ASize div 5 + i * (ASize * 3 div 5) div 3;
        if Odd(i) then
          x2 := ASize - ASize div 3
        else
          x2 := ASize - x1;
        c.Line(x1, y, x2, y);
      end;
    end;
  end;
  Result.TransparentColor := clFuchsia;
  Result.Transparent := True;
end;

{ Readable name of an effort level, shown in the status bar and its menu. }
function EffortCaption(const ALevel: string): string;
begin
  case LowerCase(ALevel) of
    'auto': Result := 'Auto';
    'off': Result := 'Off';
    'minimal': Result := 'Minimal';
    'low': Result := 'Low';
    'medium': Result := 'Medium';
    'high': Result := 'High';
    'xhigh': Result := 'Extra High';
    else
      Result := ALevel;
  end;
end;

{ Colour of an effort level in the status bar. Mid-tone colours, readable
  on light and dark themes; yellow is a darker amber so it shows on white. }
function EffortColor(const ALevel: string): TColor;
begin
  case LowerCase(ALevel) of
    'auto': Result := RGBToColor(40, 160, 70);      // green
    'off': Result := clGrayText;
    'minimal': Result := RGBToColor(200, 150, 0);   // yellow / amber
    'low': Result := RGBToColor(0, 150, 160);       // teal
    'medium': Result := RGBToColor(40, 110, 220);   // blue
    'high': Result := RGBToColor(215, 45, 45);      // red
    'xhigh': Result := RGBToColor(165, 50, 190);    // purple
    else
      Result := clWindowText;
  end;
end;

{ "opencode/deepseek-v4.1-flash-free" -> "deepseek-v4.1-flash-free" }
function ShortModelName(const AModel: string): string;
var
  p: integer;
begin
  Result := AModel;
  p := LastDelimiter('/', Result);
  if (p > 0) and (p < Length(Result)) then
    Result := Copy(Result, p + 1, MaxInt);
end;

function TextWidthOf(AControl: TControl; const AText: string): integer;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(4, 4);
    bmp.Canvas.Font.Assign(AControl.Font);
    Result := bmp.Canvas.TextWidth(AText);
  finally
    bmp.Free;
  end;
end;

{ TAIChatFrame }

constructor TAIChatFrame.Create(AOwner: TComponent);
begin
  FRuntimeModels := TStringList.Create;
  inherited Create(AOwner);
  SetupControls;
  SetRuntime(AI_DEFAULT_PRESET);
end;

destructor TAIChatFrame.Destroy;
begin
  // nil when loading the .lfm failed
  if Assigned(tmrPoll) then
    tmrPoll.Enabled := False;
  FreeAdapter;
  inherited Destroy;
  FreeAndNil(FRuntimeModels);
end;

procedure TAIChatFrame.ShowError(const AText: string);
begin
  FView.Add(cikError, AText);
  DoNotice(AText, True);
end;

procedure TAIChatFrame.DoNotice(const AText: string; AIsError: boolean);
begin
  if Assigned(FOnNotice) then
    FOnNotice(Self, AText, AIsError);
end;

// What the runtime calls itself, else the preset title.
function TAIChatFrame.RuntimeTitle: string;
var
  preset: TAIRuntimePreset;
begin
  if FAgentText <> '' then
    Result := FAgentText
  else if AIFindPreset(FPresetId, preset) then
    Result := preset.Title
  else
    Result := FPresetId;
end;

procedure TAIChatFrame.RememberModel;
begin
  // an empty value removes the entry: runtime default
  if FPresetId <> '' then
    FRuntimeModels.Values[FPresetId] := GetModel;
end;

// A prompt is running (or queued while the runtime starts): Send is Stop.
function TAIChatFrame.TurnRunning: boolean;
begin
  Result := Assigned(FAdapter) and ((FAdapter.State = aasBusy) or
    ((FAdapter.State = aasStarting) and FTurnQueued));
end;

function TAIChatFrame.ReadyToSend: boolean;
begin
  Result := not TurnRunning and (SendBlockReason = '');
end;

// Why Send is disabled; empty when the runtime answered and the chosen model
// is one it offers.
function TAIChatFrame.SendBlockReason: string;
begin
  Result := '';
  if not Assigned(FAdapter) then
    Exit('No runtime');
  if FAdapter.State = aasFailed then
    Exit('The runtime did not start; New conversation tries again');
  if not FWasReady then
    Exit('Waiting for the runtime ...');
  if not FAdapter.ModelAvailable(FAdapter.Model) then
    Exit(Format('Model "%s" is not available in this runtime', [FAdapter.Model]));
end;

// Controls are in the .lfm; this adds what depends on code: the runtime
// list, the effort levels and the owner drawn conversation view.
procedure TAIChatFrame.SetupControls;
var
  i: integer;
  icon: TBitmap;
begin
  cbbRuntime.Items.Clear;
  for i := 0 to AIPresetCount - 1 do
    cbbRuntime.Items.Add(AIPreset(i).Title);
  cbbEffort.Items.Clear;
  for i := 0 to High(AI_EFFORT_LEVELS) do
    cbbEffort.Items.Add(AI_EFFORT_LEVELS[i]);
  cbbEffort.ItemIndex := 0;
  // Placeholder. TMemo.TextHint does not work for a multi-line edit on
  // Windows (EM_SETCUEBANNER is single-line only), so a windowed label sits
  // on the memo while it is empty; a TLabel would be painted behind it.
  FInputHint := TStaticText.Create(Self);
  FInputHint.Parent := pnlInput;
  FInputHint.Caption := 'Shift+Enter to line break';
  FInputHint.AutoSize := True;
  FInputHint.Color := clWindow;
  FInputHint.Font.Color := clGrayText;
  FInputHint.Cursor := crIBeam;
  // a few pixels right of the caret, which stays visible
  FInputHint.Left := mem_Input.Left + Scale96ToScreen(6);
  FInputHint.Top := mem_Input.Top + Scale96ToScreen(3);
  FInputHint.OnClick := @InputHintClick;
  FInputHint.BringToFront;
  mem_Input.OnChange := @InputChange;

  icon := MakeToolIcon(tiNew, Scale96ToScreen(16));
  try
    btnNew.Glyph := icon;
  finally
    icon.Free;
  end;
  icon := MakeToolIcon(tiLog, Scale96ToScreen(16));
  try
    btnLog.Glyph := icon;
  finally
    icon.Free;
  end;
  FitComboWidths;

  FView := TAIChatView.Create(Self);
  FView.Parent := pnlView;
  FView.Align := alClient;
  FView.OnButtonClick := @ViewButtonClick;
end;

procedure TAIChatFrame.InputChange(Sender: TObject);
begin
  FInputHint.Visible := mem_Input.Text = '';
  if FInputHint.Visible then
    FInputHint.BringToFront;
end;

procedure TAIChatFrame.InputHintClick(Sender: TObject);
begin
  FocusInput;
end;

// Model and effort boxes are as wide as their text: the effort box fits its
// longest level, the model box fits the current model and its drop-down list
// fits the longest model id.
procedure TAIChatFrame.FitComboWidths;

  function Fit(ACombo: TComboBox; const AText: string): integer;
  begin
    Result := TextWidthOf(ACombo, AText) + GetSystemMetrics(SM_CXVSCROLL) +
      Scale96ToScreen(16);
    if Result < Scale96ToScreen(MIN_COMBO_WIDTH) then
      Result := Scale96ToScreen(MIN_COMBO_WIDTH);
    if Result > Scale96ToScreen(MAX_COMBO_WIDTH) then
      Result := Scale96ToScreen(MAX_COMBO_WIDTH);
  end;

var
  i: integer;
  longest, s: string;
begin
  longest := '';
  for i := 0 to cbbEffort.Items.Count - 1 do
    if Length(cbbEffort.Items[i]) > Length(longest) then
      longest := cbbEffort.Items[i];
  cbbEffort.Width := Fit(cbbEffort, longest);

  longest := cbbModel.Text;
  for i := 0 to cbbModel.Items.Count - 1 do
    if TextWidthOf(cbbModel, cbbModel.Items[i]) > TextWidthOf(cbbModel, longest) then
      longest := cbbModel.Items[i];
  s := cbbModel.Text;
  if s = '' then
    s := 'runtime default';
  cbbModel.Width := Fit(cbbModel, s);
  cbbModel.ItemWidth := TextWidthOf(cbbModel, longest) + Scale96ToScreen(24);
end;

procedure TAIChatFrame.SetRuntime(const APresetId: string; const ACommand: string;
  const AArguments: string);
var
  i: integer;
  keep: string;
begin
  FPresetId := APresetId;
  FCommand := ACommand;
  FArguments := AArguments;
  for i := 0 to AIPresetCount - 1 do
    if SameText(AIPreset(i).Id, APresetId) then
      cbbRuntime.ItemIndex := i;
  keep := GetModel;
  FreeAdapter;
  cbbModel.Items.Clear;
  cbbModel.Text := keep;
  FModelCount := -1;
  CreateAdapter;
end;

procedure TAIChatFrame.CreateAdapter;
var
  keepModel, keepEffort: string;
begin
  keepModel := GetModel;
  keepEffort := GetEffort;
  try
    FAdapter := AICreateAdapter(FPresetId, FCommand, FArguments);
  except
    on E: Exception do
    begin
      FAdapter := nil;
      ShowError(E.Message);
      UpdateControls;
      Exit;
    end;
  end;
  FAdapter.OnEvent := @AdapterEvent;
  FAdapter.OnPermission := @AdapterPermission;
  FAdapter.OnReadFile := @AdapterReadFile;
  FAdapter.WorkingDir := FWorkingDir;
  FAdapter.McpUrl := FMcpUrl;
  FAdapter.McpToken := FMcpToken;
  FAdapter.Model := keepModel;
  FAdapter.Effort := keepEffort;
  FAgentText := FAdapter.Command;
  FModelText := '';
  FSessionText := '';
  FUsageText := '';
  FWasReady := False;
  FTurnQueued := False;
  FActiveNotice := '';
  // Start on the next tick, not here: the host may still switch runtime
  // (SetRuntime right after Create), and only the last one should run.
  FStartPending := True;
  tmrPoll.Enabled := True;
  FillModels;
  UpdateControls;
  UpdateStatus;
end;

procedure TAIChatFrame.FreeAdapter;
begin
  if not Assigned(FAdapter) then
    Exit;
  tmrPoll.Enabled := False;
  FAdapter.OnEvent := nil;
  FAdapter.OnPermission := nil;
  try
    FAdapter.Stop;
  except
  end;
  FreeAndNil(FAdapter);
  FCurrentText := nil;
  FCurrentThinking := nil;
end;

procedure TAIChatFrame.UpdateControls;
var
  turn: boolean;
  reason, key, curModel: string;
begin
  if Assigned(FAdapter) and (FAdapter.State = aasReady) then
  begin
    FWasReady := True;
    // Tell the host once per runtime/model pair. The key leaves out the
    // title: the runtime may report a better name ("pi" -> "Pi") later.
    curModel := CurrentModel;
    if curModel = '' then
      curModel := 'runtime default';
    key := FPresetId + #9 + curModel;
    if key <> FActiveNotice then
    begin
      FActiveNotice := key;
      DoNotice('runtime ' + RuntimeTitle + ' active, model ' + curModel, False);
    end;
  end;
  turn := TurnRunning;
  if turn then
  begin
    btnSend.Caption := 'Stop';
    btnSend.Enabled := True;
    btnSend.Hint := 'Stop the running turn (Esc)';
  end
  else
  begin
    reason := SendBlockReason;
    btnSend.Caption := 'Send';
    btnSend.Enabled := reason = '';
    if reason = '' then
      btnSend.Hint := 'Send (Enter)'
    else
      btnSend.Hint := reason;
  end;
  cbbRuntime.Enabled := not turn;
  btnNew.Enabled := not turn;
end;

procedure TAIChatFrame.UpdateStatus;
var
  s: string;

  procedure AddPart(const APart: string);
  begin
    if APart = '' then
      Exit;
    if s <> '' then
      s := s + '  ·  ';
    s := s + APart;
  end;

var
  curModel: string;
begin
  lblRuntime.Caption := RuntimeTitle;

  curModel := CurrentModel;
  if curModel = '' then
  begin
    lblModel.Caption := 'default model';
    lblModel.Hint := 'Change model (now: runtime default)';
  end
  else
  begin
    lblModel.Caption := ShortModelName(curModel);
    lblModel.Hint := 'Change model (now: ' + curModel + ')';
  end;
  lblEffort.Caption := EffortCaption(GetEffort);
  lblEffort.Font.Color := EffortColor(GetEffort);

  s := '';
  if Assigned(FAdapter) then
  begin
    AddPart(AIStateToString(FAdapter.State));
    // a disabled button shows no hint: say it here
    if FWasReady and not FAdapter.ModelAvailable(FAdapter.Model) then
      AddPart('model not available');
  end;
  AddPart(FUsageText);
  AddPart(FMcpStatus);
  lblStatus.Caption := s;
  lblStatus.Hint := FSessionText;
end;

// The model in use: the user's choice, else what the runtime reported.
function TAIChatFrame.CurrentModel: string;
begin
  Result := '';
  if Assigned(FAdapter) then
    Result := FAdapter.Model;
  if Result = '' then
    Result := FModelText;
end;

function TAIChatFrame.AddStatusMenuItem(const ACaption: string; ATag: integer;
  AChecked, AEnabled: boolean; AHandler: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(FStatusMenu);
  Result.Caption := ACaption;
  Result.Tag := ATag;
  Result.RadioItem := True;
  Result.Checked := AChecked;
  Result.Enabled := AEnabled;
  Result.OnClick := AHandler;
  FStatusMenu.Items.Add(Result);
end;

// Status bar labels open a menu right where they are; the combo boxes at
// the top stay and both stay in sync.
procedure TAIChatFrame.ShowStatusMenu(ALabel: TLabel);
var
  i: integer;
  busy: boolean;
  chosen, itemCaption: string;
  p: TPoint;
begin
  if not Assigned(FStatusMenu) then
    FStatusMenu := TPopupMenu.Create(Self);
  FStatusMenu.Items.Clear;
  busy := TurnRunning;

  if ALabel = lblRuntime then
    for i := 0 to AIPresetCount - 1 do
    begin
      itemCaption := AIPreset(i).Title;
      if AIRuntimeExecutable(AIPreset(i).Id) = '' then
        itemCaption := itemCaption + #9 + '(not found)';
      AddStatusMenuItem(itemCaption, i,
        SameText(AIPreset(i).Id, FPresetId), not busy, @RuntimeMenuClick);
    end
  else if ALabel = lblModel then
  begin
    chosen := '';
    if Assigned(FAdapter) then
      chosen := FAdapter.Model;
    AddStatusMenuItem('Runtime default', -1, chosen = '', True, @ModelMenuClick);
    if Assigned(FAdapter) and (Length(FAdapter.Models) > 0) then
    begin
      AddStatusMenuItem('-', -2, False, True, nil);
      for i := 0 to High(FAdapter.Models) do
      begin
        itemCaption := ShortModelName(FAdapter.Models[i].Id);
        if itemCaption <> FAdapter.Models[i].Id then
          itemCaption := itemCaption + #9 + FAdapter.Models[i].Id;
        AddStatusMenuItem(itemCaption, i, FAdapter.Models[i].Id = CurrentModel, True,
          @ModelMenuClick);
      end;
    end
    else
      AddStatusMenuItem('(the list arrives when the runtime starts)', -2, False,
        False, nil);
  end
  else if ALabel = lblEffort then
    for i := 0 to High(AI_EFFORT_LEVELS) do
      AddStatusMenuItem(EffortCaption(AI_EFFORT_LEVELS[i]), i, AI_EFFORT_LEVELS[i] = GetEffort, True,
        @EffortMenuClick);

  p := ALabel.ClientToScreen(Point(0, 0));
  FStatusMenu.PopUp(p.X, p.Y);
end;

procedure TAIChatFrame.RuntimeMenuClick(Sender: TObject);
begin
  cbbRuntime.ItemIndex := TMenuItem(Sender).Tag;
  cbbRuntimeSelect(cbbRuntime);
  UpdateStatus;
end;

procedure TAIChatFrame.ModelMenuClick(Sender: TObject);
var
  i: integer;
begin
  i := TMenuItem(Sender).Tag;
  if i = -1 then
    SetModel('')
  else if Assigned(FAdapter) and (i >= 0) and (i <= High(FAdapter.Models)) then
    SetModel(FAdapter.Models[i].Id);
  DoSettingsChanged;
  UpdateStatus;
end;

procedure TAIChatFrame.EffortMenuClick(Sender: TObject);
begin
  SetEffort(AI_EFFORT_LEVELS[TMenuItem(Sender).Tag]);
  DoSettingsChanged;
  UpdateStatus;
end;

procedure TAIChatFrame.lblRuntimeClick(Sender: TObject);
begin
  ShowStatusMenu(lblRuntime);
end;

procedure TAIChatFrame.lblModelClick(Sender: TObject);
begin
  ShowStatusMenu(lblModel);
end;

procedure TAIChatFrame.lblEffortClick(Sender: TObject);
begin
  ShowStatusMenu(lblEffort);
end;

procedure TAIChatFrame.StatusLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := TLabel(Sender).Font.Style + [fsUnderline];
end;

procedure TAIChatFrame.StatusLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := TLabel(Sender).Font.Style - [fsUnderline];
end;

procedure TAIChatFrame.FillModels;
var
  i: integer;
  keep: string;
begin
  if not Assigned(FAdapter) or (Length(FAdapter.Models) = FModelCount) then
    Exit;
  FModelCount := Length(FAdapter.Models);
  FFillingModels := True;
  try
    keep := cbbModel.Text;
    cbbModel.Items.BeginUpdate;
    try
      cbbModel.Items.Clear;
      for i := 0 to High(FAdapter.Models) do
        cbbModel.Items.Add(FAdapter.Models[i].Id);
    finally
      cbbModel.Items.EndUpdate;
    end;
    // Show what the runtime uses until the user picks something else.
    if keep = '' then
      keep := FModelText;
    cbbModel.Text := keep;
  finally
    FFillingModels := False;
  end;
  FitComboWidths;
  // the chosen model can be checked against the list now
  UpdateControls;
end;

procedure TAIChatFrame.SendPrompt(const AText: string);
var
  reason: string;
begin
  if Trim(AText) = '' then
    Exit;
  if not Assigned(FAdapter) then
    CreateAdapter;
  if not Assigned(FAdapter) or TurnRunning then
    Exit;
  reason := SendBlockReason;
  if reason <> '' then
  begin
    FView.Add(cikError, reason);
    Exit;
  end;
  FView.Add(cikUser, AText);
  FCurrentText := nil;
  FCurrentThinking := nil;
  FStartPending := False;
  FTurnQueued := True;
  tmrPoll.Enabled := True;
  try
    FAdapter.Prompt(AText);
  except
    on E: Exception do
      ShowError(E.Message);
  end;
  UpdateControls;
  UpdateStatus;
end;

procedure TAIChatFrame.Send;
var
  s: string;
begin
  if TurnRunning then
  begin
    FAdapter.Cancel;
    Exit;
  end;
  s := mem_Input.Text;
  // keep the text while the runtime or the model is not usable
  if (Trim(s) = '') or (SendBlockReason <> '') then
    Exit;
  mem_Input.Clear;
  SendPrompt(s);
end;

procedure TAIChatFrame.NewChat;
begin
  if TurnRunning then
    Exit;
  FView.Clear;
  FCurrentText := nil;
  FCurrentThinking := nil;
  FUsageText := '';
  if not Assigned(FAdapter) or (FAdapter.State = aasFailed) then
    // the runtime did not start: try again
    SetRuntime(FPresetId, FCommand, FArguments)
  else if FAdapter.State <> aasStarting then
  begin
    try
      FAdapter.NewSession;
    except
      on E: Exception do
        ShowError(E.Message);
    end;
    tmrPoll.Enabled := True;
  end;
  UpdateStatus;
end;

procedure TAIChatFrame.FocusInput;
begin
  if mem_Input.CanFocus then
    mem_Input.SetFocus;
end;

procedure TAIChatFrame.AdapterEvent(Sender: TObject; const AEvent: TAIEvent);
var
  item: TAIChatItem;
begin
  case AEvent.Kind of
    aekText:
    begin
      if not Assigned(FCurrentText) or (FView.LastItem <> FCurrentText) then
        FCurrentText := FView.Add(cikAssistant);
      FView.AppendText(FCurrentText, AEvent.Text);
    end;
    aekThinking:
    begin
      if not Assigned(FCurrentThinking) or (FView.LastItem <> FCurrentThinking) then
        FCurrentThinking := FView.Add(cikThinking);
      FView.AppendText(FCurrentThinking, AEvent.Text);
    end;
    aekToolCall, aekToolUpdate:
    begin
      item := nil;
      if AEvent.ToolCallId <> '' then
        item := FView.FindItem(cikTool, AEvent.ToolCallId);
      if not Assigned(item) then
      begin
        item := FView.Add(cikTool);
        item.Key := AEvent.ToolCallId;
        item.Title := 'tool';
      end;
      if AEvent.ToolTitle <> '' then
        item.Title := AEvent.ToolTitle;
      if AEvent.ToolPath <> '' then
        item.Path := AEvent.ToolPath;
      item.Status := AEvent.ToolStatus;
      if AEvent.Text <> '' then
        item.Text := AEvent.Text;
      FView.Changed(item);
    end;
    aekPlan:
    begin
      item := FView.FindItem(cikPlan, 'plan');
      if not Assigned(item) or (FView.LastItem <> item) then
      begin
        item := FView.Add(cikPlan);
        item.Key := 'plan';
      end;
      item.Text := Trim(AEvent.Text);
      FView.Changed(item);
    end;
    aekError:
      ShowError(AEvent.Text);
    aekDone:
    begin
      if (AEvent.StopReason <> '') and (AEvent.StopReason <> 'end_turn') and
        (AEvent.StopReason <> 'error') then
        FView.Add(cikInfo, 'Stopped: ' + AEvent.StopReason);
      FTurnQueued := False;
      FCurrentText := nil;
      FCurrentThinking := nil;
      FillModels;
    end;
    aekStatus:
    begin
      if Copy(AEvent.Text, 1, 7) = 'agent: ' then
        FAgentText := Copy(AEvent.Text, 8, MaxInt)
      else if Copy(AEvent.Text, 1, 7) = 'model: ' then
      begin
        FModelText := Copy(AEvent.Text, 8, MaxInt);
        FillModels;
      end
      else if Copy(AEvent.Text, 1, 9) = 'session: ' then
        FSessionText := AEvent.Text;
      UpdateStatus;
      UpdateControls;
    end;
    aekUsage:
    begin
      FUsageText := AEvent.Text;
      UpdateStatus;
    end;
    aekLog:
      AddLog(AEvent.Text);
  end;
  if AEvent.Kind = aekDone then
    UpdateControls;
end;

procedure TAIChatFrame.AdapterPermission(Sender: TObject;
  const ARequest: TAIPermissionRequest);
var
  item: TAIChatItem;
  i: integer;
begin
  item := FView.Add(cikPermission, ARequest.Detail);
  item.Key := ARequest.Key;
  item.Title := ARequest.ToolTitle;
  SetLength(item.Buttons, Length(ARequest.Options));
  for i := 0 to High(ARequest.Options) do
  begin
    item.Buttons[i].Id := ARequest.Options[i].Id;
    item.Buttons[i].Caption := ARequest.Options[i].Name;
    item.Buttons[i].Kind := ARequest.Options[i].Kind;
  end;
  FView.Changed(item);
  // A new item while the text streams starts a new text item after it.
  FCurrentText := nil;
end;

procedure TAIChatFrame.ShowToolPermission(const ARequest: TAIPermissionRequest);
begin
  // same card; ViewButtonClick routes "mcp:" keys to OnToolPermission
  AdapterPermission(Self, ARequest);
end;

procedure TAIChatFrame.AddLog(const AText: string);
begin
  if not mem_Log.Visible then
    Exit;
  if mem_Log.Lines.Count > MAX_LOG_LINES then
    mem_Log.Lines.Clear;
  mem_Log.Lines.Add(Copy(AText, 1, 2000));
end;

procedure TAIChatFrame.SetMcpServer(const AUrl, AToken, AStatus: string);
var
  serverMoved: boolean;
begin
  serverMoved := (AUrl <> FMcpUrl) or (AToken <> FMcpToken);
  FMcpUrl := AUrl;
  FMcpToken := AToken;
  FMcpStatus := AStatus;
  // the runtime reads the server at start; restart it on a change
  if serverMoved and Assigned(FAdapter) and not TurnRunning then
    SetRuntime(FPresetId, FCommand, FArguments)
  else
    UpdateStatus;
end;

function TAIChatFrame.AdapterReadFile(Sender: TObject; const APath: string;
  out AContent: string): boolean;
begin
  Result := Assigned(FOnReadFile) and FOnReadFile(Self, APath, AContent);
end;

procedure TAIChatFrame.ViewButtonClick(Sender: TObject; AItem: TAIChatItem;
  const AButtonId: string);
var
  i: integer;
var
  isTool: boolean;
begin
  if AItem.Answer <> '' then
    Exit;
  isTool := Copy(AItem.Key, 1, 4) = 'mcp:';
  if not isTool and not Assigned(FAdapter) then
    Exit;
  for i := 0 to High(AItem.Buttons) do
    if AItem.Buttons[i].Id = AButtonId then
      AItem.Answer := AItem.Buttons[i].Caption;
  FView.Changed(AItem);
  if isTool then
  begin
    if Assigned(FOnToolPermission) then
      FOnToolPermission(Self, AItem.Key, AButtonId);
  end
  else
    FAdapter.RespondPermission(AItem.Key, AButtonId);
end;

procedure TAIChatFrame.tmrPollTimer(Sender: TObject);
var
  rounds: integer;
begin
  if not Assigned(FAdapter) then
  begin
    tmrPoll.Enabled := False;
    Exit;
  end;
  if FStartPending and not FHoldStart then
  begin
    // the runtime starts right away, so Send knows whether it answers and
    // the model box gets the runtime's list
    FStartPending := False;
    try
      FAdapter.Start;
    except
      on E: Exception do
        ShowError(E.Message);
    end;
    UpdateControls;
    UpdateStatus;
  end;
  // Drain a burst of output, but give the UI back regularly.
  rounds := 0;
  // many chunks per tick: lay the conversation out once, at the end
  FView.BeginUpdate;
  try
    while FAdapter.Poll and (rounds < 20) do
      Inc(rounds);
  finally
    FView.EndUpdate;
  end;
  // the list may arrive without a model status (runtime started for it)
  FillModels;
  if FAdapter.State in [aasIdle, aasStopped, aasFailed] then
  begin
    tmrPoll.Enabled := False;
    UpdateControls;
    UpdateStatus;
  end;
end;

procedure TAIChatFrame.cbbRuntimeSelect(Sender: TObject);
var
  i: integer;
  newId: string;
begin
  if (cbbRuntime.ItemIndex < 0) or
    SameText(AIPreset(cbbRuntime.ItemIndex).Id, FPresetId) then
    Exit;
  // Keep the current runtime when the new one is not installed.
  if AIRuntimeExecutable(AIPreset(cbbRuntime.ItemIndex).Id) = '' then
  begin
    ShowError(AIRuntimeMissingMessage(AIPreset(cbbRuntime.ItemIndex).Id));
    for i := 0 to AIPresetCount - 1 do
      if SameText(AIPreset(i).Id, FPresetId) then
        cbbRuntime.ItemIndex := i;
    Exit;
  end;
  // The old runtime's model means nothing to the new one: bring back the
  // model last used with it. Free the adapter first, SetRuntime keeps the
  // adapter's model otherwise.
  RememberModel;
  newId := AIPreset(cbbRuntime.ItemIndex).Id;
  FreeAdapter;
  FFillingModels := True;
  try
    cbbModel.Text := FRuntimeModels.Values[newId];
  finally
    FFillingModels := False;
  end;
  SetRuntime(newId);
  FView.Add(cikInfo, 'Runtime: ' + AIPreset(cbbRuntime.ItemIndex).Title);
  DoSettingsChanged;
end;

procedure TAIChatFrame.cbbModelChange(Sender: TObject);
begin
  if FFillingModels then
    Exit;
  FitComboWidths;
  if not Assigned(FAdapter) then
    Exit;
  if FAdapter.Model = Trim(cbbModel.Text) then
    Exit;
  // The runtime's own model is only shown, not chosen.
  if (FAdapter.Model = '') and (Trim(cbbModel.Text) = FModelText) then
    Exit;
  FAdapter.Model := Trim(cbbModel.Text);
  DoSettingsChanged;
  UpdateControls;
  UpdateStatus;
end;

procedure TAIChatFrame.cbbEffortSelect(Sender: TObject);
begin
  if Assigned(FAdapter) then
    FAdapter.Effort := GetEffort;
  DoSettingsChanged;
  UpdateStatus;
end;

procedure TAIChatFrame.btnNewClick(Sender: TObject);
begin
  NewChat;
  FocusInput;
end;

procedure TAIChatFrame.btnLogClick(Sender: TObject);
begin
  mem_Log.Visible := btnLog.Down;
  splLog.Visible := mem_Log.Visible;
  if mem_Log.Visible then
    splLog.Top := mem_Log.Top - splLog.Height;
end;

procedure TAIChatFrame.btnSendClick(Sender: TObject);
begin
  Send;
  FocusInput;
end;

procedure TAIChatFrame.mem_InputKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and not (ssShift in Shift) and not (ssCtrl in Shift) then
  begin
    Key := 0;
    if not TurnRunning then
      Send;
  end
  else if (Key = VK_ESCAPE) and TurnRunning then
  begin
    Key := 0;
    FAdapter.Cancel;
  end;
end;

function TAIChatFrame.GetEffort: string;
begin
  if cbbEffort.ItemIndex < 0 then
    Result := 'auto'
  else
    Result := cbbEffort.Items[cbbEffort.ItemIndex];
end;

function TAIChatFrame.GetModel: string;
begin
  // The model the user chose; the box may only show the runtime default.
  if Assigned(FAdapter) then
    Result := FAdapter.Model
  else
    Result := Trim(cbbModel.Text);
end;

procedure TAIChatFrame.SetEffort(const AValue: string);
var
  i: integer;
begin
  i := cbbEffort.Items.IndexOf(LowerCase(AValue));
  if i < 0 then
    i := 0;
  cbbEffort.ItemIndex := i;
  if Assigned(FAdapter) then
    FAdapter.Effort := GetEffort;
end;

procedure TAIChatFrame.SetModel(const AValue: string);
begin
  cbbModel.Text := AValue;
  FitComboWidths;
  if Assigned(FAdapter) then
    FAdapter.Model := Trim(AValue);
  UpdateControls;
end;

procedure TAIChatFrame.SetWorkingDir(const AValue: string);
begin
  FWorkingDir := AValue;
  // relative image paths in answers are relative to the project
  FView.BaseDir := AValue;
  if Assigned(FAdapter) then
    FAdapter.WorkingDir := AValue;
end;

procedure TAIChatFrame.SetHoldStart(AValue: boolean);
begin
  if FHoldStart = AValue then
    Exit;
  FHoldStart := AValue;
  // the timer stopped while the start was held
  if not FHoldStart and FStartPending and Assigned(FAdapter) then
    tmrPoll.Enabled := True;
end;

procedure TAIChatFrame.DoSettingsChanged;
begin
  RememberModel;
  if Assigned(FOnSettingsChanged) then
    FOnSettingsChanged(Self);
end;

end.

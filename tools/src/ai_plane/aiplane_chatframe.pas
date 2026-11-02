unit aiplane_chatframe;

{
  The AI Plane chat as a frame, so the IDE window and the tester app show the
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
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Buttons, LCLType,
  LCLIntf,
  aiplane_types, aiplane_presets, aiplane_chatview;

type

  { TAIChatFrame }

  TAIChatFrame = class(TFrame)
    btnLog: TSpeedButton;
    btnNew: TSpeedButton;
    btnSend: TButton;
    cbbEffort: TComboBox;
    cbbModel: TComboBox;
    cbbRuntime: TComboBox;
    lblStatus: TLabel;
    mem_Input: TMemo;
    mem_Log: TMemo;
    pnlInput: TPanel;
    pnlTop: TPanel;
    pnlView: TPanel;
    splLog: TSplitter;
    tmrPoll: TTimer;
    procedure cbbRuntimeSelect(Sender: TObject);
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
    procedure SetupControls;
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
    procedure DoSettingsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Switches runtime. ACommand/AArguments override the preset when not
      empty. Stops the current runtime. }
    procedure SetRuntime(const APresetId: string; const ACommand: string = '';
      const AArguments: string = '');
    procedure SendPrompt(const AText: string);
    procedure NewChat;
    procedure FocusInput;
    property Adapter: TAIAdapter read FAdapter;
    property View: TAIChatView read FView;
    property PresetId: string read FPresetId;
    property Command: string read FCommand;
    property Arguments: string read FArguments;
    property Model: string read GetModel write SetModel;
    property Effort: string read GetEffort write SetEffort;
    property WorkingDir: string read FWorkingDir write SetWorkingDir;
    property OnReadFile: TAIReadFileHandler read FOnReadFile write FOnReadFile;
    property OnSettingsChanged: TNotifyEvent read FOnSettingsChanged write FOnSettingsChanged;
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
      FView.Add(cikError, E.Message);
      UpdateControls;
      Exit;
    end;
  end;
  FAdapter.OnEvent := @AdapterEvent;
  FAdapter.OnPermission := @AdapterPermission;
  FAdapter.OnReadFile := @AdapterReadFile;
  FAdapter.WorkingDir := FWorkingDir;
  FAdapter.Model := keepModel;
  FAdapter.Effort := keepEffort;
  FAgentText := FAdapter.Command;
  FModelText := '';
  FSessionText := '';
  FUsageText := '';
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
  busy: boolean;
begin
  busy := Assigned(FAdapter) and FAdapter.IsBusy;
  if busy then
    btnSend.Caption := 'Stop'
  else
    btnSend.Caption := 'Send';
  btnSend.Enabled := Assigned(FAdapter);
  cbbRuntime.Enabled := not busy;
  btnNew.Enabled := not busy;
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

begin
  s := '';
  AddPart(FAgentText);
  AddPart(FModelText);
  if Assigned(FAdapter) then
    AddPart(AIStateToString(FAdapter.State));
  AddPart(FUsageText);
  lblStatus.Caption := s;
  lblStatus.Hint := FSessionText;
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
end;

procedure TAIChatFrame.SendPrompt(const AText: string);
begin
  if Trim(AText) = '' then
    Exit;
  if not Assigned(FAdapter) then
    CreateAdapter;
  if not Assigned(FAdapter) or FAdapter.IsBusy then
    Exit;
  FView.Add(cikUser, AText);
  FCurrentText := nil;
  FCurrentThinking := nil;
  tmrPoll.Enabled := True;
  try
    FAdapter.Prompt(AText);
  except
    on E: Exception do
      FView.Add(cikError, E.Message);
  end;
  UpdateControls;
  UpdateStatus;
end;

procedure TAIChatFrame.Send;
var
  s: string;
begin
  if Assigned(FAdapter) and FAdapter.IsBusy then
  begin
    FAdapter.Cancel;
    Exit;
  end;
  s := mem_Input.Text;
  if Trim(s) = '' then
    Exit;
  mem_Input.Clear;
  SendPrompt(s);
end;

procedure TAIChatFrame.NewChat;
begin
  if Assigned(FAdapter) and FAdapter.IsBusy then
    Exit;
  FView.Clear;
  FCurrentText := nil;
  FCurrentThinking := nil;
  FUsageText := '';
  if Assigned(FAdapter) then
  begin
    try
      FAdapter.NewSession;
    except
      on E: Exception do
        FView.Add(cikError, E.Message);
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
      FView.Add(cikError, AEvent.Text);
    aekDone:
    begin
      if (AEvent.StopReason <> '') and (AEvent.StopReason <> 'end_turn') and
        (AEvent.StopReason <> 'error') then
        FView.Add(cikInfo, 'Stopped: ' + AEvent.StopReason);
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
      if mem_Log.Visible then
      begin
        if mem_Log.Lines.Count > MAX_LOG_LINES then
          mem_Log.Lines.Clear;
        mem_Log.Lines.Add(Copy(AEvent.Text, 1, 2000));
      end;
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

function TAIChatFrame.AdapterReadFile(Sender: TObject; const APath: string;
  out AContent: string): boolean;
begin
  Result := Assigned(FOnReadFile) and FOnReadFile(Self, APath, AContent);
end;

procedure TAIChatFrame.ViewButtonClick(Sender: TObject; AItem: TAIChatItem;
  const AButtonId: string);
var
  i: integer;
begin
  if not Assigned(FAdapter) or (AItem.Answer <> '') then
    Exit;
  for i := 0 to High(AItem.Buttons) do
    if AItem.Buttons[i].Id = AButtonId then
      AItem.Answer := AItem.Buttons[i].Caption;
  FView.Changed(AItem);
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
  // Drain a burst of output, but give the UI back regularly.
  rounds := 0;
  while FAdapter.Poll and (rounds < 20) do
    Inc(rounds);
  if FAdapter.State in [aasIdle, aasStopped, aasFailed] then
  begin
    tmrPoll.Enabled := False;
    UpdateControls;
    UpdateStatus;
  end;
end;

procedure TAIChatFrame.cbbRuntimeSelect(Sender: TObject);
begin
  if (cbbRuntime.ItemIndex < 0) or
    SameText(AIPreset(cbbRuntime.ItemIndex).Id, FPresetId) then
    Exit;
  cbbModel.Text := '';
  SetRuntime(AIPreset(cbbRuntime.ItemIndex).Id);
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
end;

procedure TAIChatFrame.cbbEffortSelect(Sender: TObject);
begin
  if Assigned(FAdapter) then
    FAdapter.Effort := GetEffort;
  DoSettingsChanged;
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
    if not (Assigned(FAdapter) and FAdapter.IsBusy) then
      Send;
  end
  else if (Key = VK_ESCAPE) and Assigned(FAdapter) and FAdapter.IsBusy then
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
end;

procedure TAIChatFrame.SetWorkingDir(const AValue: string);
begin
  FWorkingDir := AValue;
  if Assigned(FAdapter) then
    FAdapter.WorkingDir := AValue;
end;

procedure TAIChatFrame.DoSettingsChanged;
begin
  if Assigned(FOnSettingsChanged) then
    FOnSettingsChanged(Self);
end;

end.

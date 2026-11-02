program aiplane_tester;

{
  Standalone window around TAIChatFrame: the same chat the IDE shows, built
  in seconds instead of rebuilding Lazarus.

    aiplane_tester [--runtime claude|opencode|pi|copilot|claude-acp|mock]
                   [--command <exe>] [--args "<a b>"] [--cwd <dir>]
                   [--prompt "<a>|<b>"] [--screenshot <file.png>] [--timeout <s>]
                   [--settings detect|test]
                   [--select all|x1,y1,x2,y2] [--selection-out <file>]

  --prompt sends the prompts one after another (separated by |), and
  --screenshot then saves the window to a PNG and quits. A turn that is
  still running after --timeout seconds (default 5) is left as it is.
  --settings opens the settings window (the AI Harness tab of the IDE
  configuration), runs Detect or Run Test, and the screenshot shows it.
  --select selects text after the prompts (as a mouse drag between two
  points of the conversation, or everything); --selection-out writes the
  text that Ctrl+C would copy. Together they let
  a script check the UI without anyone clicking.

  Runtime, model, effort, command and working directory are kept in
  aiplane_tester.ini in the user's config folder. "mock" runs
  ../mock_acp_agent (built by run_tests), handy for UI work without tokens.

  Build: lazbuild aiplane_tester.lpi
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces, Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, Menus, LCLType,
  Dialogs, IniFiles, StdCtrls, aiplane_types,
  aiplane_presets, aiplane_chatframe, aiplane_settingsframe;

type

  { TTesterForm }

  TTesterForm = class(TForm)
  private
    FChat: TAIChatFrame;
    FIni: TIniFile;
    FMock: boolean;
    FScript: TStringList;
    FScriptTimer: TTimer;
    FScriptWait: integer;
    FScriptLimit: integer;
    FScreenshot: string;
    FSettingsForm: TForm;
    FSettings: TAIPlaneSettingsFrame;
    FSettingsAction: string;
    procedure BuildMenu;
    procedure CreateSettingsForm;
    procedure MenuSettings(Sender: TObject);
    procedure ScriptTick(Sender: TObject);
    procedure ApplySelection;
    procedure LoadSettings;
    procedure SaveSettings(Sender: TObject);
    procedure ApplyRuntime(const APresetId, ACommand, AArguments: string);
    procedure MenuWorkingDir(Sender: TObject);
    procedure MenuMock(Sender: TObject);
    procedure MenuNew(Sender: TObject);
    procedure MenuQuit(Sender: TObject);
    procedure UpdateCaption;
  public
    constructor Create(AOwner: TComponent); override;
    procedure FocusChat;
    destructor Destroy; override;
  end;

function Option(const AName: string; const ADefault: string = ''): string;
var
  i: integer;
begin
  Result := ADefault;
  for i := 1 to ParamCount - 1 do
    if ParamStr(i) = AName then
      Exit(ParamStr(i + 1));
end;

function MockAgentPath: string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim +
    'mock_acp_agent' + ExtractFileExt(ParamStr(0)));
end;

{ TTesterForm }

constructor TTesterForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := 'Agent Plane Tester';
  Width := 520;
  Height := 720;
  Position := poScreenCenter;
  FIni := TIniFile.Create(GetAppConfigDir(False) + 'aiplane_tester.ini');
  BuildMenu;
  FChat := TAIChatFrame.Create(Self);
  FChat.Parent := Self;
  FChat.Align := alClient;
  LoadSettings;
  FChat.OnSettingsChanged := @SaveSettings;
  FScript := TStringList.Create;
  FScript.Delimiter := '|';
  FScript.StrictDelimiter := True;
  FScript.DelimitedText := Option('--prompt');
  FScreenshot := Option('--screenshot');
  FScriptLimit := StrToIntDef(Option('--timeout'), 5) * 10;
  FSettingsAction := Option('--settings');
  if (FScript.Count > 0) or (FScreenshot <> '') or (FSettingsAction <> '') then
  begin
    FScriptTimer := TTimer.Create(Self);
    FScriptTimer.Interval := 100;
    FScriptTimer.OnTimer := @ScriptTick;
  end;
  ActiveControl := nil;
end;

destructor TTesterForm.Destroy;
begin
  if Assigned(FIni) then
    SaveSettings(nil);
  FIni.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TTesterForm.ScriptTick(Sender: TObject);
var
  bmp: TBitmap;
  png: TPortableNetworkGraphic;
  busy: boolean;
  shotForm: TForm;
begin
  if FSettingsAction <> '' then
  begin
    CreateSettingsForm;
    FSettingsForm.Show;
    if FSettingsAction = 'test' then
      FSettings.RunTest
    else
      FSettings.DetectModels;
    FSettingsAction := '';
    Exit;
  end;
  // also waits for the runtime to answer before the first prompt
  busy := (Assigned(FChat.Adapter) and FChat.Adapter.IsBusy) or
    ((FScript.Count > 0) and not FChat.ReadyToSend) or
    (Assigned(FSettings) and FSettings.Busy);
  Inc(FScriptWait);
  // A turn that waits for a permission click is not waited for forever.
  if busy and (FScriptWait < FScriptLimit) then
    Exit;
  FScriptWait := 0;
  if FScript.Count > 0 then
  begin
    FChat.SendPrompt(FScript[0]);
    FScript.Delete(0);
    Exit;
  end;
  FScriptTimer.Enabled := False;
  if FScreenshot <> '' then
  begin
    Application.ProcessMessages;
    shotForm := Self;
    if Assigned(FSettingsForm) and FSettingsForm.Visible then
      shotForm := FSettingsForm;
    ApplySelection;
    bmp := shotForm.GetFormImage;
    png := TPortableNetworkGraphic.Create;
    try
      png.Assign(bmp);
      png.SaveToFile(FScreenshot);
    finally
      png.Free;
      bmp.Free;
    end;
    Close;
  end;
end;

procedure TTesterForm.ApplySelection;
var
  sel, outFile: string;
  parts: TStringList;
begin
  sel := Option('--select');
  outFile := Option('--selection-out');
  if sel = '' then
    Exit;
  if sel = 'all' then
    FChat.View.SelectAll
  else
  begin
    parts := TStringList.Create;
    try
      parts.CommaText := sel;
      if parts.Count = 4 then
        FChat.View.SelectRegion(StrToIntDef(parts[0], 0), StrToIntDef(parts[1], 0),
          StrToIntDef(parts[2], 0), StrToIntDef(parts[3], 0));
    finally
      parts.Free;
    end;
  end;
  Application.ProcessMessages;
  if outFile <> '' then
    with TStringList.Create do
    try
      Text := FChat.View.SelectedText;
      SaveToFile(outFile);
    finally
      Free;
    end;
end;

procedure TTesterForm.BuildMenu;
var
  mm: TMainMenu;
  topItem: TMenuItem;

  procedure Add(AParent: TMenuItem; const ACaption: string; AHandler: TNotifyEvent;
    AKey: word = 0);
  var
    mi: TMenuItem;
  begin
    mi := TMenuItem.Create(mm);
    mi.Caption := ACaption;
    mi.OnClick := AHandler;
    if AKey <> 0 then
      mi.ShortCut := ShortCut(AKey, [ssCtrl]);
    AParent.Add(mi);
  end;

begin
  ForceDirectories(GetAppConfigDir(False));
  mm := TMainMenu.Create(Self);
  Menu := mm;
  topItem := TMenuItem.Create(mm);
  topItem.Caption := '&Chat';
  mm.Items.Add(topItem);
  Add(topItem, '&New conversation', @MenuNew, VK_N);
  Add(topItem, '&Settings...', @MenuSettings);
  Add(topItem, '&Working directory...', @MenuWorkingDir);
  Add(topItem, 'Use &mock agent', @MenuMock);
  Add(topItem, '-', nil);
  Add(topItem, '&Quit', @MenuQuit, VK_Q);
end;

procedure TTesterForm.ApplyRuntime(const APresetId, ACommand, AArguments: string);
begin
  FChat.SetRuntime(APresetId, ACommand, AArguments);
  UpdateCaption;
end;

procedure TTesterForm.LoadSettings;
var
  runtime, command, args, cwd: string;
  i: integer;
begin
  runtime := Option('--runtime', FIni.ReadString('chat', 'runtime', AI_DEFAULT_PRESET));
  command := Option('--command', FIni.ReadString('chat', 'command', ''));
  args := Option('--args', FIni.ReadString('chat', 'arguments', ''));
  cwd := Option('--cwd', FIni.ReadString('chat', 'cwd', GetCurrentDir));
  FChat.WorkingDir := cwd;
  for i := 0 to AIPresetCount - 1 do
    FChat.RuntimeModels.Values[AIPreset(i).Id] :=
      FIni.ReadString('models', AIPreset(i).Id, '');
  FChat.Model := FIni.ReadString('chat', 'model', '');
  FChat.Effort := FIni.ReadString('chat', 'effort', 'auto');
  FMock := runtime = 'mock';
  if FMock then
    ApplyRuntime(AI_PRESET_OPENCODE, MockAgentPath, '')
  else
    ApplyRuntime(runtime, command, args);
end;

procedure TTesterForm.SaveSettings(Sender: TObject);
var
  i: integer;
begin
  if FMock then
    FIni.WriteString('chat', 'runtime', 'mock')
  else
  begin
    FIni.WriteString('chat', 'runtime', FChat.PresetId);
    FIni.WriteString('chat', 'command', FChat.Command);
    FIni.WriteString('chat', 'arguments', FChat.Arguments);
    // the mock agent's models are not the real runtime's
    for i := 0 to AIPresetCount - 1 do
      FIni.WriteString('models', AIPreset(i).Id,
        FChat.RuntimeModels.Values[AIPreset(i).Id]);
  end;
  FIni.WriteString('chat', 'model', FChat.Model);
  FIni.WriteString('chat', 'effort', FChat.Effort);
  FIni.WriteString('chat', 'cwd', FChat.WorkingDir);
  if Sender <> nil then
  begin
    FMock := False;
    UpdateCaption;
  end;
end;

procedure TTesterForm.CreateSettingsForm;
var
  bottom: TPanel;
  btn: TButton;
begin
  if Assigned(FSettingsForm) then
    Exit;
  FSettingsForm := TForm.CreateNew(Self);
  FSettingsForm.Caption := 'AI Harness Settings';
  FSettingsForm.Width := 600;
  FSettingsForm.Height := 440;
  FSettingsForm.Position := poMainFormCenter;
  bottom := TPanel.Create(FSettingsForm);
  bottom.Parent := FSettingsForm;
  bottom.Align := alBottom;
  bottom.Height := 44;
  bottom.BevelOuter := bvNone;
  btn := TButton.Create(FSettingsForm);
  btn.Parent := bottom;
  btn.Caption := 'Cancel';
  btn.ModalResult := mrCancel;
  btn.Cancel := True;
  btn.SetBounds(bottom.Width - 96, 8, 88, 28);
  btn.Anchors := [akTop, akRight];
  btn := TButton.Create(FSettingsForm);
  btn.Parent := bottom;
  btn.Caption := 'Save';
  btn.ModalResult := mrOK;
  btn.Default := True;
  btn.SetBounds(bottom.Width - 192, 8, 88, 28);
  btn.Anchors := [akTop, akRight];
  FSettings := TAIPlaneSettingsFrame.Create(FSettingsForm);
  FSettings.Parent := FSettingsForm;
  FSettings.Align := alClient;
  FSettings.RuntimeModels.Assign(FChat.RuntimeModels);
  FSettings.Runtime := FChat.PresetId;
  FSettings.Command := FChat.Command;
  FSettings.Arguments := FChat.Arguments;
  FSettings.Model := FChat.Model;
  FSettings.Effort := FChat.Effort;
  FSettings.WorkingDir := FChat.WorkingDir;
end;

procedure TTesterForm.MenuSettings(Sender: TObject);
begin
  FreeAndNil(FSettingsForm);
  FSettings := nil;
  CreateSettingsForm;
  if FSettingsForm.ShowModal = mrOK then
  begin
    FMock := False;
    FChat.RuntimeModels.Assign(FSettings.RuntimeModels);
    FChat.Model := FSettings.Model;
    FChat.Effort := FSettings.Effort;
    ApplyRuntime(FSettings.Runtime, FSettings.Command, FSettings.Arguments);
    SaveSettings(nil);
  end;
  FreeAndNil(FSettingsForm);
  FSettings := nil;
end;

procedure TTesterForm.MenuWorkingDir(Sender: TObject);
var
  dlg: TSelectDirectoryDialog;
begin
  dlg := TSelectDirectoryDialog.Create(nil);
  try
    dlg.InitialDir := FChat.WorkingDir;
    if dlg.Execute then
    begin
      FChat.WorkingDir := dlg.FileName;
      // The runtime keeps its directory until it restarts.
      ApplyRuntime(FChat.PresetId, FChat.Command, FChat.Arguments);
      SaveSettings(nil);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TTesterForm.MenuMock(Sender: TObject);
begin
  if not FileExists(MockAgentPath) then
  begin
    ShowMessage('Build the mock agent first (tools/src/test/run_tests): ' + MockAgentPath);
    Exit;
  end;
  FMock := True;
  FChat.Model := '';
  ApplyRuntime(AI_PRESET_OPENCODE, MockAgentPath, '');
  SaveSettings(nil);
end;

procedure TTesterForm.FocusChat;
begin
  FChat.FocusInput;
end;

procedure TTesterForm.MenuNew(Sender: TObject);
begin
  FChat.NewChat;
end;

procedure TTesterForm.MenuQuit(Sender: TObject);
begin
  Close;
end;

procedure TTesterForm.UpdateCaption;
var
  runtimeName: string;
begin
  if FMock then
    runtimeName := 'mock agent'
  else
    runtimeName := FChat.PresetId;
  Caption := 'Agent Plane Tester - ' + runtimeName + ' - ' + FChat.WorkingDir;
end;

var
  form: TTesterForm;
begin
  RequireDerivedFormResource := False;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TTesterForm, form);
  form.Show;
  form.FocusChat;
  Application.Run;
end.

unit aiplane_ide;

{
  AI Plane inside the Lazarus IDE: the dockable window that hosts
  TAIChatFrame, and the settings it shares with the AI Harness tab of
  FastPlaz/Configuration.

  - Settings live in the FastPlaz config.json (GlobalConfig) under "aiplane".
  - The runtime works in the directory of the active project.
  - Files open in the source editor are served from the editor buffer, so the
    agent reads unsaved changes too.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  LazIDEIntf, IDEWindowIntf, SrcEditorIntf,
  aiplane_types, aiplane_chatframe;

const
  AIPLANE_WINDOW_NAME = 'AIPlaneWindow';
  RS_AIPLANE_MENU = 'AI Plane';
  RS_AIPLANE_CAPTION = 'AI Plane';

type
  TAIPlaneSettings = record
    Runtime: string;
    Command: string;
    Arguments: string;
    Model: string;
    Effort: string;
  end;

  { TAIPlaneWindow }

  TAIPlaneWindow = class(TForm)
  private
    FChat: TAIChatFrame;
    FProjectDir: string;
    FApplying: boolean;
    procedure ChatSettingsChanged(Sender: TObject);
    function ReadEditorBuffer(Sender: TObject; const APath: string;
      out AContent: string): boolean;
    procedure FormActivate(Sender: TObject);
    procedure SyncProjectDirectory;
  public
    constructor Create(AOwner: TComponent); override;
    { Applies the stored settings; keeps the conversation when the runtime
      stays the same. }
    procedure ApplySettings;
    property Chat: TAIChatFrame read FChat;
  end;

var
  AIPlaneWindow: TAIPlaneWindow = nil;

function AIPlaneLoadSettings: TAIPlaneSettings;
procedure AIPlaneSaveSettings(const ASettings: TAIPlaneSettings);
{ Directory of the active project, or the user directory. }
function AIPlaneProjectDirectory: string;
{ Registers the window creator; call once from Register. }
procedure AIPlaneRegisterWindow;
procedure AIPlaneShowWindow(Sender: TObject);
{ Called after the configuration was saved. }
procedure AIPlaneSettingsChanged;

implementation

{$R *.lfm}

uses
  Variants, de_common, aiplane_presets, aiplane_chatview;

function AIPlaneLoadSettings: TAIPlaneSettings;
begin
  LoadConfiguration;
  Result.Runtime := GlobalConfig['aiplane/runtime'];
  Result.Command := GlobalConfig['aiplane/command'];
  Result.Arguments := GlobalConfig['aiplane/arguments'];
  Result.Model := GlobalConfig['aiplane/model'];
  Result.Effort := GlobalConfig['aiplane/effort'];
  if Result.Runtime = '' then
    Result.Runtime := AI_DEFAULT_PRESET;
  if Result.Effort = '' then
    Result.Effort := 'auto';
end;

procedure AIPlaneSaveSettings(const ASettings: TAIPlaneSettings);
begin
  LoadConfiguration;
  GlobalConfig['aiplane/runtime'] := ASettings.Runtime;
  GlobalConfig['aiplane/command'] := ASettings.Command;
  GlobalConfig['aiplane/arguments'] := ASettings.Arguments;
  GlobalConfig['aiplane/model'] := ASettings.Model;
  GlobalConfig['aiplane/effort'] := ASettings.Effort;
  GlobalConfig.Flush;
end;

function AIPlaneProjectDirectory: string;
begin
  Result := '';
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
    Result := ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
  if (Result = '') or not DirectoryExists(Result) then
    Result := GetUserDir;
  Result := ExcludeTrailingPathDelimiter(Result);
end;

procedure CreateAIPlaneWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if CompareText(aFormName, AIPLANE_WINDOW_NAME) <> 0 then
    Exit;
  IDEWindowCreators.CreateForm(AIPlaneWindow, TAIPlaneWindow, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm := AIPlaneWindow;
end;

procedure AIPlaneRegisterWindow;
begin
  IDEWindowCreators.Add(AIPLANE_WINDOW_NAME, @CreateAIPlaneWindow, nil,
    '250', '120', '+460', '+640');
end;

procedure AIPlaneShowWindow(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(AIPLANE_WINDOW_NAME, True);
  if Assigned(AIPlaneWindow) then
    AIPlaneWindow.FChat.FocusInput;
end;

procedure AIPlaneSettingsChanged;
begin
  if Assigned(AIPlaneWindow) then
    AIPlaneWindow.ApplySettings;
end;

{ TAIPlaneWindow }

constructor TAIPlaneWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := RS_AIPLANE_CAPTION;
  FChat := TAIChatFrame.Create(Self);
  FChat.Parent := Self;
  FChat.Align := alClient;
  FChat.OnReadFile := @ReadEditorBuffer;
  FProjectDir := AIPlaneProjectDirectory;
  FChat.WorkingDir := FProjectDir;
  ApplySettings;
  FChat.OnSettingsChanged := @ChatSettingsChanged;
  OnActivate := @FormActivate;
end;

procedure TAIPlaneWindow.ApplySettings;
var
  settings: TAIPlaneSettings;
begin
  if Assigned(FChat.Adapter) and FChat.Adapter.IsBusy then
    Exit;
  settings := AIPlaneLoadSettings;
  FApplying := True;
  try
    FChat.Model := settings.Model;
    FChat.Effort := settings.Effort;
    if not SameText(settings.Runtime, FChat.PresetId) or
      (settings.Command <> FChat.Command) or (settings.Arguments <> FChat.Arguments) then
      FChat.SetRuntime(settings.Runtime, settings.Command, settings.Arguments);
  finally
    FApplying := False;
  end;
end;

procedure TAIPlaneWindow.ChatSettingsChanged(Sender: TObject);
var
  settings: TAIPlaneSettings;
begin
  if FApplying then
    Exit;
  // Choices made in the chat become the new defaults.
  settings := AIPlaneLoadSettings;
  if not SameText(settings.Runtime, FChat.PresetId) then
  begin
    settings.Command := FChat.Command;
    settings.Arguments := FChat.Arguments;
  end;
  settings.Runtime := FChat.PresetId;
  settings.Model := FChat.Model;
  settings.Effort := FChat.Effort;
  AIPlaneSaveSettings(settings);
end;

function TAIPlaneWindow.ReadEditorBuffer(Sender: TObject; const APath: string;
  out AContent: string): boolean;
var
  editor: TSourceEditorInterface;
begin
  Result := False;
  AContent := '';
  if SourceEditorManagerIntf = nil then
    Exit;
  editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(APath);
  if editor = nil then
    Exit;
  AContent := editor.GetText(False);
  Result := True;
end;

procedure TAIPlaneWindow.SyncProjectDirectory;
var
  dir: string;
begin
  dir := AIPlaneProjectDirectory;
  if SameFileName(dir, FProjectDir) then
    Exit;
  if Assigned(FChat.Adapter) and FChat.Adapter.IsBusy then
    Exit;
  FProjectDir := dir;
  FChat.WorkingDir := dir;
  // The runtime keeps its directory until it restarts.
  FChat.SetRuntime(FChat.PresetId, FChat.Command, FChat.Arguments);
  FChat.View.Add(cikInfo, 'Project directory: ' + dir);
end;

procedure TAIPlaneWindow.FormActivate(Sender: TObject);
begin
  SyncProjectDirectory;
end;

end.

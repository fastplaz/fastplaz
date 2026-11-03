unit aiplane_ide;

{
  Agent Plane inside the Lazarus IDE: the dockable window that hosts
  TAIChatFrame, and the settings it shares with the AI Harness tab of
  FastPlaz/Configuration.

  - Settings live in the FastPlaz config.json (GlobalConfig) under "aiplane".
  - The runtime works in the directory of the active project.
  - Files open in the source editor are served from the editor buffer, so the
    agent reads unsaved changes too.
  - The window runs the IDE's MCP server (aiplane_mcpserver, tools in
    aiplane_tools_ide) on 127.0.0.1 when "aiplane/mcp_enabled" is on (default),
    port "aiplane/mcp_port" (default 17845, next free port when taken).
  - The IDE Messages window (view "FastPlaz") gets a line when the runtime
    and model become active, when the MCP server starts or fails, and when
    the agent reports an error.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  LazIDEIntf, ProjectIntf, IDEWindowIntf, SrcEditorIntf, IDEMsgIntf, IDEExternToolIntf,
  aiplane_types, aiplane_chatframe, aiplane_mcpserver, aiplane_tools_ide,
  aiplane_tools_fastplaz;

const
  AIPLANE_WINDOW_NAME = 'AIPlaneWindow';
  RS_AIPLANE_MENU = '&Agent Plane';
  RS_AIPLANE_CAPTION = 'Agent Plane';

type
  TAIPlaneSettings = record
    Runtime: string;
    Command: string;
    Arguments: string;
    Model: string;
    Effort: string;
    McpEnabled: boolean;
    McpPort: integer;
  end;

  { TAIPlaneWindow }

  TAIPlaneWindow = class(TForm)
  private
    FChat: TAIChatFrame;
    FProjectDir: string;
    FApplying: boolean;
    FMcp: TAIMcpServer;
    FTools: TAIIdeTools;
    FFastPlazTools: TAIFastPlazTools;
    FMcpTimer: TTimer;
    FMcpPort: integer;
    FMcpNotice: string;
    procedure UpdateMcpServer(const ASettings: TAIPlaneSettings);
    procedure McpTimer(Sender: TObject);
    procedure McpPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
    procedure McpLog(Sender: TObject; const AText: string);
    procedure ChatNotice(Sender: TObject; const AText: string; AIsError: boolean);
    procedure log(AMessage: string; ACategory: string = 'Agent Plane';
      UrgencyLevel: TMessageLineUrgency = mluImportant);
    procedure ToolPermissionAnswer(Sender: TObject; const AKey, AOptionId: string);
    procedure ChatSettingsChanged(Sender: TObject);
    function ReadEditorBuffer(Sender: TObject; const APath: string;
      out AContent: string): boolean;
    procedure FormActivate(Sender: TObject);
    function ProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
    procedure SyncProjectDirectory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Applies the stored settings; keeps the conversation when the runtime
      stays the same. }
    procedure ApplySettings;
    property Chat: TAIChatFrame read FChat;
    property Mcp: TAIMcpServer read FMcp;
  end;

var
  AIPlaneWindow: TAIPlaneWindow = nil;

function AIPlaneLoadSettings: TAIPlaneSettings;
procedure AIPlaneSaveSettings(const ASettings: TAIPlaneSettings);
{ The model last used with each runtime ("preset=model", aiplane/models/...). }
procedure AIPlaneLoadRuntimeModels(AList: TStrings);
procedure AIPlaneSaveRuntimeModels(AList: TStrings);
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
  Result.Runtime := GlobalConfig.GetString('aiplane/runtime');
  Result.Command := GlobalConfig.GetString('aiplane/command');
  Result.Arguments := GlobalConfig.GetString('aiplane/arguments');
  Result.Model := GlobalConfig.GetString('aiplane/model');
  Result.Effort := GlobalConfig.GetString('aiplane/effort');
  if Result.Runtime = '' then
    Result.Runtime := AI_DEFAULT_PRESET;
  if Result.Effort = '' then
    Result.Effort := 'auto';
  Result.McpEnabled := GlobalConfig.GetValue(UnicodeString('aiplane/mcp_enabled'), True);
  Result.McpPort := GlobalConfig.GetValue(UnicodeString('aiplane/mcp_port'),
    integer(AI_MCP_DEFAULT_PORT));
  if (Result.McpPort < 1024) or (Result.McpPort > 65535) then
    Result.McpPort := AI_MCP_DEFAULT_PORT;
end;

procedure AIPlaneSaveSettings(const ASettings: TAIPlaneSettings);
begin
  LoadConfiguration;
  GlobalConfig.SetString('aiplane/runtime', ASettings.Runtime);
  GlobalConfig.SetString('aiplane/command', ASettings.Command);
  GlobalConfig.SetString('aiplane/arguments', ASettings.Arguments);
  GlobalConfig.SetString('aiplane/model', ASettings.Model);
  // the model last used with this runtime, brought back when switching to it
  if ASettings.Runtime <> '' then
    GlobalConfig.SetString('aiplane/models/' + ASettings.Runtime, ASettings.Model);
  GlobalConfig.SetString('aiplane/effort', ASettings.Effort);
  GlobalConfig.SetValue(UnicodeString('aiplane/mcp_enabled'), ASettings.McpEnabled);
  GlobalConfig.SetValue(UnicodeString('aiplane/mcp_port'), ASettings.McpPort);
  GlobalConfig.Flush;
end;

procedure AIPlaneLoadRuntimeModels(AList: TStrings);
var
  i: integer;
  presetId: string;
begin
  LoadConfiguration;
  for i := 0 to AIPresetCount - 1 do
  begin
    presetId := AIPreset(i).Id;
    AList.Values[presetId] := GlobalConfig.GetString('aiplane/models/' + presetId);
  end;
end;

procedure AIPlaneSaveRuntimeModels(AList: TStrings);
var
  i: integer;
  presetId: string;
begin
  LoadConfiguration;
  for i := 0 to AIPresetCount - 1 do
  begin
    presetId := AIPreset(i).Id;
    GlobalConfig.SetString('aiplane/models/' + presetId, AList.Values[presetId]);
  end;
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
  FChat.OnToolPermission := @ToolPermissionAnswer;
  FChat.OnNotice := @ChatNotice;

  FMcp := TAIMcpServer.Create;
  FMcp.OnPermission := @McpPermission;
  FMcp.OnLog := @McpLog;
  FTools := TAIIdeTools.Create;
  FTools.Register(FMcp);
  FFastPlazTools := TAIFastPlazTools.Create;
  FFastPlazTools.Register(FMcp);
  FMcpTimer := TTimer.Create(Self);
  FMcpTimer.Enabled := False;
  FMcpTimer.Interval := 40;
  FMcpTimer.OnTimer := @McpTimer;

  // The IDE restores its windows before it opens the start project: wait
  // for the project, or the runtime starts in the user directory and
  // restarts in the project directory right after.
  FChat.HoldStart := not (Assigned(LazarusIDE) and LazarusIDE.IDEStarted);
  if Assigned(LazarusIDE) then
    LazarusIDE.AddHandlerOnProjectOpened(@ProjectOpened);

  ApplySettings;
  FChat.OnSettingsChanged := @ChatSettingsChanged;
  OnActivate := @FormActivate;
end;

destructor TAIPlaneWindow.Destroy;
begin
  if Assigned(LazarusIDE) then
    LazarusIDE.RemoveAllHandlersOfObject(Self);
  if Assigned(FMcpTimer) then
    FMcpTimer.Enabled := False;
  if Assigned(FChat) then
  begin
    FChat.OnToolPermission := nil;
    FChat.OnNotice := nil;
  end;
  FreeAndNil(FMcp);
  FreeAndNil(FTools);
  FreeAndNil(FFastPlazTools);
  inherited Destroy;
end;

// Starts, stops or moves the MCP server and tells the chat where it is.
procedure TAIPlaneWindow.UpdateMcpServer(const ASettings: TAIPlaneSettings);
var
  notice: string;
begin
  if ASettings.McpEnabled then
  begin
    if not FMcp.Active or (FMcpPort <> ASettings.McpPort) then
    begin
      FMcpPort := ASettings.McpPort;
      FMcp.Start(FMcpPort);
    end;
  end
  else if FMcp.Active then
    FMcp.Stop;
  FMcpTimer.Enabled := FMcp.Active;
  // short in the status line; the address goes to the Messages window
  if FMcp.Active then
  begin
    FChat.SetMcpServer(FMcp.Url, FMcp.Token, 'MCP');
    notice := 'MCP server active on ' + FMcp.Url;
  end
  else if ASettings.McpEnabled then
  begin
    FChat.SetMcpServer('', '', 'MCP: ' + FMcp.LastError);
    notice := 'MCP server not started: ' + FMcp.LastError;
  end
  else
  begin
    FChat.SetMcpServer('', '', '');
    notice := 'MCP server off';
  end;
  // only changes, ApplySettings runs on every settings save
  if notice <> FMcpNotice then
  begin
    if not FMcp.Active and ASettings.McpEnabled then
      log(notice, 'Agent Plane', mluError)
    else if FMcp.Active or (FMcpNotice <> '') then
      // "off" only when it was on before, not when the window opens
      log(notice);
    FMcpNotice := notice;
  end;
end;

procedure TAIPlaneWindow.ChatNotice(Sender: TObject; const AText: string;
  AIsError: boolean);
begin
  if AIsError then
    log(AText, 'Agent Plane', mluError)
  else
    log(AText);
end;

procedure TAIPlaneWindow.log(AMessage: string; ACategory: string;
  UrgencyLevel: TMessageLineUrgency);
begin
  // IDE messages window and LCL controls are not thread safe
  if (GetCurrentThreadId <> MainThreadID) or not Assigned(IDEMessagesWindow) then
    Exit;
  if ACategory = '' then
    IDEMessagesWindow.AddCustomMessage(UrgencyLevel, AMessage, '', 0, 0, AI_FASTPLAZ_MESSAGE_VIEW)
  else
    IDEMessagesWindow.AddCustomMessage(UrgencyLevel, ACategory + ': ' + AMessage,
      '', 0, 0, AI_FASTPLAZ_MESSAGE_VIEW);
end;

procedure TAIPlaneWindow.McpTimer(Sender: TObject);
var
  rounds: integer;
begin
  rounds := 0;
  while FMcp.Poll and (rounds < 20) do
    Inc(rounds);
end;

procedure TAIPlaneWindow.McpPermission(Sender: TObject;
  const ARequest: TAIPermissionRequest);
begin
  FChat.ShowToolPermission(ARequest);
end;

procedure TAIPlaneWindow.McpLog(Sender: TObject; const AText: string);
begin
  FChat.AddLog('[mcp] ' + AText);
end;

procedure TAIPlaneWindow.ToolPermissionAnswer(Sender: TObject;
  const AKey, AOptionId: string);
begin
  FMcp.RespondPermission(AKey, AOptionId);
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
    UpdateMcpServer(settings);
    AIPlaneLoadRuntimeModels(FChat.RuntimeModels);
    FChat.RuntimeModels.Values[settings.Runtime] := settings.Model;
    // Runtime first: the new model on the old, running runtime would be
    // announced as an active pair for a moment.
    if not SameText(settings.Runtime, FChat.PresetId) or
      (settings.Command <> FChat.Command) or (settings.Arguments <> FChat.Arguments) then
      FChat.SetRuntime(settings.Runtime, settings.Command, settings.Arguments);
    FChat.Model := settings.Model;
    FChat.Effort := settings.Effort;
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
  // Still held: nothing ran in the old directory, the start picks this one.
  // The runtime keeps its directory until it restarts.
  if not FChat.HoldStart then
    FChat.SetRuntime(FChat.PresetId, FChat.Command, FChat.Arguments);
  FChat.View.Add(cikInfo, 'Project directory: ' + dir);
end;

procedure TAIPlaneWindow.FormActivate(Sender: TObject);
begin
  SyncProjectDirectory;
  // fallback when no project-opened call came
  if FChat.HoldStart and Assigned(LazarusIDE) and LazarusIDE.IDEStarted then
    FChat.HoldStart := False;
end;

function TAIPlaneWindow.ProjectOpened(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin
  Result := mrOk;
  SyncProjectDirectory;
  FChat.HoldStart := False;
end;

end.

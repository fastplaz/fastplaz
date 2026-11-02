unit menu_experts;

{$mode objfpc}{$H+}

interface

uses
  Controls, Dialogs, Clipbrd,
  // INTF
  LazIDEIntf, MenuIntf, IDEWindowIntf, IDECommands, ProjectIntf,
  IDEExternToolIntf, SrcEditorIntf, LCLType,
  Classes, SysUtils, UTF8Process, process;

const
  FASTPLAZ_EXPERT_MAINMENU_NAME = 'mnu_FastPlazExpertMainMenu';
  FASTPLAZ_EXPERT_MAINMENU_CAPTION = 'FastPla&z';

var
  oMenuExpert: TIDEMenuSection = nil;
  icRevealInFinder: TIDECommand;
  icCopyFilePath: TIDECommand;
  icCopyDirectoryPath: TIDECommand;
  icOpenTerminalFromHere: TIDECommand;
  icFastCode: TIDECommand;

procedure CreateIDEMenus;
procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);
procedure CreatePackage_Proc(ASender: TObject);
procedure RevealInFinder_Proc(ASender: TObject);
procedure CopyFilePath_Proc(ASender: TObject);
procedure CopyDirectoryPath_Proc(ASender: TObject);

implementation

uses fastplaz_tools_register, tools_lib, about_fastplaz, webstructure_wzd,
  themestructure_wzd, modsimple_lib, modsimple_wzd, modsimplejson_lib,
  model_lib, model_wzd, packageapp_wzd, packageapp_lib,
  json_tools, regex_tester,
  //Database Explorer
  de_connector, de_dbbrowser, de_common, fastcode_configuration,
  //AI Plane
  aiplane_ide;

// Asks for the module name, then creates "<permalink>_controller.pas" with
// the given descriptor. The descriptor reads ModulTypeName and Permalink.
procedure GenerateModule(ADescriptorClass: TProjectFileDescriptorClass);
var
  wizard: TfModuleSimpleWizard;
  fileName: string;
begin
  wizard := TfModuleSimpleWizard.Create(nil);
  try
    if wizard.ShowModal <> mrOk then
      Exit;
    ModulTypeName := ControllerTypeName(wizard.edt_ModuleName.Text);
    Permalink := SanitizePermalink(wizard.edt_Permalink.Text);
    if Permalink = '' then
      Permalink := SanitizePermalink(wizard.edt_ModuleName.Text);
  finally
    wizard.Free;
  end;
  if Permalink = '' then
    Permalink := 'sample';
  fileName := SanitizeIdentifier(Permalink) + '_controller.pas';

  bCreateProject := False;
  bExpert := True;
  try
    LazarusIDE.DoNewEditorFile(ADescriptorClass.Create, fileName, '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  finally
    bExpert := False;
  end;
end;

procedure SimpleModuleGenerator_Proc(ASender: TObject);
begin
  GenerateModule(TFileDescDefaultModule);
end;

procedure JsonModuleGenerator_Proc(ASender: TObject);
begin
  GenerateModule(TFileDescJSONModule);
end;

procedure ModelGenerator_Proc(ASender: TObject);
var
  wizard: TfModelWizard;
begin
  wizard := TfModelWizard.Create(nil);
  try
    if wizard.ShowModal <> mrOk then
      Exit;
    ModelName := SanitizeIdentifier(StringReplace(UcWords(wizard.edt_ModelName.Text),
      ' ', '', [rfReplaceAll]), 'Sample');
  finally
    wizard.Free;
  end;
  CustomTableName := '';

  bCreateProject := False;
  bExpert := True;
  try
    LazarusIDE.DoNewEditorFile(TFileDescModel.Create,
      LowerCase(ModelName) + '_model.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  finally
    bExpert := False;
  end;
end;

procedure CreatePackage_Proc(ASender: TObject);
var
  wizard: TfPackageWizard;
  projectName, projectFileName, targetDir: string;
  packageLib: TPackageAppLib;
begin
  wizard := TfPackageWizard.Create(nil);
  try
    if wizard.ShowModal <> mrOk then
      Exit;
    projectName := SanitizeProjectName(wizard.edt_ProjectName.Text);
    targetDir := IncludeTrailingPathDelimiter(wizard.edt_TargetDir.Text);
  finally
    wizard.Free;
  end;

  packageLib := TPackageAppLib.Create;
  try
    if not packageLib.GenerateStructure(targetDir, 'Simple', projectName) then
    begin
      log('Full Package App: some files could not be copied to ' + targetDir,
        '', mluError);
      Exit;
    end;
  finally
    packageLib.Free;
  end;

  // the template project is always named fastplaz.lpr
  projectFileName := targetDir + 'source' + DirectorySeparator +
    'app' + DirectorySeparator + DEFAULT_PROJECT_NAME + '.lpr';
  log('Full Package App installed on: ' + targetDir, '', mluProgress);
  log('Project File: ' + projectFileName, '', mluImportant);
  LazarusIDE.DoOpenProjectFile(projectFileName, [ofProjectLoading]);
end;

procedure RevealInFinder_Proc(ASender: TObject);
var
  s: string;
  srcEdit: TSourceEditorInterface;
begin
  srcEdit := SourceEditorManagerIntf.ActiveEditor;
  if srcEdit = nil then
    Exit;

  if FileExists(srcEdit.FileName) then
  begin
    try
      {$ifdef WINDOWS}
      RunCommand('explorer', ['/select,' + srcEdit.FileName], s);
      {$endif}
      {$ifdef DARWIN}
      RunCommand('open', ['-R', srcEdit.FileName], s);
      {$endif}
      {$ifdef LINUX}
      RunCommand('xdg-open', [ExtractFilePath(srcEdit.FileName)], s);
      {$endif}
    except
      on E: Exception do
      begin
        ShowMessage(E.Message);
      end;
    end;
  end;
end;

procedure CopyFilePath_Proc(ASender: TObject);
var
  srcEdit: TSourceEditorInterface;
begin
  srcEdit := SourceEditorManagerIntf.ActiveEditor;
  if srcEdit = nil then
    Exit;

  Clipboard.AsText := srcEdit.FileName;
end;

procedure CopyDirectoryPath_Proc(ASender: TObject);
var
  srcEdit: TSourceEditorInterface;
begin
  srcEdit := SourceEditorManagerIntf.ActiveEditor;
  if srcEdit = nil then
    Exit;

  Clipboard.AsText := ExtractFileDir(srcEdit.FileName);
end;

procedure FastCodeConfiguration_Proc(ASender: TObject);
begin
  if not Assigned(fFastCodeConfiguration) then
    fFastCodeConfiguration := TfFastCodeConfiguration.Create(nil);
  fFastCodeConfiguration.Prepare;
  fFastCodeConfiguration.Show;
end;

procedure FastCode_Proc(ASender: TObject);
var
  currentLine: string;
begin
  currentLine := GetCurrentLine().Trim;
  if currentLine.IsEmpty then
    Exit;
  if not currentLine.StartsWith(RS_FAST_CODE_MARK) then
  begin
    log('Line must start with the // comment mark');
    Exit;
  end;
  currentLine := currentLine.Replace(RS_FAST_CODE_MARK, '', [rfReplaceAll]).Trim;
  if currentLine.IsEmpty then
    Exit;

  log('Find something about "' + currentLine + '"');
  GetFastCodeSuggestion(currentLine);
end;

procedure CreateWebStructure_Proc(ASender: TObject);
var
  wizard: TfWebStructure;
begin
  wizard := TfWebStructure.Create(nil);
  try
    if wizard.ShowModal = mrOk then
      wizard.CreateStructure(wizard.edt_TargetDir.Text);
  finally
    wizard.Free;
  end;
end;

procedure CreateThemeStructure_Proc(ASender: TObject);
var
  wizard: TfThemeStructure;
begin
  wizard := TfThemeStructure.Create(nil);
  try
    if wizard.ShowModal = mrOk then
      wizard.CreateTheme(wizard.edt_ThemeName.Text, wizard.edt_TargetDir.Text);
  finally
    wizard.Free;
  end;
end;

procedure JSONTools_Proc(ASender: TObject);
begin
  if not Assigned(fJSONTools) then
    fJSONTools := TfJSONTools.Create(nil);
  fJSONTools.Show;
end;

procedure RegexTester_Proc(ASender: TObject);
begin
  if not Assigned(fRegex) then
    fRegex := TfRegex.Create(nil);
  fRegex.Show;
end;

// Directory of the active project; falls back to the active editor file
// when the project has not been saved yet.
function ActiveProjectDirectory: string;
var
  srcEdit: TSourceEditorInterface;
begin
  Result := '';
  if Assigned(LazarusIDE.ActiveProject) then
    Result := ExtractFileDir(LazarusIDE.ActiveProject.ProjectInfoFile);
  if (Result = '') or not DirectoryExists(Result) then
  begin
    Result := '';
    srcEdit := SourceEditorManagerIntf.ActiveEditor;
    if Assigned(srcEdit) and FileExists(srcEdit.FileName) then
      Result := ExtractFileDir(srcEdit.FileName);
  end;
end;

{$if not defined(WINDOWS) and not defined(DARWIN)}
// First terminal emulator found: $TERMINAL, then the common ones.
function FindTerminalExecutable: string;
const
  TERMINALS: array[0..8] of string = ('x-terminal-emulator', 'gnome-terminal',
    'konsole', 'xfce4-terminal', 'mate-terminal', 'lxterminal', 'tilix',
    'kitty', 'xterm');
var
  envPath, terminalName: string;
begin
  envPath := GetEnvironmentVariable('PATH');
  terminalName := GetEnvironmentVariable('TERMINAL');
  if terminalName <> '' then
  begin
    Result := ExeSearch(terminalName, envPath);
    if Result <> '' then
      Exit;
  end;
  for terminalName in TERMINALS do
  begin
    Result := ExeSearch(terminalName, envPath);
    if Result <> '' then
      Exit;
  end;
  Result := '';
end;
{$endif}

// Opens the platform terminal with ADirectory as its working directory:
// cmd.exe on Windows, Terminal.app on macOS, the default emulator elsewhere.
procedure OpenTerminalIn(const ADirectory: string);
var
  directory: string;
  terminal: TProcessUTF8;
begin
  directory := ADirectory;
  if (directory = '') or not DirectoryExists(directory) then
  begin
    ShowMessage(RS_OPEN_TERMINAL_NO_DIRECTORY);
    Exit;
  end;

  terminal := TProcessUTF8.Create(nil);
  try
    try
      terminal.CurrentDirectory := directory;
      {$if defined(WINDOWS)}
      terminal.Executable := GetEnvironmentVariable('COMSPEC');
      if terminal.Executable = '' then
        terminal.Executable := 'cmd.exe';
      terminal.Options := [poNewConsole];
      {$elseif defined(DARWIN)}
      terminal.Executable := 'open';
      terminal.Parameters.Add('-a');
      terminal.Parameters.Add('Terminal');
      terminal.Parameters.Add(directory);
      {$else}
      terminal.Executable := FindTerminalExecutable;
      if terminal.Executable = '' then
      begin
        ShowMessage(RS_OPEN_TERMINAL_NOT_FOUND);
        Exit;
      end;
      {$endif}
      terminal.Execute;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    terminal.Free;
  end;
end;

procedure OpenTerminal_Proc(ASender: TObject);
begin
  OpenTerminalIn(ActiveProjectDirectory);
end;

procedure OpenTerminalFromHere_Proc(ASender: TObject);
var
  srcEdit: TSourceEditorInterface;
begin
  srcEdit := SourceEditorManagerIntf.ActiveEditor;
  if (srcEdit = nil) or not FileExists(srcEdit.FileName) then
  begin
    ShowMessage(RS_OPEN_TERMINAL_NO_DIRECTORY);
    Exit;
  end;
  OpenTerminalIn(ExtractFileDir(srcEdit.FileName));
end;

procedure About_Proc(ASender: TObject);
begin
  if fAboutFastplaz = nil then
    fAboutFastplaz := TfAboutFastplaz.Create(nil);
  fAboutFastplaz.ShowModal;
end;

procedure CreateIDEMenus;
var
  cat: TIDECommandCategory;
  key, copyPathKey, copyDirectoryKey: TIDEShortCut;
begin
  oMenuExpert := RegisterIDESubMenu(mnuMain, FASTPLAZ_EXPERT_MAINMENU_NAME,
    FASTPLAZ_EXPERT_MAINMENU_CAPTION);

  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_ModuleCreator',
    rs_Mod_Default_Name, nil, @SimpleModuleGenerator_Proc, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_ModuleJsonCreator',
    rs_Mod_JSON_Name, nil, @JsonModuleGenerator_Proc, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_ModelCreator',
    rs_Model_Name, nil, @ModelGenerator_Proc, nil);

  CreateIDEMenuSeparator(oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_FullPackage',
    'Create Full Package Application', nil, @CreatePackage_Proc, nil);
  CreateIDEMenuSeparator(oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_CreateWebStructure',
    'Create Web Directory Structure', nil, @CreateWebStructure_Proc, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_CreateThemeStructure',
    'Create Theme Structure', nil, @CreateThemeStructure_Proc, nil);

  // AI Plane, Database Explorer, JSON Tools
  CreateIDEMenuSeparator(oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_AIPlane',
    RS_AIPLANE_MENU, nil, @AIPlaneShowWindow, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_DatabaseExplorer',
    RS_DATABASE_EXPLORER_MENU, nil, @ViewDBConnector, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_JSONTools',
    RS_JSON_TOOLS_MENU, nil, @JSONTools_Proc, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_RegexTester',
    RS_REGEX_TESTER_MENU, nil, @RegexTester_Proc, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_OpenTerminal',
    RS_OPEN_TERMINAL_MENU, nil, @OpenTerminal_Proc, nil);
  CreateIDEMenuSeparator(oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_FastCodeConfiguration',
    RS_FAST_CODE_CONFIGURATION, nil, @FastCodeConfiguration_Proc, nil);

  CreateIDEMenuSeparator(oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_About', 'About',
    nil, @About_Proc, nil, 'icon_information');

  // add a menu item in the file menu
  CreateIDEMenuSeparator(itmFileNew);
  RegisterIDEMenuCommand(itmFileNew, 'mnu_FastPlaz_File_New',
    'New Web Application Package ...', nil, @CreatePackage_Proc, nil);

  // register FDE window
  IDEWindowCreators.Add(FDE_WINDOW_NAME, @CreateIDEConnectorWindow,
    nil, '250', '250', '', '');
  IDEWindowCreators.Add(FDE_BROWSER_WINDOW_NAME, @CreateIDEBrowserWindow,
    nil, '250', '250', '', '');
  AIPlaneRegisterWindow;

  // reveal in finder
  key := IDEShortCut(VK_E, [ssAlt, ssShift], VK_UNKNOWN, []);
  cat := IDECommandList.FindCategoryByName(CommandCategoryTextEditingName);
  icRevealInFinder := RegisterIDECommand(cat, rsRevealInFinder,
    rsRevealInFinder, key, nil, @RevealInFinder_Proc);

  // copy file path
  copyPathKey := IDEShortCut(VK_F, [ssAlt, ssShift], VK_UNKNOWN, []);
  icCopyFilePath := RegisterIDECommand(cat, rsCopyFilePath,
    rsCopyFilePath, copyPathKey, nil, @CopyFilePath_Proc);

  // copy directory path
  copyDirectoryKey := IDEShortCut(VK_D, [ssAlt, ssShift], VK_UNKNOWN, []);
  icCopyDirectoryPath := RegisterIDECommand(cat, rsCopyDirectoryPath,
    rsCopyDirectoryPath, copyDirectoryKey, nil, @CopyDirectoryPath_Proc);

  // open terminal in the directory of the active file (no default shortcut)
  icOpenTerminalFromHere := RegisterIDECommand(cat, rsOpenTerminalFromHere,
    rsOpenTerminalFromHere, IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []), nil,
    @OpenTerminalFromHere_Proc);

  // add a menu item in the source editor
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'RevealInFinder',
    rsRevealInFinder, nil, nil, icRevealInFinder, 'reveal_in_finder');
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'CopyFilePath',
    rsCopyFilePath, nil, nil, icCopyFilePath, 'copy_file_path');
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'CopyDirectoryPath',
    rsCopyDirectoryPath, nil, nil, icCopyDirectoryPath, 'copy_directory_path');
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'OpenTerminalFromHere',
    rsOpenTerminalFromHere, nil, nil, icOpenTerminalFromHere);

  // FAST CODE ---
  key := IDEShortCut(VK_O, [ssCtrl, ssAlt], VK_UNKNOWN, []);
  icFastCode := RegisterIDECommand(cat, RS_FAST_CODE,
    RS_FAST_CODE, key, nil, @FastCode_Proc);

  // add a fast code menu item in the source editor
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'FastCode',
    RS_FAST_CODE, nil, nil, icFastCode, 'fast_code');
end;

procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);
begin
  RegisterIDEMenuCommand(poParent, '', '-');
end;

end.

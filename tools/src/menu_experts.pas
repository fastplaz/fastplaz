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
  icFastCode: TIDECommand;

procedure CreateIDEMenus;
procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);
procedure CreatePackage_Proc(ASender: TObject);
procedure RevealInFinder_Proc(ASender: TObject);
procedure CopyFilePath_Proc(ASender: TObject);
procedure CopyDirectoryPath_Proc(ASender: TObject);

implementation

uses fastplaz_tools_register, about_fastplaz, webstructure_wzd, themestructure_wzd,
  modsimple_lib, modsimple_wzd, modsimplejson_lib, model_lib, model_wzd,
  packageapp_wzd, packageapp_lib,
  json_tools, regex_tester,
  //Database Explorer
  de_connector, de_dbbrowser, de_common, fastcode_configuration;

procedure NewAppGenerator_Proc(ASender: TObject);
begin
  // prepare for next features
end;

procedure SimpleModuleGenerator_Proc(ASender: TObject);
begin
  with TfModuleSimpleWizard.Create(nil) do
  begin
    if ShowModal <> mrOk then
    begin
      Free;
      Exit;
    end;

    bCreateProject := False;
    bExpert := True;
    if edt_ModuleName.Text <> '' then
      ModulTypeName := 'T' + StringReplace(UcWords(edt_ModuleName.Text),
        ' ', '', [rfReplaceAll]) + 'Controller';
    Permalink := edt_Permalink.Text;
    if Permalink = '' then
    begin
      Permalink := StringReplace(UcWords(edt_ModuleName.Text),
        ' ', '', [rfReplaceAll]);
    end;

    LazarusIDE.DoNewEditorFile(TFileDescDefaultModule.Create,
      Permalink + '_controller.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);

    Free;
  end;
  bExpert := False;
end;

procedure JsonModuleGenerator_Proc(ASender: TObject);
begin
  with TfModuleSimpleWizard.Create(nil) do
  begin
    if ShowModal <> mrOk then
    begin
      Free;
      Exit;
    end;

    bCreateProject := False;
    bExpert := True;
    if edt_ModuleName.Text <> '' then
      ModulTypeName := 'T' + StringReplace(UcWords(edt_ModuleName.Text),
        ' ', '', [rfReplaceAll]) + 'Controller';
    Permalink := edt_Permalink.Text;
    if Permalink = '' then
    begin
      Permalink := StringReplace(UcWords(edt_ModuleName.Text),
        ' ', '', [rfReplaceAll]);
    end;

    LazarusIDE.DoNewEditorFile(TFileDescJSONModule.Create,
      Permalink + '_controller.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);

    Free;
  end;
  bExpert := False;
end;

procedure ModelGenerator_Proc(ASender: TObject);
begin
  with TfModelWizard.Create(nil) do
  begin
    if ShowModal <> mrOk then
    begin
      Free;
      Exit;
    end;

    bCreateProject := False;
    bExpert := True;
    if edt_ModelName.Text <> '' then
      ModelName := StringReplace(UcWords(edt_ModelName.Text), ' ',
        '', [rfReplaceAll]);

    LazarusIDE.DoNewEditorFile(TFileDescModel.Create,
      LowerCase(ModelName) + '_model.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);

    Free;
  end;
  bExpert := False;
end;

procedure CreatePackage_Proc(ASender: TObject);
var
  projectTitle, projectName, projectFileName, targetDir: string;
begin
  with TfPackageWizard.Create(nil) do
  begin
    if ShowModal <> mrOk then
    begin
      Free;
      Exit;
    end;

    projectTitle := ucwords(edt_ProjectName.Text);
    projectName := LowerCase(edt_ProjectName.Text);
    projectName := StringReplace(projectName, ' ', '', [rfReplaceAll]);
    projectName := StringReplace(projectName, '.', '', [rfReplaceAll]);
    targetDir := IncludeTrailingPathDelimiter(edt_TargetDir.Text);
    Free;
  end;

  with TPackageAppLib.Create do
  begin
    GenerateStructure(targetDir, 'Simple', projectName);
    Free;
  end;

  projectName := 'fastplaz'; // force
  projectFileName := targetDir + 'source' + DirectorySeparator +
    'app' + DirectorySeparator + projectName + '.lpr';
  log('Full Package App installed on: ' + targetDir, '', mluProgress);
  log('Project File: ' + projectFileName, '', mluImportant);
  LazarusIDE.DoOpenProjectFile(projectFileName, [ofProjectLoading]);
end;

procedure RevealInFinder_Proc(ASender: TObject);
var
  s, pathName: string;
  srcEdit: TSourceEditorInterface;
begin
  srcEdit := SourceEditorManagerIntf.ActiveEditor;
  if srcEdit = nil then
    Exit;

  if FileExists(srcEdit.FileName) then
  begin
    try
      {$ifdef WINDOWS}
      RunCommand('explorer',['/select,' + srcEdit.FileName], s);
      {$endif}
      {$ifdef DARWIN}
      RunCommand('open',['-R', srcEdit.FileName], s);
      {$endif}
      {$ifdef LINUX}
      pathName := ExtractFilePath(srcEdit.FileName);
      RunCommand('xdg-open',[pathName], s);
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
  currentLine, answer: string;
begin
  currentLine := GetCurrentLine().Trim;
  if currentLine.IsEmpty then Exit;
  if not currentLine.StartsWith(RS_FAST_CODE_MARK) then
  begin
    log('Line must started with // comment');
    Exit
  end;
  currentLine:= currentLine.Replace(RS_FAST_CODE_MARK, '', [rfReplaceAll]).Trim;
  if currentLine.IsEmpty then Exit;

  log('Find something about "'+currentLine+'"');
  if not Assigned(fFastCodeConfiguration) then
    fFastCodeConfiguration := TfFastCodeConfiguration.Create(nil);
  answer := GetFastCodeSuggestion(currentLine);
end;

procedure CreateWebStructure_Proc(ASender: TObject);
begin
  with TfWebStructure.Create(nil) do
  begin
    if ShowModal = mrOk then
    begin
      CreateStructure(edt_TargetDir.Text);
    end;
    Free;
  end;
end;

procedure CreateThemeStructure_Proc(ASender: TObject);
begin
  with TfThemeStructure.Create(nil) do
  begin
    if ShowModal = mrOk then
    begin
      CreateTheme(edt_ThemeName.Text, edt_TargetDir.Text);
    end;
    Free;
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

  // prepare for next features
  {
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_AppCreator',
    rs_Project_Name, nil, @NewAppGenerator_Proc, nil);
  CreateIDEMenuSeparator(oMenuExpert);
  }

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

  // Database Explorer, JSON Tools
  CreateIDEMenuSeparator(oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_DatabaseExplorer',
    RS_DATABASE_EXPLORER_MENU, nil, @ViewDBConnector, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_JSONTools',
    RS_JSON_TOOLS_MENU, nil, @JSONTools_Proc, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaz_RegexTester',
    RS_REGEX_TESTER_MENU, nil, @RegexTester_Proc, nil);
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

  // reveal in finder
  key := IDEShortCut(VK_E,[ssAlt,ssShift],VK_UNKNOWN,[]);
  cat := IDECommandList.FindCategoryByName(CommandCategoryTextEditingName);
  icRevealInFinder := RegisterIDECommand(cat, rsRevealInFinder,
    rsRevealInFinder, key, nil, @RevealInFinder_Proc);

  // copy file path
  copyPathKey := IDEShortCut(VK_F,[ssAlt,ssShift],VK_UNKNOWN,[]);
  icCopyFilePath := RegisterIDECommand(cat, rsCopyFilePath,
    rsCopyFilePath, copyPathKey, nil, @CopyFilePath_Proc);

  // copy directory path
  copyDirectoryKey := IDEShortCut(VK_D,[ssAlt,ssShift],VK_UNKNOWN,[]);
  icCopyDirectoryPath := RegisterIDECommand(cat, rsCopyDirectoryPath,
    rsCopyDirectoryPath, copyDirectoryKey, nil, @CopyDirectoryPath_Proc);

  // add a menu item in the source editor
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'RevealInFinder',
    rsRevealInFinder, nil, nil, icRevealInFinder, 'reveal_in_finder');
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'CopyFilePath',
    rsCopyFilePath, nil, nil, icCopyFilePath, 'copy_file_path');
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'CopyDirectoryPath',
    rsCopyDirectoryPath, nil, nil, icCopyDirectoryPath, 'copy_directory_path');

  // FAST CODE ---
  key := IDEShortCut(VK_O,[ssCtrl, ssAlt],VK_UNKNOWN,[]);
  cat := IDECommandList.FindCategoryByName(CommandCategoryTextEditingName);
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

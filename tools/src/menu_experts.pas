unit menu_experts;

{$mode objfpc}{$H+}

interface

uses
  Controls, Dialogs,
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

procedure CreateIDEMenus;
procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);
procedure CreatePackage_Proc(ASender: TObject);
procedure RevealInFinder_Proc(ASender: TObject);

implementation

uses fastplaz_tools_register, about_fastplaz, webstructure_wzd, themestructure_wzd,
  modsimple_lib, modsimple_wzd, modsimplejson_lib, model_lib, model_wzd,
  packageapp_wzd, packageapp_lib,
  json_tools, regex_tester,
  //Database Explorer
  de_connector, de_dbbrowser, de_common;

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
  key: TIDEShortCut;
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

  // add a menu item in the source editor
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'RevealInFinder',
    rsRevealInFinder, nil, nil, icRevealInFinder, 'reveal_in_finder');



end;

procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);
begin
  RegisterIDEMenuCommand(poParent, '', '-');
end;

end.

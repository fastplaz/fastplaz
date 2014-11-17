unit menu_experts;

{$mode objfpc}{$H+}

interface

uses
  Controls, Dialogs,
  LazIDEIntf, MenuIntf, IDECommands,
  Classes, SysUtils;

const
  FASTPLAZ_EXPERT_MAINMENU_NAME = 'mnu_FastPlazExpertMainMenu';
  FASTPLAZ_EXPERT_MAINMENU_CAPTION = 'FastPla&z';

var
  oMenuExpert: TIDEMenuSection = nil;

procedure CreateIDEMenus;
procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);

implementation

uses fastplaz_tools_register, about_fastplaz, webstructure_wzd,
  modsimple_lib, modsimple_wzd, modsimplejson_lib, model_lib, model_wzd;

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
        ' ', '', [rfReplaceAll]) + 'Module';
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
        ' ', '', [rfReplaceAll]) + 'Module';
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

procedure About_Proc(ASender: TObject);
begin
  if fAboutFastplaz = nil then
    fAboutFastplaz := TfAboutFastplaz.Create(nil);
  fAboutFastplaz.ShowModal;
end;



procedure CreateIDEMenus;
var
  Key: TIDEShortCut;
begin
  oMenuExpert := RegisterIDESubMenu(mnuMain, FASTPLAZ_EXPERT_MAINMENU_NAME,
    FASTPLAZ_EXPERT_MAINMENU_CAPTION);

  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaze_ModuleCreator',
    rs_Mod_Default_Name, nil, @SimpleModuleGenerator_Proc, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaze_ModuleJsonCreator',
    rs_Mod_JSON_Name, nil, @JsonModuleGenerator_Proc, nil);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaze_ModelCreator',
    rs_Model_Name, nil, @ModelGenerator_Proc, nil);

  CreateIDEMenuSeparator(oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlaze_CreateWebStructure',
    'Create Web Directory Structure', nil, @CreateWebStructure_Proc, nil);
  CreateIDEMenuSeparator(oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlazAbout', 'About',
    nil, @About_Proc, nil, 'icon_information');
end;

procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);
begin
  RegisterIDEMenuCommand(poParent, '', '-');
end;

end.

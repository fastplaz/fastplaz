unit menu_experts;

{$mode objfpc}{$H+}

interface

uses
  Controls, Dialogs,
  MenuIntf, IDECommands,
  Classes, SysUtils;

const
  FASTPLAZ_EXPERT_MAINMENU_NAME = 'mnu_FastPlazExpertMainMenu';
  FASTPLAZ_EXPERT_MAINMENU_CAPTION = 'FastPla&z';

var
  oMenuExpert: TIDEMenuSection = nil;

procedure CreateIDEMenus;
procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);

implementation

uses about_fastplaz, webstructure_wzd;

procedure About_Proc(ASender: TObject);
begin
  if fAboutFastplaz = nil then
    fAboutFastplaz := TfAboutFastplaz.Create(nil);
  fAboutFastplaz.ShowModal;
end;

procedure CreateWebStructure_Proc(ASender: TObject);
begin
  with TfWebStructure.Create(nil) do
  begin
    if ShowModal = mrOk then
    begin
      CreateStructure( edt_TargetDir.Text);
    end;
    Free;
  end;

end;

procedure CreateIDEMenus;
var
  Key: TIDEShortCut;
begin
  oMenuExpert := RegisterIDESubMenu(mnuMain, FASTPLAZ_EXPERT_MAINMENU_NAME,
    FASTPLAZ_EXPERT_MAINMENU_CAPTION);

  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlazeCreateWebStructure',
    'Create Web Directory Structure', nil, @CreateWebStructure_Proc, nil);
  CreateIDEMenuSeparator( oMenuExpert);
  RegisterIDEMenuCommand(oMenuExpert, 'mnu_FastPlazAbout', 'About', nil,
    @About_Proc, nil, 'icon_information');
end;

procedure CreateIDEMenuSeparator(poParent: TIDEMenuSection);
begin
  RegisterIDEMenuCommand(poParent, '', '-');
end;

end.


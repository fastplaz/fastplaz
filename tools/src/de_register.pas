unit de_register;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ButtonPanel, Menus, Spin,
  de_connector, de_dbbrowser, de_common,
  // INTF
  IDECommands, IDEMsgIntf, MenuIntf, PackageIntf, SrcEditorIntf,
  IDEExternToolIntf, IDEWindowIntf, LazIDEIntf;

var
  ViewDBConnectorCmd: TIDECommand;
  oDBEMenuExpert: TIDEMenuSection = nil;
  oExistingMenu: TIDEMenuItem = nil;


procedure Register;
procedure log(const Msg: string; AFileName: string = '';
  ATheUrgency: TMessageLineUrgency = mluNone);

implementation

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
begin
  oDBEMenuExpert := RegisterIDESubMenu(mnuMain, FASTPLAZ_EXPERT_DBE_MAINMENU_NAME,
    FASTPLAZ_EXPERT_DBE_MAINMENU_CAPTION);

  oExistingMenu := mnuMain.FindByName('mnu_FastPlazExpertMainMenu');
  if oExistingMenu <> nil then
  begin
    //
  end;

  RegisterIDEMenuCommand(oDBEMenuExpert, 'mnu_FastPlaz_DatabaseExplorer',
    RS_DATABASE_EXPLORER_MENU, nil, @ViewDBConnector, nil);

  // register FDE window
  IDEWindowCreators.Add(FDE_WINDOW_NAME, @CreateIDEConnectorWindow, nil, '250', '250', '', '');
  IDEWindowCreators.Add(FDE_BROWSER_WINDOW_NAME, @CreateIDEBrowserWindow, nil, '250', '250', '', '');
end;

procedure log(const Msg: string; AFileName: string = '';
  ATheUrgency: TMessageLineUrgency = mluNone);
begin
  IDEMessagesWindow.AddCustomMessage(ATheUrgency, Msg, AFileName, 0, 0, 'FastPlaz');
end;

end.

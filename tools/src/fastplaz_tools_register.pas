unit fastplaz_tools_register;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf, IDEExternToolIntf,
  PackageIntf, Classes, SysUtils;

const
  FastPlaz = 'FastPlaz';
  _APP_SLOGAN = 'Fast Web Development Framework for Pascal';
  {$ifdef windows}
  _APP_EXTENSION = '.exe';
  {$else}
  _APP_EXTENSION = '.bin';
  {$endif}

  FASTPLAZ_TOOLS_PACKAGE = 'fastplaz_tools';
  FASTPLAZ_RUNTIME_PACKAGE = 'fastplaz_runtime';

procedure Register;
procedure log(const Msg: string; AFileName: string = '';
  ATheUrgency: TMessageLineUrgency = mluNote);

// Expanded directory of an installed package with a trailing path delimiter,
// '' when the package is not known to the IDE.
function PackageDirectory(const APackageName: string): string;

var
  bCreateProject: boolean = False;
  bExpert: boolean = False;
  FastPlazRuntimeDirectory: string;
  ModulTypeName, Permalink: string;

implementation

uses modsimple_lib, modsimplejson_lib, model_lib, project_lib, projectapi_lib,
  menu_experts;

function PackageDirectory(const APackageName: string): string;
var
  Pkg: TIDEPackage;
begin
  Result := '';
  if PackageEditingInterface = nil then
    Exit;
  Pkg := PackageEditingInterface.FindPackageWithName(APackageName);
  if Pkg = nil then
    Exit;
  Result := IncludeTrailingPathDelimiter(Pkg.DirectoryExpanded);
end;

procedure log(const Msg: string; AFileName: string; ATheUrgency: TMessageLineUrgency);
begin
  if IDEMessagesWindow <> nil then
    IDEMessagesWindow.AddCustomMessage(ATheUrgency, Msg, AFileName, 0, 0, FastPlaz);
end;

procedure Register;
begin
  CreateIDEMenus;

  RegisterNewItemCategory(TNewIDEItemCategory.Create(FastPlaz));
  RegisterProjectDescriptor(TProjectFastPlazDescriptor.Create, FastPlaz);
  RegisterProjectDescriptor(TProjectAPIFastPlazDescriptor.Create, FastPlaz);

  RegisterProjectFileDescriptor(TFileDescDefaultModule.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescJSONModule.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescModel.Create, FastPlaz);
end;

end.

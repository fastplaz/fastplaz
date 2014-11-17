unit fastplaz_tools_register;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf, PackageIntf,
  Classes, SysUtils;

const
  FastPlaz = 'FastPlaz';
  _APP_SLOGAN = 'Fast Pascal Framework for Web Development';

procedure Register;
procedure log(const Msg: string);
function ucwords(const str: string): string;

var
  bCreateProject: boolean = False;
  bExpert: boolean = False;
  FastPlazRuntimeDirectory : string;
  ModulTypeName, Permalink: string;
  ModelName, TableName: string;

implementation

uses modsimple_lib, modsimplejson_lib, model_lib, project_lib, webstructure_lib, menu_experts;

function ucwords(const str: string): string;
var
  i: integer;
  s: string;
begin
  s := ' ' + lowerCase(str);
  for i := 1 to Length(s) do
  begin
    if s[i] = ' ' then
      s[i + 1] := upcase(s[i + 1]);
  end;
  Result := trim(s);
end;

procedure log(const Msg: string);
begin
  IDEMessagesWindow.AddMsg( FastPlaz + ' : ' + Msg, '', 0, nil);
end;

procedure Register;
begin
  CreateIDEMenus;

  //RegisterUnit('fastplaz_tools_register', @fastplaz_tools_register.Register);
  RegisterNewItemCategory(TNewIDEItemCategory.Create(FastPlaz));
  RegisterProjectDescriptor(TFileDescProject.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescDefaultModule.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescJSONModule.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescModel.Create, FastPlaz);
  //RegisterProjectFileDescriptor(TWebStructure.Create, FastPlaz);
end;


initialization
  //RegisterPackage('FastPlaz', @Register);


end.


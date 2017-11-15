{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_tools;

interface

uses
  about_fastplaz, fastplaz_tools_register, menu_experts, model_lib, model_wzd, 
  modsimple_lib, modsimple_wzd, modsimplejson_lib, project_lib, project_wzd, 
  projectapi_lib, projectapi_wzd, projectgenerator_lib, themestructure_wzd, 
  webstructure_lib, webstructure_wzd, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fastplaz_tools_register', @fastplaz_tools_register.Register);
end;

initialization
  RegisterPackage('fastplaz_tools', @Register);
end.

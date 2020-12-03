{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_tools;

{$warn 5023 off : no warning about unused units}
interface

uses
  thread_custom, about_fastplaz, de_common, fastplaz_tools_register, 
  json_tools, menu_experts, model_lib, model_wzd, modsimple_lib, 
  modsimple_wzd, modsimplejson_lib, packageapp_lib, packageapp_wzd, 
  project_lib, project_wzd, projectapi_lib, projectapi_wzd, 
  projectgenerator_lib, regex_tester, themestructure_wzd, webstructure_lib, 
  webstructure_wzd, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fastplaz_tools_register', @fastplaz_tools_register.Register);
end;

initialization
  RegisterPackage('fastplaz_tools', @Register);
end.

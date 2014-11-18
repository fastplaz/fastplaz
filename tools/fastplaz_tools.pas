{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_tools;

interface

uses
  fastplaz_tools_register, modsimple_lib, modsimple_wzd, modsimplejson_lib, model_lib, model_wzd, menu_experts, 
  project_lib, project_wzd, about_fastplaz, webstructure_lib, webstructure_wzd, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fastplaz_tools_register', @fastplaz_tools_register.Register);
end;

initialization
  RegisterPackage('fastplaz_tools', @Register);
end.

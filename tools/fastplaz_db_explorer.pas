{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_db_explorer;

{$warn 5023 off : no warning about unused units}
interface

uses
  de_dbbrowser, de_connector, de_register, de_common, config_lib, 
  de_dbmemoedit, db_controller, model_lib, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('de_register', @de_register.Register);
end;

initialization
  RegisterPackage('fastplaz_db_explorer', @Register);
end.

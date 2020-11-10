{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_components;

{$warn 5023 off : no warning about unused units}
interface

uses
  remotedataset_component, http_lib, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('remotedataset_component', @remotedataset_component.Register);
end;

initialization
  RegisterPackage('fastplaz_components', @Register);
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_integration;

interface

uses
  whois_integration, kamusibacor_integration, telegram_integration, 
  kamuskemdikbud_integration, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fastplaz_integration', @Register);
end.

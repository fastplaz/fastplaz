{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_integration;

interface

uses
  clarifai_integration, currencyibacor_integration, 
  facebookmessenger_integration, googleplacesearch_integration, 
  kamusibacor_integration, kamuskemdikbud_integration, line_integration, 
  ombd_integration, resiibacor_integration, telegram_integration, 
  whois_integration, witai_integration, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fastplaz_integration', @Register);
end.

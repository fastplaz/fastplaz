{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_integration;

interface

uses
  clarifai_integration, cognitiveocr_integration, currencyibacor_integration, 
  facebookmessenger_integration, googleplacesearch_integration, 
  ibacorpolicenumber_integration, kamusibacor_integration, 
  kamuskemdikbud_integration, kawalpemilu_integration, line_integration, 
  ombd_integration, resiibacor_integration, telegram_integration, 
  whois_integration, witai_integration, yandextranslate_integration, 
  zomato_integration, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fastplaz_integration', @Register);
end.

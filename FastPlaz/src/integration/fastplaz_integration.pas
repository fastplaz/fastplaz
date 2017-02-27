{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_integration;

interface

uses
  alquranindonesia_integration, clarifai_integration, 
  cognitiveanalyze_integration, cognitivedomainspecific_integration, 
  cognitiveocr_integration, currencyibacor_integration, 
  facebookmessenger_integration, googleplacesearch_integration, 
  ibacorpolicenumber_integration, kamusibacor_integration, 
  kamuskemdikbud_integration, kamussunda_integration, kawalpemilu_integration, 
  line_integration, ombd_integration, resiibacor_integration, 
  telegram_integration, whois_integration, witai_integration, 
  yandextranslate_integration, zomato_integration, bmkg_integration, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fastplaz_integration', @Register);
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_integration;

interface

uses
  alquranindonesia_integration, apixu_integration, bca_integration, 
  bmkg_integration, botframework_integration, clarifai_integration, 
  cognitiveanalyze_integration, cognitivecustomvision_integration, 
  cognitivedomainspecific_integration, cognitiveocr_integration, 
  currencyibacor_integration, facebookmessenger_integration, 
  googledistancematrix_integration, googleplacesearch_integration, 
  ibacorpolicenumber_integration, ibacortrainschedule_integration, 
  jobplanet_integration, kamusibacor_integration, kamuskemdikbud_integration, 
  kamussunda_integration, kawalpemilu_integration, 
  kloudlesscalendar_integration, line_integration, 
  maskofajadwalshalat_integration, ombd_integration, 
  openweathermap_integration, portalpulsa_integration, rajaongkir_integration, 
  resiibacor_integration, telegram_integration, whois_integration, 
  witai_integration, yandextranslate_integration, zomato_integration, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fastplaz_integration', @Register);
end.

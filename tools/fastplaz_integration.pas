{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_integration;

{$warn 5023 off : no warning about unused units}
interface

uses
  alquranindonesia_integration, apixu_integration, bca_integration, 
  bmkg_integration, botframework_integration, clarifai_integration, 
  cognitiveanalyze_integration, cognitivecustomvision_integration, 
  cognitivedomainspecific_integration, cognitiveocr_integration, 
  elasticsearch_integration, facebookmessenger_integration, 
  firebase_integration, googleanalytics_integration, 
  googledistancematrix_integration, googlegeocoding_integration, 
  googleplacesearch_integration, graphql_integration, 
  kamuskemdikbud_integration, kamussunda_integration, 
  kloudlesscalendar_integration, line_integration, ombd_integration, 
  openweathermap_integration, portalpulsa_integration, rajaongkir_integration, 
  telegram_integration, thesaurus_integration, whois_integration, 
  witai_integration, yandextranslate_integration, zomato_integration, 
  openai_integration, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fastplaz_integration', @Register);
end.

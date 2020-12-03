{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_runtime;

{$warn 5023 off : no warning about unused units}
interface

uses
  about_controller, array_helpers, datetime_helpers, integer_helpers, 
  json_helpers, string_helpers, common, config_lib, database_lib, 
  datetime_lib, html_lib, http_lib, image_lib, json_lib, language_lib, 
  logutil_lib, mailer_lib, math_lib, nlp_lib, recaptcha_lib, rss_lib, 
  serialize_lib, stemmingnazief_lib, stemmingnaziefredis_lib, 
  verbal_expressions_lib, versioninfo_lib, modvar_model, session_model, 
  docs_controller, error_controller, exception_controller, fastplaz_handler, 
  info_controller, initialize_controller, module_controller, 
  session_controller, theme_controller, modvar_util, 
  fastplaz_runtime_register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fastplaz_runtime_register', @fastplaz_runtime_register.Register
    );
end;

initialization
  RegisterPackage('fastplaz_runtime', @Register);
end.

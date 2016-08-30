program fastplaz;

{$mode objfpc}{$H+}
{$include define_fastplaz.inc}

uses
  SysUtils, fpcgi, common, fastplaz_handler, database_lib, gettext,
  theme_controller, example_controller, about_controller, logutil_lib, html_lib,
  language_lib, versioninfo_lib, routes, mainapp, initialize_controller,
  error_controller, docs_controller, session_controller, info_controller,
  module_controller, config_lib, mailer_lib;

{$R *.res}

begin
  Application.Title := string( Config.GetValue(_SYSTEM_SITENAME, _APP));
  Application.Email := string( Config.GetValue(_SYSTEM_WEBMASTER_EMAIL,
    'webmaster@' + string( GetEnvironmentVariable('SERVER_NAME'))));
  Application.DefaultModuleName := string( Config.GetValue(_SYSTEM_MODULE_DEFAULT, 'main'));
  Application.ModuleVariable := string( Config.GetValue(_SYSTEM_MODULE_VARIABLE, 'mod'));
  Application.AllowDefaultModule := True;
  Application.RedirectOnErrorURL := string( Config.GetValue(_SYSTEM_ERROR_URL, '/'));
  Application.RedirectOnError:= Config.GetValue( _SYSTEM_ERROR_REDIRECT, false);

  Application.OnGetModule := @FastPlasAppandler.OnGetModule;


  {$ifdef short_url}
  Application.PreferModuleName := True;
  {$else}
  Application.PreferModuleName := False;
  {$endif}

  Application.Initialize;
  Application.Run;
end.

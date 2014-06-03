program fastplaz;

{$mode objfpc}{$H+}
{$include define.inc}

uses
  SysUtils,
  fpcgi,
  main,
  common,
  fastplaz_handler,
  database_lib,
  gettext,
  theme_controller,
  example_controller,
  about_controller,
  logutil_lib,
  html_lib, language_lib, versioninfo_lib, routes, initialize_controller,
  error_controller, docs_controller, session_controller, info_controller,
module_controller;

{$R *.res}

begin
  Application.Title := Config.GetValue(_SYSTEM_SITENAME, _APP);
  Application.Email := Config.GetValue(_SYSTEM_WEBMASTER_EMAIL,
    'webmaster@' + GetEnvironmentVariable('SERVER_NAME'));
  Application.DefaultModuleName := Config.GetValue(_SYSTEM_MODULE_DEFAULT, 'main');
  Application.ModuleVariable := Config.GetValue(_SYSTEM_MODULE_VARIABLE, 'mod');
  Application.AllowDefaultModule := True;
  Application.RedirectOnErrorURL := Config.GetValue(_SYSTEM_ERROR_URL, '/');
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

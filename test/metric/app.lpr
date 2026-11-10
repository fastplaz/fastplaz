program app;

{$mode objfpc}{$H+}

uses
  {$IFNDEF Windows}cthreads, {$ENDIF}
  fastplaz_app, sysutils, fastplaz_handler, common, app_controller, routes;

begin
  Application.Title := string( Config.GetValue(_SYSTEM_SITENAME, _APP));
  Application.Email := string( Config.GetValue(_SYSTEM_WEBMASTER_EMAIL, 'webmas'
    +'ter@' + GetEnvironmentVariable('SERVER_NAME')));
  Application.DefaultModuleName := string( Config.GetValue(
    _SYSTEM_MODULE_DEFAULT, 'main'));
  Application.ModuleVariable := string( Config.GetValue(
    _SYSTEM_MODULE_VARIABLE, 'mod'));
  Application.AllowDefaultModule := True;
  Application.RedirectOnErrorURL := string( Config.GetValue(_SYSTEM_ERROR_URL, 
    '/'));
  Application.RedirectOnError:= Config.GetValue( _SYSTEM_ERROR_REDIRECT, false);

  Application.OnGetModule := @FastPlasAppandler.OnGetModule;
  Application.PreferModuleName := True;
  {$if FPC_FULLVERSION >= 30004}
  Application.LegacyRouting := True;
  {$endif}

  // startup banner, standalone mode only; see OnStandaloneBanner to change it
  ShowStandaloneBanner;

  Application.Initialize;
  Application.Run;
end.

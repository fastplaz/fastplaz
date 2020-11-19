program fastplaz;

{$mode objfpc}{$H+}

uses
  {$IFNDEF Windows}cthreads,{$ENDIF}
  fpcgi, sysutils, fastplaz_handler, common, app_controller, app_routes,
  example_controller, warehouse_model, database_controller, example_form_controller;

begin
  Application.Title:='Fastplaz';
  Application.Email := string( Config.GetValue(_SYSTEM_WEBMASTER_EMAIL,UTF8Decode('webmaster@' + GetEnvironmentVariable('SERVER_NAME'))));
  Application.DefaultModuleName := string( Config.GetValue(_SYSTEM_MODULE_DEFAULT, 'main'));
  Application.ModuleVariable := string( Config.GetValue(_SYSTEM_MODULE_VARIABLE, 'mod'));
  Application.AllowDefaultModule := True;
  Application.RedirectOnErrorURL := string( Config.GetValue(_SYSTEM_ERROR_URL, '/'));
  Application.RedirectOnError:= Config.GetValue( _SYSTEM_ERROR_REDIRECT, false);

  Application.OnGetModule := @FastPlasAppandler.OnGetModule;
  Application.PreferModuleName := True;
  //{$if (fpc_version=3) and (fpc_release>=0) and (fpc_patch>=4)}
  {$if FPC_FULlVERSION >= 30004}
  Application.LegacyRouting := True;
  {$endif}

  Application.Initialize;
  Application.Run;
end.

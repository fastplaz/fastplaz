program standalone;

{
  FastPlaz standalone example.

  Run public_html/standalone.bin and open http://127.0.0.1:8080
  The same binary runs as CGI when started by a web server.
  See ../README.md
}

{$mode objfpc}{$H+}

uses
  {$IFNDEF Windows}cthreads,{$ENDIF}
  fastplaz_app, sysutils, fastplaz_handler, common,
  main_controller, serverinfo_controller, routes;

begin
  Application.Title := string(Config.GetValue(_SYSTEM_SITENAME, _APP));
  Application.Email := string(Config.GetValue(_SYSTEM_WEBMASTER_EMAIL, 'webmaster@localhost'));
  Application.DefaultModuleName := string(Config.GetValue(_SYSTEM_MODULE_DEFAULT, 'main'));
  Application.ModuleVariable := string(Config.GetValue(_SYSTEM_MODULE_VARIABLE, 'mod'));
  Application.AllowDefaultModule := True;
  Application.RedirectOnErrorURL := string(Config.GetValue(_SYSTEM_ERROR_URL, '/'));
  Application.RedirectOnError := Config.GetValue(_SYSTEM_ERROR_REDIRECT, False);

  Application.OnGetModule := @FastPlasAppandler.OnGetModule;
  Application.PreferModuleName := True;
  {$if FPC_FULLVERSION >= 30004}
  Application.LegacyRouting := True;
  {$endif}

  if IsStandalone then
    WriteLn(Format('FastPlaz standalone server: http://%s:%d/  (Ctrl+C to stop)',
      [HTTPAddress, HTTPPort]));

  Application.Initialize;
  Application.Run;
end.

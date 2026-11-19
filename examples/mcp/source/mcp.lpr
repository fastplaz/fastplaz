program mcp;

{
  FastPlaz MCP server example.

  Standalone: run public_html/mcp.bin, the MCP endpoint is
  http://127.0.0.1:17846/mcp
  CGI: the same binary behind Apache, http://yourhost/mcp
  See ../README.md
}

{$mode objfpc}{$H+}

uses
  {$IFNDEF Windows}cthreads,{$ENDIF}
  fastplaz_app, sysutils, fastplaz_handler, common,
  example_tools, routes;

begin
  Application.Title := string(Config.GetValue(_SYSTEM_SITENAME, _APP));
  Application.Email := string(Config.GetValue(_SYSTEM_WEBMASTER_EMAIL, 'webmaster@localhost'));
  Application.DefaultModuleName := string(Config.GetValue(_SYSTEM_MODULE_DEFAULT, 'mcp'));
  Application.ModuleVariable := string(Config.GetValue(_SYSTEM_MODULE_VARIABLE, 'mod'));
  Application.AllowDefaultModule := True;
  Application.RedirectOnErrorURL := string(Config.GetValue(_SYSTEM_ERROR_URL, '/'));
  Application.RedirectOnError := Config.GetValue(_SYSTEM_ERROR_REDIRECT, False);

  Application.OnGetModule := @FastPlasAppandler.OnGetModule;
  Application.PreferModuleName := True;
  {$if FPC_FULLVERSION >= 30004}
  Application.LegacyRouting := True;
  {$endif}

  ShowStandaloneBanner;

  Application.Initialize;
  Application.Run;
end.

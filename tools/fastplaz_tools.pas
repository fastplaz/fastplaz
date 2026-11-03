{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fastplaz_tools;

{$warn 5023 off : no warning about unused units}
interface

uses
  thread_custom, about_fastplaz, db_controller, de_common, 
  de_connection_editor, de_connector, de_dbbrowser, fastplaz_tools_register, 
  json_tools, menu_experts, model_lib, model_wzd, modsimple_lib, 
  modsimple_wzd, modsimplejson_lib, packageapp_lib, packageapp_wzd, 
  project_lib, project_wzd, projectapi_lib, projectapi_wzd, regex_tester, 
  themestructure_wzd, webstructure_lib, webstructure_wzd, 
  fastcode_configuration, de_dbmemoedit, config_lib, regexpr_lib, 
  regexpr_unicodedata_lib, regex_tester_lib, openai_integration, tools_lib, 
  aiplane_types, aiplane_process, aiplane_acp, aiplane_claude, 
  aiplane_presets, aiplane_chatview, aiplane_chatframe, aiplane_settingsframe, 
  aiplane_ide, aiplane_markdown, aiplane_codehighlight, aiplane_pi, 
  aiplane_httpserver, aiplane_mcpserver, aiplane_tools_ide, aiplane_tools_fastplaz,
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fastplaz_tools_register', @fastplaz_tools_register.Register);
end;

initialization
  RegisterPackage('fastplaz_tools', @Register);
end.

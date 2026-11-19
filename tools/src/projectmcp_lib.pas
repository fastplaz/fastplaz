unit projectmcp_lib;

{
  "File | New | FastPlaz | MCP Server Application": a FastPlaz program whose
  endpoint /mcp is an MCP (Model Context Protocol) server. Same wizard and
  web structure as the API application, plus the "mcp" section in
  config/config.json and the .htaccess rule that passes the bearer token to
  the CGI program.
}

{$mode objfpc}{$H+}

interface

uses
  LazFileUtils, Forms, Controls, FileUtil, Dialogs,
  ProjectIntf, PackageIntf, LazIDEIntf,
  tools_lib, Classes, SysUtils;

resourcestring
  rs_ProjectMCP_Name = 'MCP Server Application';
  rs_ProjectMCP_Description = 'create an MCP (Model Context Protocol) server based on FastPlaz';

type

  { TProjectMcpFastPlazDescriptor }

  TProjectMcpFastPlazDescriptor = class(TProjectDescriptor)
  private
    FHeader: TStringList;
    FProjectName: string;
    FTitle: string;
    FTargetExecutable: string;
    FCreateStructure: boolean;
  protected
    // the wizard runs here: the IDE only aborts cleanly before InitProject
    function DoInitDescriptor: TModalResult; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

implementation

uses fastplaz_tools_register, project_lib, projectapi_wzd, webstructure_lib,
  mcp_lib, mcpcontroller_lib, IDEExternToolIntf;

const
  DEFAULT_PROJECT_NAME = 'fastplazmcp';

{ TProjectMcpFastPlazDescriptor }

constructor TProjectMcpFastPlazDescriptor.Create;
begin
  inherited Create;
  Name := 'FFastPlazMCPApplication';
  FHeader := TStringList.Create;
end;

destructor TProjectMcpFastPlazDescriptor.Destroy;
begin
  FHeader.Free;
  inherited Destroy;
end;

function TProjectMcpFastPlazDescriptor.GetLocalizedName: string;
begin
  Result := rs_ProjectMCP_Name;
end;

function TProjectMcpFastPlazDescriptor.GetLocalizedDescription: string;
begin
  Result := rs_ProjectMCP_Description;
end;

function TProjectMcpFastPlazDescriptor.DoInitDescriptor: TModalResult;
var
  wizard: TfProjectAPIWizard;
begin
  FProjectName := DEFAULT_PROJECT_NAME;
  FTargetExecutable := '.' + DirectorySeparator + FProjectName + _APP_EXTENSION;

  FHeader.Clear;
  wizard := TfProjectAPIWizard.Create(nil);
  try
    wizard.Caption := 'Project MCP Server Wizard';
    wizard.lbl_Title.Caption := 'FastPlaz MCP Server Project Wizard';
    wizard.edt_ProjectName.Text := DEFAULT_PROJECT_NAME;
    wizard.cbx_GenerateStructure.Caption := 'Generate Web Directory/Files Structure';
    wizard.edt_WebRootDir.Text := GetUserDir;
    if wizard.ShowModal <> mrOk then
      Exit(mrCancel);
    FTitle := UcWords(wizard.edt_ProjectName.Text);
    FProjectName := SanitizeProjectName(wizard.edt_ProjectName.Text, DEFAULT_PROJECT_NAME);
    if (wizard.edt_WebRootDir.Text <> '') and (wizard.edt_WebRootDir.Text <> GetUserDir) then
      FTargetExecutable := IncludeTrailingPathDelimiter(wizard.edt_WebRootDir.Text) +
        FProjectName + _APP_EXTENSION;
    FHeader.Assign(wizard.mem_Header.Lines);
    FCreateStructure := wizard.cbx_GenerateStructure.Checked;
  finally
    wizard.Free;
  end;
  Result := mrOk;
end;

function TProjectMcpFastPlazDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  structure: TWebStructure;
  webRoot: string;
begin
  Result := inherited InitProject(AProject);
  if Result <> mrOk then
    Exit;
  SetupFastPlazProject(AProject, FProjectName, FTitle, FTargetExecutable);

  webRoot := ExtractFilePath(FTargetExecutable);
  if FCreateStructure then
  begin
    structure := TWebStructure.Create;
    try
      if not structure.GenerateStructure(webRoot, FProjectName + _APP_EXTENSION,
        FHeader, API_TEMPLATE_FOLDER) then
        log(structure.ErrorMessage, '', mluError);
    finally
      structure.Free;
    end;
    if FilenameIsAbsolute(webRoot) then
    begin
      if not SetConfigMcpSection(IncludeTrailingPathDelimiter(webRoot) +
        'config' + DirectorySeparator + 'config.json') then
        log('Cannot add the "mcp" section to config.json', '', mluWarning);
      if not SetHtaccessMcpAuth(IncludeTrailingPathDelimiter(webRoot) + '.htaccess') then
        log('Cannot add the Authorization rule to .htaccess', '', mluWarning);
    end;
  end;
  ApplyServerConfig(webRoot);

  Result := mrOk;
end;

function TProjectMcpFastPlazDescriptor.CreateStartFiles(AProject:
  TLazProject): TModalResult;
var
  controllerUnit: string;
  routes: TFileDescMcpRoutes;
  controller: TFileDescMcpModule;
begin
  controllerUnit := LowerCase(FProjectName) + '_controller';
  ModulTypeName := McpControllerTypeName(FProjectName);
  Permalink := MCP_DEFAULT_PERMALINK;
  McpProjectServerName := FProjectName;
  bCreateProject := True;
  bExpert := False;
  routes := TFileDescMcpRoutes.Create;
  controller := TFileDescMcpModule.Create;
  try
    routes.ControllerUnit := controllerUnit;
    // the IDE does not take ownership of the descriptors
    LazarusIDE.DoNewEditorFile(routes, 'routes.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
    LazarusIDE.DoNewEditorFile(controller, controllerUnit + '.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  finally
    bCreateProject := False;
    McpProjectServerName := '';
    routes.Free;
    controller.Free;
  end;

  OpenRuntimeReadme('README-new mcp project.txt');
  log('MCP endpoint: POST /' + Permalink + ' (standalone: http://' +
    APP_HTTP_ADDRESS_DEFAULT + ':' + IntToStr(APP_HTTP_PORT_DEFAULT) + '/' +
    Permalink + ')', '', mluProgress);
  Result := mrOk;
end;

end.

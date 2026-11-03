unit projectapi_lib;

{$mode objfpc}{$H+}

interface

uses
  LazFileUtils, Forms, Controls, FileUtil, Dialogs,
  ProjectIntf, PackageIntf, LazIDEIntf,
  Classes, SysUtils;

resourcestring
  rs_ProjectAPI_Name = 'API Application';
  rs_ProjectAPI_Description = 'create API application based on FastPlaz';

type

  { TProjectAPIFastPlazDescriptor }

  TProjectAPIFastPlazDescriptor = class(TProjectDescriptor)
  private
    Header: TStringList;
    ProjectName: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

implementation

uses fastplaz_tools_register, tools_lib, modsimple_lib, project_lib,
  projectapi_wzd, webstructure_lib, IDEExternToolIntf;

{ TProjectAPIFastPlazDescriptor }

constructor TProjectAPIFastPlazDescriptor.Create;
begin
  inherited Create;
  Name := 'FFastPlazAPIApplication';
  Header := TStringList.Create;
end;

destructor TProjectAPIFastPlazDescriptor.Destroy;
begin
  Header.Free;
  inherited Destroy;
end;

function TProjectAPIFastPlazDescriptor.GetLocalizedName: string;
begin
  Result := rs_ProjectAPI_Name;
end;

function TProjectAPIFastPlazDescriptor.GetLocalizedDescription: string;
begin
  Result := rs_ProjectAPI_Description;
end;

function TProjectAPIFastPlazDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  wizard: TfProjectAPIWizard;
  structure: TWebStructure;
  projectTitle, targetExecutable: string;
  isCreateStructure: boolean;
  runMode: TAppRunMode;
begin
  ProjectName := 'fastplazapi';
  targetExecutable := '.' + DirectorySeparator + ProjectName + _APP_EXTENSION;

  Header.Clear;
  wizard := TfProjectAPIWizard.Create(nil);
  try
    wizard.edt_WebRootDir.Text := GetUserDir;
    if wizard.ShowModal <> mrOk then
      Exit(mrCancel);
    projectTitle := UcWords(wizard.edt_ProjectName.Text);
    ProjectName := SanitizeProjectName(wizard.edt_ProjectName.Text, 'fastplazapi');
    if (wizard.edt_WebRootDir.Text <> '') and (wizard.edt_WebRootDir.Text <> GetUserDir) then
      targetExecutable := IncludeTrailingPathDelimiter(wizard.edt_WebRootDir.Text) +
        ProjectName + _APP_EXTENSION;
    Header.Assign(wizard.mem_Header.Lines);
    isCreateStructure := wizard.cbx_GenerateStructure.Checked;
    runMode := wizard.RunMode;
  finally
    wizard.Free;
  end;

  Result := inherited InitProject(AProject);
  if Result <> mrOk then
    Exit;
  SetupFastPlazProject(AProject, ProjectName, projectTitle, targetExecutable);

  // web dir structure
  if isCreateStructure then
  begin
    structure := TWebStructure.Create;
    try
      if not structure.GenerateStructure(ExtractFilePath(targetExecutable),
        ProjectName + _APP_EXTENSION, Header, API_TEMPLATE_FOLDER) then
        log(structure.ErrorMessage, '', mluError);
    finally
      structure.Free;
    end;
  end;
  ApplyRunMode(ExtractFilePath(targetExecutable), runMode);

  Result := mrOk;
end;

function TProjectAPIFastPlazDescriptor.CreateStartFiles(AProject:
  TLazProject): TModalResult;
begin
  bCreateProject := True;
  bExpert := False;
  _GlobalProjectName := ProjectName;
  try
    LazarusIDE.DoNewEditorFile(TFileRouteDescModule.Create, 'routes.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
    LazarusIDE.DoNewEditorFile(TFileDescDefaultModule.Create(True),
      LowerCase(ProjectName) + '_controller.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  finally
    bCreateProject := False;
  end;

  OpenRuntimeReadme('README-new api project.txt');
  Result := mrOk;
end;

end.

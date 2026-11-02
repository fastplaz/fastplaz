unit project_lib;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, Controls,
  LazIDEIntf, LazarusPackageIntf, ProjectIntf, PackageIntf,
  Classes, SysUtils;

resourcestring
  rs_Project_Name = 'Simple Application';
  rs_Project_Description = 'create web application based on FastPlaz';

type

  { TProjectFastPlazDescriptor }

  TProjectFastPlazDescriptor = class(TProjectDescriptor)
  private
    ProjectName: string;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

// Source of the main program of a FastPlaz web application.
function FastPlazProgramSource(const AProjectName: string): string;

// Creates the main file and sets the compiler options shared by the
// project wizards.
procedure SetupFastPlazProject(AProject: TLazProject;
  const AProjectName, ATitle, ATargetExecutable: string);

// Opens the readme that ships with the runtime package, when it exists.
procedure OpenRuntimeReadme(const AReadmeName: string);

implementation

uses fastplaz_tools_register, tools_lib, modsimple_lib, project_wzd, webstructure_lib,
  IDEExternToolIntf;

function FastPlazProgramSource(const AProjectName: string): string;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    with Source do
    begin
      Add('program ' + AProjectName + ';');
      Add('');
      Add('{$mode objfpc}{$H+}');
      Add('');
      Add('uses');
      Add('  {$IFNDEF Windows}cthreads,{$ENDIF}');
      // fastplaz_app: the same binary runs as CGI under a web server and as
      // a standalone HTTP server (http://127.0.0.1:8080) when started directly
      Add('  fastplaz_app, sysutils, fastplaz_handler, common, ' + LowerCase(AProjectName) + '_controller;');
      Add('');
      Add('{$R *.res}');
      Add('');
      Add('begin');
      Add('  Application.Title := string( Config.GetValue(_SYSTEM_SITENAME, _APP));');
      Add('  Application.Email := string( Config.GetValue(_SYSTEM_WEBMASTER_EMAIL, ''webmaster@'' + GetEnvironmentVariable(''SERVER_NAME'')));');
      Add('  Application.DefaultModuleName := string( Config.GetValue(_SYSTEM_MODULE_DEFAULT, ''main''));');
      Add('  Application.ModuleVariable := string( Config.GetValue(_SYSTEM_MODULE_VARIABLE, ''mod''));');
      Add('  Application.AllowDefaultModule := True;');
      Add('  Application.RedirectOnErrorURL := string( Config.GetValue(_SYSTEM_ERROR_URL, ''/''));');
      Add('  Application.RedirectOnError:= Config.GetValue( _SYSTEM_ERROR_REDIRECT, false);');
      Add('');
      Add('  Application.OnGetModule := @FastPlasAppandler.OnGetModule;');
      Add('  Application.PreferModuleName := True;');
      Add('  {$if FPC_FULLVERSION >= 30004}');
      Add('  Application.LegacyRouting := True;');
      Add('  {$endif}');
      Add('');
      Add('  if IsStandalone then');
      Add('    WriteLn(Format(''FastPlaz standalone server: http://%s:%d/  (Ctrl+C to stop)'',');
      Add('      [HTTPAddress, HTTPPort]));');
      Add('');
      Add('  Application.Initialize;');
      Add('  Application.Run;');
      Add('end.');
    end;
    Result := Source.Text;
  finally
    Source.Free;
  end;
end;

procedure SetupFastPlazProject(AProject: TLazProject;
  const AProjectName, ATitle, ATargetExecutable: string);
var
  MainFile: TLazProjectFile;
begin
  MainFile := AProject.CreateProjectFile(AProjectName + '.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;
  {$ifdef windows}
  AProject.MainFile.SetSourceText(FastPlazProgramSource(AProjectName), True);
  {$else}
  AProject.MainFile.SetSourceText(FastPlazProgramSource(AProjectName));
  {$endif}

  // package
  AProject.AddPackageDependency(FASTPLAZ_RUNTIME_PACKAGE);

  // compiler options
  AProject.Title := ATitle;
  AProject.LazCompilerOptions.UnitOutputDirectory :=
    'lib' + DirectorySeparator + '$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.Win32GraphicApp := False;
  AProject.LazCompilerOptions.TargetFilenameApplyConventions := False;
  AProject.LazCompilerOptions.TargetFilename := ATargetExecutable;
  AProject.LazCompilerOptions.ConfigFilePath := 'extra.cfg';
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
end;

procedure OpenRuntimeReadme(const AReadmeName: string);
var
  filename: string;
begin
  FastPlazRuntimeDirectory := PackageDirectory(FASTPLAZ_RUNTIME_PACKAGE);
  if FastPlazRuntimeDirectory = '' then
    Exit;
  filename := FastPlazRuntimeDirectory + '..' + DirectorySeparator +
    'docs' + DirectorySeparator + AReadmeName;
  if FileExists(filename) then
    LazarusIDE.DoOpenEditorFile(filename, -1, -1, [ofAddToRecent]);
end;

{ TProjectFastPlazDescriptor }

constructor TProjectFastPlazDescriptor.Create;
begin
  inherited Create;
  Name := 'FFastPlazApplication';
end;

function TProjectFastPlazDescriptor.GetLocalizedName: string;
begin
  Result := rs_Project_Name;
end;

function TProjectFastPlazDescriptor.GetLocalizedDescription: string;
begin
  Result := rs_Project_Description;
end;

function TProjectFastPlazDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  wizard: TfProjectWizard;
  structure: TWebStructure;
  projectTitle, targetExecutable: string;
  isCreateStructure: boolean;
begin
  ProjectName := DEFAULT_PROJECT_NAME;
  targetExecutable := '.' + DirectorySeparator + ProjectName + _APP_EXTENSION;

  wizard := TfProjectWizard.Create(nil);
  try
    wizard.edt_WebRootDir.Text := GetUserDir;
    if wizard.ShowModal <> mrOk then
      Exit(mrCancel);
    projectTitle := UcWords(wizard.edt_ProjectName.Text);
    ProjectName := SanitizeProjectName(wizard.edt_ProjectName.Text);
    if (wizard.edt_WebRootDir.Text <> '') and (wizard.edt_WebRootDir.Text <> GetUserDir) then
      targetExecutable := IncludeTrailingPathDelimiter(wizard.edt_WebRootDir.Text) +
        ProjectName + _APP_EXTENSION;
    isCreateStructure := wizard.cbx_GenerateStructure.Checked;
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
        ProjectName + _APP_EXTENSION) then
        log(structure.ErrorMessage, '', mluError);
    finally
      structure.Free;
    end;
  end;

  Result := mrOk;
end;

function TProjectFastPlazDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  bCreateProject := True;
  bExpert := False;
  _GlobalProjectName := ProjectName;
  try
    LazarusIDE.DoNewEditorFile(TFileRouteDescModule.Create, 'routes.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
    LazarusIDE.DoNewEditorFile(TFileDescDefaultModule.Create,
      LowerCase(ProjectName) + '_controller.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  finally
    bCreateProject := False;
  end;

  OpenRuntimeReadme('README-new project.txt');
  Result := mrOk;
end;


end.

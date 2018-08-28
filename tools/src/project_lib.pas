unit project_lib;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, Controls, LazIDEIntf, LazarusPackageIntf, ProjectIntf,
  NewItemIntf, IDEMsgIntf, PackageIntf,
  Classes, SysUtils;

resourcestring
  rs_Project_Name = 'Create New FastPlaz Application';
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

implementation

uses fastplaz_tools_register, modsimple_lib, project_wzd, webstructure_lib;

{ TProjectFastPlazDescriptor }

constructor TProjectFastPlazDescriptor.Create;
begin
  inherited Create;
  Name := 'FFastPlazApplication';
end;

function TProjectFastPlazDescriptor.GetLocalizedName: string;
begin
  //Result:=inherited GetLocalizedName;
  Result := rs_Project_Name;
end;

function TProjectFastPlazDescriptor.GetLocalizedDescription: string;
begin
  //Result:=inherited GetLocalizedDescription;
  Result := rs_Project_Description;
end;

function TProjectFastPlazDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  Source: TStringList;
  MainFile: TLazProjectFile;
  projectTitle, targetExecutable: string;
  isCreateStructure: boolean;
begin
  ProjectName := 'fastplaz';
  targetExecutable := '.' + DirectorySeparator + ProjectName + _APP_EXTENSION;

  with TfProjectWizard.Create(nil) do
  begin
    edt_WebRootDir.Text := GetUserDir;
    if ShowModal <> mrOk then
    begin
      Result := mrNo;
      Free;
      Exit;
    end;
    projectTitle := ucwords(edt_ProjectName.Text);
    ProjectName := LowerCase(edt_ProjectName.Text);
    ProjectName := StringReplace(ProjectName, ' ', '', [rfReplaceAll]);
    ProjectName := StringReplace(ProjectName, '.', '', [rfReplaceAll]);
    if edt_WebRootDir.Text <> '' then
    begin
      if edt_WebRootDir.Text <> GetUserDir then
        targetExecutable := IncludeTrailingPathDelimiter(edt_WebRootDir.Text) +
          ProjectName + _APP_EXTENSION;
    end;
    isCreateStructure := cbx_GenerateStructure.Checked;
    Free;
  end;

  Result := inherited InitProject(AProject);
  MainFile := AProject.CreateProjectFile(ProjectName + '.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;

  // project source
  Source := TStringList.Create;
  with Source do
  begin
    Add('program ' + ProjectName + ';');
    Add('');
    Add('{$mode objfpc}{$H+}');
    Add('');
    Add('uses');
    Add('  {$IFNDEF Windows}cthreads,{$ENDIF}');
    Add('  fpcgi, sysutils, fastplaz_handler, common, main;');
    Add('');
    Add('{$R *.res}');
    Add('');
    Add('begin');
    Add('  Application.Title := string( Config.GetValue(_SYSTEM_SITENAME, _APP));');
    Add('  Application.Email := string( Config.GetValue(_SYSTEM_WEBMASTER_EMAIL,UTF8Decode(''webmaster@'' + GetEnvironmentVariable(''SERVER_NAME''))));');
    Add('  Application.DefaultModuleName := string( Config.GetValue(_SYSTEM_MODULE_DEFAULT, ''main''));');
    Add('  Application.ModuleVariable := string( Config.GetValue(_SYSTEM_MODULE_VARIABLE, ''mod''));');
    Add('  Application.AllowDefaultModule := True;');
    Add('  Application.RedirectOnErrorURL := string( Config.GetValue(_SYSTEM_ERROR_URL, ''/''));');
    Add('  Application.RedirectOnError:= Config.GetValue( _SYSTEM_ERROR_REDIRECT, false);');
    Add('');
    Add('  Application.OnGetModule := @FastPlasAppandler.OnGetModule;');
    Add('  Application.PreferModuleName := True;');
    Add('  {$if (fpc_version=3) and (fpc_release>=0) and (fpc_patch>=4)}');
    Add('  Application.LegacyRouting := True;');
    Add('  {$endif}');
    Add('');
    Add('  Application.Initialize;');
    Add('  Application.Run;');
    Add('end.');
  end;
  {$ifdef windows}
  AProject.MainFile.SetSourceText(Source.Text, True);
  {$else}
  AProject.MainFile.SetSourceText(Source.Text);
  {$endif}
  FreeAndNil(Source);

  // package
  AProject.AddPackageDependency('fastplaz_runtime');
  AProject.AddPackageDependency('LCL');

  // compiler options
  AProject.Title := projectTitle;
  AProject.LazCompilerOptions.UnitOutputDirectory :=
    'lib' + DirectorySeparator + '$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.Win32GraphicApp := False;
  AProject.LazCompilerOptions.TargetFilename := targetExecutable;
  AProject.LazCompilerOptions.TargetFilenameApplyConventions := False;
  //AProject.LazCompilerOptions.CustomConfigFile := True;
  AProject.LazCompilerOptions.ConfigFilePath := 'extra.cfg';
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];

  // web dir structure
  if isCreateStructure then
  begin
    with TWebStructure.Create do
    begin
      GenerateStructure(ExtractFilePath(targetExecutable), ProjectName + _APP_EXTENSION);
      Free;
    end;

  end;

  Result := mrOk;
end;

function TProjectFastPlazDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
var
  Pkg: TIDEPackage;
  filename: string;
begin
  //Result:=inherited CreateStartFiles(AProject);
  Pkg := PackageEditingInterface.FindPackageWithName('fastplaz_runtime');
  FastPlazRuntimeDirectory := Pkg.DirectoryExpanded;

  bCreateProject := True;
  bExpert := False;
  LazarusIDE.DoNewEditorFile(TFileRouteDescModule.Create, 'routes.pas', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  LazarusIDE.DoNewEditorFile(TFileDescDefaultModule.Create, 'main.pas', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);

  // open readme file
  filename := FastPlazRuntimeDirectory + '..' + DirectorySeparator +
    'docs' + DirectorySeparator + 'README-new project.txt';


  if FileExists(filename) then
  begin
    LazarusIDE.DoOpenEditorFile(filename, -1, -1, [ofAddToRecent]);
  end;

  bCreateProject := False;
  Result := mrOk;
end;


end.

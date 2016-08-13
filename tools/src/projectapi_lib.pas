unit projectapi_lib;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls,
  ProjectIntf, PackageIntf, LazIDEIntf,
  Classes, SysUtils;

resourcestring
  rs_ProjectAPI_Name = 'Create New API Application';
  rs_ProjectAPI_Description = 'create API application based on FastPlaz';

type

  { TProjectAPIFastPlazDescriptor }

  TProjectAPIFastPlazDescriptor = class(TProjectDescriptor)
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

uses fastplaz_tools_register, modsimple_lib, projectapi_wzd;

{ TProjectAPIFastPlazDescriptor }

constructor TProjectAPIFastPlazDescriptor.Create;
begin
  inherited Create;
  Name := 'FFastPlazAPIApplication';
end;

function TProjectAPIFastPlazDescriptor.GetLocalizedName: string;
begin
  //Result:=inherited GetLocalizedName;
  Result := rs_ProjectAPI_Name;
end;

function TProjectAPIFastPlazDescriptor.GetLocalizedDescription: string;
begin
  //Result:=inherited GetLocalizedDescription;
  Result := rs_ProjectAPI_Description;
end;

function TProjectAPIFastPlazDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  Source: TStringList;
  MainFile: TLazProjectFile;
  projectTitle, targetExecutable: string;
  isCreateStructure: boolean;
begin
  //Result := inherited InitProject(AProject);
  ProjectName := 'fastplazapi';
  targetExecutable := '.' + DirectorySeparator;

  with TfProjectAPIWizard.Create(nil) do
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
        targetExecutable := IncludeTrailingPathDelimiter(edt_WebRootDir.Text) + ProjectName + _APP_EXTENSION;
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
  //AProject.LazCompilerOptions.CustomConfigFile := True;
  AProject.LazCompilerOptions.ConfigFilePath := 'extra.cfg';
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];

  // web dir structure
  if isCreateStructure then
  begin

  end;

  Result := mrOk;
end;

function TProjectAPIFastPlazDescriptor.CreateStartFiles(AProject:
  TLazProject): TModalResult;
var
  Pkg: TIDEPackage;
  filename: string;
begin
  //Result := inherited CreateStartFiles(AProject);
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
    'docs' + DirectorySeparator + 'README-new api project.txt';


  if FileExists(filename) then
  begin
    LazarusIDE.DoOpenEditorFile(filename, -1, -1, [ofAddToRecent]);
  end;

  bCreateProject := False;
  Result := mrOk;
end;

end.



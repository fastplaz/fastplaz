unit projectapi_lib;

{$mode objfpc}{$H+}

interface

uses
  LazFileUtils,
  Forms, Controls, FileUtil, Dialogs,
  ProjectIntf, PackageIntf, LazIDEIntf,
  Classes, SysUtils;

resourcestring
  rs_ProjectAPI_Name = 'Create New API Application';
  rs_ProjectAPI_Description = 'create API application based on FastPlaz';

type

  { TProjectAPIFastPlazDescriptor }

  TProjectAPIFastPlazDescriptor = class(TProjectDescriptor)
  private
    Header: TStringList;
    ProjectName: string;
    function ScanDirAndCopy(SourceDirectory, TargetDirectory: string): boolean;
    function GenerateStructure(TargetDirectory: string;
      DefaultBinaryFile: string = ''): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

implementation

uses fastplaz_tools_register, modsimple_lib, projectapi_wzd;

const
  CSS_APISTRUCTURE_FAILED = 'Failed create directory structure';
  CSS_API_TEMPLATEFOLDER = 'api';

{ TProjectAPIFastPlazDescriptor }

function TProjectAPIFastPlazDescriptor.ScanDirAndCopy(SourceDirectory,
  TargetDirectory: string): boolean;
var
  Rec: TSearchRec;
  R: integer;
  lsFileSource, lsFileTarget, lsTgl, lsSubDir: string;
begin
  Result := True;
  try
    R := FindFirst(SourceDirectory, faAnyFile, Rec);
    while R = 0 do
    begin
      if ((Rec.Attr and faDirectory) <> 0) and (Rec.Name <> '.') and
        (Rec.Name <> '..') then
      begin
        lsSubDir := extractfilepath(SourceDirectory) + Rec.Name;
        ForceDirectories(TargetDirectory + DirectorySeparator + Rec.Name);
        ScanDirAndCopy(lsSubDir + DirectorySeparator + '*', TargetDirectory +
          DirectorySeparator + Rec.Name);
      end
      else
      begin
        if ((Rec.Name <> '.') and (Rec.Name <> '..')) then
        begin
          lsFileSource := extractfilepath(SourceDirectory) + Rec.Name;
          lsFileTarget := TargetDirectory + DirectorySeparator + Rec.Name;
          CopyFile(lsFileSource, lsFileTarget);
        end;
      end;
      R := FindNext(Rec);
    end;
  finally
    FindClose(Rec);
  end;
end;

function TProjectAPIFastPlazDescriptor.GenerateStructure(TargetDirectory: string;
  DefaultBinaryFile: string): boolean;
var
  Pkg: TIDEPackage;
  fastplaz_package_dir, s: string;
  i: integer;
  headerHtaccess : TStringList;
begin
  TargetDirectory := IncludeTrailingPathDelimiter(TargetDirectory);
  if not ForceDirectoriesUTF8(TargetDirectory) then
  begin
    ShowMessage(CSS_APISTRUCTURE_FAILED);
    exit;
  end;

  Pkg := PackageEditingInterface.FindPackageWithName('fastplaz_tools');
  fastplaz_package_dir := Pkg.DirectoryExpanded;
  ScanDirAndCopy(fastplaz_package_dir + DirectorySeparator +
    'templates' + DirectorySeparator + CSS_API_TEMPLATEFOLDER +
    DirectorySeparator + '*',
    TargetDirectory);

  // root .htaccess
  //s := LazarusIDE.ActiveProject.LazCompilerOptions.TargetFilename;
  s := ExtractFileName(TargetDirectory);
  if s = '' then
    s := 'your_binary_file';
  if DefaultBinaryFile <> '' then
    s := DefaultBinaryFile;
  with TStringList.Create do
  begin
    LoadFromFile(fastplaz_package_dir + 'templates' + DirectorySeparator +
      CSS_API_TEMPLATEFOLDER + DirectorySeparator + '.htaccess');
    Text := StringReplace(Text, 'your_binary_file', s, [rfReplaceAll]);

    headerHtaccess := TStringList.Create;
    headerHtaccess.Text:= '';
    for i := 0 to Header.Count-1 do
    begin
      s := 'SetEnvIf '+Header[i]+' "(.*)" '+Header[i]+'=$1';
      headerHtaccess.Add( s);
    end;
    Text := StringReplace(Text, '#environment', headerHtaccess.Text, [rfReplaceAll]);
    headerHtaccess.Free;
    try
      SaveToFile(TargetDirectory + DirectorySeparator + '.htaccess');
    except
    end;
    Free;
  end;

  Result := False;
end;

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
  targetExecutable := '.' + DirectorySeparator + ProjectName + _APP_EXTENSION;

  Header.Clear;
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
        targetExecutable := IncludeTrailingPathDelimiter(edt_WebRootDir.Text) +
          ProjectName + _APP_EXTENSION;
    end;
    Header.Text := mem_Header.Lines.Text;
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
    Add('  fpcgi, sysutils, fastplaz_handler, common, ' + LowerCase(ProjectName) + '_controller;');
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
  AProject.LazCompilerOptions.TargetFilenameApplyConventions := False;
  AProject.LazCompilerOptions.TargetFilename := targetExecutable;
  //AProject.LazCompilerOptions.CustomConfigFile := True;
  AProject.LazCompilerOptions.ConfigFilePath := 'extra.cfg';
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];

  // web dir structure
  if isCreateStructure then
  begin
    GenerateStructure(ExtractFilePath(targetExecutable), ProjectName + _APP_EXTENSION);
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
  _GlobalProjectName := ProjectName;
  LazarusIDE.DoNewEditorFile(TFileRouteDescModule.Create, 'routes.pas', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  LazarusIDE.DoNewEditorFile(TFileDescDefaultModule.Create( True),
    LowerCase(ProjectName) + '_controller.pas', '',
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

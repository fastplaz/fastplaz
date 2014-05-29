unit project_lib;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, Controls, LazIDEIntf, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf,
  Classes, SysUtils;

resourcestring
  rs_Project_Name = 'Create New FastPlaz Application';
  rs_Project_Description = 'create web application based on FastPlaz';

type

  { TFileDescProject }

  TFileDescProject = class(TProjectDescriptor)
  private
    ProjectName : string;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

implementation

uses fastplaz_tools_register, modsimple_lib;

{ TFileDescProject }

constructor TFileDescProject.Create;
begin
  inherited Create;
  Name:='FFastPlazApplication';
end;

function TFileDescProject.GetLocalizedName: string;
begin
  //Result:=inherited GetLocalizedName;
  Result:=rs_Project_Name;
end;

function TFileDescProject.GetLocalizedDescription: string;
begin
  //Result:=inherited GetLocalizedDescription;
  Result:=rs_Project_Description;
end;

function TFileDescProject.InitProject(AProject: TLazProject): TModalResult;
var
  source: TStringList;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);
  ProjectName := 'fastplaz';
  MainFile:=AProject.CreateProjectFile('myproject.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // project source
  source:= TStringList.Create;
  with source do
  begin
    Add('program '+ProjectName+';');
    Add('');
    Add('{$mode objfpc}{$H+}');
    Add('');
    Add('uses');
    Add('  fpcgi, sysutils, fastplaz_handler, common, main;');
    Add('');
    Add('{$R *.res}');
    Add('');
    Add('begin');
    Add('  Application.Title := Config.GetValue(_SYSTEM_SITENAME, _APP);');
    Add('  Application.Email := Config.GetValue(_SYSTEM_WEBMASTER_EMAIL,''webmaster@'' + GetEnvironmentVariable(''SERVER_NAME''));');
    Add('  Application.DefaultModuleName := Config.GetValue(_SYSTEM_MODULE_DEFAULT, ''main'');');
    Add('  Application.ModuleVariable := Config.GetValue(_SYSTEM_MODULE_VARIABLE, ''mod'');');
    Add('  Application.AllowDefaultModule := True;');
    Add('  Application.RedirectOnErrorURL := Config.GetValue(_SYSTEM_ERROR_URL, ''/'');');
    Add('  Application.RedirectOnError:= Config.GetValue( _SYSTEM_ERROR_REDIRECT, false);');
    Add('');
    Add('  Application.OnGetModule := @FastPlasAppandler.OnGetModule;');
    Add('  Application.PreferModuleName := True;');
    Add('');
    Add('  Application.Initialize;');
    Add('  Application.Run;');
    Add('end.');

  end;
  AProject.MainFile.SetSourceText( source.Text, true);
  FreeAndNil(source);

  // package
  AProject.AddPackageDependency('fastplaz_runtime');
  AProject.AddPackageDependency('LCL');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  Result:= mrOK;
end;

function TFileDescProject.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  //Result:=inherited CreateStartFiles(AProject);
  bCreateProject:=True;
  LazarusIDE.DoNewEditorFile(TFileRouteDescModel.Create,'routes.pas','',
    [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  LazarusIDE.DoNewEditorFile(TFileDescDefaultModule.Create,'main.pas','',
    [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  bCreateProject:=False;
  Result:= mrOK;
end;


end.


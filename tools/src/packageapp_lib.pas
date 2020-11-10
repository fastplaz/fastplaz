unit packageapp_lib;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, Controls, LazIDEIntf, LazarusPackageIntf, ProjectIntf,
  PackageIntf, LazFileUtils, Classes, SysUtils;

resourcestring
  rs_PackageApp_Name = 'Full Package Application';
  rs_PackageApp_Description = 'Create Full Package web application based on FastPlaz';

type

  { TPackageAppLib }

  TPackageAppLib = class
  private
    ProjectName: string;
  public
    constructor Create;
    function GenerateStructure(TargetDirectory: string; APackageName: string;
      AProjectName: string = 'fastplaz'): boolean;
  end;

  { TPackageAppDescriptor }

  TPackageAppDescriptor = class(TProjectDescriptor)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
  end;


implementation

uses fastplaz_tools_register, packageapp_wzd, menu_experts;

const
  CSS_PACKAGESTRUCTURE_FAILED = 'Failed create directory structure';

{ TPackageAppDescriptor }

constructor TPackageAppDescriptor.Create;
begin
  inherited Create;
  Name := 'FFastPlazPackageApplication';
end;

destructor TPackageAppDescriptor.Destroy;
begin
  inherited Destroy;
end;

function TPackageAppDescriptor.GetLocalizedName: string;
begin
  Result := rs_PackageApp_Name;
end;

function TPackageAppDescriptor.GetLocalizedDescription: string;
begin
  Result := rs_PackageApp_Description;
end;

function TPackageAppDescriptor.InitProject(AProject: TLazProject): TModalResult;
begin
  //Result := inherited InitProject(AProject);
  CreatePackage_Proc(nil);
  Result := mrCancel;
end;

{ TPackageAppLib }

function TPackageAppLib.GenerateStructure(TargetDirectory: string;
  APackageName: string; AProjectName: string): boolean;
var
  Pkg: TIDEPackage;
  fastplazPackageDir: string;
begin
  TargetDirectory := IncludeTrailingPathDelimiter(TargetDirectory);
  if not ForceDirectoriesUTF8(TargetDirectory) then
  begin
    ShowMessage(CSS_PACKAGESTRUCTURE_FAILED);
    exit;
  end;

  Pkg := PackageEditingInterface.FindPackageWithName('fastplaz_tools');
  fastplazPackageDir := Pkg.DirectoryExpanded;
  ScanDirAndCopy(fastplazPackageDir + DirectorySeparator + 'templates' +
    DirectorySeparator + 'packages' + DirectorySeparator + APackageName +
    DirectorySeparator + '*',
    TargetDirectory, AProjectName);
  DeleteFile(TargetDirectory + 'version.json');
end;

constructor TPackageAppLib.Create;
begin
end;


end.




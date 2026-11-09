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
  public
    // Copies templates/packages/<APackageName> into TargetDirectory,
    // %projectname% in file names becomes AProjectName.
    function GenerateStructure(TargetDirectory: string; APackageName: string;
      AProjectName: string = 'fastplaz'): boolean;
  end;

  { TPackageAppDescriptor }

  TPackageAppDescriptor = class(TProjectDescriptor)
  protected
    // runs the wizard, which opens the generated project itself, and always
    // aborts the "new project" of the IDE: returning mrCancel later, from
    // InitProject, would make the IDE create a default project
    function DoInitDescriptor: TModalResult; override;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;


implementation

uses fastplaz_tools_register, tools_lib, menu_experts;

const
  CSS_PACKAGESTRUCTURE_FAILED = 'Failed to create the directory structure';
  CSS_PACKAGE_NOT_FOUND = 'Package template not found: ';

{ TPackageAppDescriptor }

constructor TPackageAppDescriptor.Create;
begin
  inherited Create;
  Name := 'FFastPlazPackageApplication';
end;

function TPackageAppDescriptor.GetLocalizedName: string;
begin
  Result := rs_PackageApp_Name;
end;

function TPackageAppDescriptor.GetLocalizedDescription: string;
begin
  Result := rs_PackageApp_Description;
end;

function TPackageAppDescriptor.DoInitDescriptor: TModalResult;
begin
  CreatePackage_Proc(nil);
  Result := mrCancel;
end;

{ TPackageAppLib }

function TPackageAppLib.GenerateStructure(TargetDirectory: string;
  APackageName: string; AProjectName: string): boolean;
var
  templateDir: string;
begin
  Result := False;
  TargetDirectory := IncludeTrailingPathDelimiter(TargetDirectory);
  if not ForceDirectories(TargetDirectory) then
  begin
    ShowMessage(CSS_PACKAGESTRUCTURE_FAILED + LineEnding + TargetDirectory);
    Exit;
  end;

  templateDir := PackageDirectory(FASTPLAZ_TOOLS_PACKAGE);
  if templateDir <> '' then
    templateDir := templateDir + 'templates' + DirectorySeparator + 'packages' +
      DirectorySeparator + APackageName;
  if (templateDir = '') or (not DirectoryExists(templateDir)) then
  begin
    ShowMessage(CSS_PACKAGE_NOT_FOUND + templateDir);
    Exit;
  end;

  Result := CopyDirectory(templateDir, TargetDirectory, AProjectName);
  // the version file belongs to the template, not to the new application
  DeleteFile(TargetDirectory + 'version.json');
end;

end.

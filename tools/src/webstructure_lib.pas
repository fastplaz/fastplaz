unit webstructure_lib;

{$mode objfpc}{$H+}

interface

uses
  FileUtil, fpjson, jsonConf, jsonparser, jsonscanner,
  Dialogs, Controls, LazarusPackageIntf, ProjectIntf, NewItemIntf,
  IDEMsgIntf, LazIDEIntf, PackageIntf,
  Classes, SysUtils;

const
  CSS_WEBSTRUCTURE_FAILED = 'Failed create directory structure';

type

  { TWebStructure }

  TWebStructure = class
  private
    function ScanDirAndCopy(SourceDirectory, TargetDirectory: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GenerateStructure(TargetDirectory: string;
      DefaultBinaryFile: string = ''): boolean;
    function GenerateThemeStructure(const ThemeName, TargetDirectory: string): boolean;
  end;

implementation

uses fastplaz_tools_register;

{ TWebStructure }

constructor TWebStructure.Create;
begin
end;

destructor TWebStructure.Destroy;
begin
  inherited Destroy;
end;

function TWebStructure.GenerateStructure(TargetDirectory: string;
  DefaultBinaryFile: string): boolean;
var
  Pkg: TIDEPackage;
  fastplaz_package_dir, s: string;
begin
  TargetDirectory := IncludeTrailingPathDelimiter(TargetDirectory);
  if not ForceDirectoriesUTF8(TargetDirectory) then
  begin
    ShowMessage(CSS_WEBSTRUCTURE_FAILED);
    exit;
  end;

  Pkg := PackageEditingInterface.FindPackageWithName('fastplaz_tools');
  fastplaz_package_dir := Pkg.DirectoryExpanded;
  ScanDirAndCopy(fastplaz_package_dir + DirectorySeparator + 'templates' +
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
    LoadFromFile(fastplaz_package_dir + 'templates' + DirectorySeparator + '.htaccess');
    Text := StringReplace(Text, 'your_binary_file', s, [rfReplaceAll]);
    try
      SaveToFile(TargetDirectory + DirectorySeparator + '.htaccess');
    except
    end;
    Free;
  end;

  Result := False;
end;

function TWebStructure.GenerateThemeStructure(
  const ThemeName, TargetDirectory: string): boolean;
var
  Pkg: TIDEPackage;
  fastplaz_package_dir, dirTheme: string;
begin
  Result := False;
  dirTheme := TargetDirectory + DirectorySeparator + ThemeName;
  if not ForceDirectoriesUTF8(dirTheme) then
  begin
    ShowMessage(CSS_WEBSTRUCTURE_FAILED);
    exit;
  end;

  Pkg := PackageEditingInterface.FindPackageWithName('fastplaz_tools');
  fastplaz_package_dir := Pkg.DirectoryExpanded;
  ScanDirAndCopy(fastplaz_package_dir + DirectorySeparator +
    'templates/themes/default' + DirectorySeparator + '*',
    dirTheme);

end;

function TWebStructure.ScanDirAndCopy(SourceDirectory, TargetDirectory: string): boolean;
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

end.

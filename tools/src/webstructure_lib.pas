unit webstructure_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  WEB_TEMPLATE_FOLDER = 'web';
  API_TEMPLATE_FOLDER = 'api';

type

  { TWebStructure }

  TWebStructure = class
  private
    FErrorMessage: string;
    function TemplateDirectory(const AFolder: string): string;
  public
    // Copies templates/<ATemplateFolder> into TargetDirectory and writes the
    // root .htaccess for DefaultBinaryFile (plus one SetEnvIf line per
    // header name in AHeaders). ErrorMessage tells why it returned False.
    function GenerateStructure(TargetDirectory: string;
      DefaultBinaryFile: string = ''; AHeaders: TStrings = nil;
      const ATemplateFolder: string = WEB_TEMPLATE_FOLDER): boolean;
    // Copies the default theme to TargetDirectory/ThemeName.
    function GenerateThemeStructure(const ThemeName, TargetDirectory: string): boolean;
    property ErrorMessage: string read FErrorMessage;
  end;

implementation

uses fastplaz_tools_register, tools_lib;

const
  CSS_WEBSTRUCTURE_FAILED = 'Failed to create the directory structure';
  CSS_TEMPLATE_NOT_FOUND = 'Template folder not found: ';
  HTACCESS_FILENAME = '.htaccess';

{ TWebStructure }

function TWebStructure.TemplateDirectory(const AFolder: string): string;
begin
  Result := PackageDirectory(FASTPLAZ_TOOLS_PACKAGE);
  if Result = '' then
    Exit;
  Result := Result + 'templates' + DirectorySeparator + AFolder;
end;

function TWebStructure.GenerateStructure(TargetDirectory: string;
  DefaultBinaryFile: string; AHeaders: TStrings;
  const ATemplateFolder: string): boolean;
var
  templateDir, binaryName: string;
  htaccess: TStringList;
begin
  Result := False;
  FErrorMessage := '';
  TargetDirectory := IncludeTrailingPathDelimiter(TargetDirectory);
  if not ForceDirectories(TargetDirectory) then
  begin
    FErrorMessage := CSS_WEBSTRUCTURE_FAILED + ': ' + TargetDirectory;
    Exit;
  end;

  templateDir := TemplateDirectory(ATemplateFolder);
  if (templateDir = '') or (not DirectoryExists(templateDir)) then
  begin
    FErrorMessage := CSS_TEMPLATE_NOT_FOUND + templateDir;
    Exit;
  end;

  if not CopyDirectory(templateDir, TargetDirectory) then
  begin
    FErrorMessage := 'Some template files could not be copied to ' + TargetDirectory;
    Exit;
  end;

  // root .htaccess
  binaryName := DefaultBinaryFile;
  if binaryName = '' then
    binaryName := ExtractFileName(ExcludeTrailingPathDelimiter(TargetDirectory));
  if binaryName = '' then
    binaryName := HTACCESS_BINARY_PLACEHOLDER;
  htaccess := TStringList.Create;
  try
    try
      htaccess.LoadFromFile(IncludeTrailingPathDelimiter(templateDir) + HTACCESS_FILENAME);
      htaccess.Text := BuildHtaccess(htaccess.Text, binaryName, AHeaders);
      htaccess.SaveToFile(TargetDirectory + HTACCESS_FILENAME);
    except
      on E: Exception do
      begin
        FErrorMessage := HTACCESS_FILENAME + ': ' + E.Message;
        Exit;
      end;
    end;
  finally
    htaccess.Free;
  end;

  log('Web structure created: ' + TargetDirectory);
  Result := True;
end;

function TWebStructure.GenerateThemeStructure(
  const ThemeName, TargetDirectory: string): boolean;
var
  templateDir, dirTheme: string;
begin
  Result := False;
  FErrorMessage := '';
  if Trim(ThemeName) = '' then
  begin
    FErrorMessage := 'Theme name is empty';
    Exit;
  end;
  dirTheme := IncludeTrailingPathDelimiter(TargetDirectory) + Trim(ThemeName);
  if not ForceDirectories(dirTheme) then
  begin
    FErrorMessage := CSS_WEBSTRUCTURE_FAILED + ': ' + dirTheme;
    Exit;
  end;

  // the theme template ships inside the web template
  templateDir := TemplateDirectory(WEB_TEMPLATE_FOLDER + DirectorySeparator +
    'themes' + DirectorySeparator + 'default');
  if (templateDir = '') or (not DirectoryExists(templateDir)) then
  begin
    FErrorMessage := CSS_TEMPLATE_NOT_FOUND + templateDir;
    Exit;
  end;

  if not CopyDirectory(templateDir, dirTheme) then
  begin
    FErrorMessage := 'Some template files could not be copied to ' + dirTheme;
    Exit;
  end;

  log('Theme structure created: ' + dirTheme);
  Result := True;
end;

end.

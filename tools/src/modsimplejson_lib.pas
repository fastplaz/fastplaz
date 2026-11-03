unit modsimplejson_lib;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, Controls, LazarusPackageIntf, ProjectIntf,
  Classes, SysUtils;

resourcestring
  rs_Mod_JSON_Name = 'Create API Controller (JSON)';
  rs_Mod_JSON_Description = 'Create API Controller (JSON)';

type

  { TFileDescJSONModule }

  TFileDescJSONModule = class(TFileDescPascalUnit)
  private
  public
    constructor Create; override;
    function Init(var NewFilename: string; NewOwner: TObject;
      var NewSource: string; Quiet: boolean): TModalResult; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const Filename, SourceName,
      ResourceName: string): string;
      override;
    function GetImplementationSource(const Filename, SourceName,
      ResourceName: string): string; override;
    function CreateSource(const Filename, SourceName, ResourceName: string): string;
      override;
  end;

implementation

uses fastplaz_tools_register, tools_lib, modsimple_lib;

{ TFileDescJSONModule }

constructor TFileDescJSONModule.Create;
begin
  inherited Create;
  DefaultFileExt := '.pas';
  VisibleInNewDialog := True;
end;

function TFileDescJSONModule.Init(var NewFilename: string; NewOwner: TObject;
  var NewSource: string; Quiet: boolean): TModalResult;
begin
  Result := inherited Init(NewFilename, NewOwner, NewSource, Quiet);
  if Result <> mrOk then
    Exit;
  if bExpert then
    Exit; // the menu already asked
  if bCreateProject or Quiet then
  begin
    ModulTypeName := 'TMainModule';
    Permalink := 'main';
    Exit;
  end;
  if not AskModuleName('TJsonModule', 'json') then
    Exit(mrCancel);
  if NewFilename = '' then
    NewFilename := SanitizeIdentifier(Permalink) + '_controller.pas';
end;

function TFileDescJSONModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', fastplaz_app, fpjson, json_lib, HTTPDefs, fastplaz_handler, database_lib, dateutils, string_helpers, datetime_helpers, json_helpers, array_helpers';
end;

function TFileDescJSONModule.GetLocalizedName: string;
begin
  Result := rs_Mod_JSON_Name;
end;

function TFileDescJSONModule.GetLocalizedDescription: string;
begin
  Result := rs_Mod_JSON_Description;
end;

function TFileDescJSONModule.GetInterfaceSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  with str do
  begin
    Add('type');
    Add('  ' + ModulTypeName + ' = class(TMyCustomController)');
    Add('  private');
    Add('  public');
    Add('    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;');
    Add('    destructor Destroy; override;');
    Add('');
    Add('    procedure Get; override;');
    Add('    procedure Post; override;');
    Add('    procedure Options; override;');
    Add('  end;');
    Add('');
  end;
  Result := str.Text;
  FreeAndNil(str);
end;

function TFileDescJSONModule.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  Result := inherited GetImplementationSource(FileName, SourceName, ResourceName);
  str := TStringList.Create;
  with str do
  begin
    Add('uses common;');
    Add('');

    Add('constructor ' + ModulTypeName +
      '.CreateNew(AOwner: TComponent; CreateMode: integer);');
    Add('Begin');
    Add('  inherited CreateNew(AOwner, CreateMode);');
    Add('End;');
    Add('');

    Add('destructor ' + ModulTypeName + '.Destroy;');
    Add('Begin');
    Add('  inherited Destroy;');
    Add('End;');
    Add('');

    Add('// GET Method Handler');
    Add('procedure ' + ModulTypeName + '.Get;');
    Add('var');
    Add('  json : TJSONUtil;');
    Add('Begin');
    Add('  json := TJSONUtil.Create;');
    Add('');
    Add('  json[''code''] := Int16(0);');
    Add('  json[''variable''] := ''value'';');
    Add('  json[''path01/path02/var01''] := ''value01'';');
    Add('  json[''path01/path02/var02''] := ''value02'';');
    Add('  json[''msg''] := ''Ok'';');
    Add('');
    Add('  //---');
    Add('  Response.ContentType := ''application/json'';');
    Add('  Response.Content := json.AsJSON;');
    Add('  json.Free;');
    Add('End;');
    Add('');

    Add('// POST Method Handler');
    Add('procedure ' + ModulTypeName + '.Post;');
    Add('Begin');
    Add('  Response.Content := '''';');
    Add('End;');
    Add('');

    Add('// OPTIONS Method Handler');
    Add('procedure ' + ModulTypeName + '.Options;');
    Add('Begin');
    Add('  Response.Code := 204;');
    Add('  Response.Content := '''';');
    Add('End;');
    Add('');

    Add('');
    Add('');
  end;
  Result := Result + str.Text;
  FreeAndNil(str);

  if not bCreateProject then
  begin
    Result := Result + LineEnding + 'initialization' + LineEnding +
      '  // -> http://yourdomainname/' + ResourceName + LineEnding +
      '  // The following line should be moved to a file "routes.pas"' +
      LineEnding + '  Route[ ''' + Permalink + '''] := ' + ModulTypeName +
      ';' + LineEnding + LineEnding;
  end;
end;

function TFileDescJSONModule.CreateSource(
  const Filename, SourceName, ResourceName: string): string;
begin
  Result := inherited CreateSource(Filename, SourceName, Permalink);
  log('module "' + ModulTypeName + '" created', Filename);
end;

end.

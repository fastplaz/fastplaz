unit modsimplejson_lib;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, Controls, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf,
  Classes, SysUtils;

resourcestring
  rs_Mod_JSON_Name = 'JSON module';
  rs_Mod_JSON_Description = 'create fastplaz simple json module';

type

  { TFileDescJSONModule }

  TFileDescJSONModule = class(TFileDescPascalUnit)
  private
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const Filename, SourceName,
      ResourceName: string): string;
      override;
    function GetImplementationSource(const Filename, SourceName,
      ResourceName: string): string; override;
    function GetResourceSource(const ResourceName: string): string; override;
    function CreateSource(const Filename, SourceName, ResourceName: string): string;
      override;
    procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); override;
  end;

implementation

uses fastplaz_tools_register, modsimple_wzd;

{ TFileDescJSONModule }

constructor TFileDescJSONModule.Create;
begin
  inherited Create;
  //Name:=rs_Mod_JSON_Name;
  DefaultFileExt := '.pas';
  VisibleInNewDialog := True;
end;

function TFileDescJSONModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', fpcgi, fpjson, HTTPDefs, fastplaz_handler, database_lib';
end;

function TFileDescJSONModule.GetLocalizedName: string;
begin
  Result := inherited GetLocalizedName;
  Result := rs_Mod_JSON_Name;
end;

function TFileDescJSONModule.GetLocalizedDescription: string;
begin
  Result := inherited GetLocalizedDescription;
  Result := rs_Mod_JSON_Description;
end;

function TFileDescJSONModule.GetInterfaceSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  //Result:=inherited GetInterfaceSource(Filename, SourceName, ResourceName);
  str := TStringList.Create;
  with str do
  begin
    Add('type');
    Add('  ' + ModulTypeName + ' = class(TMyCustomWebModule)');
    //Add('    procedure RequestHandler(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
    Add('  private');
    Add('  public');
    Add('    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;');
    Add('    destructor Destroy; override;');
    Add('');
    Add('    procedure Get; override;');
    Add('    procedure Post; override;');
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
    Add('uses common, json_lib;');
    Add('');

    Add('constructor ' + ModulTypeName +
      '.CreateNew(AOwner: TComponent; CreateMode: integer);');
    Add('Begin');
    Add('  inherited CreateNew(AOwner, CreateMode);');
    //Add('  OnRequest := @RequestHandler;');
    Add('End;');
    Add('');

    Add('destructor ' + ModulTypeName + '.Destroy;');
    Add('Begin');
    Add('  inherited Destroy;');
    Add('End;');
    Add('');

    //Add('procedure ' + ModulTypeName + '.RequestHandler(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
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


function TFileDescJSONModule.GetResourceSource(const ResourceName: string): string;
begin
  Result := inherited GetResourceSource(ResourceName);
end;

function TFileDescJSONModule.CreateSource(
  const Filename, SourceName, ResourceName: string): string;
begin
  if not bExpert then
  begin;
    Permalink := 'json';
    ModulTypeName := 'TJsonModule';
    if not bCreateProject then
    begin
      with TfModuleSimpleWizard.Create(nil) do
      begin
        if ShowModal = mrOk then
        begin
          if edt_ModuleName.Text <> '' then
            ModulTypeName := 'T' + StringReplace(UcWords(edt_ModuleName.Text),
              ' ', '', [rfReplaceAll]) + 'Module';
          Permalink := edt_Permalink.Text;
          if Permalink = '' then
          begin
            Permalink := StringReplace(UcWords(edt_ModuleName.Text),
              ' ', '', [rfReplaceAll]);
          end;
        end;
        Free;
      end;
    end
    else
    begin
      ModulTypeName := 'TMainModule';
      Permalink := 'main';
    end;

  end;
  Result := inherited CreateSource(Filename, SourceName, Permalink);
  log('module "' + ModulTypeName + '" created');
end;

procedure TFileDescJSONModule.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  inherited UpdateDefaultPascalFileExtension(DefPasExt);
end;


end.

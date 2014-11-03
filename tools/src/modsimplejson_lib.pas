unit modsimplejson_lib;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, Controls, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf,
  Classes, SysUtils;

resourcestring
  rs_Mod_JSON_Name = 'FastPlaz - simple json module';
  rs_Mod_JSON_Description = 'create fastplaz simple json module';

type

  { TFileDescJSONModule }

  TFileDescJSONModule = class(TFileDescPascalUnit)
  private
    ModulTypeName, Permalink: string;
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(
      const Filename, SourceName, ResourceName: string): string;
      override;
    function GetImplementationSource(
      const Filename, SourceName, ResourceName: string): string; override;
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
    Add('    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
    Add('  private');
    Add('  public');
    Add('    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;');
    Add('    destructor Destroy; override;');
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
    Add('procedure ' + ModulTypeName +
      '.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
    Add('var');
    Add('  o, response_json : TJSONObject;');
    Add('Begin');
    Add('  response_json := TJSONObject.Create;');
    Add('  o := TJSONObject.Create;');
    Add('');
    Add('  // example');
    Add('  o.Add( ''msg'', ''OK'');');
    Add('  o.Add( ''variable'', ''value'');');
    Add('');
    Add('  response_json.Add( ''code'', 0);');
    Add('  response_json.Add( ''response'', o);');
    Add('  // example - end');
    Add('');
    Add('  Response.Content := response_json.AsJSON;');
    Add('  FreeAndNil( response_json);');
    Add('  Handled := True;');
    Add('End;');
    Add('');

    Add('constructor ' + ModulTypeName +
      '.CreateNew(AOwner: TComponent; CreateMode: integer);');
    Add('Begin');
    Add('  inherited CreateNew(AOwner, CreateMode);');
    Add('  OnRequest := @DataModuleRequest;');
    Add('End;');
    Add('');

    Add('destructor ' + ModulTypeName + '.Destroy;');
    Add('Begin');
    Add('  inherited Destroy;');
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
      '  // The following line should be moved to a file "routes.pas"'
      + LineEnding + '  AddRoute(''' + Permalink + ''',' + ModulTypeName + ');' +
      LineEnding + LineEnding;
  end;
end;


function TFileDescJSONModule.GetResourceSource(const ResourceName: string): string;
begin
  Result := inherited GetResourceSource(ResourceName);
end;

function TFileDescJSONModule.CreateSource(
  const Filename, SourceName, ResourceName: string): string;
begin
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
  Result := inherited CreateSource(Filename, SourceName, Permalink);
  log('module "' + ModulTypeName + '" created');
end;

procedure TFileDescJSONModule.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  inherited UpdateDefaultPascalFileExtension(DefPasExt);
end;


end.

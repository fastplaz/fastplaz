unit model_lib;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, Controls, LazarusPackageIntf, ProjectIntf,
  Classes, SysUtils;

resourcestring
  rs_Model_Name = 'Database Model Generator';
  rs_Model_Description = 'create unit for model database';

type

  { TFileDescModel }

  TFileDescModel = class(TFileDescPascalUnit)
  private
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetUnitDirectives: string; virtual;
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

uses fastplaz_tools_register, model_wzd;

{ TFileDescModel }

constructor TFileDescModel.Create;
begin
  inherited Create;
  //DefaultSourceName:= 'modelname_model';
  DefaultFileExt := '.pas';
  VisibleInNewDialog := True;
  IsPascalUnit := True;
end;

function TFileDescModel.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', database_lib, string_helpers, dateutils, datetime_helpers';
end;

function TFileDescModel.GetLocalizedName: string;
begin
  Result := inherited GetLocalizedName;
  Result := rs_Model_Name;
end;

function TFileDescModel.GetLocalizedDescription: string;
begin
  Result := inherited GetLocalizedDescription;
  Result := rs_Model_Description;
end;

function TFileDescModel.GetUnitDirectives: string;
begin
  Result:='{$mode objfpc}{$H+}';
  if Owner is TLazProject then
    Result:=CompilerOptionsToUnitDirectives(TLazProject(Owner).LazCompilerOptions);
end;

function TFileDescModel.GetInterfaceSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  //Result:=inherited GetInterfaceSource(Filename, SourceName, ResourceName);
  str := TStringList.Create;
  with str do
  begin
    Add('type');
    Add('');
    Add('{ '+ModelName+' }');
    Add('');
    Add('  ' + ModelName + ' = class(TSimpleModel)');
    Add('  private');
    Add('  public');
    Add('    constructor Create(const DefaultTableName: string = '''');');
    Add('  end;');
    Add('');
  end;
  Result := str.Text;
  FreeAndNil(str);
end;

function TFileDescModel.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
  str := TStringList.Create;
  with str do
  begin
    Add('constructor ' + ModelName + '.Create(const DefaultTableName: string = '''');');
    Add('Begin');
    Add('  inherited Create( DefaultTableName); // table name = ' +
      LowerCase(TableName) + 's') ;
    Add('  //inherited Create(''yourtablename''); // if use custom tablename');
    Add('End;');
    Add('');
  end;
  Result := Result + str.Text;
  FreeAndNil(str);

end;


function TFileDescModel.GetResourceSource(const ResourceName: string): string;
begin
  Result := inherited GetResourceSource(ResourceName);
end;

function TFileDescModel.CreateSource(
  const Filename, SourceName, ResourceName: string): string;
begin
  if not bExpert then
  begin;
    ModelName := 'Sample';
    with TfModelWizard.Create(nil) do
    begin
      if ShowModal = mrOk then
      begin
        if edt_ModelName.Text <> '' then
          ModelName := edt_ModelName.Text;
      end;
      Free;
    end;
  end;
  DefaultFilename := LowerCase(ModelName) + '_model.pas';
  DefaultFileExt := '.pas';
  DefaultSourceName := LowerCase(ModelName) + '_model';

  TableName := StringReplace(LowerCase(ModelName), ' ', '_', [rfReplaceAll]);
  ModelName := 'T' + StringReplace(UcWords(ModelName), ' ', '', [rfReplaceAll]) + 'Model';
  Result := inherited CreateSource(LowerCase(ModelName) + '_model.pas',
    SourceName, ModelName);
  log('model "' + ModelName + '" created', DefaultFilename + DefaultFileExt);
end;

procedure TFileDescModel.UpdateDefaultPascalFileExtension(const DefPasExt: string);
begin
  inherited UpdateDefaultPascalFileExtension(DefPasExt);
end;



end.

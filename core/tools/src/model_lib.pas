unit model_lib;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, Controls, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf,
  Classes, SysUtils;

resourcestring
  rs_Model_Name = 'FastPlaz - Model Generator';
  rs_Model_Description = 'create unit for model database';

type

  { TFileDescModel }

  TFileDescModel = class(TFileDescPascalUnit)
  private
    ModelName,
    TableName: string;
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
    function GetResourceSource(const ResourceName: string): string; override;
    function CreateSource(const Filename, SourceName, ResourceName: string): string; override;
    procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); override;
  end;


implementation

uses fastplaz_tools_register, model_wzd;

{ TFileDescModel }

constructor TFileDescModel.Create;
begin
  inherited Create;
  DefaultFileExt:='.pas';
  VisibleInNewDialog:=true;
end;

function TFileDescModel.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', database_lib';
end;

function TFileDescModel.GetLocalizedName: string;
begin
  Result:=inherited GetLocalizedName;
  Result:=rs_Model_Name;
end;

function TFileDescModel.GetLocalizedDescription: string;
begin
  Result:=inherited GetLocalizedDescription;
  Result:=rs_Model_Description;
end;

function TFileDescModel.GetInterfaceSource(const Filename, SourceName,
  ResourceName: string): string;
var
  str : TStringList;
begin
  //Result:=inherited GetInterfaceSource(Filename, SourceName, ResourceName);
  str := TStringList.Create;
  with str do begin
    Add( 'type');
    Add( '  '+ModelName+' = class(TSimpleModel)');
    Add( '  private');
    Add( '  public');
    Add( '    constructor Create(const DefaultTableName: string = '''');');
    Add( '  end;');
    Add( '');
  end;
  Result := str.Text;
  FreeAndNil(str);
end;

function TFileDescModel.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
var
  str : TStringList;
begin
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  str := TStringList.Create;
  with str do begin
    Add('constructor '+ModelName+'.Create(const DefaultTableName: string = '''');');
    Add('Begin');
    Add('  inherited Create(); // table name = ' + LowerCase(TableName));
    Add('  //inherited Create(''yourtablename''); // if use custom tablename');
    Add('End;');
    Add('');
  end;
  Result := Result+ str.Text;
  FreeAndNil(str);

end;


function TFileDescModel.GetResourceSource(const ResourceName: string
  ): string;
begin
  Result:=inherited GetResourceSource(ResourceName);
end;

function TFileDescModel.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  ModelName:= 'Sample';
  with TfModelWizard.Create(nil) do
  begin
    if ShowModal = mrOK then
    begin
      if edt_ModelName.Text <> '' then
        ModelName:= edt_ModelName.Text;
    end;
    Free;
  end;
  TableName := StringReplace(LowerCase(ModelName), ' ', '_', [rfReplaceAll]);
  ModelName := 'T'+ StringReplace(UcWords(ModelName), ' ', '', [rfReplaceAll]);
  Result:=inherited CreateSource(Filename, SourceName, ModelName);
  log( 'model "' + ModelName + '" created');
end;

procedure TFileDescModel.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  inherited UpdateDefaultPascalFileExtension(DefPasExt);
end;



end.


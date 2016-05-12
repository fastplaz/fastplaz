unit modvar_model;

{$mode objfpc}{$H+}

interface

uses
  common, database_lib, serialize_lib,
  Math, Classes, SysUtils;

const
  MODVAR_TABLE_NAME = 'module_vars';
  MODVAR_PRIMARY_KEY = 'id';
  MODVAR_FIELD_MODULENAME = 'modname';
  MODVAR_FIELD_VARIABLE = 'name';
  MODVAR_FIELD_VALUE = 'value';
  {$include '../../../define_cms.inc'}

type

  { TModvarModel }

  TModvarModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;

    function GetCustom(const ModuleName: string; const VariableName: string;
      DefaultValue: string = ''): variant;
    function SetCustom(const ModuleName: string; const VariableName: string;
      const ValueOfVariable: string): boolean;
  end;

implementation

constructor TModvarModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(MODVAR_TABLE_NAME); // table name = users
  primaryKey := MODVAR_PRIMARY_KEY;
end;

destructor TModvarModel.Destroy;
begin
  inherited Destroy;
end;

function TModvarModel.GetCustom(const ModuleName: string;
  const VariableName: string; DefaultValue: string): variant;
begin
  Result := DefaultValue;
  if FindFirst([MODVAR_FIELD_MODULENAME + '="' + ModuleName + '"',
    MODVAR_FIELD_VARIABLE + '="' + VariableName + '"']) then
  begin
    Result := Data.FieldValues[MODVAR_FIELD_VALUE];
  end;
end;

function TModvarModel.SetCustom(const ModuleName: string;
  const VariableName: string; const ValueOfVariable: string): boolean;
var
  sql: string;
begin
  if FindFirst([MODVAR_FIELD_MODULENAME + '="' + ModuleName + '"',
    MODVAR_FIELD_VARIABLE + '="' + VariableName + '"']) then
  begin
    SetFieldValue( MODVAR_FIELD_VALUE, ValueOfVariable);
    Result := Save( MODVAR_PRIMARY_KEY + '=' + string( Value[MODVAR_PRIMARY_KEY]));
  end
  else
  begin
    New;
    SetFieldValue( MODVAR_FIELD_MODULENAME, ModuleName);
    SetFieldValue( MODVAR_FIELD_VARIABLE, VariableName);
    SetFieldValue( MODVAR_FIELD_VALUE, ValueOfVariable);
    Result := Save();
  end;
end;

end.

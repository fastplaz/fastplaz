unit modvar_model;

{$mode objfpc}{$H+}

interface

uses
  common, database_lib, serialize_lib,
  Math, Variants, Classes, SysUtils;

const
  MODVAR_TABLE_NAME = 'module_vars';
  MODVAR_PRIMARY_KEY = 'mvid';
  MODVAR_FIELD_MODULENAME = 'modname';
  MODVAR_FIELD_VARIABLE = 'name';
  MODVAR_FIELD_VALUE = 'value';
  { $include '../../../define_cms.inc'}

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
  private
    function WhereModuleVariable(const ModuleName, VariableName: string): TStringArray;
  end;

implementation

constructor TModvarModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(MODVAR_TABLE_NAME); // table name = module_vars
  primaryKey := MODVAR_PRIMARY_KEY;
end;

function TModvarModel.WhereModuleVariable(const ModuleName, VariableName: string): TStringArray;
begin
  Result := TStringArray.Create(
    MODVAR_FIELD_MODULENAME + '=' + QuoteValue(ModuleName),
    MODVAR_FIELD_VARIABLE + '=' + QuoteValue(VariableName));
end;

destructor TModvarModel.Destroy;
begin
  inherited Destroy;
end;

function TModvarModel.GetCustom(const ModuleName: string;
  const VariableName: string; DefaultValue: string): variant;
begin
  Result := DefaultValue;
  // the names were pasted into the WHERE clause with double quotes:
  // a quote in a name broke the statement, and "..." is MySQL only
  if FindFirst(WhereModuleVariable(ModuleName, VariableName)) then
  begin
    Result := Data.FieldValues[MODVAR_FIELD_VALUE];
    if VarIsNull(Result) then
      Result := DefaultValue;
  end;
end;

function TModvarModel.SetCustom(const ModuleName: string;
  const VariableName: string; const ValueOfVariable: string): boolean;
begin
  if FindFirst(WhereModuleVariable(ModuleName, VariableName)) then
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

unit warehouse_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, database_lib, string_helpers, dateutils,
  datetime_helpers, array_helpers;

type

{ TWarehouseModel }

  TWarehouseModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation


uses common;

constructor TWarehouseModel.Create(const DefaultTableName: string = '');
begin
  inherited Create( DefaultTableName); // table name = warehouses
  primaryKey := 'id';
end;

end.


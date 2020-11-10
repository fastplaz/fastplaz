unit db_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, database_lib, string_helpers, dateutils,
  datetime_helpers, array_helpers;

type

{ TDbModel }

  TDbModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation


uses common;

constructor TDbModel.Create(const DefaultTableName: string = '');
begin
  inherited Create( DefaultTableName);
end;

end.


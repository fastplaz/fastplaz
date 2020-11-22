unit contact_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, database_lib, string_helpers, dateutils,
  datetime_helpers, array_helpers;

type

{ TContactModel }

  TContactModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation


uses common;

constructor TContactModel.Create(const DefaultTableName: string = '');
begin
  inherited Create( DefaultTableName); // table name = contacts
  //inherited Create('yourtablename'); // if use custom tablename
end;

end.


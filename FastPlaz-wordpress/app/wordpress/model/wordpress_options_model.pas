unit wordpress_options_model;

{$mode objfpc}{$H+}

interface

uses
  database_lib,
  Classes, SysUtils;

type

  { TWordpressOptions }

  TWordpressOptions = class(TSimpleModel)
  public
    constructor Create(const DefaultTableName: string = '');
  end;


implementation

uses fastplaz_handler;

{ TWordpressOptions }

constructor TWordpressOptions.Create(const DefaultTableName: string);
begin
  inherited Create(AppData.table_prefix+ '_options');
end;

end.


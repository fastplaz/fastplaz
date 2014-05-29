unit wordpress_tags_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

type

  { TWordpressTags }

  TWordpressTags = class(TSimpleModel)
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

uses fastplaz_handler;

{ TWordpressTags }

constructor TWordpressTags.Create(const DefaultTableName: string);
begin
  inherited Create(AppData.table_prefix+ '_terms');
end;

end.


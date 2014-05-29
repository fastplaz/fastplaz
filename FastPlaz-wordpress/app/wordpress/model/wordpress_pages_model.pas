unit wordpress_pages_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

type

  { TWordpressPages }

  TWordpressPages = class(TSimpleModel)
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

uses fastplaz_handler;

{ TWordpressPages }

constructor TWordpressPages.Create(const DefaultTableName: string);
begin
  inherited Create(AppData.table_prefix+ '_posts');
end;

end.


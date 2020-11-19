unit database_controller;

{$mode objfpc}{$H+}

interface

uses
  warehouse_model,
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs,
    fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TDatabaseController = class(TMyCustomController)
  private
    warehouses: TWarehouseModel;
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList
      ): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses theme_controller, common;

constructor TDatabaseController.CreateNew(AOwner: TComponent; 
  CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  warehouses := TWarehouseModel.Create();
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TDatabaseController.Destroy;
begin
  if Assigned(warehouses) then
    warehouses.Free;
  inherited Destroy;
end;

// Init First
procedure TDatabaseController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TDatabaseController.Get;
var
  queryString: string;
  whereAsArray: TStringArray;
begin
  ThemeUtil.Assign('$databaseActive', '0');

  // contoh pengambilan nilai dari query string
  queryString := _GET['contoh_querystring'];

  if queryString.IsEmpty then
  begin
    DataBaseInit('/database?contoh_querystring=1'); // jika ada error akan diredirect ke url tersebut

    warehouses.AddJoin('locations', 'id', 'warehouses.location_id', ['code','name country']);
    if not warehouses.Find(whereAsArray) then
    begin
      //
    end;

    ThemeUtil.Assign('$databaseActive', '1');
  end;

  ThemeUtil.Assign('$Title', 'Database Model');
  ThemeUtil.Layout := 'master'; // custom layout: master.html
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TDatabaseController.Post;
begin
  Redirect(BaseURL+'/database');
end;

function TDatabaseController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin
  ThemeUtil.AssignVar['$Warehouses'] := @warehouses.Data;
  Result := ThemeUtil.RenderFromContent(nil, '', 'modules/database/main.html');
end;


end.


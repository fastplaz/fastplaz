unit example_foreach_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, Math,
    fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TForeachController = class(TMyCustomController)
  private
    json: TJSONUtil;
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

constructor TForeachController.CreateNew(AOwner: TComponent; 
  CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  json := TJSONUtil.Create;
end;

destructor TForeachController.Destroy;
begin
  if Assigned(json) then
    json.Free;
  inherited Destroy;
end;

// Init First
procedure TForeachController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TForeachController.Get;
begin
  ThemeUtil.Assign('$Title', 'Foreach Example (json)');

  json.LoadFromJsonString(LoadFromFile('files/data-warehouse.json'));
  ThemeUtil.AssignVar['$Warehouses'] := @json.Data;

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  ThemeUtil.Layout := 'master';
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TForeachController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TForeachController.Tag_MainContent_Handler(
  const TagName: string; Params: TStringList): string;
begin
  Result := ThemeUtil.RenderFromContent(nil, '', 'modules/example/foreach.html');
end;

end.


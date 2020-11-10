unit example_controller;

{$mode objfpc}{$H+}

interface

uses
  Math,
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs,
  fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TExampleController = class(TMyCustomController)
  private
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses theme_controller, common;

constructor TExampleController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TExampleController.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TExampleController.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TExampleController.Get;
begin
  randomize();

  ThemeUtil.Assign('$Title', 'basic example');
  ThemeUtil.Assign('$variableName',
    'Dan ini adalah kontent yang dikirim melalui variable: $variableName.<br />Di dalam kode HTML cukup dituliskan [$variableName].');
  ThemeUtil.Assign('$currentDate', Now.IncMinute(RandomRange(-2000,2000)).AsString);

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  ThemeUtil.Layout := 'master';
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TExampleController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TExampleController.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := ThemeUtil.RenderFromContent(nil, '', 'modules/example/main.html');
end;

end.



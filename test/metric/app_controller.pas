unit app_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fastplaz_app, fpjson, HTTPDefs,
  fastplaz_handler, database_lib, dateutils;

type

  { TAppController }

  TAppController = class(TMyCustomController)
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

constructor TAppController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TAppController.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TAppController.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TAppController.Get;
begin
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TAppController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TAppController.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin

  // your code here
  Result := h3('Hello "Main" Module ... FastPlaz !');

end;


end.

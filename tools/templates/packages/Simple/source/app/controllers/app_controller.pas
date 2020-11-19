unit app_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
    fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TFastplazController = class(TMyCustomController)
  private
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

constructor TFastplazController.CreateNew(AOwner: TComponent; 
  CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TFastplazController.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TFastplazController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TFastplazController.Get;
begin
  ThemeUtil.Assign('$Greeting', 'Welcome To Our Example Tour!');
  ThemeUtil.Assign('$ModuleName', 'Dashboard');

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  ThemeUtil.Layout := 'home';
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TFastplazController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TFastplazController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin

  // your code here
  Result:=h3('Hello "Main" Module ... FastPlaz !');

end;


end.


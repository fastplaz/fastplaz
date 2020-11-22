unit logout_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
    fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TLogoutController = class(TMyCustomController)
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

constructor TLogoutController.CreateNew(AOwner: TComponent; CreateMode: integer
  );
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TLogoutController.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TLogoutController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TLogoutController.Get;
begin
  Session.Clear;
  Redirect(BaseURL+'login');
end;

// POST Method Handler
procedure TLogoutController.Post;
begin
  Session.Clear;
  Redirect(BaseURL+'login');
end;

function TLogoutController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin

  // your code here
  Result:=h3('Hello "Logout" Module ... FastPlaz !');

end;



initialization
  // -> http://yourdomainname/logout
  // The following line should be moved to a file "routes.pas"
  Route[ '/logout'] := TLogoutController;

end.


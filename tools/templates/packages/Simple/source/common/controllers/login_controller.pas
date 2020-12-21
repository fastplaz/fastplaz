unit login_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
    fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TLoginController = class(TMyCustomController)
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

constructor TLoginController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TLoginController.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TLoginController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TLoginController.Get;
var
  username, lastLogin: String;
begin
  username := _SESSION['username'];
  lastLogin := _SESSION['lastLogin'];
  ThemeUtil.Assign('$username', username);
  ThemeUtil.Assign('$lastLogin', lastLogin);

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  ThemeUtil.Layout := 'login';
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TLoginController.Post;
var
  email, passWord: string;
begin
  email := _POST['email'];
  passWord := _POST['password'];

  if not email.IsEmail then
  begin
    FlashMessages := 'Email tidak sesuai';
    Redirect('/login');
  end;



  //TODO: Validation Check



  // Save to session
  _SESSION['username'] := email;
  _SESSION['lastLogin'] := Now.AsString;

  ThemeUtil.Assign('$username', email);
  Redirect(BaseURL+'login');
end;

function TLoginController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin
end;

initialization
  // -> http://yourdomainname/login
  // The following line should be moved to a file "routes.pas"
  Route[ '/login'] := TLoginController;

end.


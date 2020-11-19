unit example_form_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
    fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers;

type
  TFormController = class(TMyCustomController)
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

constructor TFormController.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TFormController.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TFormController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TFormController.Get;
begin
  ThemeUtil.Assign('$Title', 'Form Example');

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  ThemeUtil.Layout := 'master';
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TFormController.Post;
var
  fullName, email, message: string;
begin
  fullName := _POST['fullName'];
  email := _POST['email'];
  message := _POST['message'];

  // Proteksi dengan CSRF sederhana
  if not isValidCSRF then
  begin
    FlashMessages:= 'Maaf, gunakan pengisian form ini dengan sebaik-baiknya.';
    Redirect('/example/form/');
  end;

  // Validasi sederhana
  if fullName.IsEmpty or email.IsEmpty or message.IsEmpty
    or (not email.IsEmail) then
  begin
    FlashMessages:= 'Silakan isi dengan lengkap dan benar';
    Redirect('/example/form/');
  end;

  //TODO: Save to database

  ThemeUtil.Assign('$Title', 'Form Example');
  ThemeUtil.Assign('$fullName', fullName);
  ThemeUtil.Assign('$email', email);
  ThemeUtil.Assign('$message', message);

  ThemeUtil.Assign('maincontent', ThemeUtil.RenderFromContent(nil, '', 'modules/example/thankyou.html'));
  ThemeUtil.Layout := 'master';
  Response.Content := ThemeUtil.Render();
end;

function TFormController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin
  Result := ThemeUtil.RenderFromContent(nil, '', 'modules/example/form.html');
end;


end.


unit example_form_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
  fastplaz_handler, database_lib, dateutils,
  string_helpers, datetime_helpers, array_helpers;

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

const
  UPLOAD_FOLDERS = 'uploads/';
  permittedFileType: array [0..3] of string = ('.jpg', '.jpeg', '.png', '.pdf');

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
  ThemeUtil.Assign('$FileUpload', _SESSION['fileupload']);

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  ThemeUtil.Layout := 'master';
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TFormController.Post;
var
  i: integer;
  targetPath,
  fullName, email, message: string;
begin
  fullName := _POST['fullName'];
  email := _POST['email'];
  message := _POST['message'];

  // Proteksi dengan CSRF sederhana
  if not isValidCSRF then
  begin
    FlashMessages := 'Maaf, gunakan pengisian form ini dengan sebaik-baiknya.';
    Redirect('/example/form/');
  end;

  // Validasi sederhana
  if fullName.IsEmpty or email.IsEmpty or message.IsEmpty
    or (not email.IsEmail) then
  begin
    FlashMessages:= 'Silakan isi dengan lengkap dan benar';
    Redirect('/example/form/');
  end;

  // Document check
  targetPath := IncludeTrailingPathDelimiter(GetCurrentDir) + UPLOAD_FOLDERS;
  if not DirectoryIsWritable(targetPath) then
  begin
    FlashMessages := 'Tidak bisa akses tulis ke direktori "uploads/".';
  end
  else
  begin
    _SESSION['fileupload'] := '';
    for i := 0 to Request.Files.Count - 1 do
    begin
      // only for permitted file type
      if not ((ExtractFileExt(Request.Files[i].FileName)) in permittedFileType) then
      begin
        FlashMessages := 'File "' + Request.Files[i].FileName + '" tidak diijinkan.';
        Continue;
      end;

      if not FileCopy(Request.Files[i].LocalFileName, targetPath +
        Request.Files[i].FileName) then
      begin
        //TODO: add error handling
      end;

      _SESSION['fileupload'] := Request.Files[i].FileName;
    end;

  end;//if not DirectoryIsWritable(targetPath)
  // Document check - end


  //TODO: Save to database

  ThemeUtil.Assign('$Title', 'Form Example');
  ThemeUtil.Assign('$fullName', fullName);
  ThemeUtil.Assign('$email', email);
  ThemeUtil.Assign('$message', message.Replace('\r\n','<br />'));
  ThemeUtil.Assign('$FileUpload', _SESSION['fileupload']);

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


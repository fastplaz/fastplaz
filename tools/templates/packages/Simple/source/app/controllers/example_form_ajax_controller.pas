unit example_form_ajax_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs,
  fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers,
  array_helpers;

type
  TFormAjaxController = class(TMyCustomController)
  private
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common;

const
  UPLOAD_FOLDERS = 'uploads/';
  permittedFileType: array [0..3] of string = ('.jpg', '.jpeg', '.png', '.pdf');

constructor TFormAjaxController.CreateNew(AOwner: TComponent; 
  CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TFormAjaxController.Destroy;
begin
  inherited Destroy;
end;

// Init First
procedure TFormAjaxController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TFormAjaxController.Get;
begin
  OutputJson(400, 'False');
end;

// POST Method Handler
procedure TFormAjaxController.Post;
var
  i: integer;
  targetPath,
  fullName, email, message, errMessage: string;
begin
  fullName := _POST['fullName'];
  email := _POST['email'];
  message := _POST['message'];
  errMessage := '';

  // Proteksi dengan CSRF sederhana
  if not isValidCSRF then
  begin
    //OutputJson(400, 'Maaf, gunakan pengisian form ini dengan sebaik-baiknya.');
  end;

  // Validasi sederhana
  if fullName.IsEmpty or email.IsEmpty or message.IsEmpty
    or (not email.IsEmail) then
  begin
    OutputJson(400, 'Silakan isi dengan lengkap dan benar');
  end;

  targetPath := IncludeTrailingPathDelimiter(GetCurrentDir) + UPLOAD_FOLDERS;
  if not DirectoryIsWritable(targetPath) then
  begin
    OutputJson(400, 'Tidak bisa akses tulis ke direktori "uploads/".');
  end
  else
  begin
    for i := 0 to Request.Files.Count - 1 do
    begin
      // only for permitted file type
      if not ((ExtractFileExt(Request.Files[i].FileName)) in permittedFileType) then
      begin
        errMessage := #13'File "' + Request.Files[i].FileName + '" tidak diijinkan.';
        Continue;
      end;

      if not FileCopy(Request.Files[i].LocalFileName, targetPath +
        Request.Files[i].FileName) then
      begin
        //TODO: add error handling
      end;

    end;

  end;//if not DirectoryIsWritable(targetPath)
  // Document check - end

  if errMessage.IsNotEmpty then
    OutputJson(400, errMessage);

  OutputJson(0, 'OK');
end;

end.


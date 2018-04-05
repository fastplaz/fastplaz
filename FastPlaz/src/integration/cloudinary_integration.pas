unit cloudinary_integration;

{

  // USAGE:
  respon: IHTTPResponse;

  with TCloudinaryIntegration.Create do
  begin
    URL := 'https://api.cloudinary.com/v1_1/youraccountname/image/upload';
    ApiKey := 'yourapikey';
    ApiSecret := 'yourapisecreat';
    UploadPreset := 'theuploadpreset';

    respon := UploadFile('/folder/folder/filename.png');


    Free;
  end;

}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}
{$DEFINE CLOUDINARY_INTEGRATION}

interface

uses
  http_lib,
  Classes, SysUtils;

{$ifdef CLOUDINARY_INTEGRATION}

type

  { TCloudinaryIntegration }

  TCloudinaryIntegration = class(TInterfacedObject)
  private
    FApiKey: string;
    FApiSecret: string;
    FIsSuccessfull: boolean;
    FPublicId: string;
    FUploadPreset: string;
    FURL: string;
  public
    constructor Create;
    function UploadFile(FileName: string; Public_Id: string = ''): IHTTPResponse;
  published
    property URL: string read FURL write FURL;
    property PublicId: string read FPublicId write FPublicId;
    property ApiKey: string read FApiKey write FApiKey;
    property ApiSecret: string read FApiSecret write FApiSecret;
    property UploadPreset: string read FUploadPreset write FUploadPreset;
    property IsSuccessfull: boolean read FIsSuccessfull;
  end;

{$endif}

implementation

{$ifdef CLOUDINARY_INTEGRATION}

{ TCloudinaryIntegration }

constructor TCloudinaryIntegration.Create;
begin
  FIsSuccessfull := False;

end;

function TCloudinaryIntegration.UploadFile(FileName: string;
  Public_Id: string): IHTTPResponse;
begin
  FIsSuccessfull := False;
  with THTTPLib.Create(URL) do
  begin
    ContentType := 'application/x-www-form-urlencoded';
    if Public_Id = '' then
       FormData[ 'public_id'] := ChangeFileExt( ExtractFileName( FileName), '');
    FormData[ 'api_key'] := ( FApiKey);
    FormData[ 'api_secret'] := ( FApiSecret);
    FormData[ 'upload_preset'] := ( FUploadPreset);
    AddFile( FileName, 'file');

    Result := Post;
    FIsSuccessfull := IsSuccessfull;
    Free;
  end;
end;

{$endif}


end.


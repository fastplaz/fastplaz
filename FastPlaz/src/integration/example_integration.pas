unit example_integration;

{
  // USAGE:


}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  http_lib,
  Classes, SysUtils;

{$ifdef EXAMPLE_INTEGRATION}
{$endif}

type

  { TExampleIntegration }

  TExampleIntegration = class(TInterfacedObject)
  private
    FIsSuccessfull: boolean;
    FURL: string;
  public
    constructor Create;
    function SendExample(FileName: string; Public_Id: string = ''): IHTTPResponse;
  published
    property URL: string read FURL write FURL;
    property IsSuccessfull: boolean read FIsSuccessfull;
  end;

implementation

const
  EXAMPLE_BASEURL = 'https://api.example.fastplaz.com/bot%s/';

var
  Response: IHTTPResponse;

{ TExampleIntegration }

constructor TExampleIntegration.Create;
begin
  FIsSuccessfull := False;

end;

function TExampleIntegration.SendExample(FileName: string; Public_Id: string): IHTTPResponse;
begin
  FIsSuccessfull := False;
  with THTTPLib.Create(URL) do
  begin
    ContentType := 'application/x-www-form-urlencoded';
    //AddHeader('Connection', 'keep-alive');
    //AddHeader('Cache-Control', 'no-cache');
    //AddHeader('Accept', '*/*');
    if Public_Id = '' then
      FormData['public_id'] := ChangeFileExt(ExtractFileName(FileName), '');
    FormData['api_key'] := 'apikey';
    FormData['api_secret'] := 'apisecret';
    AddFile(FileName, 'file');

    Result := Post;
    FIsSuccessfull := IsSuccessfull;
    Free;
  end;
end;


end.



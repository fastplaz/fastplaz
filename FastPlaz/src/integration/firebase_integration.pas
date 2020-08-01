{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit firebase_integration;

{
  // GOOGLE FIREBASE
  https://firebase.google.com/
  https://firebase.google.com/docs/reference/rest/database/
  https://firebase.google.com/docs/database/rest/retrieve-data

  // USAGE:
  [x] Get Data
  with TFirebaseIntegration.Create do
  begin
    ProjectID := 'your-project-id';
    Path := 'users/jack/name.json';
    if Get then
    begin
      variable := Data['fieldname'];
      ...

    end;

    Free;
  end;

  [x] Write Data
  with TFirebaseIntegration.Create do
  begin
    ProjectID := 'your-project-id';
    Path := 'users/jack/name.json';
    Data['first'] := 'first';
    Data['last'] := 'last';
    if not Put then
    begin
      // failed
    end;

    Free;
  end;


}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  http_lib, json_lib,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

{$ifdef FIREBASE_INTEGRATION}
{$endif}

type

  { TFirebaseIntegration }

  TFirebaseIntegration = class(TInterfacedObject)
  private
    FData: TJSONUtil;
    FIsSuccessfull: boolean;
    FPath: string;
    FProjectID: string;
    FURL: string;

    FhttpClient: THTTPLib;
    FhttpResponse: IHTTPResponse;
    function prepare: Boolean;
    function generateURL: string;
    function GetResultCode: integer;
    function GetResultText: string;
  public
    constructor Create;
    destructor Destroy;

    function Get: Boolean;
    function Put: Boolean;
    function Post: Boolean;
    function Patch: Boolean;
    function Delete: Boolean;
    function Read: Boolean;
    function Write: Boolean;
    function Update: Boolean;

  published
    property URL: string read FURL write FURL;
    property IsSuccessfull: boolean read FIsSuccessfull;

    property ProjectID: string read FProjectID write FProjectID;
    property Path: string read FPath write FPath;
    property Data: TJSONUtil read FData write FData;

    property ResultCode: integer read GetResultCode;
    property ResultText: string read GetResultText;
  end;

implementation

const
  FIREBASE_BASEURL = 'https://%s.firebaseio.com/';

var
  Response: IHTTPResponse;

{ TFirebaseIntegration }

constructor TFirebaseIntegration.Create;
begin
  FIsSuccessfull := False;
  FhttpClient := THTTPLib.Create;
  FData := TJSONUtil.Create;
end;

destructor TFirebaseIntegration.Destroy;
begin
  FData.Free;
  FhttpClient.Free;
end;

function TFirebaseIntegration.generateURL: string;
begin
  Result := Format(FIREBASE_BASEURL, [FProjectID]) + FPath;
end;

function TFirebaseIntegration.GetResultCode: integer;
begin
  Result := -1;
  if not Assigned(FhttpResponse) then
    Exit;
  Result := FhttpResponse.ResultCode;
end;

function TFirebaseIntegration.GetResultText: string;
begin
  Result := '';
  if not Assigned(FhttpResponse) then
    Exit;
  Result := FhttpResponse.ResultText;
end;

function TFirebaseIntegration.Get: Boolean;
begin
  Result := prepare;
  if not Result then
    Exit;

  FData.LoadFromJsonString( '{}');
  FhttpResponse := FhttpClient.Get;
  if FhttpResponse.ResultCode <> 200 then
    Exit;

  try
    if FhttpResponse.ResultText <> 'null' then
      FData.LoadFromJsonString( FhttpResponse.ResultText);
  except
  end;

  Result := True;
end;

function TFirebaseIntegration.Put: Boolean;
begin
  Result := prepare;
  if not Result then
    Exit;

  FhttpResponse := FhttpClient.Put;
  if FhttpResponse.ResultCode <> 200 then
    Exit;

  Result := True;
end;

function TFirebaseIntegration.Post: Boolean;
begin
  Result := prepare;
  if not Result then
    Exit;

  //ContentType := 'application/x-www-form-urlencoded';
  //AddHeader('Accept', '*/*');
  //FormData['api_key'] := 'apikey';

  FhttpResponse := FhttpClient.Post;
  if FhttpResponse.ResultCode <> 200 then
    Exit;

  Result := True;
end;

function TFirebaseIntegration.Patch: Boolean;
begin
  Result := prepare;
  if not Result then
    Exit;

  FhttpResponse := FhttpClient.Patch;
  if FhttpResponse.ResultCode <> 200 then
    Exit;

  Result := True;
end;

function TFirebaseIntegration.Delete: Boolean;
begin
  Result := prepare;
  if not Result then
    Exit;

  FhttpResponse := FhttpClient.Delete;
  if FhttpResponse.ResultCode <> 200 then
    Exit;

  Result := True;
end;

function TFirebaseIntegration.Read: Boolean;
begin
  Result := Get;
end;

function TFirebaseIntegration.Write: Boolean;
begin
  Result := Put;
end;

function TFirebaseIntegration.Update: Boolean;
begin
  Result := Post;
end;

function TFirebaseIntegration.prepare: Boolean;
begin
  Result := False;
  if FProjectID = '' then
    Exit;
  FhttpClient.URL := generateURL;
  FhttpClient.RequestBody := TStringStream.Create(FData.AsJSON);
  Result := True;
end;


end.



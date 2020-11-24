unit bca_integration;

{$mode objfpc}{$H+}

interface

uses
  base64,
  common, http_lib, json_lib, logutil_lib,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

type

  { TBCAIntegration }

  TBCAIntegration = class(TInterfacedObject)
  private
    FClientID: string;
    FClientSecret: string;
    FResultCode: integer;
    FResultText: string;
    FSandBox: boolean;
    function getAuthorization: string;

  public
    constructor Create;
    destructor Destroy;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property SandBox: boolean read FSandBox write FSandBox;

    property ClientID: string read FClientID write FClientID;
    property ClientSecret: string read FClientSecret write FClientSecret;
    property Authorization: string read getAuthorization;

    function GetToken: string;
  end;


implementation

const
  _BCA_SANDBOX_AUTHORIZATION = '';
  _BCA_SANDBOX_URL = 'https://sandbox.bca.co.id/';
  _BCA_URL_GETTOKEN = 'api/oauth/token';

var
  Response: IHTTPResponse;


{ TBCAIntegration }

function TBCAIntegration.getAuthorization: string;
begin
  Result := EncodeStringBase64(FClientID + ':' + FClientSecret);
  if FSandBox then
    Result := _BCA_SANDBOX_AUTHORIZATION;
end;

constructor TBCAIntegration.Create;
begin
  FClientID := '';
  FClientSecret := '';
  FSandBox := False;
end;

destructor TBCAIntegration.Destroy;
begin

end;

function TBCAIntegration.GetToken: string;
var
  urlTarget: string;
begin
  Result := '';
  if not FSandBox then
  begin
    if FClientID = '' then
      Exit;
    if FClientSecret = '' then
      Exit;
  end;

  urlTarget := _BCA_SANDBOX_URL;
  urlTarget := urlTarget + _BCA_URL_GETTOKEN;
  with THTTPLib.Create(urlTarget) do
  begin
    //ContentType := 'application/x-www-form-urlencoded';
    AddHeader('Authorization', 'Basic ' + Authorization);
    FormData['grant_type'] := 'client_credentials';
    Response := Post;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      Result := FResultText;
    end;
    Free;
  end;
end;

end.

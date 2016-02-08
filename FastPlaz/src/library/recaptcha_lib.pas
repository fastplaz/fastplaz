unit recaptcha_lib;

{
This is FORK a PHP library that handles calling reCAPTCHA.
http://www.google.com/recaptcha
Get a reCAPTCHA API Key
  https://www.google.com/recaptcha/admin/create
}

{$mode objfpc}{$H+}

interface

uses
  fpcgi, fastplaz_handler, common,
  fphttpclient,
  Classes, SysUtils;

const
  ReCaptcha_SignupUrl = 'https://www.google.com/recaptcha/admin';
  ReCaptcha_SiteVerifyUrl_v1 = 'http://www.google.com/recaptcha/api/verify?';
  ReCaptcha_SiteVerifyUrl_v2 = 'http://www.google.com/recaptcha/api/siteverify?';
  ReCaptcha_Version = 'php_1.0';

type

  { TReCaptcha }

  TReCaptcha = class
  private
    FSecretKey: string;
    FErrorCodes: string;
    FVersion: string;
    function _submitHttpGet(SiteURL: string; Data: array of string): string;
    function _submitHttpPost(SiteURL: string; Data: array of string): string;
  public
    constructor Create(SecretKey: string);
    destructor Destroy; override;
    function isValid(): boolean;
    function verifyResponse(const RemoteIP, ResponRecaptcha, Challenge: string): boolean;


    property ErrorCodes: string read FErrorCodes;
    property Version: string read FVersion;
  end;


implementation

{ TReCaptcha }

function TReCaptcha._submitHttpGet(SiteURL: string; Data: array of string): string;
var
  parameter: string;
begin
  parameter := EncodeQueryString(Data);
  Result := file_get_contents(SiteURL + parameter);
end;

function TReCaptcha._submitHttpPost(SiteURL: string; Data: array of string): string;
var
  s, parameter: string;
begin
  parameter := EncodeQueryString(Data);
  with TFPHTTPClient.Create(nil) do
  begin
    try
      s := FormPost(SiteURL, parameter);
      Result := s;
    except
      on e: Exception do
      begin
        Result := e.Message;
      end;
    end;
    Free;
  end;
end;

constructor TReCaptcha.Create(SecretKey: string);
begin
  FSecretKey := SecretKey;
  FVersion := 'v1';
end;

destructor TReCaptcha.Destroy;
begin
  inherited Destroy;
end;

function TReCaptcha.isValid: boolean;
var
  response: string;
begin
  Result := False;
  FVersion := _POST['recaptcha_version'];
  if FVersion = '' then
    FVersion := 'v1';
  response := _POST['g-recaptcha-response']; // v2
  if response = '' then
    response := _POST['recaptcha_response_field'];

  FErrorCodes := 'missing-input';
  if response = '' then
    Exit;

  Result := verifyResponse(Application.EnvironmentVariable['REMOTE_ADDR'],
    response, _POST['recaptcha_challenge_field']);
end;

function TReCaptcha.verifyResponse(
  const RemoteIP, ResponRecaptcha, Challenge: string): boolean;
var
  s: string;
begin
  Result := False;
  if ((RemoteIP = '') or (ResponRecaptcha = '')) then
  begin
    FErrorCodes := 'missing-input';
    Exit;
  end;

  if Version = 'v1' then
  begin
    if Challenge = '' then
    begin
      FErrorCodes := 'incorrect-captcha-sol';
      Exit;
    end;
    s := _submitHttpPost(ReCaptcha_SiteVerifyUrl_v1,
      ['privatekey=' + FSecretKey, 'remoteip=' + RemoteIP, 'challenge=' +
      Challenge, 'response=' + ResponRecaptcha]);
    with Explode(s, #10) do
    begin
      try
        s := trim(ValueFromIndex[0]);
        if s = 'true' then
          Result := True;
        FErrorCodes := ValueFromIndex[1];
      except
      end;
      Free;
    end;
  end //-- v1 - end
  else
  begin //-- v2
    s := _submitHttpGet(ReCaptcha_SiteVerifyUrl_v2,
      ['secret=' + FSecretKey, 'remoteip=' + RemoteIP, 'response=' +
      ResponRecaptcha, 'v=' + ReCaptcha_Version]);

    // TODO: processing v2
  end;
end;

end.



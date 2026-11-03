unit recaptcha_lib;

{
  reCAPTCHA v2 verification (https://developers.google.com/recaptcha/docs/verify)

  USAGE
    with TReCaptcha.Create(SecretKey) do
    begin
      if not isValid then
        ... ErrorCodes ...
      Free;
    end;

  isValid reads g-recaptcha-response from the POST data and calls the
  siteverify endpoint with a form POST (secret, response, remoteip).
  The legacy v1 API (api/verify, challenge/response fields) was shut down
  by Google and is no longer supported: it always fails with
  'unsupported-version'.
}

{$mode objfpc}{$H+}

interface

uses
  json_lib, fpjson,
  fastplaz_app, fastplaz_handler, common,
  fphttpclient, opensslsockets,
  Classes, SysUtils;

const
  ReCaptcha_SignupUrl = 'https://www.google.com/recaptcha/admin';
  ReCaptcha_SiteVerifyUrl_v2 = 'https://www.google.com/recaptcha/api/siteverify';
  ReCaptcha_Version = 'fastplaz_2.0';
  ReCaptcha_Timeout = 10000; // ms

type

  { TReCaptcha }

  TReCaptcha = class
  private
    FSecretKey: string;
    FErrorCodes: string;
    FVersion: string;
    FTimeout: integer;
  protected
    // Sends the verification request; overridable for tests.
    // Returns the response body, raises on transport errors.
    function SubmitSiteVerify(Params: TStrings): string; virtual;
  public
    constructor Create(SecretKey: string);
    destructor Destroy; override;
    function isValid(): boolean;
    function verifyResponse(const RemoteIP, ResponRecaptcha: string;
      const Challenge: string = ''): boolean;
    // Interprets a siteverify JSON body; sets ErrorCodes.
    function ParseSiteVerifyResponse(const AJson: string): boolean;

    property ErrorCodes: string read FErrorCodes;
    property Version: string read FVersion write FVersion;
    property Timeout: integer read FTimeout write FTimeout;
  end;


implementation

{ TReCaptcha }

function TReCaptcha.SubmitSiteVerify(Params: TStrings): string;
var
  client: TFPHTTPClient;
begin
  client := TFPHTTPClient.Create(nil);
  try
    client.ConnectTimeout := FTimeout;
    client.IOTimeout := FTimeout;
    client.AddHeader('User-Agent', 'fastplaz-recaptcha/' + ReCaptcha_Version);
    // FormPost URL-encodes every field, so a token containing '&' or '='
    // cannot inject parameters and the secret never appears in a URL
    Result := client.FormPost(ReCaptcha_SiteVerifyUrl_v2, Params);
  finally
    client.Free;
  end;
end;

constructor TReCaptcha.Create(SecretKey: string);
begin
  inherited Create;
  FSecretKey := SecretKey;
  FVersion := 'v2';
  FTimeout := ReCaptcha_Timeout;
  FErrorCodes := '';
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
  // the version is a server side decision; it used to be taken from the
  // POST data, which let a client force the dead v1 endpoint
  response := _POST['g-recaptcha-response'];
  FErrorCodes := 'missing-input';
  if response = '' then
    Exit;
  Result := verifyResponse(Application.EnvironmentVariable['REMOTE_ADDR'], response);
end;

function TReCaptcha.ParseSiteVerifyResponse(const AJson: string): boolean;
var
  json: TJSONUtil;
  d: TJSONData;
begin
  Result := False;
  FErrorCodes := 'invalid-response';
  json := TJSONUtil.Create;
  try
    if not json.LoadFromJsonString(AJson) or not Assigned(json.Data) then
      Exit;
    d := json.Data.FindPath('success');
    if Assigned(d) and (d.JSONType = jtBoolean) and d.AsBoolean then
    begin
      FErrorCodes := '0';
      Result := True;
      Exit;
    end;
    d := json.Data.FindPath('error-codes');
    if Assigned(d) and (d.JSONType = jtArray) and (d.Count > 0) then
      FErrorCodes := d.Items[0].AsString
    else
      FErrorCodes := 'invalid-response';
  finally
    json.Free;
  end;
end;

function TReCaptcha.verifyResponse(const RemoteIP, ResponRecaptcha: string;
  const Challenge: string): boolean;
var
  params: TStringList;
  body: string;
begin
  Result := False;
  if ((RemoteIP = '') or (ResponRecaptcha = '')) then
  begin
    FErrorCodes := 'missing-input';
    Exit;
  end;
  if FSecretKey = '' then
  begin
    FErrorCodes := 'missing-input-secret';
    Exit;
  end;
  if Version <> 'v2' then
  begin
    FErrorCodes := 'unsupported-version';
    Exit;
  end;

  params := TStringList.Create;
  try
    params.Add('secret=' + FSecretKey);
    params.Add('response=' + ResponRecaptcha);
    params.Add('remoteip=' + RemoteIP);
    try
      body := SubmitSiteVerify(params);
    except
      on E: Exception do
      begin
        // a transport failure is not "captcha wrong"; let the caller see it
        FErrorCodes := 'network-error: ' + E.Message;
        Exit;
      end;
    end;
  finally
    params.Free;
  end;
  Result := ParseSiteVerifyResponse(body);
end;

end.

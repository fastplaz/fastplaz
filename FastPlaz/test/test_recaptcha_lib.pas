unit test_recaptcha_lib;

{$mode objfpc}{$H+}

interface

uses
  recaptcha_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { TFakeReCaptcha - captures the request and answers with a canned body }

  TFakeReCaptcha = class(TReCaptcha)
  public
    Sent: TStringList;
    Reply: string;
    RaiseError: boolean;
    Calls: integer;
    constructor Create(SecretKey: string);
    destructor Destroy; override;
  protected
    function SubmitSiteVerify(Params: TStrings): string; override;
  end;

  { TReCaptchaTest }

  TReCaptchaTest = class(TTestCase)
  private
    FRc: TFakeReCaptcha;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParseSuccess;
    procedure TestParseFailureWithCodes;
    procedure TestParseFailureWithoutCodes;
    procedure TestParseInvalidJson;
    procedure TestParseSuccessNotBoolean;
    procedure TestVerifySendsFormFields;
    procedure TestVerifyTokenIsNotSplitOnAmpersand;
    procedure TestVerifyMissingInput;
    procedure TestVerifyMissingSecret;
    procedure TestVerifyUnsupportedVersion;
    procedure TestVerifyNetworkError;
    procedure TestVerifyRejected;
  end;

implementation

{ TFakeReCaptcha }

constructor TFakeReCaptcha.Create(SecretKey: string);
begin
  inherited Create(SecretKey);
  Sent := TStringList.Create;
  Reply := '{"success": true}';
end;

destructor TFakeReCaptcha.Destroy;
begin
  Sent.Free;
  inherited Destroy;
end;

function TFakeReCaptcha.SubmitSiteVerify(Params: TStrings): string;
begin
  Inc(Calls);
  Sent.Assign(Params);
  if RaiseError then
    raise Exception.Create('connect refused');
  Result := Reply;
end;

{ TReCaptchaTest }

procedure TReCaptchaTest.SetUp;
begin
  FRc := TFakeReCaptcha.Create('SECRET');
end;

procedure TReCaptchaTest.TearDown;
begin
  FRc.Free;
end;

procedure TReCaptchaTest.TestParseSuccess;
begin
  AssertTrue(FRc.ParseSiteVerifyResponse('{"success": true, "challenge_ts": "x", "hostname": "h"}'));
  AssertEquals('0', FRc.ErrorCodes);
end;

procedure TReCaptchaTest.TestParseFailureWithCodes;
begin
  AssertFalse(FRc.ParseSiteVerifyResponse(
    '{"success": false, "error-codes": ["timeout-or-duplicate", "x"]}'));
  AssertEquals('timeout-or-duplicate', FRc.ErrorCodes);
end;

procedure TReCaptchaTest.TestParseFailureWithoutCodes;
begin
  AssertFalse(FRc.ParseSiteVerifyResponse('{"success": false}'));
  AssertEquals('invalid-response', FRc.ErrorCodes);
end;

procedure TReCaptchaTest.TestParseInvalidJson;
begin
  AssertFalse(FRc.ParseSiteVerifyResponse('Could not connect'));
  AssertEquals('invalid-response', FRc.ErrorCodes);
  AssertFalse(FRc.ParseSiteVerifyResponse(''));
end;

procedure TReCaptchaTest.TestParseSuccessNotBoolean;
begin
  // "true" as a string must not pass
  AssertFalse(FRc.ParseSiteVerifyResponse('{"success": "true"}'));
end;

procedure TReCaptchaTest.TestVerifySendsFormFields;
begin
  AssertTrue(FRc.verifyResponse('10.0.0.1', 'TOKEN'));
  AssertEquals(1, FRc.Calls);
  AssertEquals('SECRET', FRc.Sent.Values['secret']);
  AssertEquals('TOKEN', FRc.Sent.Values['response']);
  AssertEquals('10.0.0.1', FRc.Sent.Values['remoteip']);
  AssertEquals(3, FRc.Sent.Count);
end;

procedure TReCaptchaTest.TestVerifyTokenIsNotSplitOnAmpersand;
begin
  // the token used to be concatenated into the query string unencoded
  AssertTrue(FRc.verifyResponse('10.0.0.1', 'T&secret=EVIL&x=1'));
  AssertEquals('SECRET', FRc.Sent.Values['secret']);
  AssertEquals('T&secret=EVIL&x=1', FRc.Sent.Values['response']);
  AssertEquals(3, FRc.Sent.Count);
end;

procedure TReCaptchaTest.TestVerifyMissingInput;
begin
  AssertFalse(FRc.verifyResponse('', 'TOKEN'));
  AssertEquals('missing-input', FRc.ErrorCodes);
  AssertFalse(FRc.verifyResponse('10.0.0.1', ''));
  AssertEquals('missing-input', FRc.ErrorCodes);
  AssertEquals(0, FRc.Calls);
end;

procedure TReCaptchaTest.TestVerifyMissingSecret;
var
  rc: TFakeReCaptcha;
begin
  rc := TFakeReCaptcha.Create('');
  try
    AssertFalse(rc.verifyResponse('10.0.0.1', 'TOKEN'));
    AssertEquals('missing-input-secret', rc.ErrorCodes);
    AssertEquals(0, rc.Calls);
  finally
    rc.Free;
  end;
end;

procedure TReCaptchaTest.TestVerifyUnsupportedVersion;
begin
  FRc.Version := 'v1';
  AssertFalse(FRc.verifyResponse('10.0.0.1', 'TOKEN', 'challenge'));
  AssertEquals('unsupported-version', FRc.ErrorCodes);
  AssertEquals('nothing sent to the dead endpoint', 0, FRc.Calls);
end;

procedure TReCaptchaTest.TestVerifyNetworkError;
begin
  FRc.RaiseError := True;
  AssertFalse(FRc.verifyResponse('10.0.0.1', 'TOKEN'));
  AssertEquals('network-error: connect refused', FRc.ErrorCodes);
end;

procedure TReCaptchaTest.TestVerifyRejected;
begin
  FRc.Reply := '{"success": false, "error-codes": ["invalid-input-response"]}';
  AssertFalse(FRc.verifyResponse('10.0.0.1', 'TOKEN'));
  AssertEquals('invalid-input-response', FRc.ErrorCodes);
end;

initialization
  RegisterTest(TReCaptchaTest);

end.

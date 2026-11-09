unit test_fastplaz_handler;

{
  Helpers of fastplaz_handler that do not need a web request.
}

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler, common, Classes, SysUtils, fpcunit, testregistry;

type

  { TFastPlazHandlerTest }

  TFastPlazHandlerTest = class(TTestCase)
  published
    procedure TestSanitizeHeaderValue;
    procedure TestHeaderToEnvName;
    procedure TestRequestAccessorsExist;
  end;

implementation

procedure TFastPlazHandlerTest.TestSanitizeHeaderValue;
begin
  AssertEquals('http://example.com/x', SanitizeHeaderValue('http://example.com/x'));
  AssertEquals('http://example.com/xSet-Cookie: a=b',
    SanitizeHeaderValue('http://example.com/x'#13#10'Set-Cookie: a=b'));
  AssertEquals('ab', SanitizeHeaderValue('a'#10'b'#0));
  AssertEquals('trimmed', SanitizeHeaderValue('  trimmed '#13));
  AssertEquals('', SanitizeHeaderValue(#13#10));
end;

procedure TFastPlazHandlerTest.TestHeaderToEnvName;
begin
  AssertEquals('HTTP_X_REQUESTED_WITH', HeaderToEnvName('X-Requested-With'));
  AssertEquals('HTTP_X_REQUESTED_WITH', HeaderToEnvName('HTTP_X_REQUESTED_WITH'));
  AssertEquals('HTTP_X_REQUESTED_WITH', HeaderToEnvName(' x-requested-with '));
  AssertEquals('HTTP_USER_AGENT', HeaderToEnvName('User-Agent'));
  AssertEquals('', HeaderToEnvName(''));
end;

procedure TFastPlazHandlerTest.TestRequestAccessorsExist;
begin
  // created in the unit initialization, before the session controller
  AssertNotNull(_SERVER);
  AssertNotNull(_GET);
  AssertNotNull(_POST);
  AssertNotNull(_SESSION);
  AssertNotNull(SessionController);
  _SetEnv('FASTPLAZ_TEST_VAR', 'value');
  AssertEquals('value', _SERVER['FASTPLAZ_TEST_VAR']);
  _SetEnv('FASTPLAZ_TEST_VAR', '');
end;

initialization
  RegisterTest(TFastPlazHandlerTest);

end.

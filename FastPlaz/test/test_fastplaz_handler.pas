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
    procedure TestSiteURL;
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

procedure TFastPlazHandlerTest.TestSiteURL;
begin
  // the Host header keeps the port the client used
  AssertEquals('http://example.com', SiteURL('', 'example.com', 'example.com', '80'));
  AssertEquals('http://example.com:8080', SiteURL('', 'example.com:8080', 'example.com', '8080'));
  AssertEquals('https://example.com:8443', SiteURL('on', 'Example.COM:8443', '', ''));
  AssertEquals('http://[::1]:17845', SiteURL('', '[::1]:17845', '', ''));
  // no Host header (HTTP/1.0): server name, port only when not the default
  AssertEquals('http://example.com', SiteURL('', '', 'example.com', '80'));
  AssertEquals('http://example.com:8080', SiteURL('', '', 'example.com', '8080'));
  AssertEquals('https://example.com', SiteURL('ON', '', 'example.com', '443'));
  AssertEquals('https://example.com:80', SiteURL('on', '', 'example.com', '80'));
  AssertEquals('http://example.com', SiteURL('', '', 'example.com', 'abc'));
  // a Host header with anything but a host and a port is not used
  AssertEquals('http://example.com', SiteURL('', 'evil.com/x', 'example.com', '80'));
  AssertEquals('http://example.com', SiteURL('', 'evil.com'#13#10'Set-Cookie: a=b', 'example.com', '80'));
  AssertEquals('http://example.com', SiteURL('', 'user@evil.com', 'example.com', '80'));
end;

initialization
  RegisterTest(TFastPlazHandlerTest);

end.

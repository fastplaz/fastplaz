unit test_fastplaz_handler;

{
  Helpers of fastplaz_handler that do not need a web request.
}

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler, common, Classes, SysUtils, fphttp, fpjson, jsonparser,
  custweb, fpcunit, testregistry;

type

  { TFastPlazHandlerTest }

  TFastPlazHandlerTest = class(TTestCase)
  published
    procedure TestSanitizeHeaderValue;
    procedure TestHeaderToEnvName;
    procedure TestRequestAccessorsExist;
    procedure TestSiteURL;
  end;

  { TRouteMatchTest: MatchRoute() over routes registered by this test
    (paths under /zq/, so the unanchored framework routes do not match) }

  TRouteMatchTest = class(TTestCase)
  private
    FFields: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAnchoredPattern;
    procedure TestFirstMatchWins;
    procedure TestGroupsAndNamedGroups;
    procedure TestThirdGroupIsAct;
    procedure TestNoMatchLeavesFields;
    procedure TestUnregisteredModuleIsSkipped;
    procedure TestEmptyModuleVariable;
    procedure TestPatternChangeIsRecompiled;
  end;

  { TExceptionPageTest: status and content of the error page }

  TExceptionPageTest = class(TTestCase)
  published
    procedure TestStatusOfFpwebNoModule;
    procedure TestStatusOfOtherExceptions;
    procedure TestHtmlWithoutDetails;
    procedure TestHtmlWithDetailsIsEncoded;
    procedure TestJson;
  end;

  TZqModule = class(TMyCustomController);

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

{ TRouteMatchTest }

procedure TRouteMatchTest.SetUp;
begin
  FFields := TStringList.Create;
end;

procedure TRouteMatchTest.TearDown;
begin
  FFields.Free;
end;

procedure TRouteMatchTest.TestAnchoredPattern;
begin
  AssertEquals('zqhome', MatchRoute('/zq', FFields, 'mod'));
  AssertEquals('zqhome', FFields.Values['mod']);
  FFields.Clear;
  AssertEquals('zqhome', MatchRoute('/zq/', FFields, 'mod'));
  FFields.Clear;
  // PathInfo without the leading slash
  AssertEquals('zqhome', MatchRoute('zq', FFields, 'mod'));
  FFields.Clear;
  // ^/zq/?$ does not take longer paths, the catch-all does
  AssertEquals('zqany', MatchRoute('/zq/other', FFields, 'mod'));
  AssertEquals('zqany', FFields.Values['mod']);
end;

procedure TRouteMatchTest.TestFirstMatchWins;
begin
  // ^/zq/item/... is added before the catch-all ^/zq/.+
  AssertEquals('zqitem', MatchRoute('/zq/item/7/abc', FFields, 'mod'));
end;

procedure TRouteMatchTest.TestGroupsAndNamedGroups;
var
  i: integer;
begin
  AssertEquals('zqitem', MatchRoute('/zq/item/42/hello/', FFields, 'mod'));
  AssertEquals('42', FFields.Values['$1']);
  AssertEquals('hello', FFields.Values['$2']);
  AssertEquals('hello', FFields.Values['slug']);
  // the unnamed group must not add a field without a name
  for i := 0 to FFields.Count - 1 do
    AssertTrue('field without a name: ' + FFields[i], FFields.Names[i] <> '');
end;

procedure TRouteMatchTest.TestThirdGroupIsAct;
begin
  AssertEquals('zqact', MatchRoute('/zq/act/x/y/z', FFields, 'mod'));
  AssertEquals('x', FFields.Values['$1']);
  AssertEquals('z', FFields.Values['$3']);
  AssertEquals('z', FFields.Values['act']);
end;

procedure TRouteMatchTest.TestNoMatchLeavesFields;
begin
  FFields.Values['keep'] := '1';
  AssertEquals('', MatchRoute('/nothing-here', FFields, 'mod'));
  AssertEquals(1, FFields.Count);
  AssertEquals('1', FFields.Values['keep']);
end;

procedure TRouteMatchTest.TestUnregisteredModuleIsSkipped;
begin
  // a pattern without a registered module is passed over
  AssertEquals('zqany', MatchRoute('/zq/ghost', FFields, 'mod'));
end;

procedure TRouteMatchTest.TestEmptyModuleVariable;
begin
  AssertEquals('zqhome', MatchRoute('/zq/', FFields, ''));
  AssertEquals(0, FFields.Count);
end;

procedure TRouteMatchTest.TestPatternChangeIsRecompiled;
begin
  // compiled and cached by the first lookup
  AssertEquals('zqre', MatchRoute('/zq/re1', FFields, 'mod'));
  // the same route registered again with another pattern
  Route.Add('zqre', '^/zq/re2$', TZqModule);
  try
    FFields.Clear;
    AssertEquals('zqre', MatchRoute('/zq/re2', FFields, 'mod'));
    FFields.Clear;
    AssertEquals('zqany', MatchRoute('/zq/re1', FFields, 'mod'));
  finally
    Route.Add('zqre', '^/zq/re1$', TZqModule);
  end;
end;

{ TExceptionPageTest }

function WebError(const AStatusText: string; ACode: integer): EFPWebError;
begin
  Result := EFPWebError.Create('Could not determine HTTP module for request "x"');
  Result.StatusCode := ACode;
  Result.StatusText := AStatusText;
end;

procedure TExceptionPageTest.TestStatusOfFpwebNoModule;
var
  e: EFPWebError;
begin
  // custweb raises 400 "Not found" when no module takes the request
  e := WebError('Not found', 400);
  try
    AssertEquals(404, RequestExceptionStatus(e));
  finally
    e.Free;
  end;
  // a real bad request stays 400
  e := WebError('Bad Request', 400);
  try
    AssertEquals(400, RequestExceptionStatus(e));
  finally
    e.Free;
  end;
end;

procedure TExceptionPageTest.TestStatusOfOtherExceptions;
var
  e: Exception;
begin
  e := WebError('Forbidden', 403);
  try
    AssertEquals(403, RequestExceptionStatus(e));
  finally
    e.Free;
  end;
  e := Exception.Create('plain');
  try
    AssertEquals(500, RequestExceptionStatus(e));
    e.HelpContext := 503;
    AssertEquals(503, RequestExceptionStatus(e));
    // a HelpContext that is not an HTTP error status
    e.HelpContext := 42;
    AssertEquals(500, RequestExceptionStatus(e));
  finally
    e.Free;
  end;
  AssertEquals(500, RequestExceptionStatus(nil));
end;

procedure TExceptionPageTest.TestHtmlWithoutDetails;
var
  e: Exception;
  s: string;
begin
  e := Exception.Create('secret: password=abc');
  try
    s := RequestExceptionContent(e, 500, False, False, '  $0001 Get, line 9 of x.pas');
    AssertTrue(Pos('500 Internal Server Error', s) > 0);
    AssertEquals(0, Pos('secret', s));
    AssertEquals(0, Pos('x.pas', s));
    AssertEquals(0, Pos('Exception', s));
  finally
    e.Free;
  end;
end;

procedure TExceptionPageTest.TestHtmlWithDetailsIsEncoded;
var
  e: Exception;
  s: string;
begin
  e := Exception.Create('module "<script>alert(1)</script>"');
  try
    s := RequestExceptionContent(e, 404, True, False, 'trace <b>');
    AssertTrue(Pos('404 Not Found', s) > 0);
    AssertTrue(Pos('&lt;script&gt;', s) > 0);
    AssertTrue(Pos('&quot;', s) > 0);
    AssertEquals(0, Pos('<script>', s));
    AssertTrue(Pos('trace &lt;b&gt;', s) > 0);
  finally
    e.Free;
  end;
end;

procedure TExceptionPageTest.TestJson;
var
  e: Exception;
  j: TJSONData;
begin
  e := Exception.Create('boom');
  try
    j := GetJSON(RequestExceptionContent(e, 500, False, True, 't'));
    try
      AssertEquals(500, TJSONObject(j).Get('code', 0));
      AssertEquals('Internal Server Error', TJSONObject(j).Get('msg', ''));
      AssertNull(TJSONObject(j).Find('message'));
      AssertNull(TJSONObject(j).Find('trace'));
    finally
      j.Free;
    end;
    j := GetJSON(RequestExceptionContent(e, 500, True, True, 't'));
    try
      AssertEquals('Exception', TJSONObject(j).Get('exception', ''));
      AssertEquals('boom', TJSONObject(j).Get('message', ''));
      AssertEquals('t', TJSONObject(j).Get('trace', ''));
    finally
      j.Free;
    end;
  finally
    e.Free;
  end;
end;

initialization
  RegisterTest(TFastPlazHandlerTest);
  RegisterTest(TRouteMatchTest);
  RegisterTest(TExceptionPageTest);
  Route.Add('zqre', '^/zq/re1$', TZqModule);
  Route.Add('zqhome', '^/zq/?$', TZqModule);
  Route.Add('zqitem', '^/zq/item/([0-9]+)/(?P<slug>[a-z]+)/?$', TZqModule);
  Route.Add('zqact', '^/zq/act/([a-z])/([a-z])/([a-z])$', TZqModule);
  // pattern of a module that is not registered (any more)
  Route.Add('zqghost', '^/zq/ghost$', TZqModule);
  ModuleFactory.Delete(ModuleFactory.IndexOfModule('zqghost'));
  Route.Add('zqany', '^/zq/.+', TZqModule);

end.

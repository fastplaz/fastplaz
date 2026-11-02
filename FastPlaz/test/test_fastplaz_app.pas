unit test_fastplaz_app;

{
  fastplaz_app helpers that do not need a running server: CGI/header name
  mapping, CGI variables outside a request and the static file filter of
  the standalone mode.
}

{$mode objfpc}{$H+}

interface

uses
  fastplaz_app, Classes, SysUtils, fpcunit, testregistry;

type

  { TFastPlazAppTest }

  TFastPlazAppTest = class(TTestCase)
  private
    FRoot, FOldDir: string;
    procedure WriteFile(const ARelative: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCGINameToHeaderName;
    procedure TestServerVariableOutsideRequest;
    procedure TestNoRequestOutsideHandler;
    procedure TestStaticAssetsAreFound;
    procedure TestStaticOnlyAssetExtensions;
    procedure TestStaticRejectsTraversalAndHidden;
    procedure TestStaticRejectsZtempAndMissing;
  end;

implementation

uses common;

procedure RemoveTree(const ADir: string);
var
  sr: TSearchRec;
  dir: string;
begin
  dir := IncludeTrailingPathDelimiter(ADir);
  if FindFirst(dir + '*', faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then
        Continue;
      if (sr.Attr and faDirectory) <> 0 then
        RemoveTree(dir + sr.Name)
      else
        DeleteFile(dir + sr.Name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
  RemoveDir(ADir);
end;

procedure TFastPlazAppTest.WriteFile(const ARelative: string);
var
  f: string;
  lst: TStringList;
begin
  f := FRoot + StringReplace(ARelative, '/', DirectorySeparator, [rfReplaceAll]);
  ForceDirectories(ExtractFilePath(f));
  lst := TStringList.Create;
  try
    lst.Text := 'x';
    lst.SaveToFile(f);
  finally
    lst.Free;
  end;
end;

procedure TFastPlazAppTest.SetUp;
begin
  FOldDir := GetCurrentDir;
  FRoot := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'fpz_app_test_' +
    IntToStr(GetProcessID) + DirectorySeparator;
  ForceDirectories(FRoot);
  WriteFile('themes/default/style/style.css');
  WriteFile('themes/default/js/app.js');
  WriteFile('themes/default/img/logo.PNG');
  WriteFile('themes/default/templates/home.html');
  WriteFile('files/a b.css');
  WriteFile('config/config.json');
  WriteFile('config/logo.png');
  WriteFile('ztemp/cache/x.css');
  WriteFile('.hidden/x.css');
  WriteFile('themes/.htaccess');
  WriteFile('app.bin');
  // a sibling directory sharing the prefix of the root
  WriteFile('../fpz_app_test_' + IntToStr(GetProcessID) + 'x/evil.css');
  SetCurrentDir(FRoot);
end;

procedure TFastPlazAppTest.TearDown;
begin
  SetCurrentDir(FOldDir);
  RemoveTree(ExcludeTrailingPathDelimiter(FRoot));
  RemoveTree(ExcludeTrailingPathDelimiter(FRoot) + 'x');
end;

procedure TFastPlazAppTest.TestCGINameToHeaderName;
begin
  AssertEquals('X-Requested-With', CGINameToHeaderName('HTTP_X_REQUESTED_WITH'));
  AssertEquals('User-Agent', CGINameToHeaderName('HTTP_USER_AGENT'));
  AssertEquals('Host', CGINameToHeaderName('http_host'));
  AssertEquals('X-Forwarded-For', CGINameToHeaderName(' HTTP_X_FORWARDED_FOR '));
  AssertEquals('Cookie', CGINameToHeaderName('COOKIE'));
end;

procedure TFastPlazAppTest.TestServerVariableOutsideRequest;
begin
  // no request: the process environment, as in CGI
  _SetEnv('FASTPLAZ_APP_TEST_VAR', 'value');
  try
    AssertEquals('value', ServerVariable('FASTPLAZ_APP_TEST_VAR'));
    AssertEquals('value', Application.EnvironmentVariable['FASTPLAZ_APP_TEST_VAR']);
  finally
    _SetEnv('FASTPLAZ_APP_TEST_VAR', '');
  end;
end;

procedure TFastPlazAppTest.TestNoRequestOutsideHandler;
begin
  AssertNull('request outside a handler', CurrentRequest);
  AssertNull('response outside a handler', CurrentResponse);
  AssertNotNull('application', Application);
end;

procedure TFastPlazAppTest.TestStaticAssetsAreFound;
var
  sep: string;
begin
  sep := DirectorySeparator;
  AssertEquals('themes' + sep + 'default' + sep + 'style' + sep + 'style.css',
    StaticFileForPath('/themes/default/style/style.css'));
  AssertEquals('query string ignored',
    'themes' + sep + 'default' + sep + 'js' + sep + 'app.js',
    StaticFileForPath('/themes/default/js/app.js?v=3'));
  AssertEquals('extension case',
    'themes' + sep + 'default' + sep + 'img' + sep + 'logo.PNG',
    StaticFileForPath('/themes/default/img/logo.PNG'));
  AssertEquals('percent encoded', 'files' + sep + 'a b.css',
    StaticFileForPath('/files/a%20b.css'));
  AssertEquals('double slash', 'themes' + sep + 'default' + sep + 'style' + sep + 'style.css',
    StaticFileForPath('//themes//default/style/style.css'));
  // assets under config/ are allowed, as the template .htaccess does
  AssertEquals('config' + sep + 'logo.png', StaticFileForPath('/config/logo.png'));
end;

procedure TFastPlazAppTest.TestStaticOnlyAssetExtensions;
begin
  AssertEquals('json', '', StaticFileForPath('/config/config.json'));
  AssertEquals('html template', '', StaticFileForPath('/themes/default/templates/home.html'));
  AssertEquals('binary', '', StaticFileForPath('/app.bin'));
  AssertEquals('root', '', StaticFileForPath('/'));
  AssertEquals('directory', '', StaticFileForPath('/themes/default/style'));
end;

procedure TFastPlazAppTest.TestStaticRejectsTraversalAndHidden;
begin
  AssertEquals('dot dot', '', StaticFileForPath('/../fpz_app_test_' +
    IntToStr(GetProcessID) + 'x/evil.css'));
  AssertEquals('dot dot inside', '', StaticFileForPath('/themes/../themes/default/style/style.css'));
  AssertEquals('encoded dot dot', '', StaticFileForPath('/%2e%2e/x/evil.css'));
  AssertEquals('backslash', '', StaticFileForPath('/themes\default\style\style.css'));
  AssertEquals('encoded backslash', '', StaticFileForPath('/themes%5cdefault%5cstyle%5cstyle.css'));
  AssertEquals('drive / stream', '', StaticFileForPath('/c:/windows/x.css'));
  AssertEquals('hidden dir', '', StaticFileForPath('/.hidden/x.css'));
  AssertEquals('htaccess', '', StaticFileForPath('/themes/.htaccess'));
  AssertEquals('nul', '', StaticFileForPath('/themes/default/style/style.css%00.png'));
end;

procedure TFastPlazAppTest.TestStaticRejectsZtempAndMissing;
begin
  AssertEquals('ztemp', '', StaticFileForPath('/ztemp/cache/x.css'));
  AssertEquals('ztemp case', '', StaticFileForPath('/ZTemp/cache/x.css'));
  AssertEquals('missing', '', StaticFileForPath('/themes/default/style/none.css'));
end;

initialization
  RegisterTest(TFastPlazAppTest);
end.

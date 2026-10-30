unit test_regex_tester_lib;

{$mode objfpc}{$H+}

interface

uses
  regex_tester_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { TRegexTesterLibTest }

  TRegexTesterLibTest = class(TTestCase)
  private
    FMatches, FGroups: TStringList;
    function Execute(const AExpression, AText: string): integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyExpression;
    procedure TestEmptyText;
    procedure TestNoMatch;
    procedure TestMultipleMatches;
    procedure TestListsAreCleared;
    procedure TestInvalidExpressionRaises;
    procedure TestSingleGroupIsListed;
    procedure TestNamedAndUnnamedGroups;
    procedure TestGroupsFromFirstMatch;
    procedure TestEmptyMatchTerminates;
    procedure TestEmailRegex;
    procedure TestUrlRegexWithPath;
    procedure TestUrlRegexLongTld;
    procedure TestUrlRegexNoSpaceInHost;
    procedure TestIpRegexValid;
    procedure TestIpRegexInvalid;
  end;

implementation

function TRegexTesterLibTest.Execute(const AExpression, AText: string): integer;
begin
  Result := RegexExecute(AExpression, AText, FMatches, FGroups);
end;

procedure TRegexTesterLibTest.SetUp;
begin
  FMatches := TStringList.Create;
  FGroups := TStringList.Create;
end;

procedure TRegexTesterLibTest.TearDown;
begin
  FGroups.Free;
  FMatches.Free;
end;

procedure TRegexTesterLibTest.TestEmptyExpression;
begin
  AssertEquals(0, Execute('', 'abc'));
  AssertEquals(0, FMatches.Count);
end;

procedure TRegexTesterLibTest.TestEmptyText;
begin
  AssertEquals(0, Execute('\d+', ''));
  AssertEquals(0, FMatches.Count);
end;

procedure TRegexTesterLibTest.TestNoMatch;
begin
  AssertEquals(0, Execute('\d+', 'abc'));
  AssertEquals(0, FMatches.Count);
  AssertEquals(0, FGroups.Count);
end;

procedure TRegexTesterLibTest.TestMultipleMatches;
begin
  AssertEquals(3, Execute('\d+', 'a1 b22 c333'));
  AssertEquals('1', FMatches[0]);
  AssertEquals('22', FMatches[1]);
  AssertEquals('333', FMatches[2]);
end;

procedure TRegexTesterLibTest.TestListsAreCleared;
begin
  FMatches.Add('old');
  FGroups.Add('old');
  AssertEquals(0, Execute('\d+', 'abc'));
  AssertEquals(0, FMatches.Count);
  AssertEquals(0, FGroups.Count);
end;

procedure TRegexTesterLibTest.TestInvalidExpressionRaises;
var
  raised: boolean;
begin
  raised := False;
  try
    Execute('(abc', 'abc');
  except
    on E: Exception do
      raised := True;
  end;
  AssertTrue('invalid expression must raise', raised);
end;

procedure TRegexTesterLibTest.TestSingleGroupIsListed;
begin
  AssertEquals(1, Execute('(\d+)', 'abc 42'));
  AssertEquals(1, FGroups.Count);
  AssertEquals('1 ' + REGEX_NONAME_GROUP + ' = 42', FGroups[0]);
end;

procedure TRegexTesterLibTest.TestNamedAndUnnamedGroups;
begin
  AssertEquals(1, Execute('(?P<user>\w+)@(\w+)', 'luri@carik'));
  AssertEquals(2, FGroups.Count);
  AssertEquals('1 user = luri', FGroups[0]);
  AssertEquals('2 ' + REGEX_NONAME_GROUP + ' = carik', FGroups[1]);
end;

procedure TRegexTesterLibTest.TestGroupsFromFirstMatch;
begin
  AssertEquals(2, Execute('(\d+)', '10 20'));
  AssertEquals(1, FGroups.Count);
  AssertEquals('1 ' + REGEX_NONAME_GROUP + ' = 10', FGroups[0]);
end;

procedure TRegexTesterLibTest.TestEmptyMatchTerminates;
begin
  AssertTrue(Execute('x*', 'ab') > 0);
end;

procedure TRegexTesterLibTest.TestEmailRegex;
begin
  AssertEquals(2, Execute(REGEX_EMAIL, 'mail luri@carik.id or info@fastplaz.com.'));
  AssertEquals('luri@carik.id', FMatches[0]);
  AssertEquals('info@fastplaz.com', FMatches[1]);
end;

procedure TRegexTesterLibTest.TestUrlRegexWithPath;
begin
  AssertEquals(1, Execute(REGEX_URL, 'see http://www.fastplaz.com/docs/index.html now'));
  AssertEquals('http://www.fastplaz.com/docs/index.html', FMatches[0]);
end;

procedure TRegexTesterLibTest.TestUrlRegexLongTld;
begin
  AssertEquals(1, Execute(REGEX_URL, 'https://aksiide.online'));
  AssertEquals('https://aksiide.online', FMatches[0]);
end;

procedure TRegexTesterLibTest.TestUrlRegexNoSpaceInHost;
begin
  // "foo bar" is not a valid host, so nothing must match
  AssertEquals(0, Execute(REGEX_URL, 'https://foo bar.com'));
  AssertEquals(1, Execute(REGEX_URL, 'https://foo.bar.com'));
  AssertEquals('https://foo.bar.com', FMatches[0]);
end;

procedure TRegexTesterLibTest.TestIpRegexValid;
begin
  AssertEquals(1, Execute(REGEX_IP, '127.0.0.1'));
  AssertEquals(1, Execute(REGEX_IP, '192.168.10.254'));
end;

procedure TRegexTesterLibTest.TestIpRegexInvalid;
begin
  AssertEquals(0, Execute(REGEX_IP, '256.1.1.1'));
  AssertEquals(0, Execute(REGEX_IP, '1.2.3'));
end;

initialization
  RegisterTest(TRegexTesterLibTest);

end.

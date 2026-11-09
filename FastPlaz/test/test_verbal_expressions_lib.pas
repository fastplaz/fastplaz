unit test_verbal_expressions_lib;

{$mode objfpc}{$H+}

interface

uses
  verbal_expressions_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { TVerbalExpressionsTest }

  TVerbalExpressionsTest = class(TTestCase)
  private
    FVE: TVerbalExpressions;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUsageExample;
    procedure TestDigit;
    procedure TestWord;
    procedure TestTab;
    procedure TestLineBreak;
    procedure TestSomethingBut;
    procedure TestAnythingBut;
    procedure TestAnyOfEscapesDash;
    procedure TestRangeArray;
    procedure TestSanitize;
    procedure TestReplaceLiteral;
    procedure TestReplaceExample;
    procedure TestMultilineModifier;
    procedure TestIgnoreCaseModifier;
    procedure TestStartEndOfLineToggle;
    procedure TestClean;
    procedure TestInvalidRawExpressionDoesNotRaise;
  end;

implementation

procedure TVerbalExpressionsTest.SetUp;
begin
  FVE := TVerbalExpressions.Create;
end;

procedure TVerbalExpressionsTest.TearDown;
begin
  FVE.Free;
end;

procedure TVerbalExpressionsTest.TestUsageExample;
begin
  FVE.StartOfLine.Has('http').Maybe('s').Has('://').Maybe('www.').AnythingBut(' ').EndOfLine(False);
  AssertTrue(FVE.IsMatch('https://fastplaz.com'));
  AssertTrue(FVE.IsMatch('http://www.fastplaz.com/x'));
  AssertFalse(FVE.IsMatch('ftp://fastplaz.com'));
end;

procedure TVerbalExpressionsTest.TestDigit;
begin
  // used to be the regex \\d (a backslash followed by 'd')
  FVE.StartOfLine.Digit.EndOfLine;
  AssertEquals('^\d$', FVE.Expression);
  AssertTrue(FVE.IsMatch('5'));
  AssertFalse(FVE.IsMatch('\d'));
  AssertFalse(FVE.IsMatch('a'));
end;

procedure TVerbalExpressionsTest.TestWord;
begin
  FVE.StartOfLine.word.EndOfLine;
  AssertTrue(FVE.IsMatch('x'));
  AssertTrue(FVE.IsMatch('_'));
  AssertFalse(FVE.IsMatch('-'));
end;

procedure TVerbalExpressionsTest.TestTab;
begin
  FVE.Tab;
  AssertTrue(FVE.IsMatch('a' + #9 + 'b'));
  AssertFalse(FVE.IsMatch('a b'));
end;

procedure TVerbalExpressionsTest.TestLineBreak;
begin
  FVE.LineBreak;
  AssertTrue(FVE.IsMatch('a' + #10 + 'b'));
  AssertTrue(FVE.IsMatch('a' + #13#10 + 'b'));
  AssertFalse(FVE.IsMatch('ab'));
  FVE.Clean;
  FVE.BR;
  AssertTrue(FVE.IsMatch(#10));
end;

procedure TVerbalExpressionsTest.TestSomethingBut;
begin
  // used to require a literal 'b' after the run
  FVE.StartOfLine.SomethingBut(' ').EndOfLine;
  AssertTrue(FVE.IsMatch('hello'));
  AssertFalse(FVE.IsMatch('hel lo'));
  AssertFalse(FVE.IsMatch(''));
end;

procedure TVerbalExpressionsTest.TestAnythingBut;
begin
  FVE.StartOfLine.AnythingBut('xyz').EndOfLine;
  // (TRegExpr never matches an empty input string, so '' is not tested)
  AssertTrue(FVE.IsMatch('abc'));
  AssertFalse(FVE.IsMatch('aybc'));
end;

procedure TVerbalExpressionsTest.TestAnyOfEscapesDash;
begin
  // AnyOf('a-z') means the three characters a, - and z, not a range
  FVE.StartOfLine.AnyOf('a-z').EndOfLine;
  AssertTrue(FVE.IsMatch('a'));
  AssertTrue(FVE.IsMatch('-'));
  AssertTrue(FVE.IsMatch('z'));
  AssertFalse(FVE.IsMatch('m'));
  FVE.Clean;
  FVE.StartOfLine.AnythingBut('a-z').EndOfLine;
  AssertTrue(FVE.IsMatch('mmm'));
  AssertFalse(FVE.IsMatch('m-m'));
end;

procedure TVerbalExpressionsTest.TestRangeArray;
begin
  FVE.StartOfLine.Range(['a-c', '0-2']).EndOfLine;
  AssertEquals('^[a-c0-2]$', FVE.Expression);
  AssertTrue(FVE.IsMatch('b'));
  AssertTrue(FVE.IsMatch('1'));
  AssertFalse(FVE.IsMatch('d'));
end;

procedure TVerbalExpressionsTest.TestSanitize;
begin
  AssertEquals('a\.b\*c\[d\]e\-f\/g', FVE.Sanitize('a.b*c[d]e-f/g'));
  AssertEquals('', FVE.Sanitize(''));
  FVE.Find('1.5');
  AssertTrue(FVE.IsMatch('1.5'));
  AssertFalse('dot is literal', FVE.IsMatch('1x5'));
end;

procedure TVerbalExpressionsTest.TestReplaceLiteral;
begin
  // $0 / $1 in the replacement used to be expanded
  FVE.Find('a');
  AssertEquals('$0$0 b', FVE.Replace('a b', '$0$0'));
  AssertEquals('\n b', FVE.Replace('a b', '\n'));
end;

procedure TVerbalExpressionsTest.TestReplaceExample;
begin
  AssertEquals('We have a blue house', FVE.Find('red').Replace('We have a red house', 'blue'));
  FVE.Clean;
  FVE.Find('bird');
  AssertEquals('Replace duck with a duck', FVE.Replace('Replace bird with a duck', 'duck'));
  AssertEquals('no match unchanged', 'xyz', FVE.Replace('xyz', 'q'));
end;

procedure TVerbalExpressionsTest.TestMultilineModifier;
begin
  FVE.StartOfLine.Has('two').EndOfLine;
  AssertTrue('m is the default', FVE.IsMatch('one' + #10 + 'two' + #10 + 'three'));
  FVE.Modifiers := '';
  AssertFalse('without m ^$ anchor the whole string', FVE.IsMatch('one' + #10 + 'two' + #10 + 'three'));
  AssertTrue(FVE.IsMatch('two'));
end;

procedure TVerbalExpressionsTest.TestIgnoreCaseModifier;
begin
  FVE.Has('abc');
  AssertFalse(FVE.IsMatch('ABC'));
  FVE.Modifiers := 'mi';
  AssertTrue(FVE.IsMatch('xABCx'));
end;

procedure TVerbalExpressionsTest.TestStartEndOfLineToggle;
begin
  FVE.StartOfLine.Has('a').EndOfLine;
  AssertEquals('^(a)$', FVE.Expression);
  FVE.StartOfLine(False).EndOfLine(False);
  AssertEquals('(a)', FVE.Expression);
  AssertTrue(FVE.IsMatch('xax'));
end;

procedure TVerbalExpressionsTest.TestClean;
begin
  FVE.StartOfLine.Has('a').EndOfLine;
  FVE.Modifiers := 'i';
  FVE.Clean;
  AssertEquals('', FVE.Expression);
  AssertEquals('', FVE.Source);
  AssertEquals('m', FVE.Modifiers);
  AssertEquals('', FVE.LastAdded);
end;

procedure TVerbalExpressionsTest.TestInvalidRawExpressionDoesNotRaise;
begin
  FVE.Has('(unclosed', False);
  AssertFalse(FVE.IsMatch('(unclosed'));
  AssertEquals('(unclosed', FVE.Replace('(unclosed', 'x'));
end;

initialization
  RegisterTest(TVerbalExpressionsTest);

end.

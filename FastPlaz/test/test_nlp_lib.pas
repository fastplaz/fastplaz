unit test_nlp_lib;

{$mode objfpc}{$H+}

interface

uses
  nlp_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { TNLPLibTest }

  TNLPLibTest = class(TTestCase)
  private
    FNLP: TNLP;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIdenticalIsOne;
    procedure TestDisjointIsZero;
    procedure TestPartialOverlap;
    procedure TestEmptyInputIsZero;
    procedure TestNonLatinOnlyIsZero;
    procedure TestRepeatedWordsStayBounded;
    procedure TestStopWordsIgnored;
    procedure TestCustomStopWords;
    procedure TestStopWordsAssignCopies;
    procedure TestCaseAndPunctuation;
    procedure TestMethodNone;
  end;

implementation

const
  EPS = 1e-9;

procedure TNLPLibTest.SetUp;
begin
  FNLP := TNLP.Create;
end;

procedure TNLPLibTest.TearDown;
begin
  FNLP.Free;
end;

procedure TNLPLibTest.TestIdenticalIsOne;
begin
  AssertEquals(1.0, FNLP.Similarity('saya makan nasi', 'saya makan nasi'), EPS);
  AssertEquals(1.0, FNLP.Similarity('nasi makan saya', 'saya makan nasi'), EPS);
end;

procedure TNLPLibTest.TestDisjointIsZero;
begin
  AssertEquals(0.0, FNLP.Similarity('merah kuning', 'hijau biru'), EPS);
end;

procedure TNLPLibTest.TestPartialOverlap;
begin
  // {a,b} vs {a,c}: 1 / sqrt(2*2) = 0.5
  AssertEquals(0.5, FNLP.Similarity('a b', 'a c'), EPS);
  // {a,b,c} vs {a}: 1 / sqrt(3*1)
  AssertEquals(1 / sqrt(3), FNLP.Similarity('a b c', 'a'), EPS);
end;

procedure TNLPLibTest.TestEmptyInputIsZero;
begin
  // used to raise a floating point exception (division by zero) and leak
  AssertEquals(0.0, FNLP.Similarity('', 'abc'), EPS);
  AssertEquals(0.0, FNLP.Similarity('abc', ''), EPS);
  AssertEquals(0.0, FNLP.Similarity('', ''), EPS);
  AssertEquals(0.0, FNLP.Similarity('   ', 'abc'), EPS);
end;

procedure TNLPLibTest.TestNonLatinOnlyIsZero;
begin
  AssertEquals(0.0, FNLP.Similarity('123 456', 'abc'), EPS);
  AssertEquals(0.0, FNLP.Similarity('123', '456'), EPS);
end;

procedure TNLPLibTest.TestRepeatedWordsStayBounded;
var
  s: double;
begin
  // used to return 2.0 for ('a a a a', 'a')
  s := FNLP.Similarity('a a a a', 'a');
  AssertEquals(1.0, s, EPS);
  s := FNLP.Similarity('a  b', 'a b');
  AssertTrue(s <= 1.0);
  AssertEquals(1.0, s, EPS);
end;

procedure TNLPLibTest.TestStopWordsIgnored;
begin
  // 'di' and 'yang' are built-in stop words
  AssertEquals(1.0, FNLP.Similarity('rumah di kota', 'rumah kota yang'), EPS);
  AssertEquals(0.0, FNLP.Similarity('di yang', 'di yang'), EPS);
  AssertTrue(FNLP.IsStopWords('di'));
  AssertFalse(FNLP.IsStopWords('rumah'));
end;

procedure TNLPLibTest.TestCustomStopWords;
begin
  FNLP.StopWords.Add('rumah');
  AssertTrue(FNLP.IsStopWords('rumah'));
  AssertEquals(1.0, FNLP.Similarity('rumah kota', 'kota'), EPS);
end;

procedure TNLPLibTest.TestStopWordsAssignCopies;
var
  mine: TStringList;
begin
  // assigning used to swap the owned list (leak + double free)
  mine := TStringList.Create;
  try
    mine.Add('kota');
    FNLP.StopWords := mine;
    AssertTrue(FNLP.StopWords <> mine);
    AssertTrue(FNLP.IsStopWords('kota'));
  finally
    mine.Free;
  end;
  AssertTrue('still usable after the source was freed', FNLP.IsStopWords('kota'));
end;

procedure TNLPLibTest.TestCaseAndPunctuation;
begin
  AssertEquals(1.0, FNLP.Similarity('Saya, Makan!', 'saya makan'), EPS);
end;

procedure TNLPLibTest.TestMethodNone;
begin
  AssertEquals(0.0, FNLP.Similarity('a', 'a', smNone), EPS);
end;

initialization
  RegisterTest(TNLPLibTest);

end.

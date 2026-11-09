unit test_math_lib;

{$mode objfpc}{$H+}

interface

uses
  math_lib, Math, Classes, SysUtils, fpcunit, testregistry;

type

  { TMathLibTest }

  TMathLibTest = class(TTestCase)
  published
    procedure TestFact;
    procedure TestFactNegativeIsNaN;
    procedure TestFactorialAlias;
    procedure TestPower;
    procedure TestPowerNegativeExponent;
    procedure TestPowerZeroToNegativeIsNaN;
    procedure TestChooseKnownValues;
    procedure TestChooseZeroK;
    procedure TestChooseSymmetry;
    procedure TestChooseInvalidIsZero;
    procedure TestStirlingKnownValues;
    procedure TestStirlingNegativeIsNaN;
  end;

implementation

const
  EPS = 1e-9;

procedure TMathLibTest.TestFact;
begin
  AssertEquals('0!', 1, Fact(0), EPS);
  AssertEquals('1!', 1, Fact(1), EPS);
  AssertEquals('5!', 120, Fact(5), EPS);
  AssertEquals('10!', 3628800, Fact(10), EPS);
end;

procedure TMathLibTest.TestFactNegativeIsNaN;
begin
  AssertTrue('Fact(-1) is NaN', IsNan(Fact(-1)));
  AssertTrue('Fact(-100) is NaN', IsNan(Fact(-100)));
end;

procedure TMathLibTest.TestFactorialAlias;
begin
  AssertEquals(Fact(7), Factorial(7), EPS);
end;

// math_lib.Power is hidden by Math.Power when Math comes later in the uses
// clause, so it is qualified here.
procedure TMathLibTest.TestPower;
begin
  AssertEquals('2^0', 1, math_lib.Power(2, 0), EPS);
  AssertEquals('2^10', 1024, math_lib.Power(2, 10), EPS);
  AssertEquals('(-1)^3', -1, math_lib.Power(-1, 3), EPS);
  AssertEquals('(-1)^4', 1, math_lib.Power(-1, 4), EPS);
  AssertEquals('0^5', 0, math_lib.Power(0, 5), EPS);
  AssertEquals('0^0', 1, math_lib.Power(0, 0), EPS);
end;

procedure TMathLibTest.TestPowerNegativeExponent;
begin
  AssertEquals('2^-1', 0.5, math_lib.Power(2, -1), EPS);
  AssertEquals('4^-2', 0.0625, math_lib.Power(4, -2), EPS);
end;

procedure TMathLibTest.TestPowerZeroToNegativeIsNaN;
begin
  AssertTrue(IsNan(math_lib.Power(0, -1)));
end;

procedure TMathLibTest.TestChooseKnownValues;
begin
  AssertEquals('C(5,2)', 10, Choose(5, 2), EPS);
  AssertEquals('C(5,1)', 5, Choose(5, 1), EPS);
  AssertEquals('C(5,5)', 1, Choose(5, 5), EPS);
  AssertEquals('C(10,3)', 120, Choose(10, 3), EPS);
  AssertEquals('C(20,10)', 184756, Choose(20, 10), EPS);
  AssertEquals('C(0,0)', 1, Choose(0, 0), EPS);
end;

procedure TMathLibTest.TestChooseZeroK;
begin
  // used to return n + 1
  AssertEquals('C(5,0)', 1, Choose(5, 0), EPS);
  AssertEquals('C(1,0)', 1, Choose(1, 0), EPS);
  AssertEquals('C(100,0)', 1, Choose(100, 0), EPS);
end;

procedure TMathLibTest.TestChooseSymmetry;
var
  k: integer;
begin
  for k := 0 to 12 do
    AssertEquals('C(12,k) = C(12,12-k) for k=' + IntToStr(k),
      Choose(12, k), Choose(12, 12 - k), EPS);
end;

procedure TMathLibTest.TestChooseInvalidIsZero;
begin
  AssertEquals('n < k', 0, Choose(3, 5), EPS);
  AssertEquals('negative n', 0, Choose(-1, 0), EPS);
  AssertEquals('negative k', 0, Choose(5, -1), EPS);
end;

procedure TMathLibTest.TestStirlingKnownValues;
begin
  // Stirling numbers of the second kind
  AssertEquals('S(0,0)', 1, Stirling(0, 0), EPS);
  AssertEquals('S(3,2)', 3, Stirling(3, 2), EPS);
  AssertEquals('S(4,2)', 7, Stirling(4, 2), EPS);
  AssertEquals('S(5,3)', 25, Stirling(5, 3), EPS);
  AssertEquals('S(6,3)', 90, Stirling(6, 3), EPS);
  AssertEquals('S(5,5)', 1, Stirling(5, 5), EPS);
  AssertEquals('S(3,0)', 0, Stirling(3, 0), EPS);
end;

procedure TMathLibTest.TestStirlingNegativeIsNaN;
begin
  AssertTrue(IsNan(Stirling(-1, 2)));
  AssertTrue(IsNan(Stirling(2, -1)));
end;

initialization
  RegisterTest(TMathLibTest);

end.

unit math_lib;

{
  USAGE
  Factorial:
    n := fact(10);

  Invalid arguments (negative n or k) return NaN for Fact, Factorial and
  Stirling; Choose returns 0 as the binomial coefficient is defined that way.
  Power(m, p) with a negative p returns 1 / m^-p, or NaN when m is 0.
}

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  Math, SysUtils;

function Fact(n: integer): extended;
function Factorial(n: integer): extended;
function Power(m, p: integer): extended;
function Choose(n, k: integer): extended;
function Stirling(n, k: integer): extended;

implementation

function Fact(n: integer): extended;
var
  i: integer;
begin
  if n < 0 then
    Exit(Math.NaN);
  Result := 1;
  for i := 2 to n do
    Result := Result * i;
end;

function Factorial(n: integer): extended;
begin
  Result := Fact(n);
end;

function Power(m, p: integer): extended;
var
  i: integer;
begin
  if (p < 0) and (m = 0) then
    Exit(Math.NaN);
  Result := 1;
  for i := 1 to Abs(p) do
    Result := Result * extended(m);
  if p < 0 then
    Result := 1 / Result;
end;

function Choose(n, k: integer): extended;
var
  delta, iMax, i: integer;
begin
  Result := 0;
  if (n < 0) or (k < 0) or (n < k) then
    Exit;
  if (k = 0) or (n = k) then
    Exit(1);
  // multiply the shorter side: C(n,k) = C(n,n-k)
  if k < (n - k) then
  begin
    delta := n - k;
    iMax := k;
  end
  else
  begin
    delta := k;
    iMax := n - k;
  end;
  Result := extended(delta) + extended(1);
  for i := 2 to iMax do
    Result := (Result * extended(delta + i)) / extended(i);
end;

function Stirling(n, k: integer): extended;
var
  sum, a, b, c: extended;
  j: integer;
begin
  if (n < 0) or (k < 0) then
    Exit(Math.NaN);
  sum := 0;
  for j := 0 to k do
  begin
    a := Power(-1, k - j);
    b := Choose(k, j);
    c := Power(j, n);
    sum := sum + (a * b * c);
  end;
  Result := sum / Factorial(k);
end;

end.

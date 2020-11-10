unit math_lib;

{
  USAGE
  Factorial:
    n := fact(10);

}

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  Math,
  strutils, Classes, SysUtils;

function Fact(n: integer): extended;
function Factorial(n: integer): extended;
function Power(m, p: integer): extended;
function Choose(n, k: integer): extended;
function Stirling(n, k: integer): extended;

implementation

function Fact(n: integer): extended;
begin
  if (n = 0) then
    fact := 1
  else
    fact := n * fact(n - 1);
end;

function Factorial(n: integer): extended;
begin
  factorial := fact(n);
end;

function Power(m, p: integer): extended;
var
  e: extended; (*result is a reserved word*)
  i: integer;
begin
  e := 1;
  for i := 1 to p do
    e := e * extended(m);
  power := e;
end;

function Choose(n, k: integer): extended;
var
  delta, iMax, i: integer;
  ans: extended;
begin
  if (n < 0) or (k < 0) then
    exit; (*note that in our form version of this we don't allow the user
             to enter a negative number, however they could copy and paste one
             in we should either check and prevent that*)
  if n < k then
  begin
    Choose := 0;
    exit;
  end;
  if n = k then
  begin
    Choose := 1;
    exit;
  end;
  if k < (n - k) then
  begin
    delta := n - k;
    imax := k;
  end
  else
  begin
    delta := k;
    Imax := n - k;
  end;
  ans := extended(delta) + extended(1);
  for i := 2 to iMax do
    ans := (ans * (extended(delta + i))) / extended(i);
  Choose := ans;
end;

function Stirling(n, k: integer): extended;
var
  sum, a, b, c: extended;
  j: integer;
begin
  sum := 0;
  for j := 0 to k do
  begin
    a := Power(-1, k - j); (*could write a procedure that checks if
                             k-j is even or odd if even then return 1
                             if odd -1 but this is way the formula usually
                             shows it*)
    b := Choose(k, j);
    c := Power(j, n);
    sum := sum + (a * b * c);
  end;
  Stirling := sum / factorial(k);
end;

end.







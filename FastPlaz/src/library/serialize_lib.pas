unit serialize_lib;

{
  CREDIT: unknown resource

}

{$mode objfpc}{$H+}
{ $include define.inc}

interface

function serialize(v: variant): string;
function unserialize(const s: string): variant;

implementation

uses Variants, SysUtils, common;

function unserialize(const s: string): variant;
var
  i, l: integer;

  function consumefield: variant; forward;

  function consumearray: variant;
  var
    j, k, fields: integer;
  begin
    Inc(i, 2);
    j := i;
    while (j < l) and (s[j] in ['0'..'9']) do
      Inc(j);
    fields := strtointdef(copy(s, i, j - i), 0);
    i := j + 2;
    Result := vararraycreate([0, fields - 1], varvariant);
    for j := 0 to fields - 1 do
    begin
      Inc(i, 2);
      k := i;
      while (k < l) and (s[k] in ['0'..'9']) do
        Inc(k);
      i := k + 1;
      Result[j] := consumefield;
    end;
  end;

  function consumestring: variant;
  var
    j, len: integer;
    s2: string;
  begin
    Inc(i, 2);
    j := i;
    while (j < l) and (s[j] in ['0'..'9']) do
      Inc(j);
    len := strtointdef(copy(s, i, j - i), 0);
    i := j;
    s2 := copy(s, i + 2, len);
    Result := s2;
    Inc(i, len + 4); // +2 are the quotes, + two :
  end;

  function consumeinteger: variant;
  var
    j, len: integer;
  begin
    Inc(i, 2);
    j := i;
    while (j < l) and (s[j] in ['-', '0'..'9']) do
      Inc(j);
    Result := strtointdef(copy(s, i, j - i + 1), 0);
    i := j + 1;
  end;

  function consumeboolean: variant;
  var
    j, len: integer;
  begin
    Inc(i, 2);
    j := i;
    while (j < l) and (s[j] in ['-', '0'..'9']) do
      Inc(j);
    //if j <> (i + 1) then
    //  raise EConvertError.Create('not a boolean');
    Result := s[i] = '1';
    i := j + 1;
  end;

  function consumedouble: variant;
  var
    j, len: integer;
  begin
    Inc(i, 2);
    j := i;
    while (j < l) and (s[j] in ['.', 'E', 'e', '-', '0'..'9']) do
      Inc(j);
    Result := strtofloat(copy(s, i, j - i));
    i := j + 1;
  end;

  function consumefield: variant;
  begin
    case s[i] of
      'a': Result := consumearray;
      's': Result := consumestring;
      'b': Result := consumeboolean;
      'i': Result := consumeinteger;
      'd': Result := consumedouble;
      'n':
      begin
        Result := varnull;
        Inc(i, 2); // ; also
      end;
    end;
  end;

begin
  i := 1;
  l := length(s);
  try
    //Result := varnull;
    Result := '';
    if i < l then  // tokens are >1
      Result := consumefield;
  except
  end;
end;

function serialize(v: variant): string;

  function servariant(v: variant): string; forward;

  function servarray(v: variant): string;
  var
    i: integer;
  begin
    Result := 'a:' + IntToStr(vararrayhighbound(v, 1) -
      vararraylowbound(v, 1) + 1) + ':{';
    ;
    for i := VarArrayLowBound(v, 1) to vararrayhighbound(v, 1) do
      Result := Result + 'i:' + IntToStr(i) + ':' + servariant(v[i]) + ';';
    Result := Result + '}';
  end;

const
  booltonumber: array[boolean] of char = ('0', '1');

  function servariant(v: variant): string;
  var
    i: integer;

  begin
    Result := '';
    i := VarType(v);
    if (i and vararray) = vararray then
      i := vararray;
    case i of
      varinteger: Result := 'i:' + vartostr(v);  // was inttostr(vr)
      varolestr,
      varstring: Result := 's:' + IntToStr(length(v)) + ':"' + vartostr(v) + '"';
      // why vartostr needed?
      vararray: Result := servarray(v);
      varnull: Result := 'N';
      vardouble: Result := 'd:' + floattostr(extended(v));   // why typecast needed?
      varboolean: Result := 'b:' + booltonumber[boolean(v)];  // why typecast needed?
    end;
  end;

begin
  try
    Result := servariant(v);
  except
  end;
end;

end.

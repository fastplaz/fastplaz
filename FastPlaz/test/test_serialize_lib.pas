unit test_serialize_lib;

{$mode objfpc}{$H+}

interface

uses
  serialize_lib, Variants, Classes, SysUtils, fpcunit, testregistry;

type

  { TSerializeLibTest }

  TSerializeLibTest = class(TTestCase)
  published
    // writer
    procedure TestSerializeInteger;
    procedure TestSerializeOtherIntegerTypes;
    procedure TestSerializeString;
    procedure TestSerializeUnicodeString;
    procedure TestSerializeBoolean;
    procedure TestSerializeDouble;
    procedure TestSerializeNull;
    procedure TestSerializeArray;
    procedure TestSerializeNestedArray;
    // reader, PHP format
    procedure TestUnserializeScalars;
    procedure TestUnserializeArray;
    procedure TestUnserializeIntegerInArray;
    procedure TestUnserializeNullInArray;
    procedure TestUnserializeStringWithSpecials;
    // reader, legacy FastPlaz format
    procedure TestUnserializeLegacyTopLevel;
    procedure TestUnserializeLegacyArray;
    // robustness
    procedure TestUnserializeMalformed;
    procedure TestUnserializeHugeArrayLength;
    procedure TestRoundTrip;
  end;

implementation

procedure TSerializeLibTest.TestSerializeInteger;
begin
  AssertEquals('i:5;', serialize(5));
  AssertEquals('i:-12;', serialize(-12));
  AssertEquals('i:0;', serialize(0));
end;

procedure TSerializeLibTest.TestSerializeOtherIntegerTypes;
var
  v: variant;
begin
  v := Int64(9007199254740993);
  AssertEquals('i:9007199254740993;', serialize(v));
  v := Byte(200);
  AssertEquals('i:200;', serialize(v));
  v := SmallInt(-3);
  AssertEquals('i:-3;', serialize(v));
end;

procedure TSerializeLibTest.TestSerializeString;
begin
  AssertEquals('s:5:"hello";', serialize('hello'));
  AssertEquals('s:0:"";', serialize(''));
  AssertEquals('s:7:"a";b:"c";', serialize('a";b:"c'));
end;

procedure TSerializeLibTest.TestSerializeUnicodeString;
var
  v: variant;
begin
  // varUString used to produce '' silently
  v := UnicodeString('abc');
  AssertEquals('s:3:"abc";', serialize(v));
  // length is in bytes: "é" is two bytes in UTF-8
  AssertEquals('s:6:"Halo' + #$C3#$A9 + '";', serialize('Halo' + #$C3#$A9));
end;

procedure TSerializeLibTest.TestSerializeBoolean;
begin
  AssertEquals('b:1;', serialize(True));
  AssertEquals('b:0;', serialize(False));
end;

procedure TSerializeLibTest.TestSerializeDouble;
begin
  AssertEquals('d:1.5;', serialize(1.5));
  AssertEquals('d:-0.25;', serialize(-0.25));
  AssertEquals('D:1E20;', UpperCase(serialize(1e20)));
end;

procedure TSerializeLibTest.TestSerializeNull;
begin
  AssertEquals('N;', serialize(Null));
  AssertEquals('N;', serialize(Unassigned));
end;

procedure TSerializeLibTest.TestSerializeArray;
var
  a: variant;
begin
  a := VarArrayCreate([0, 2], varVariant);
  a[0] := 5;
  a[1] := 'x';
  a[2] := True;
  AssertEquals('a:3:{i:0;i:5;i:1;s:1:"x";i:2;b:1;}', serialize(a));
  a := VarArrayCreate([0, -1], varVariant);
  AssertEquals('a:0:{}', serialize(a));
end;

procedure TSerializeLibTest.TestSerializeNestedArray;
var
  a, inner: variant;
begin
  inner := VarArrayCreate([0, 1], varVariant);
  inner[0] := 1;
  inner[1] := 2;
  a := VarArrayCreate([0, 1], varVariant);
  a[0] := inner;
  a[1] := Null;
  AssertEquals('a:2:{i:0;a:2:{i:0;i:1;i:1;i:2;}i:1;N;}', serialize(a));
end;

procedure TSerializeLibTest.TestUnserializeScalars;
var
  v: variant;
begin
  v := unserialize('i:5;');
  AssertEquals(5, Integer(v));
  v := unserialize('i:-7;');
  AssertEquals(-7, Integer(v));
  v := unserialize('s:5:"hello";');
  AssertEquals('hello', string(v));
  v := unserialize('b:1;');
  AssertTrue(Boolean(v));
  v := unserialize('b:0;');
  AssertFalse(Boolean(v));
  v := unserialize('d:1.5;');
  AssertEquals(1.5, Double(v), 1e-12);
  v := unserialize('N;');
  AssertTrue(VarIsNull(v));
end;

procedure TSerializeLibTest.TestUnserializeArray;
var
  v: variant;
begin
  v := unserialize('a:3:{i:0;i:5;i:1;s:1:"x";i:2;b:1;}');
  AssertTrue(VarIsArray(v));
  AssertEquals(0, VarArrayLowBound(v, 1));
  AssertEquals(2, VarArrayHighBound(v, 1));
  AssertEquals(5, Integer(v[0]));
  AssertEquals('x', string(v[1]));
  AssertTrue(Boolean(v[2]));
  v := unserialize('a:0:{}');
  AssertTrue(VarIsArray(v));
  AssertEquals(-1, VarArrayHighBound(v, 1));
end;

procedure TSerializeLibTest.TestUnserializeIntegerInArray;
var
  v: variant;
begin
  // integers inside arrays used to come back as 0
  v := unserialize('a:2:{i:0;i:42;i:1;i:-1;}');
  AssertEquals(42, Integer(v[0]));
  AssertEquals(-1, Integer(v[1]));
end;

procedure TSerializeLibTest.TestUnserializeNullInArray;
var
  v: variant;
begin
  v := unserialize('a:3:{i:0;N;i:1;i:2;i:2;s:1:"z";}');
  AssertTrue(VarIsNull(v[0]));
  AssertEquals(2, Integer(v[1]));
  AssertEquals('z', string(v[2]));
end;

procedure TSerializeLibTest.TestUnserializeStringWithSpecials;
var
  v: variant;
begin
  v := unserialize('s:7:"a";b:"c";');
  AssertEquals('a";b:"c', string(v));
  v := unserialize('s:6:"Halo' + #$C3#$A9 + '";');
  AssertEquals('Halo' + #$C3#$A9, string(v));
  v := unserialize('s:0:"";');
  AssertEquals('', string(v));
end;

procedure TSerializeLibTest.TestUnserializeLegacyTopLevel;
var
  v: variant;
begin
  // the old writer omitted the closing ';' and wrote 'N'
  v := unserialize('i:5');
  AssertEquals(5, Integer(v));
  v := unserialize('d:1.5');
  AssertEquals('last digit used to be dropped', 1.5, Double(v), 1e-12);
  v := unserialize('s:5:"hello"');
  AssertEquals('hello', string(v));
  v := unserialize('b:1');
  AssertTrue(Boolean(v));
  v := unserialize('N');
  AssertTrue(VarIsNull(v));
end;

procedure TSerializeLibTest.TestUnserializeLegacyArray;
var
  v: variant;
begin
  // old array format: keys terminated by ':' instead of ';'
  v := unserialize('a:3:{i:0:i:5;i:1:s:1:"x";i:2:d:2.5;}');
  AssertEquals(5, Integer(v[0]));
  AssertEquals('x', string(v[1]));
  AssertEquals(2.5, Double(v[2]), 1e-12);
end;

procedure TSerializeLibTest.TestUnserializeMalformed;
begin
  AssertEquals('', string(unserialize('')));
  AssertEquals('', string(unserialize('x:1;')));
  AssertEquals('', string(unserialize('i:abc;')));
  AssertEquals('', string(unserialize('s:10:"short";')));
  AssertEquals('', string(unserialize('a:2:{i:0;i:1;}')));
  AssertEquals('', string(unserialize('a:1:{i:5;i:1;}')));
  AssertEquals('', string(unserialize('i:5;garbage')));
  AssertEquals('', string(unserialize('a:1:{i:0;a:1:{i:0;}')));
end;

procedure TSerializeLibTest.TestUnserializeHugeArrayLength;
begin
  // must fail fast without trying to allocate the array
  AssertEquals('', string(unserialize('a:999999999:{')));
  AssertEquals('', string(unserialize('s:999999999:"x";')));
end;

procedure TSerializeLibTest.TestRoundTrip;
var
  a, inner, v: variant;
begin
  inner := VarArrayCreate([0, 2], varVariant);
  inner[0] := 'nested "quoted";';
  inner[1] := -3.75;
  inner[2] := Null;
  a := VarArrayCreate([0, 4], varVariant);
  a[0] := 123;
  a[1] := 'text';
  a[2] := False;
  a[3] := inner;
  a[4] := Int64(1) shl 40;
  v := unserialize(serialize(a));
  AssertTrue(VarIsArray(v));
  AssertEquals(123, Integer(v[0]));
  AssertEquals('text', string(v[1]));
  AssertFalse(Boolean(v[2]));
  AssertEquals('nested "quoted";', string(v[3][0]));
  AssertEquals(-3.75, Double(v[3][1]), 1e-12);
  AssertTrue(VarIsNull(v[3][2]));
  AssertEquals(Int64(1) shl 40, Int64(v[4]));
end;

initialization
  RegisterTest(TSerializeLibTest);

end.

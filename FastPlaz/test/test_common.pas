unit test_common;

{$mode objfpc}{$H+}

interface

uses
  common, Classes, SysUtils, fpcunit, testregistry;

type

  { TCommonTest }

  TCommonTest = class(TTestCase)
  published
    // base64
    procedure TestBase64EncodePadding;
    procedure TestBase64RoundTrip;
    procedure TestBase64EncodeEmpty;
    // html
    procedure TestHTMLDecodeNamedEntities;
    procedure TestHTMLDecodeNumericEntities;
    procedure TestHTMLDecodeUnknownEntityKept;
    procedure TestHTMLDecodeUnterminatedAmpersand;
    procedure TestHTMLDecodePlainText;
    procedure TestStripHTML;
    procedure TestStripHTMLUnterminatedTag;
    // numbers in strings
    procedure TestStripNumber;
    procedure TestStripNonNumber;
    procedure TestFormatPhoneNumber;
    procedure TestHexToInt;
    procedure TestUrlDecode;
    procedure TestUrlEncodeDecodeRoundTrip;
    // random
    procedure TestRandomBytesLength;
    procedure TestRandomBytesDiffer;
    procedure TestRandomStringLengthAndPrefix;
    procedure TestRandomStringCharset;
    procedure TestRandomStringZeroLength;
    procedure TestRandomStringUnique;
    procedure TestRandomStringRangeInclusive;
    procedure TestRandomStringLeadingCapital;
    procedure TestRandomStringDontUse;
    procedure TestRandomStringNoCharset;
    // network
    procedure TestIsIPAddressV4;
    procedure TestIsIPAddressV4Invalid;
    procedure TestIsIPAddressV6;
    // strings
    procedure TestImplode;
    procedure TestExplode;
    procedure TestUCStoString;
    procedure TestUCStoStringPair;
    procedure TestS2i;
    procedure TestS2b;
  end;

implementation

{ TCommonTest }

procedure TCommonTest.TestBase64EncodePadding;
begin
  AssertEquals('YQ==', base64_encode('a'));
  AssertEquals('YWI=', base64_encode('ab'));
  AssertEquals('YWJj', base64_encode('abc'));
  AssertEquals('YWJjZA==', base64_encode('abcd'));
end;

procedure TCommonTest.TestBase64RoundTrip;
const
  S = 'FastPlaz: Fast Pascal Framework, ' + #0 + #255 + 'bin';
begin
  AssertEquals(S, base64_decode(base64_encode(S)));
end;

procedure TCommonTest.TestBase64EncodeEmpty;
begin
  AssertEquals('', base64_encode(''));
end;

procedure TCommonTest.TestHTMLDecodeNamedEntities;
begin
  AssertEquals('a & b < c > "d" ''e'' f', HTMLDecode('a &amp; b &lt; c &gt; &quot;d&quot; &apos;e&apos;&nbsp;f'));
end;

procedure TCommonTest.TestHTMLDecodeNumericEntities;
begin
  AssertEquals('A', HTMLDecode('&#65;'));
  AssertEquals('A', HTMLDecode('&#x41;'));
  // U+00E9 and U+1F600 as UTF-8
  AssertEquals(#$C3#$A9, HTMLDecode('&#233;'));
  AssertEquals(#$F0#$9F#$98#$80, HTMLDecode('&#128512;'));
end;

procedure TCommonTest.TestHTMLDecodeUnknownEntityKept;
begin
  AssertEquals('&copy; tail', HTMLDecode('&copy; tail'));
  AssertEquals('&#; x', HTMLDecode('&#; x'));
  AssertEquals('&#xZZ;', HTMLDecode('&#xZZ;'));
  AssertEquals(11, Length(HTMLDecode('&copy; tail')));
end;

procedure TCommonTest.TestHTMLDecodeUnterminatedAmpersand;
begin
  AssertEquals('a & b', HTMLDecode('a & b'));
  AssertEquals('a &amp', HTMLDecode('a &amp'));
  AssertEquals('&', HTMLDecode('&'));
end;

procedure TCommonTest.TestHTMLDecodePlainText;
begin
  AssertEquals('', HTMLDecode(''));
  AssertEquals('plain text', HTMLDecode('plain text'));
end;

procedure TCommonTest.TestStripHTML;
begin
  AssertEquals('hello world', string(StripHTML('<b>hello</b> <i>world</i>')));
  AssertEquals('x', string(StripHTML('<a href="#">x</a>')));
end;

procedure TCommonTest.TestStripHTMLUnterminatedTag;
begin
  // must not loop forever
  AssertEquals('a < b', string(StripHTML('a < b')));
  AssertEquals('a  c', string(StripHTML('a <b> c')));
end;

procedure TCommonTest.TestStripNumber;
begin
  AssertEquals('abc', StripNumber('a1b2c3'));
  AssertEquals('abc', StripNumber('abc'));
  AssertEquals('', StripNumber('123'));
  AssertEquals('', StripNumber(''));
end;

procedure TCommonTest.TestStripNonNumber;
begin
  AssertEquals('123', StripNonNumber('a1b2c3'));
  AssertEquals('', StripNonNumber('abc'));
  AssertEquals('', StripNonNumber(''));
end;

procedure TCommonTest.TestFormatPhoneNumber;
begin
  AssertEquals('628123456789', FormatPhoneNumber('0812-3456-789'));
  AssertEquals('628123456789', FormatPhoneNumber('+62 812 3456 789'));
  AssertEquals('', FormatPhoneNumber(''));
end;

procedure TCommonTest.TestHexToInt;
begin
  AssertEquals(0, HexToInt(''));
  AssertEquals(255, HexToInt('FF'));
  AssertEquals(255, HexToInt('ff'));
  AssertEquals(26, HexToInt('1AH'));
  AssertEquals(0, HexToInt('ZZ'));
end;

procedure TCommonTest.TestUrlDecode;
begin
  AssertEquals('A B', UrlDecode('%41+B'));
  AssertEquals('A B', UrlDecode('A%20B'));
  // truncated escapes are kept as-is instead of reading past the string
  AssertEquals('ab%4', UrlDecode('ab%4'));
  AssertEquals('ab%', UrlDecode('ab%'));
end;

procedure TCommonTest.TestUrlEncodeDecodeRoundTrip;
const
  S = 'a b&c=d/e?f#g%h';
begin
  AssertEquals(S, UrlDecode(UrlEncode(S)));
  AssertEquals(S, UrlDecode(UrlEncode(S, False)));
end;

procedure TCommonTest.TestRandomBytesLength;
begin
  AssertEquals(0, Length(RandomBytes(0)));
  AssertEquals(0, Length(RandomBytes(-1)));
  AssertEquals(16, Length(RandomBytes(16)));
  AssertEquals(1000, Length(RandomBytes(1000)));
end;

procedure TCommonTest.TestRandomBytesDiffer;
var
  a, b: TBytes;
  i: integer;
  same: boolean;
begin
  a := RandomBytes(32);
  b := RandomBytes(32);
  same := True;
  for i := 0 to 31 do
    same := same and (a[i] = b[i]);
  AssertFalse('two 32-byte draws must differ', same);
end;

procedure TCommonTest.TestRandomStringLengthAndPrefix;
var
  s: string;
begin
  AssertEquals(12, Length(RandomString(12)));
  s := RandomString(12, 'form-');
  AssertEquals(17, Length(s));
  AssertEquals('form-', Copy(s, 1, 5));
end;

procedure TCommonTest.TestRandomStringCharset;
var
  s: string;
  c: char;
begin
  s := RandomString(500);
  for c in s do
    AssertTrue('unexpected char ' + c, c in ['a'..'z', 'A'..'Z', '0'..'9']);
end;

procedure TCommonTest.TestRandomStringZeroLength;
begin
  AssertEquals('', RandomString(0));
  AssertEquals('', RandomString(-3));
  AssertEquals('pre', RandomString(0, 'pre'));
end;

procedure TCommonTest.TestRandomStringUnique;
begin
  AssertTrue(RandomString(16) <> RandomString(16));
end;

procedure TCommonTest.TestRandomStringRangeInclusive;
var
  i, len: integer;
  seenMin, seenMax: boolean;
begin
  seenMin := False;
  seenMax := False;
  for i := 1 to 200 do
  begin
    len := Length(RandomString(3, 5, False, True, True, False, True, False, False, ''));
    AssertTrue('length out of range: ' + IntToStr(len), (len >= 3) and (len <= 5));
    seenMin := seenMin or (len = 3);
    seenMax := seenMax or (len = 5);
  end;
  AssertTrue('min length never produced', seenMin);
  AssertTrue('max length never produced', seenMax);
  AssertEquals(5, Length(RandomString(5, 5, False, False, True, False, True, False, False, '')));
end;

procedure TCommonTest.TestRandomStringLeadingCapital;
var
  s: string;
begin
  s := RandomString(8, 8, True, False, True, False, False, False, False, '');
  AssertEquals(8, Length(s));
  AssertTrue(s[1] in ['A'..'Z']);
end;

procedure TCommonTest.TestRandomStringDontUse;
var
  s: string;
  c: char;
begin
  s := RandomString(300, 300, False, False, False, False, True, False, False, '0189');
  AssertEquals(300, Length(s));
  for c in s do
    AssertTrue('excluded char ' + c, c in ['2'..'7']);
end;

procedure TCommonTest.TestRandomStringNoCharset;
begin
  AssertEquals('', RandomString(5, 5, True, False, False, False, False, False, False, ''));
end;

procedure TCommonTest.TestIsIPAddressV4;
begin
  AssertTrue(isIPAddress('127.0.0.1'));
  AssertTrue(isIPAddress('0.0.0.0'));
  AssertTrue(isIPAddress('255.255.255.255'));
  AssertTrue(isIPAddress('192.168.1.10'));
end;

procedure TCommonTest.TestIsIPAddressV4Invalid;
begin
  AssertFalse(isIPAddress(''));
  AssertFalse(isIPAddress('999.999.999.999'));
  AssertFalse(isIPAddress('256.1.1.1'));
  AssertFalse(isIPAddress('x1.2.3.4y'));
  AssertFalse(isIPAddress('1.2.3'));
  AssertFalse(isIPAddress('1.2.3.4.5'));
  AssertFalse(isIPAddress('1..2.3'));
  AssertFalse(isIPAddress('1.2.3.4 '));
  AssertFalse(isIPAddress('localhost'));
end;

procedure TCommonTest.TestIsIPAddressV6;
begin
  AssertTrue(isIPAddress('::1'));
  AssertTrue(isIPAddress('2001:db8::8a2e:370:7334'));
  AssertTrue(isIPAddress('fe80:0:0:0:0:0:0:1'));
  AssertTrue(isIPAddress('::ffff:192.168.1.1'));
  AssertFalse(isIPAddress('2001:db8::zz'));
  AssertFalse(isIPAddress('1:2:3:4:5:6:7:8:9'));
  AssertFalse(isIPAddress('a:b'));
end;

procedure TCommonTest.TestImplode;
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  try
    AssertEquals('', Implode(lst));
    lst.Add('a');
    AssertEquals('a', Implode(lst));
    lst.Add('b');
    lst.Add('c');
    AssertEquals('a;b;c', Implode(lst));
    AssertEquals('a, b, c', Implode(lst, ', '));
    AssertEquals('[a]-[b]-[c]', Implode(lst, '-', '[', ']'));
  finally
    lst.Free;
  end;
end;

procedure TCommonTest.TestExplode;
var
  lst: TStrings;
begin
  lst := Explode('a, b, c', ', ');
  try
    AssertEquals(3, lst.Count);
    AssertEquals('c', lst[2]);
  finally
    lst.Free;
  end;
  AssertEquals(3, WordNumber('satu dua tiga'));
  AssertTrue(isWord('satu'));
end;

procedure TCommonTest.TestUCStoString;
begin
  AssertEquals('A', UCStoString('A'));
  AssertEquals('AB-C', UCStoString('AB-C'));
  AssertEquals(#$C3#$A9, UCStoString('é'));
  AssertEquals('plain', UCStoString('plain'));
end;

procedure TCommonTest.TestUCStoStringPair;
begin
  // U+1F600 as a surrogate pair -> 4 byte UTF-8
  AssertEquals(#$F0#$9F#$98#$80, UCStoString('😀', True));
  AssertEquals('x' + #$F0#$9F#$98#$80 + 'y', UCStoString('x😀y', True));
end;

procedure TCommonTest.TestS2i;
begin
  AssertEquals(12, s2i('12'));
  AssertEquals(-3, s2i('-3'));
  AssertEquals(0, s2i('x'));
  AssertEquals(0, s2i(''));
end;

procedure TCommonTest.TestS2b;
begin
  AssertTrue(s2b('true'));
  AssertTrue(s2b('1'));
  AssertTrue(s2b('on'));
  AssertFalse(s2b('false'));
  AssertFalse(s2b('0'));
  AssertFalse(s2b(''));
end;

initialization
  RegisterTest(TCommonTest);

end.

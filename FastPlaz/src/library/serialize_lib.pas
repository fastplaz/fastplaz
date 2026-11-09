unit serialize_lib;

{
  PHP compatible serialize()/unserialize() for variants.

    i:5;  s:5:"hello";  b:1;  d:1.5;  N;  a:2:{i:0;i:5;i:1;s:1:"x";}

  Strings are byte strings (length in bytes, UTF-8 passes through
  unchanged). Arrays are serialized from 0-based variant arrays and read
  back as 0-based variant arrays (the keys must be 0..n-1 in order).

  unserialize() also accepts the format written by older FastPlaz
  versions (array keys written as "i:0:" and scalars without the closing
  ";"), so values already stored by modvar_util keep working. It returns
  '' for malformed input.

  CREDIT: unknown resource
}

{$mode objfpc}{$H+}

interface

function serialize(v: variant): string;
function unserialize(const s: string): variant;

implementation

uses Variants, SysUtils;

const
  MAX_DEPTH = 64;

type
  TUnserializer = class
  private
    s: string;
    i, l: integer;
    depth: integer;
    procedure Fail(const AMessage: string);
    function Peek: char;
    procedure Expect(const AChar: char);
    procedure SkipOptional(const AChar: char);
    function ReadNumberText(const AllowFloat: boolean): string;
    function ReadInteger: int64;
    function ReadField: variant;
    function ReadArray: variant;
    function ReadString: variant;
  public
    function Parse(const AData: string): variant;
  end;

procedure TUnserializer.Fail(const AMessage: string);
begin
  raise EConvertError.CreateFmt('unserialize: %s at position %d', [AMessage, i]);
end;

function TUnserializer.Peek: char;
begin
  if i > l then
    Result := #0
  else
    Result := s[i];
end;

procedure TUnserializer.Expect(const AChar: char);
begin
  if Peek <> AChar then
    Fail('expected "' + AChar + '"');
  Inc(i);
end;

procedure TUnserializer.SkipOptional(const AChar: char);
begin
  if Peek = AChar then
    Inc(i);
end;

function TUnserializer.ReadNumberText(const AllowFloat: boolean): string;
var
  j: integer;
begin
  j := i;
  while (j <= l) and ((s[j] in ['-', '+', '0'..'9']) or
      (AllowFloat and (s[j] in ['.', 'E', 'e']))) do
    Inc(j);
  if j = i then
    Fail('number expected');
  Result := Copy(s, i, j - i);
  i := j;
end;

function TUnserializer.ReadInteger: int64;
begin
  if not TryStrToInt64(ReadNumberText(False), Result) then
    Fail('invalid integer');
end;

function TUnserializer.ReadString: variant;
var
  len: int64;
  Value: string;
begin
  Expect(':');
  len := ReadInteger;
  if (len < 0) or (len > l - i) then
    Fail('invalid string length');
  Expect(':');
  Expect('"');
  Value := Copy(s, i, len);
  Inc(i, len);
  Expect('"');
  SkipOptional(';');
  Result := Value;
end;

function TUnserializer.ReadArray: variant;
var
  count, k, key: int64;
  item: variant;
begin
  Inc(depth);
  if depth > MAX_DEPTH then
    Fail('nesting too deep');
  Expect(':');
  count := ReadInteger;
  // every element needs at least "i:0;N;" = 6 bytes; reject absurd sizes
  // before allocating anything
  if (count < 0) or (count * 6 > l - i) then
    Fail('invalid array length');
  Expect(':');
  Expect('{');
  Result := VarArrayCreate([0, count - 1], varVariant);
  for k := 0 to count - 1 do
  begin
    Expect('i');
    Expect(':');
    key := ReadInteger;
    if key <> k then
      Fail('unsupported array key');
    // ";" is the PHP key terminator, ":" the one of the legacy writer
    if Peek = ':' then
      Inc(i)
    else
      Expect(';');
    item := ReadField;
    Result[k] := item;
  end;
  Expect('}');
  SkipOptional(';');
  Dec(depth);
end;

function TUnserializer.ReadField: variant;
var
  t: char;
  d: double;
  fs: TFormatSettings;
  txt: string;
begin
  t := Peek;
  Inc(i);
  case t of
    'a': Result := ReadArray;
    's': Result := ReadString;
    'i':
    begin
      Expect(':');
      Result := ReadInteger;
      SkipOptional(';');
    end;
    'b':
    begin
      Expect(':');
      txt := ReadNumberText(False);
      Result := txt <> '0';
      SkipOptional(';');
    end;
    'd':
    begin
      Expect(':');
      txt := ReadNumberText(True);
      fs := DefaultFormatSettings;
      fs.DecimalSeparator := '.';
      fs.ThousandSeparator := #0;
      if not TryStrToFloat(txt, d, fs) then
        Fail('invalid float');
      Result := d;
      SkipOptional(';');
    end;
    'N', 'n':
    begin
      Result := Null;
      SkipOptional(';');
    end;
    else
      Fail('unknown type "' + t + '"');
  end;
end;

function TUnserializer.Parse(const AData: string): variant;
begin
  s := AData;
  i := 1;
  l := Length(s);
  depth := 0;
  if l = 0 then
    Fail('empty input');
  Result := ReadField;
  if i <= l then
    Fail('trailing data');
end;

function unserialize(const s: string): variant;
var
  p: TUnserializer;
begin
  Result := '';
  p := TUnserializer.Create;
  try
    try
      Result := p.Parse(s);
    except
      on E: Exception do
        Result := '';
    end;
  finally
    p.Free;
  end;
end;

function serialize(v: variant): string;

  function servariant(v: variant; ADepth: integer): string; forward;

  function servarray(v: variant; ADepth: integer): string;
  var
    i, lo, hi: integer;
  begin
    lo := VarArrayLowBound(v, 1);
    hi := VarArrayHighBound(v, 1);
    Result := 'a:' + IntToStr(hi - lo + 1) + ':{';
    for i := lo to hi do
      Result := Result + 'i:' + IntToStr(i - lo) + ';' + servariant(v[i], ADepth + 1);
    Result := Result + '}';
  end;

  function serstring(const AValue: string): string;
  begin
    Result := 's:' + IntToStr(Length(AValue)) + ':"' + AValue + '";';
  end;

  function serfloat(const AValue: double): string;
  var
    fs: TFormatSettings;
  begin
    fs := DefaultFormatSettings;
    fs.DecimalSeparator := '.';
    fs.ThousandSeparator := #0;
    Result := 'd:' + FloatToStr(AValue, fs) + ';';
  end;

  function servariant(v: variant; ADepth: integer): string;
  var
    vt: tvartype;
  begin
    Result := '';
    if ADepth > MAX_DEPTH then
      raise EConvertError.Create('serialize: nesting too deep');
    vt := VarType(v);
    if (vt and varArray) = varArray then
    begin
      Result := servarray(v, ADepth);
      Exit;
    end;
    case vt of
      varEmpty, varNull: Result := 'N;';
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord,
      varInt64, varQWord:
        Result := 'i:' + VarToStr(v) + ';';
      varSingle, varDouble, varCurrency, varDate:
        Result := serfloat(double(v));
      varBoolean:
        if boolean(v) then
          Result := 'b:1;'
        else
          Result := 'b:0;';
      varOleStr, varString, varUString:
        Result := serstring(VarToStr(v));
      else
        // anything else (objects, dispatch, ...) is stored as its text
        Result := serstring(VarToStr(v));
    end;
  end;

begin
  try
    Result := servariant(v, 0);
  except
    Result := '';
  end;
end;

end.

unit test_json_lib;

{$mode objfpc}{$H+}

interface

uses
  json_lib, fpjson, variants, Classes, SysUtils, fpcunit, testregistry;

type

  { TJSONUtilTest }

  TJSONUtilTest = class(TTestCase)
  private
    FJson: TJSONUtil;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // loading
    procedure TestLoadObject;
    procedure TestLoadEmptyStringFails;
    procedure TestLoadInvalidFails;
    procedure TestLoadArrayRoot;
    procedure TestLoadNestedArrayRoot;
    procedure TestLoadResetsModified;
    // reading
    procedure TestGetString;
    procedure TestGetInteger;
    procedure TestGetInt64;
    procedure TestGetQWord;
    procedure TestGetFloat;
    procedure TestGetBoolean;
    procedure TestGetMissingIsEmpty;
    procedure TestGetNested;
    procedure TestGetValueArray;
    procedure TestGetValueArrayMissingIsNil;
    procedure TestGetValueArrayWrongTypeIsNil;
    // writing
    procedure TestSetString;
    procedure TestSetUnicodeString;
    procedure TestSetInteger;
    procedure TestSetInt64;
    procedure TestSetFloat;
    procedure TestSetBoolean;
    procedure TestSetNull;
    procedure TestSetCreatesParents;
    procedure TestSetReplacesObject;
    procedure TestSetReplacesDifferentType;
    procedure TestSetUnsupportedTypeIsIgnored;
    procedure TestSetValueArray;
    procedure TestSetValueArrayReplacesObject;
    procedure TestSetValueArrayNilIsIgnored;
    procedure TestModifiedFlag;
    // paths
    procedure TestRelativePathAfterLoad;
    procedure TestDeletePath;
    procedure TestDeletePathMissingIsNoop;
    procedure TestClear;
    procedure TestIsEmpty;
    // items
    procedure TestItemScalar;
    procedure TestItemObject;
    procedure TestItemNested;
    procedure TestItemMissingIsNil;
    procedure TestItemNestedMissingIsNil;
    procedure TestItemsSurviveUntilDestroy;
    // files
    procedure TestSaveToFile;
    procedure TestSaveToFileEmptyNameFails;
  end;

implementation

const
  SAMPLE = '{"name":"fastplaz","count":5,"big":9007199254740993,' +
    '"huge":18446744073709551615,"ratio":0.5,"flag":true,' +
    '"list":[1,2,3],"sub":{"key":"value","deep":{"x":1}}}';

function CountOf(const ANeedle, AText: string): integer;
var
  p: integer;
begin
  Result := 0;
  p := Pos(ANeedle, AText);
  while p > 0 do
  begin
    Inc(Result);
    p := Pos(ANeedle, AText, p + 1);
  end;
end;

{ TJSONUtilTest }

procedure TJSONUtilTest.SetUp;
begin
  FJson := TJSONUtil.Create;
  AssertTrue('sample must load', FJson.LoadFromJsonString(SAMPLE));
end;

procedure TJSONUtilTest.TearDown;
begin
  FJson.Free;
end;

procedure TJSONUtilTest.TestLoadObject;
begin
  AssertFalse(FJson.IsEmpty);
  AssertEquals('fastplaz', string(FJson['name']));
end;

procedure TJSONUtilTest.TestLoadEmptyStringFails;
begin
  AssertFalse(FJson.LoadFromJsonString(''));
  AssertTrue(FJson.IsEmpty);
  AssertEquals('{}', FJson.AsJSON);
end;

procedure TJSONUtilTest.TestLoadInvalidFails;
begin
  AssertFalse(FJson.LoadFromJsonString('{"a":'));
  AssertTrue(FJson.IsEmpty);
end;

// an array root is accepted (SimpleAI stores button rows this way) and
// exposed through Data/AsArray; the named accessors are no-ops on it
procedure TJSONUtilTest.TestLoadArrayRoot;
begin
  AssertTrue(FJson.LoadFromJsonString('[1,2]'));
  AssertTrue(FJson.IsArray);
  AssertFalse(FJson.IsObject);
  AssertFalse(FJson.IsEmpty);
  AssertEquals(2, FJson.Data.Count);
  AssertEquals(2, FJson.AsArray.Count);
  AssertEquals('[1, 2]', FJson.AsJSON);
  AssertEquals(2, integer(FJson['[1]']));
  AssertNull(FJson.ValueArray['x']);
  AssertNull(FJson.Item['x']);
  FJson['x'] := 1;
  AssertFalse(FJson.Modified);
  AssertEquals('[1, 2]', FJson.AsJSON);
  FJson.ValueArray['y'] := TJSONArray.Create([1]);
  AssertEquals('[1, 2]', FJson.AsJSON);
  FJson.DeletePath('x');
  // Clear turns it back into an object document
  FJson.Clear;
  AssertTrue(FJson.IsObject);
  AssertNull(FJson.AsArray);
  FJson['x'] := 1;
  AssertEquals(1, integer(FJson['x']));
  // a scalar root is still rejected
  AssertFalse(FJson.LoadFromJsonString('1'));
  AssertTrue(FJson.IsObject);
end;

procedure TJSONUtilTest.TestLoadNestedArrayRoot;
begin
  AssertTrue(FJson.LoadFromJsonString('[[{"text":"a","callback_data":"x=1"}]]'));
  AssertEquals('a', FJson.Data.Items[0].Items[0].GetPath('text').AsString);
  AssertEquals('a', TJSONArray(FJson.Data).Items[0].Items[0].GetPath('text').AsString);
  AssertEquals('x=1', string(FJson['[0][0].callback_data']));
  // loading an object afterwards frees the array
  AssertTrue(FJson.LoadFromJsonString('{"a":1}'));
  AssertTrue(FJson.IsObject);
  AssertEquals(1, integer(FJson['a']));
end;

procedure TJSONUtilTest.TestLoadResetsModified;
begin
  FJson['x'] := 1;
  AssertTrue(FJson.Modified);
  FJson.LoadFromJsonString('{}');
  AssertFalse(FJson.Modified);
end;

procedure TJSONUtilTest.TestGetString;
begin
  AssertEquals('fastplaz', string(FJson['name']));
end;

procedure TJSONUtilTest.TestGetInteger;
begin
  AssertEquals(5, integer(FJson['count']));
end;

procedure TJSONUtilTest.TestGetInt64;
begin
  AssertEquals(int64(9007199254740993), int64(FJson['big']));
end;

procedure TJSONUtilTest.TestGetQWord;
begin
  AssertEquals(QWord(18446744073709551615), QWord(FJson['huge']));
end;

procedure TJSONUtilTest.TestGetFloat;
begin
  AssertEquals(0.5, double(FJson['ratio']), 0.0001);
end;

procedure TJSONUtilTest.TestGetBoolean;
begin
  AssertTrue(boolean(FJson['flag']));
end;

procedure TJSONUtilTest.TestGetMissingIsEmpty;
begin
  AssertEquals('', string(FJson['nope']));
  AssertEquals('', string(FJson['sub/nope']));
  AssertEquals('', string(FJson['name/nope']));
end;

procedure TJSONUtilTest.TestGetNested;
begin
  AssertEquals('value', string(FJson['sub/key']));
  AssertEquals('value', string(FJson['/sub/key']));
  AssertEquals(1, integer(FJson['sub/deep/x']));
end;

procedure TJSONUtilTest.TestGetValueArray;
var
  a: TJSONArray;
begin
  a := FJson.ValueArray['list'];
  AssertNotNull(a);
  AssertEquals(3, a.Count);
  AssertEquals(2, a.Integers[1]);
  AssertSame(a, FJson.ArrayValue['/list']);
end;

procedure TJSONUtilTest.TestGetValueArrayMissingIsNil;
begin
  AssertNull(FJson.ValueArray['nope']);
  AssertNull(FJson.ValueArray['sub/nope']);
end;

procedure TJSONUtilTest.TestGetValueArrayWrongTypeIsNil;
begin
  AssertNull(FJson.ValueArray['name']);
  AssertNull(FJson.ValueArray['sub']);
end;

procedure TJSONUtilTest.TestSetString;
begin
  FJson['name'] := 'changed';
  AssertEquals('changed', string(FJson['name']));
  FJson['new'] := 'created';
  AssertEquals('created', string(FJson['new']));
end;

procedure TJSONUtilTest.TestSetUnicodeString;
var
  u: UnicodeString;
  w: WideString;
begin
  u := 'unicode';
  FJson['u'] := u;
  AssertEquals('unicode', string(FJson['u']));
  w := 'wide';
  FJson['w'] := w;
  AssertEquals('wide', string(FJson['w']));
end;

procedure TJSONUtilTest.TestSetInteger;
begin
  FJson['count'] := 7;
  AssertEquals(7, integer(FJson['count']));
  AssertTrue(FJson.Data.Find('count') is TJSONIntegerNumber);
end;

procedure TJSONUtilTest.TestSetInt64;
var
  v: int64;
begin
  v := 9007199254740993;
  FJson['big'] := v;
  AssertEquals(v, int64(FJson['big']));
  AssertTrue(FJson.Data.Find('big') is TJSONInt64Number);
end;

procedure TJSONUtilTest.TestSetFloat;
begin
  FJson['ratio'] := 1.25;
  AssertEquals(1.25, double(FJson['ratio']), 0.0001);
end;

procedure TJSONUtilTest.TestSetBoolean;
begin
  FJson['flag'] := False;
  AssertFalse(boolean(FJson['flag']));
end;

procedure TJSONUtilTest.TestSetNull;
begin
  FJson['name'] := Null;
  AssertTrue(FJson.Data.Find('name').JSONType = jtNull);
end;

procedure TJSONUtilTest.TestSetCreatesParents;
begin
  FJson['a/b/c'] := 'deep';
  AssertEquals('deep', string(FJson['a/b/c']));
  AssertEquals('{ "c" : "deep" }', FJson.Data.FindPath('a.b').AsJSON);
end;

procedure TJSONUtilTest.TestSetReplacesObject;
begin
  FJson['sub'] := 'text';
  AssertEquals('text', string(FJson['sub']));
  // no duplicate member left behind
  AssertEquals(1, CountOf('"sub"', FJson.AsJSON));
  AssertTrue(FJson.Data.Find('sub').JSONType = jtString);
end;

procedure TJSONUtilTest.TestSetReplacesDifferentType;
begin
  FJson['count'] := 'five';
  AssertEquals('five', string(FJson['count']));
  AssertTrue(FJson.Data.Find('count').JSONType = jtString);
  FJson['count'] := True;
  AssertTrue(FJson.Data.Find('count').JSONType = jtBoolean);
end;

procedure TJSONUtilTest.TestSetUnsupportedTypeIsIgnored;
var
  v: variant;
begin
  v := VarArrayCreate([0, 1], varInteger);
  FJson['arr'] := v;
  AssertNull(FJson.Data.Find('arr'));
  AssertFalse(FJson.Modified);
end;

procedure TJSONUtilTest.TestSetValueArray;
var
  a: TJSONArray;
begin
  a := TJSONArray.Create([1, 2]);
  FJson.ValueArray['items'] := a;
  AssertSame(a, FJson.ValueArray['items']);
  AssertEquals(2, FJson.ValueArray['items'].Count);
  AssertTrue(FJson.Modified);
end;

procedure TJSONUtilTest.TestSetValueArrayReplacesObject;
begin
  FJson.ValueArray['sub'] := TJSONArray.Create(['a']);
  AssertEquals(1, FJson.ValueArray['sub'].Count);
  AssertEquals('', string(FJson['sub/key']));
end;

procedure TJSONUtilTest.TestSetValueArrayNilIsIgnored;
begin
  FJson.ValueArray['list'] := nil;
  AssertEquals(3, FJson.ValueArray['list'].Count);
end;

procedure TJSONUtilTest.TestModifiedFlag;
begin
  AssertFalse(FJson.Modified);
  FJson['x'] := 1;
  AssertTrue(FJson.Modified);
end;

procedure TJSONUtilTest.TestRelativePathAfterLoad;
begin
  // paths without a leading slash must address the loaded document
  FJson.DeletePath('count');
  AssertNull(FJson.Data.Find('count'));
  AssertNotNull(FJson.Item['name']);
end;

procedure TJSONUtilTest.TestDeletePath;
begin
  FJson.DeletePath('/sub/key');
  AssertEquals('', string(FJson['sub/key']));
  AssertEquals(1, integer(FJson['sub/deep/x']));
  FJson.DeletePath('sub');
  AssertNull(FJson.Data.Find('sub'));
end;

procedure TJSONUtilTest.TestDeletePathMissingIsNoop;
begin
  FJson.DeletePath('nope/x');
  FJson.DeletePath('');
  AssertEquals(5, integer(FJson['count']));
end;

procedure TJSONUtilTest.TestClear;
begin
  FJson.Clear;
  AssertTrue(FJson.IsEmpty);
  AssertEquals('{}', FJson.AsJSON);
end;

procedure TJSONUtilTest.TestIsEmpty;
var
  j: TJSONUtil;
begin
  j := TJSONUtil.Create;
  try
    AssertTrue(j.IsEmpty);
    j['a'] := 1;
    AssertFalse(j.IsEmpty);
  finally
    j.Free;
  end;
end;

procedure TJSONUtilTest.TestItemScalar;
begin
  AssertEquals('fastplaz', FJson.Item['name'].AsString);
  AssertFalse(FJson.Item['name'].IsObject);
end;

procedure TJSONUtilTest.TestItemObject;
begin
  AssertTrue(FJson.Item['sub'].IsObject);
  AssertEquals('value', FJson.Item['sub'].Item['key'].AsString);
  AssertTrue(Pos('"key"', FJson.Item['sub'].AsJSON) > 0);
end;

procedure TJSONUtilTest.TestItemNested;
begin
  AssertEquals(1, FJson.Item['sub/deep'].Item['x'].Data.AsInteger);
  AssertEquals('value', FJson.Item['/sub'].Item['/key'].AsString);
end;

procedure TJSONUtilTest.TestItemMissingIsNil;
begin
  AssertNull(FJson.Item['nope']);
  AssertNull(FJson.Item['sub/nope']);
  AssertNull(FJson.Item['name/nope']);
end;

procedure TJSONUtilTest.TestItemNestedMissingIsNil;
begin
  AssertNull(FJson.Item['sub'].Item['nope']);
  // a scalar has no children
  AssertNull(FJson.Item['name'].Item['x']);
end;

procedure TJSONUtilTest.TestItemsSurviveUntilDestroy;
var
  a, b: TJSONUtilItem;
begin
  a := FJson.Item['name'];
  b := FJson.Item['name'];
  AssertNotNull(a);
  AssertNotNull(b);
  AssertTrue('each access yields its own view', a <> b);
  AssertEquals(a.AsString, b.AsString);
  // reloading invalidates the views; they must not be used afterwards, but
  // the document itself stays consistent
  FJson.LoadFromJsonString('{"name":"new"}');
  AssertEquals('new', FJson.Item['name'].AsString);
end;

procedure TJSONUtilTest.TestSaveToFile;
var
  fileName: string;
  lst: TStringList;
begin
  fileName := GetTempFileName('', 'fpjson');
  try
    AssertTrue(FJson.SaveToFile(fileName));
    lst := TStringList.Create;
    try
      lst.LoadFromFile(fileName);
      AssertEquals(FJson.AsJSON, Trim(lst.Text));
    finally
      lst.Free;
    end;
  finally
    DeleteFile(fileName);
  end;
end;

procedure TJSONUtilTest.TestSaveToFileEmptyNameFails;
begin
  AssertFalse(FJson.SaveToFile(''));
end;

initialization
  RegisterTest(TJSONUtilTest);

end.

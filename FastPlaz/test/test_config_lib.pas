unit test_config_lib;

{$mode objfpc}{$H+}

interface

uses
  config_lib, fpjson, variants, Classes, SysUtils, fpcunit, testregistry;

type

  { TMyConfigTest }

  TMyConfigTest = class(TTestCase)
  private
    FDir: string;
    FConfig: TMyConfig;
    function WriteConfig(const AName, AContent: string): string;
    procedure LoadSample;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // ValidateFile
    procedure TestInitialState;
    procedure TestValidateFileValid;
    procedure TestValidateFileMissing;
    procedure TestValidateFileInvalidJson;
    procedure TestValidateFileArrayRoot;
    procedure TestValidateFileResetsPreviousError;
    // Value[]
    procedure TestValueString;
    procedure TestValueNumber;
    procedure TestValueBoolean;
    procedure TestValueArrayAsJson;
    procedure TestValueObjectAsJson;
    procedure TestValueMissingIsEmpty;
    procedure TestValueTrailingSlash;
    procedure TestSetValue;
    // GetObject
    procedure TestGetObject;
    procedure TestGetObjectTrailingSlash;
    procedure TestGetObjectEmptyIsNil;
    procedure TestGetObjectMissingIsNil;
    procedure TestGetObjectScalarIsNil;
    // SetDataValue
    procedure TestSetDataValueNewKey;
    procedure TestSetDataValueReplacesArray;
    procedure TestSetDataValueReplacesObject;
    procedure TestSetDataValueReplacesScalar;
    procedure TestSetDataValueCallerKeepsOwnership;
    procedure TestSetDataValueNilIsIgnored;
    procedure TestSetDataValueCreatesParents;
    procedure TestSetDataValueMarksModified;
  end;

implementation

const
  SAMPLE = '{"systems":{"sitename":"FastPlaz","debug":true,"port":8080,' +
    '"list":[1,2],"sub":{"a":1}},"arr":[1]}';

{ TMyConfigTest }

function TMyConfigTest.WriteConfig(const AName, AContent: string): string;
var
  lst: TStringList;
begin
  Result := FDir + AName;
  lst := TStringList.Create;
  try
    lst.Text := AContent;
    lst.SaveToFile(Result);
  finally
    lst.Free;
  end;
end;

procedure TMyConfigTest.LoadSample;
begin
  AssertTrue(FConfig.ValidateFile(WriteConfig('config.json', SAMPLE)));
end;

procedure TMyConfigTest.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_config_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FConfig := TMyConfig.Create(nil);
end;

procedure TMyConfigTest.TearDown;
var
  rec: TSearchRec;
begin
  FConfig.Free;
  if FindFirst(FDir + '*', faAnyFile, rec) = 0 then
  begin
    repeat
      if (rec.Attr and faDirectory) = 0 then
        DeleteFile(FDir + rec.Name);
    until FindNext(rec) <> 0;
    FindClose(rec);
  end;
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
end;

procedure TMyConfigTest.TestInitialState;
begin
  AssertFalse(FConfig.IsValid);
  AssertEquals(1, FConfig.Status);
  AssertEquals(_ERR_CONFIG_FILENOTEXIST, FConfig.Message);
end;

procedure TMyConfigTest.TestValidateFileValid;
var
  fileName: string;
begin
  fileName := WriteConfig('config.json', SAMPLE);
  AssertTrue(FConfig.ValidateFile(fileName));
  AssertTrue(FConfig.IsValid);
  AssertEquals(0, FConfig.Status);
  AssertEquals('', FConfig.Message);
  AssertEquals(fileName, string(FConfig.Filename));
end;

procedure TMyConfigTest.TestValidateFileMissing;
begin
  AssertFalse(FConfig.ValidateFile(FDir + 'nope.json'));
  AssertFalse(FConfig.IsValid);
  AssertEquals(1, FConfig.Status);
end;

procedure TMyConfigTest.TestValidateFileInvalidJson;
begin
  AssertFalse(FConfig.ValidateFile(WriteConfig('bad.json', '{"a": 1,' + LineEnding + ' "b": }')));
  AssertFalse(FConfig.IsValid);
  AssertEquals(2, FConfig.Status);
  AssertTrue(FConfig.Message, Pos('Config Error at line ', FConfig.Message) = 1);
end;

procedure TMyConfigTest.TestValidateFileArrayRoot;
begin
  AssertFalse(FConfig.ValidateFile(WriteConfig('arr.json', '[1,2]')));
  AssertEquals(2, FConfig.Status);
  AssertEquals(_ERR_CONFIG_INVALID, FConfig.Message);
end;

procedure TMyConfigTest.TestValidateFileResetsPreviousError;
begin
  FConfig.ValidateFile(WriteConfig('bad.json', '{'));
  AssertEquals(2, FConfig.Status);
  LoadSample;
  AssertEquals(0, FConfig.Status);
  AssertEquals('', FConfig.Message);
end;

procedure TMyConfigTest.TestValueString;
begin
  LoadSample;
  AssertEquals('FastPlaz', string(FConfig['systems/sitename']));
  AssertEquals('FastPlaz', string(FConfig.Value['systems/sitename']));
end;

procedure TMyConfigTest.TestValueNumber;
begin
  LoadSample;
  AssertEquals('8080', string(FConfig['systems/port']));
end;

procedure TMyConfigTest.TestValueBoolean;
begin
  LoadSample;
  AssertEquals('true', LowerCase(string(FConfig['systems/debug'])));
end;

procedure TMyConfigTest.TestValueArrayAsJson;
begin
  LoadSample;
  AssertEquals('[1, 2]', string(FConfig['systems/list']));
end;

procedure TMyConfigTest.TestValueObjectAsJson;
begin
  LoadSample;
  AssertEquals('{ "a" : 1 }', string(FConfig['systems/sub']));
end;

procedure TMyConfigTest.TestValueMissingIsEmpty;
begin
  LoadSample;
  AssertEquals('', string(FConfig['nope']));
  AssertEquals('', string(FConfig['systems/nope']));
  AssertEquals('', string(FConfig['systems/sitename/nope']));
end;

procedure TMyConfigTest.TestValueTrailingSlash;
begin
  LoadSample;
  AssertEquals('FastPlaz', string(FConfig['systems/sitename/']));
end;

procedure TMyConfigTest.TestSetValue;
begin
  LoadSample;
  FConfig['systems/sitename'] := 'Changed';
  AssertEquals('Changed', string(FConfig['systems/sitename']));
  FConfig['new/key'] := 'created';
  AssertEquals('created', string(FConfig['new/key']));
  AssertTrue(FConfig.Modified);
end;

procedure TMyConfigTest.TestGetObject;
var
  o: TJSONObject;
begin
  LoadSample;
  o := FConfig.GetObject('systems');
  AssertNotNull(o);
  AssertEquals('FastPlaz', o.Get('sitename', ''));
end;

procedure TMyConfigTest.TestGetObjectTrailingSlash;
begin
  LoadSample;
  AssertNotNull(FConfig.GetObject('systems/'));
  AssertNotNull(FConfig.GetObject('systems/sub/'));
end;

procedure TMyConfigTest.TestGetObjectEmptyIsNil;
begin
  LoadSample;
  AssertNull(FConfig.GetObject(''));
end;

procedure TMyConfigTest.TestGetObjectMissingIsNil;
begin
  LoadSample;
  AssertNull(FConfig.GetObject('nope'));
  AssertNull(FConfig.GetObject('systems/nope'));
end;

procedure TMyConfigTest.TestGetObjectScalarIsNil;
begin
  LoadSample;
  AssertNull(FConfig.GetObject('systems/sitename'));
end;

procedure TMyConfigTest.TestSetDataValueNewKey;
var
  d: TJSONData;
begin
  LoadSample;
  d := TJSONIntegerNumber.Create(7);
  try
    FConfig.SetDataValue('newkey', d);
  finally
    d.Free;
  end;
  AssertEquals('7', string(FConfig['newkey']));
end;

procedure TMyConfigTest.TestSetDataValueReplacesArray;
var
  d: TJSONData;
begin
  LoadSample;
  d := TJSONArray.Create([5, 6, 7]);
  try
    FConfig.SetDataValue('arr', d);
  finally
    d.Free;
  end;
  AssertEquals('[5, 6, 7]', string(FConfig['arr']));
end;

procedure TMyConfigTest.TestSetDataValueReplacesObject;
var
  d: TJSONData;
begin
  LoadSample;
  d := TJSONObject.Create(['b', 2]);
  try
    FConfig.SetDataValue('systems/sub', d);
  finally
    d.Free;
  end;
  AssertEquals('', string(FConfig['systems/sub/a']));
  AssertEquals('2', string(FConfig['systems/sub/b']));
end;

procedure TMyConfigTest.TestSetDataValueReplacesScalar;
var
  d: TJSONData;
begin
  LoadSample;
  d := TJSONArray.Create([1]);
  try
    FConfig.SetDataValue('systems/sitename', d);
  finally
    d.Free;
  end;
  AssertEquals('[1]', string(FConfig['systems/sitename']));
end;

procedure TMyConfigTest.TestSetDataValueCallerKeepsOwnership;
var
  d: TJSONObject;
begin
  LoadSample;
  d := TJSONObject.Create(['name', 'x']);
  try
    FConfig.SetDataValue('databases', d);
    // the stored node is a copy, later edits to d are not visible
    d.Strings['name'] := 'y';
    AssertEquals('x', string(FConfig['databases/name']));
  finally
    d.Free;
  end;
  AssertEquals('x', string(FConfig['databases/name']));
end;

procedure TMyConfigTest.TestSetDataValueNilIsIgnored;
begin
  LoadSample;
  FConfig.SetDataValue('arr', nil);
  AssertEquals('[1]', string(FConfig['arr']));
end;

procedure TMyConfigTest.TestSetDataValueCreatesParents;
var
  d: TJSONData;
begin
  LoadSample;
  d := TJSONString.Create('deep');
  try
    FConfig.SetDataValue('a/b/c', d);
  finally
    d.Free;
  end;
  AssertEquals('deep', string(FConfig['a/b/c']));
end;

procedure TMyConfigTest.TestSetDataValueMarksModified;
var
  d: TJSONData;
begin
  LoadSample;
  AssertFalse(FConfig.Modified);
  d := TJSONIntegerNumber.Create(1);
  try
    FConfig.SetDataValue('x', d);
  finally
    d.Free;
  end;
  AssertTrue(FConfig.Modified);
end;

initialization
  RegisterTest(TMyConfigTest);

end.

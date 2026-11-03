unit test_tools_lib;

{$mode objfpc}{$H+}

interface

uses
  tools_lib, fpjson, Classes, SysUtils, fpcunit, testregistry;

type

  { TToolsLibTest }

  TToolsLibTest = class(TTestCase)
  private
    FTempDir: string;
    procedure WriteText(const AFileName, AText: string);
    function ReadText(const AFileName: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUcWords;
    procedure TestUcWordsTrailingSpace;
    procedure TestSanitizeIdentifier;
    procedure TestSanitizeIdentifierDigitAndReserved;
    procedure TestSanitizeProjectName;
    procedure TestSanitizePermalink;
    procedure TestControllerTypeName;
    procedure TestModelClassName;
    procedure TestTableNameFromModelClass;
    procedure TestModelNameFromTablePlural;
    procedure TestModelNameFromTableNotPlural;
    procedure TestModelNameFromTableCustom;
    procedure TestCopyDirectory;
    procedure TestCopyDirectoryMissingSource;
    procedure TestHtaccessEnvironmentLines;
    procedure TestBuildHtaccess;
    procedure TestFastCodeResponseText;
    procedure TestFastCodeResponseTextEscapes;
    procedure TestFastCodeResponseTextInvalid;
    procedure TestConfigWithRunModeHTTP;
    procedure TestConfigWithRunModeCGI;
    procedure TestConfigWithRunModeKeepsServer;
    procedure TestConfigWithRunModeInvalid;
    procedure TestSetConfigRunMode;
  end;

implementation

procedure TToolsLibTest.WriteText(const AFileName, AText: string);
begin
  with TStringList.Create do
  try
    Text := AText;
    ForceDirectories(ExtractFilePath(AFileName));
    SaveToFile(AFileName);
  finally
    Free;
  end;
end;

function TToolsLibTest.ReadText(const AFileName: string): string;
begin
  with TStringList.Create do
  try
    LoadFromFile(AFileName);
    Result := Trim(Text);
  finally
    Free;
  end;
end;

procedure TToolsLibTest.SetUp;
begin
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fpz_tools_' +
    IntToStr(GetProcessID) + '_' + IntToStr(Random(100000));
end;

procedure TToolsLibTest.TearDown;
var
  Rec: TSearchRec;

  procedure RemoveTree(const ADir: string);
  var
    R: TSearchRec;
  begin
    if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, R) = 0 then
    try
      repeat
        if (R.Name = '.') or (R.Name = '..') then
          Continue;
        if (R.Attr and faDirectory) <> 0 then
          RemoveTree(IncludeTrailingPathDelimiter(ADir) + R.Name)
        else
          DeleteFile(IncludeTrailingPathDelimiter(ADir) + R.Name);
      until FindNext(R) <> 0;
    finally
      FindClose(R);
    end;
    RemoveDir(ADir);
  end;

begin
  if DirectoryExists(FTempDir) then
    RemoveTree(FTempDir);
end;

procedure TToolsLibTest.TestUcWords;
begin
  AssertEquals('My Project Name', UcWords('my PROJECT name'));
  AssertEquals('', UcWords(''));
  AssertEquals('A  B', UcWords('a  b'));
end;

procedure TToolsLibTest.TestUcWordsTrailingSpace;
begin
  // the old implementation wrote one byte past the end of the string here
  AssertEquals('My App', UcWords('my app '));
  AssertEquals('My App', UcWords(' my app'));
  AssertEquals('', UcWords('   '));
end;

procedure TToolsLibTest.TestSanitizeIdentifier;
begin
  AssertEquals('myapp', SanitizeIdentifier('my-app'));
  AssertEquals('my_app', SanitizeIdentifier('my_app'));
  AssertEquals('appv2', SanitizeIdentifier('app.v2'));
  AssertEquals('fastplaz', SanitizeIdentifier('---'));
  AssertEquals('other', SanitizeIdentifier('', 'other'));
end;

procedure TToolsLibTest.TestSanitizeIdentifierDigitAndReserved;
begin
  AssertEquals('_2fa', SanitizeIdentifier('2fa'));
  AssertEquals('program_', SanitizeIdentifier('program'));
  AssertEquals('End_', SanitizeIdentifier('End'));
  AssertEquals('ending', SanitizeIdentifier('ending'));
end;

procedure TToolsLibTest.TestSanitizeProjectName;
begin
  AssertEquals('myapp', SanitizeProjectName('My App'));
  AssertEquals('fastplaz', SanitizeProjectName(''));
  AssertEquals('shop2', SanitizeProjectName(' Shop.2 '));
end;

procedure TToolsLibTest.TestSanitizePermalink;
begin
  AssertEquals('my_module', SanitizePermalink('My Module'));
  AssertEquals('api_v1', SanitizePermalink('api/v1'));
  AssertEquals('my-module.json', SanitizePermalink('my-module.json'));
  AssertEquals('abc', SanitizePermalink('a''b"c'));
end;

procedure TToolsLibTest.TestControllerTypeName;
begin
  AssertEquals('TUserGroupController', ControllerTypeName('user group'));
  AssertEquals('TMymoduleController', ControllerTypeName('my-module'));
  AssertEquals('TSampleController', ControllerTypeName(''));
end;

procedure TToolsLibTest.TestModelClassName;
begin
  AssertEquals('TUserGroupModel', ModelClassName('user group'));
  AssertEquals('TUser_groupModel', ModelClassName('user_group'));
end;

procedure TToolsLibTest.TestTableNameFromModelClass;
begin
  AssertEquals('user_groups', TableNameFromModelClass('TUserGroupModel'));
  AssertEquals('user_groups', TableNameFromModelClass('TUser_groupModel'));
  AssertEquals('users', TableNameFromModelClass('TUserModel'));
  AssertEquals('news', TableNameFromModelClass('TNewModel'));
end;

procedure TToolsLibTest.TestModelNameFromTablePlural;
var
  modelName, customTable: string;
begin
  ModelNameFromTable('users', modelName, customTable);
  AssertEquals('user', modelName);
  AssertEquals('', customTable);

  ModelNameFromTable('user_groups', modelName, customTable);
  AssertEquals('user_group', modelName);
  AssertEquals('', customTable);
end;

procedure TToolsLibTest.TestModelNameFromTableNotPlural;
var
  modelName, customTable: string;
begin
  // "data" has no trailing s, the model needs the explicit table name
  ModelNameFromTable('data', modelName, customTable);
  AssertEquals('data', modelName);
  AssertEquals('data', customTable);
end;

procedure TToolsLibTest.TestModelNameFromTableCustom;
var
  modelName, customTable: string;
begin
  // TStatuModel maps back to "status" (TSimpleModel drops "_model", adds "s")
  ModelNameFromTable('status', modelName, customTable);
  AssertEquals('statu', modelName);
  AssertEquals('', customTable);

  // upper case table names never round trip through the lower case mapping
  ModelNameFromTable('Users', modelName, customTable);
  AssertEquals('Users', modelName);
  AssertEquals('Users', customTable);
end;

procedure TToolsLibTest.TestCopyDirectory;
var
  src, dst: string;
begin
  src := FTempDir + DirectorySeparator + 'src';
  dst := FTempDir + DirectorySeparator + 'dst';
  WriteText(src + DirectorySeparator + 'a.txt', 'A');
  WriteText(src + DirectorySeparator + '%projectname%.txt', 'P');
  WriteText(src + DirectorySeparator + 'sub' + DirectorySeparator +
    '%projectName%' + DirectorySeparator + 'b.txt', 'B');

  AssertTrue(CopyDirectory(src, dst, 'shop'));
  AssertEquals('A', ReadText(dst + DirectorySeparator + 'a.txt'));
  AssertEquals('P', ReadText(dst + DirectorySeparator + 'shop.txt'));
  AssertEquals('B', ReadText(dst + DirectorySeparator + 'sub' +
    DirectorySeparator + 'shop' + DirectorySeparator + 'b.txt'));
  AssertFalse(FileExists(dst + DirectorySeparator + '%projectname%.txt'));

  // without a project name the placeholder is kept
  AssertTrue(CopyDirectory(src, dst + '2'));
  AssertTrue(FileExists(dst + '2' + DirectorySeparator + '%projectname%.txt'));
end;

procedure TToolsLibTest.TestCopyDirectoryMissingSource;
begin
  AssertFalse(CopyDirectory(FTempDir + DirectorySeparator + 'missing',
    FTempDir + DirectorySeparator + 'dst'));
end;

procedure TToolsLibTest.TestHtaccessEnvironmentLines;
var
  headers: TStringList;
  s: string;
begin
  headers := TStringList.Create;
  try
    headers.Add('Authorization');
    headers.Add('');
    headers.Add('  X-Api-Key  ');
    headers.Add('Bad Name');
    headers.Add('Authorization');
    s := HtaccessEnvironmentLines(headers);
  finally
    headers.Free;
  end;
  AssertEquals('SetEnvIf Authorization "(.*)" Authorization=$1' + LineEnding +
    'SetEnvIf X-Api-Key "(.*)" X-Api-Key=$1' + LineEnding, s);
  AssertEquals('', HtaccessEnvironmentLines(nil));
end;

procedure TToolsLibTest.TestBuildHtaccess;
var
  headers: TStringList;
  s: string;
begin
  headers := TStringList.Create;
  try
    headers.Add('Token');
    s := BuildHtaccess('DirectoryIndex your_binary_file' + LineEnding +
      '#environment' + LineEnding + 'RewriteRule ^(.*)$ your_binary_file/$1',
      'app.bin', headers);
  finally
    headers.Free;
  end;
  AssertEquals('DirectoryIndex app.bin' + LineEnding +
    'SetEnvIf Token "(.*)" Token=$1' + LineEnding +
    'RewriteRule ^(.*)$ app.bin/$1', s);
  AssertEquals('x app.bin' + LineEnding, BuildHtaccess('x your_binary_file' +
    LineEnding + '#environment', 'app.bin'));
end;

procedure TToolsLibTest.TestFastCodeResponseText;
begin
  AssertEquals('// sum' + LineEnding + 'Result := a + b;',
    FastCodeResponseText('{"choices":[{"message":{"role":"assistant",' +
    '"content":"```pascal\n// sum\nResult := a + b;\n```"}}]}'));
end;

procedure TToolsLibTest.TestFastCodeResponseTextEscapes;
begin
  // JSON escapes are decoded by the parser, not by hand
  AssertEquals('s := ''a\b'' + #9 + "q";',
    FastCodeResponseText('{"choices":[{"message":{' +
    '"content":"s := ''a\\b'' + #9 + \"q\";"}}]}'));
end;

procedure TToolsLibTest.TestFastCodeResponseTextInvalid;
begin
  AssertEquals('', FastCodeResponseText(''));
  AssertEquals('', FastCodeResponseText('not json'));
  AssertEquals('', FastCodeResponseText('{"choices":[]}'));
  AssertEquals('', FastCodeResponseText('{"choices":[{"message":{"content":null}}]}'));
end;

procedure TToolsLibTest.TestConfigWithRunModeHTTP;
var
  data: TJSONData;
begin
  data := GetJSON(ConfigWithRunMode(
    '{ "systems" : { "sitename" : "FastPlaz" }, "database" : {} }', armHTTP));
  try
    AssertEquals('http', data.FindPath('systems.server.mode').AsString);
    AssertEquals('127.0.0.1', data.FindPath('systems.server.address').AsString);
    AssertEquals(8080, data.FindPath('systems.server.port').AsInteger);
    AssertEquals('FastPlaz', data.FindPath('systems.sitename').AsString);
    AssertNotNull(data.FindPath('database'));
  finally
    data.Free;
  end;
end;

procedure TToolsLibTest.TestConfigWithRunModeCGI;
var
  data: TJSONData;
begin
  // comments are accepted, a missing "systems" object is created
  data := GetJSON(ConfigWithRunMode('{ /* note */ "database" : {} }', armCGI));
  try
    AssertEquals('cgi', data.FindPath('systems.server.mode').AsString);
    AssertNull(data.FindPath('systems.server.port'));
    AssertNull(data.FindPath('systems.server.address'));
  finally
    data.Free;
  end;
end;

procedure TToolsLibTest.TestConfigWithRunModeKeepsServer;
var
  data: TJSONData;
begin
  data := GetJSON(ConfigWithRunMode(
    '{ "systems" : { "server" : { "mode" : "cgi", "port" : 9000, "static" : false } } }',
    armHTTP));
  try
    AssertEquals('http', data.FindPath('systems.server.mode').AsString);
    AssertEquals(9000, data.FindPath('systems.server.port').AsInteger);
    AssertEquals('127.0.0.1', data.FindPath('systems.server.address').AsString);
    AssertFalse(data.FindPath('systems.server.static').AsBoolean);
  finally
    data.Free;
  end;
end;

procedure TToolsLibTest.TestConfigWithRunModeInvalid;
begin
  AssertEquals('', ConfigWithRunMode('', armHTTP));
  AssertEquals('', ConfigWithRunMode('{ broken', armHTTP));
  AssertEquals('', ConfigWithRunMode('[1, 2]', armCGI));
end;

procedure TToolsLibTest.TestSetConfigRunMode;
var
  fileName: string;
  data: TJSONData;
begin
  fileName := FTempDir + DirectorySeparator + 'config' + DirectorySeparator +
    'config.json';
  AssertFalse('missing file', SetConfigRunMode(fileName, armHTTP));
  WriteText(fileName, '{ "systems" : { "sitename" : "x" } }');
  AssertTrue(SetConfigRunMode(fileName, armCGI));
  data := GetJSON(ReadText(fileName));
  try
    AssertEquals('cgi', data.FindPath('systems.server.mode').AsString);
    AssertEquals('x', data.FindPath('systems.sitename').AsString);
  finally
    data.Free;
  end;
  WriteText(fileName, 'not json');
  AssertFalse('invalid file', SetConfigRunMode(fileName, armCGI));
  AssertEquals('not json', Trim(ReadText(fileName)));
end;

initialization
  RegisterTest(TToolsLibTest);

end.

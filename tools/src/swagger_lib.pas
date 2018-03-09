unit swagger_lib;

{$mode objfpc}{$H+}

interface

uses
  fpjson, fphttpclient, RegExpr,
  Classes, SysUtils;

type

  { TSwaggerLib }

  TSwaggerLib = class(TInterfacedObject)
  private
    FData: TJSONData;
    FContent: TJSONObject;
    FDataString: TStringList;
    FDirectoryTarget: string;
    FIsDataLoaded: boolean;
    FProjectName: string;

    paramName, paramIn, paramType: string;
    getId, getComment: string;
    postId, postComment: string;
    putId, putComment: string;
    varGetCount, varPostCount: integer;
    varDefinition, sourceCode: TStringList;

    function getBasePath: string;
    function getContent: string;
    function GetDefinitionValue(AName: string): string;
    function getDescription: string;
    function getHost: string;
    function getPathCount: integer;
    function getTitle: string;
    function getURL: string;
    function getValue(AIndexName: string): string;
    function getValue(AIndexName: string; AUseSlash: boolean): string;
    function getVersion: string;

    procedure reset;
    function generateModURLName(const APathName: string): string;
    function generateRoutePattern(const APathName: string): string;
    function generateControllerName(const APathName: string): string;
    function generateFileName(const APathName: string): string;

    function pathContent(APath: string): string;

    function preg_replace(const RegexExpression, ReplaceString, SourceString: string;
      UseSubstitution: boolean): string;
  protected
  public
    varGlobal, varGet, varPost, varPut: TStringList;
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(AFileName: string): boolean;
    function LoadFromURL(AURL: string): boolean;
    property Data: TJSONData read FData write FData;
    property JsonData: TJSONObject read FContent;
    property Value[variable: string]: string read getValue; default;
    property Content: string read getContent;

    property URL: string read getURL;
    property Host: string read getHost;
    property BasePath: string read getBasePath;
    property PathCount: integer read getPathCount;

    property ProjectName: string read FProjectName write FProjectName;
    property Title: string read getTitle;
    property Description: string read getDescription;
    property Version: string read getVersion;

    property IsDataLoaded: boolean read FIsDataLoaded;
    property DirectoryTarget: string read FDirectoryTarget write FDirectoryTarget;
    function GenerateDefinition(const AIndex: integer): string;
    property Definiton[AName: string]: string read GetDefinitionValue;
  published
    property DefinitionGetId: string read getId;
    property DefinitionGetComment: string read getComment;
    property DefinitionPostId: string read postId;
    property DefinitionPostComment: string read postComment;
    property DefinitionPutId: string read putId;
    property DefinitionPutComment: string read putComment;
  end;

implementation

const
  FILENAME_SUFFIX = '_controller';
  TEMPLATE_SOURCE_FILE = 'templates' + DirectorySeparator + 'swagger' +
    DirectorySeparator + 'unit_template.pas';
  REGEX_PARAMETER = '{([a-zA-Z0-9]+)}';

function ucwords(const str: string): string;
var
  i: integer;
  s: string;
begin
  s := ' ' + lowerCase(str);
  for i := 1 to Length(s) do
  begin
    if s[i] = ' ' then
      s[i + 1] := upcase(s[i + 1]);
  end;
  Result := trim(s);
end;

{ TSwaggerLib }

function TSwaggerLib.getValue(AIndexName: string): string;
begin
  Result := getValue(AIndexName, True);
end;

function TSwaggerLib.getValue(AIndexName: string; AUseSlash: boolean): string;
begin
  Result := '';
  if not Assigned(FData) then
    Exit;
  try
    if AUseSlash then
      AIndexName := StringReplace(AIndexName, '/', '.', [rfReplaceAll]);
    Result := FData.GetPath(AIndexName).AsString;
  except
  end;
end;

function TSwaggerLib.getVersion: string;
begin
  Result := getValue('info/version');
end;

procedure TSwaggerLib.reset;
begin
  varPut.Clear;
  varPost.Clear;
  varGet.Clear;
  varGlobal.Clear;
end;

function TSwaggerLib.getContent: string;
begin
  Result := FDataString.Text;
end;

function TSwaggerLib.GetDefinitionValue(AName: string): string;
begin
  Result := varDefinition.Values[AName];
end;

function TSwaggerLib.getDescription: string;
begin
  Result := getValue('info/description');
end;

function TSwaggerLib.getURL: string;
begin
  Result := getValue('schemes[0]') + '://' + getValue('host') + getValue('basePath');
end;

function TSwaggerLib.getHost: string;
begin
  Result := getValue('host');
end;

function TSwaggerLib.getPathCount: integer;
begin
  Result := 0;
  if not Assigned(FData) then
    Exit;

  try
    Result := FData.GetPath('paths').Count;
  except
  end;
end;

function TSwaggerLib.getTitle: string;
begin
  Result := getValue('info/title');
end;

function TSwaggerLib.getBasePath: string;
begin
  Result := getValue('basePath');
end;

constructor TSwaggerLib.Create;
begin
  FDataString := TStringList.Create;
  FContent := TJSONObject.Create;
  FIsDataLoaded := False;
  FDirectoryTarget := '';
  FProjectName := 'Example';

  sourceCode := TStringList.Create;
  varGlobal := TStringList.Create;
  varGet := TStringList.Create;
  varPost := TStringList.Create;
  varPut := TStringList.Create;
  varDefinition := TStringList.Create;
end;

destructor TSwaggerLib.Destroy;
begin
  varDefinition.Free;
  varPut.Free;
  varPost.Free;
  varGet.Free;
  varGlobal.Free;
  FContent.Free;
  FDataString.Free;
  inherited Destroy;
end;

function TSwaggerLib.LoadFromFile(AFileName: string): boolean;
begin
  Result := False;
  FIsDataLoaded := False;
  if AFileName = '' then
    Exit;

  if not FileExists(AFileName) then
    Exit;

  FDataString.LoadFromFile(AFileName);
  try
    FData := GetJSON(FDataString.Text);
    FContent := TJSONObject(FData);
    FIsDataLoaded := True;
    Result := True;
  except
  end;
end;

function TSwaggerLib.LoadFromURL(AURL: string): boolean;
var
  http: TFPHTTPClient;
begin
  Result := False;
  FIsDataLoaded := False;
  if AURL = '' then
    Exit;

  http := TFPHTTPClient.Create(nil);
  try
    FDataString.Clear;
    FDataString.Text := http.Get(AURL);
    if http.ResponseStatusCode = 200 then
    begin
      FData := GetJSON(FDataString.Text);
      FIsDataLoaded := True;
      Result := True;
    end;
  except
  end;
  http.Free;

end;

function TSwaggerLib.generateModURLName(const APathName: string): string;
begin
  Result := preg_replace('{(.*)}', 'X', APathName, False);
  Result := Copy(Result, 2);
  Result := StringReplace(Result, '/', '', [rfReplaceAll]);
end;

function TSwaggerLib.generateRoutePattern(const APathName: string): string;
begin
  Result := preg_replace('{(.*)}', '(.*)', APathName, False);
  Result := '^' + Copy(Result, 2);
end;

function TSwaggerLib.generateControllerName(const APathName: string): string;
var
  s: string;
begin
  //T()Module
  Result := Trim(StringReplace(APathName, '/', '', [rfReplaceAll]));
  s := UpperCase(Result);
  Result[1] := s[1];
  Result := 'T' + Result + 'Module';
  Result := preg_replace('{(.*)}', 'X', Result, False);
end;

function TSwaggerLib.generateFileName(const APathName: string): string;
begin
  Result := Trim(StringReplace(APathName, '/', '', [rfReplaceAll]));
  Result := preg_replace('{(.*)}', '', Result, False);
  Result := LowerCase(Result);
  Result := Result + FILENAME_SUFFIX;
end;

function TSwaggerLib.GenerateDefinition(const AIndex: integer): string;
var
  i: integer;
  s, pathName: string;
begin
  Result := '';
  reset;

  pathName := FContent.Objects['paths'].Names[AIndex];

  // query parameters from URL patterns
  // ex:
  //   "/somethin/{id}"
  try
    with TRegExpr.Create do
    begin
      Expression := REGEX_PARAMETER;
      if Exec(pathName) then
      begin
        paramName := Match[1];
        varGlobal.Values[paramName] := 'string';
        while ExecNext do
        begin
          if Match[1] <> paramName then
          begin
            paramName := Match[1];
            varGlobal.Values[paramName] := 'string';
          end;
        end;
      end;
      Free;
    end;
  except
  end;

  // METHOD: GET
  getId := getValue('paths.' + pathName + '.get.operationId', False);
  getComment := getValue('paths.' + pathName + '.get.summary', False);
  // get parameters
  s := 'paths.' + pathName + '.get.parameters';
  varGetCount := 0;
  try
    varGetCount := FContent.GetPath(s).Count;
  except
  end;
  for i := 0 to varGetCount - 1 do
  begin
    paramName := pathContent(s + '[' + IntToStr(i) + '].name');
    paramIn := pathContent(s + '[' + IntToStr(i) + '].in');
    paramType := pathContent(s + '[' + IntToStr(i) + '].type');
    if varGlobal.IndexOfName(Trim(paramName)) = -1 then
      varGet.Values[Trim(paramName)] := paramIn;
  end;

  // METHOD: POST
  postId := getValue('paths.' + pathName + '.post.operationId', False);
  postComment := getValue('paths.' + pathName + '.post.summary', False);
  // get parameters
  s := 'paths.' + pathName + '.post.parameters';
  varPostCount := 0;
  try
    varPostCount := FContent.GetPath(s).Count;
  except
  end;
  for i := 0 to varPostCount - 1 do
  begin
    paramName := pathContent(s + '[' + IntToStr(i) + '].name');
    paramIn := pathContent(s + '[' + IntToStr(i) + '].in');
    paramType := pathContent(s + '[' + IntToStr(i) + '].type');
    if varGlobal.IndexOfName(Trim(paramName)) = -1 then
      varPost.Values[Trim(paramName)] := paramIn;
  end;

  // METHOD: PUT
  putId := getValue('paths.' + pathName + '.put.operationId', False);
  putComment := getValue('paths.' + pathName + '.put.summary', False);

  varDefinition.Values['pathName'] := FContent.Objects['paths'].Names[AIndex];
  varDefinition.Values['controllerName'] := generateControllerName(pathName);
  varDefinition.Values['routePattern'] := generateRoutePattern(pathName);
  varDefinition.Values['moduleName'] := generateModURLName(pathName);
  varDefinition.Values['fileName'] := generateFileName(pathName);

  Result := varGlobal.Text + #13'GET:'#13 + varGet.Text + #13'POST:'#13 + varPost.Text;
end;

function TSwaggerLib.pathContent(APath: string): string;
begin
  try
    Result := '';
    Result := FContent.GetPath(APath).AsString;
  except
  end;
end;

function TSwaggerLib.preg_replace(const RegexExpression, ReplaceString,
  SourceString: string; UseSubstitution: boolean): string;


begin
  try
    with TRegExpr.Create do
    begin
      Expression := RegexExpression;
      Result := Replace(SourceString, ReplaceString, UseSubstitution);
      Free;
    end;
  except
    Result := SourceString;
  end;
end;

end.

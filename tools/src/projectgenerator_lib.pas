unit projectgenerator_lib;

{$mode objfpc}{$H+}

interface

uses
  LazFileUtils,
  RegExpr, XMLConf,
  swagger_lib,
  FileUtil, FileConverter,
  Classes, SysUtils;

type

  TReadmeFormat = (rfMarkDown, rfHTML);
  TSourceFormat = (sfSwagger, sfUnknown);
  TProjectType = (ptServer, ptClient);

  { TProjectGenerator }

  TProjectGenerator = class
  private
    FDebug: boolean;
    FDirectoryTarget: string;
    FIsBeautify: boolean;
    FIsOverwrite: boolean;
    FProjectName: string;
    FProjectType: TProjectType;
    FSourceFormat: TSourceFormat;
    FReadmeFormat: TReadmeFormat;
    Ftmp_: TStringList;

    fileList, sourceCode: TStringList;

    function getBasePath: string;
    function getContent: string;
    function getDescription: string;
    function getHost: string;
    function getPathCount: integer;
    function getTitle: string;
    function getURL: string;
    function getVersion: string;
    function scanDirAndCopy(ASourceDirectory, ATargetDirectory: string): boolean;

    // Client
    function clientGenerate: boolean;

    // Server
    function serverGenerate: boolean;
    function serverGenerateStructure: boolean;
    function serverGenerateControllers: boolean;
    function serverGenerateControllersSource(const AIndex: integer): string;

    procedure reset;
    procedure sourceUpdate(AKey, AValue: string);
    procedure log(AText: string);
  public
    Swagger: TSwaggerLib;
    constructor Create;
    destructor Destroy; override;

    function Exec: boolean;
    function LoadFromFile(AFileName: string): boolean;
  published
    property Debug: boolean read FDebug write FDebug;
    property Content: string read getContent;
    property SourceFormat: TSourceFormat read FSourceFormat write FSourceFormat;
    property ReadmeFormat: TReadmeFormat read FReadmeFormat write FReadmeFormat;

    property ProjectType: TProjectType read FProjectType write FProjectType;
    property ProjectName: string read FProjectName write FProjectName;
    property DirectoryTarget: string read FDirectoryTarget write FDirectoryTarget;
    property IsOverwrite: boolean read FIsOverwrite write FIsOverwrite;
    property IsBeautify: boolean read FIsBeautify write FIsBeautify;

    property Title: string read getTitle;
    property Description: string read getDescription;
    property Version: string read getVersion;
    property URL: string read getURL;
    property Host: string read getHost;
    property BasePath: string read getBasePath;
    property PathCount: integer read getPathCount;

    property tmp_: TStringList read Ftmp_;
  end;

implementation

const
  SWAGGER_SERVER_TEMPLATE_STRUCTURE_PATH =
    'templates' + DirectorySeparator + 'swagger' + DirectorySeparator +
    'directory_structure' + DirectorySeparator + 'server' + DirectorySeparator + '*';
  SWAGGER_TEMPLATE_SOURCE_FILE =
    'templates' + DirectorySeparator + 'swagger' + DirectorySeparator +
    'unit_template.pas';
  CONTROLLER_DIRNAME = 'controller';
  REGEX_PARAMETER = '{([a-zA-Z0-9]+)}';

{ TProjectGenerator }

function TProjectGenerator.getContent: string;
begin
  if FSourceFormat = sfSwagger then
    Result := Swagger.Content;
end;

function TProjectGenerator.getBasePath: string;
begin
  Result := Swagger.BasePath;
end;

function TProjectGenerator.getDescription: string;
begin
  Result := Swagger.Description;
end;

function TProjectGenerator.getHost: string;
begin
  Result := Swagger.Host;
end;

function TProjectGenerator.getPathCount: integer;
begin
  Result := Swagger.PathCount;
end;

function TProjectGenerator.getTitle: string;
begin
  Result := Swagger.Title;
end;

function TProjectGenerator.getURL: string;
begin
  Result := Swagger.URL;
end;

function TProjectGenerator.getVersion: string;
begin
  Result := Swagger.Version;
end;

constructor TProjectGenerator.Create;
begin
  FDebug := False;
  FIsOverwrite := False;
  FIsBeautify := False;
  FProjectName := 'Example';
  FDirectoryTarget := '';
  FSourceFormat := sfSwagger;
  FReadmeFormat := rfMarkDown;
  FProjectType := ptClient;

  fileList := TStringList.Create;
  sourceCode := TStringList.Create;

  Swagger := TSwaggerLib.Create;
  Ftmp_ := TStringList.Create; //todo: remove
end;

destructor TProjectGenerator.Destroy;
begin
  Ftmp_.Free;
  Swagger.Free;
  fileList.Free;
  sourceCode.Free;
  inherited Destroy;
end;

function TProjectGenerator.scanDirAndCopy(ASourceDirectory, ATargetDirectory:
  string): boolean;
var
  Rec: TSearchRec;
  R: integer;
  lsFileSource, lsFileTarget, lsSubDir, lsTargetDir: string;
begin
  Result := False;
  try
    R := FindFirst(ASourceDirectory, faAnyFile, Rec);
    while R = 0 do
    begin
      if ((Rec.Attr and faDirectory) <> 0) and (Rec.Name <> '.') and
        (Rec.Name <> '..') then
      begin
        lsSubDir := extractfilepath(ASourceDirectory) + Rec.Name;
        lsTargetDir := StringReplace(Rec.Name, '%projectName%',
          FProjectName, [rfReplaceAll]);
        ForceDirectories(ATargetDirectory + DirectorySeparator + lsTargetDir);
        ScanDirAndCopy(lsSubDir + DirectorySeparator + '*', ATargetDirectory +
          DirectorySeparator + lsTargetDir);
      end
      else
      begin
        if ((Rec.Name <> '.') and (Rec.Name <> '..')) then
        begin
          lsFileSource := extractfilepath(ASourceDirectory) + Rec.Name;
          lsFileTarget := ATargetDirectory + DirectorySeparator + Rec.Name;
          lsFileTarget := StringReplace(lsFileTarget, '%projectName%',
            FProjectName, [rfReplaceAll]);
          CopyFile(lsFileSource, lsFileTarget);
        end;
      end;
      R := FindNext(Rec);
    end;
  finally
    FindClose(Rec);
    Result := True;
  end;
end;

function TProjectGenerator.serverGenerateStructure: boolean;
var
  s: string;
begin
  Result := False;
  s := IncludeTrailingPathDelimiter(FDirectoryTarget);
  if not ForceDirectoriesUTF8(s) then
  begin
    exit;
  end;

  Result := scanDirAndCopy(SWAGGER_SERVER_TEMPLATE_STRUCTURE_PATH, s);
end;

function TProjectGenerator.serverGenerateControllersSource(
  const AIndex: integer): string;
var
  i: integer;
  s, pathName, fileName, controllerName, moduleName, routePattern: string;
  globalVars, getVars, postVars, putVars: string;
  getSource, postSource, putSource: string;

  function addGetSource(ASource: string): string;
  begin
    getSource := getSource + #13 + ASource;
  end;

  function addPostSource(ASource: string): string;
  begin
    postSource := postSource + #13 + ASource;
  end;

  function addPutSource(ASource: string): string;
  begin
    putSource := putSource + #13 + ASource;
  end;

begin
  pathName := Swagger.JsonData.Objects['paths'].Names[AIndex];
  log('= PATH ======= ' + pathName);

  s := Swagger.GenerateDefinition(AIndex);

  pathName := Swagger.Definiton['pathName'];
  controllerName := Swagger.Definiton['controllerName'];
  routePattern := Swagger.Definiton['routePattern'];
  moduleName := Swagger.Definiton['moduleName'];
  fileName := Swagger.Definiton['fileName'];
  fileList.Add(fileName);

  // GENERATE SOURCE FROM TEMPLATE
  sourcecode.LoadFromFile(SWAGGER_TEMPLATE_SOURCE_FILE);
  sourceUpdate('%usage%', pathName);
  sourceUpdate('%projectName%', Swagger.ProjectName);
  sourceUpdate('%fileName%', fileName);
  sourceUpdate('%controllerName%', controllerName);
  sourceUpdate('%moduleName%', moduleName);
  sourceUpdate('%routePattern%', routePattern);

  globalVars := '';
  getVars := '';
  postVars := '';
  putVars := '';
  getSource := '';
  postSource := '';
  putSource := '';

  // GLOBAL
  for i := 0 to Swagger.varGlobal.Count - 1 do
  begin
    if i = 0 then
      globalVars := Swagger.varGlobal.Names[i]
    else
      globalVars := globalVars + ', ' + Swagger.varGlobal.Names[i];
    addGetSource(Format('%s := _GET[''%s''];',
      [Swagger.varGlobal.Names[i], Swagger.varGlobal.Names[i]]));
    addPostSource(Format('%s := _GET[''%s''];',
      [Swagger.varGlobal.Names[i], Swagger.varGlobal.Names[i]]));
    addPutSource(Format('%s := _GET[''%s''];',
      [Swagger.varGlobal.Names[i], Swagger.varGlobal.Names[i]]));
  end;
  if globalVars <> '' then
    globalVars := globalVars + ': String;';

  // GET
  for i := 0 to Swagger.varGet.Count - 1 do
  begin
    if i = 0 then
      getVars := Swagger.varGet.Names[i]
    else
      getVars := getVars + ', ' + Swagger.varGet.Names[i];
    addGetSource(Format('%s := _GET[''%s''];',
      [Swagger.varGet.Names[i], Swagger.varGet.Names[i]]));
    addPostSource(Format('%s := _GET[''%s''];',
      [Swagger.varGet.Names[i], Swagger.varGet.Names[i]]));
    addPutSource(Format('%s := _GET[''%s''];',
      [Swagger.varGet.Names[i], Swagger.varGet.Names[i]]));
  end;
  if getVars <> '' then
    getVars := getVars + ': String;';

  // POST
  for i := 0 to Swagger.varPost.Count - 1 do
  begin
    if i = 0 then
      postVars := Swagger.varPost.Names[i]
    else
      postVars := postVars + ', ' + Swagger.varPost.Names[i];
    s := Swagger.varPost.ValueFromIndex[i];
    case s of
      'query': addPostSource(Format('%s := _GET[''%s''];',
          [Swagger.varPost.Names[i], Swagger.varPost.Names[i]]));
      'path': addPostSource(Format('%s := _GET[''%s''];',
          [Swagger.varPost.Names[i], Swagger.varPost.Names[i]]));
      'body': addPostSource(Format('%s := Request.Content;',
          [Swagger.varPost.Names[i]]));
    end;
  end;
  if postVars <> '' then
    postVars := postVars + ': String;';

  sourceUpdate('%getId%', Swagger.DefinitionGetId);
  sourceUpdate('%getComment%', Swagger.DefinitionGetComment);
  sourceUpdate('%postId%', Swagger.DefinitionPostId);
  sourceUpdate('%postComment%', Swagger.DefinitionPostComment);
  sourceUpdate('%putId%', Swagger.DefinitionPutId);
  sourceUpdate('%putComment%', Swagger.DefinitionPutComment);

  sourceUpdate('%globalVars%', globalVars);
  sourceUpdate('%getVars%', getVars);
  sourceUpdate('%postVars%', postVars);
  sourceUpdate('%putVars%', putVars);
  sourceUpdate('%getSource%', getSource);
  sourceUpdate('%postSource%', postSource);
  sourceUpdate('%putSource%', putSource);

  Result := sourcecode.Text;
end;

procedure TProjectGenerator.reset;
begin
end;

function TProjectGenerator.serverGenerateControllers: boolean;
var
  i: integer;
  fileName, pathName: string;
begin
  Result := False;
  if not Swagger.IsDataLoaded then
    Exit;

  pathName := IncludeTrailingPathDelimiter(FDirectoryTarget) + 'source' +
    DirectorySeparator + FProjectName + DirectorySeparator +
    CONTROLLER_DIRNAME + DirectorySeparator;

  for i := 1 to Swagger.PathCount do
  //for i := 1 to 1 do
  begin
    sourceCode.Text := serverGenerateControllersSource(i - 1);
    if FIsOverwrite then
    begin
      fileName := pathName + Swagger.Definiton['fileName'] + '.pas';
      if FileExistsUTF8(fileName) then
        RenameFile(fileName, pathName + Swagger.Definiton['fileName'] +
          '-' + FormatDateTime('mmddhhnnss', Now) + '.pas');
      sourceCode.SaveToFile(fileName);

      if FIsBeautify then
      begin
        with TFileConverter.Create do
        begin
          ProcessFile(fileName);
          Free;
        end;
      end;
    end;
  end;

  Result := True;
end;

function TProjectGenerator.clientGenerate: boolean;
begin
  Result := False;
end;

function TProjectGenerator.serverGenerate: boolean;
var
  i, count : integer;
  s, pathName: string;
  lpiFile: TXMLConfig;
begin
  Result := False;
  reset;
  if not serverGenerateStructure then
    Exit;
  if not serverGenerateControllers then
    Exit;

  pathName := IncludeTrailingPathDelimiter(FDirectoryTarget) + 'source' +
    DirectorySeparator + FProjectName + DirectorySeparator;
  sourceCode.LoadFromFile(pathName + 'app.lpr');
  fileList.Text := StringReplace(fileList.Text, #13, ',', [rfReplaceAll]);
  sourceUpdate('%files%', fileList.Text);
  sourceCode.SaveToFile(pathName + 'app.lpr');
  if FIsBeautify then
  begin
    with TFileConverter.Create do
    begin
      ProcessFile(pathName + 'app.lpr');
      Free;
    end;
  end;

  // Project lpi setting
  lpiFile := TXMLConfig.Create(nil);
  lpiFile.LoadFromFile( pathName + 'app.lpi');
  lpiFile.OpenKey('ProjectOptions');
  count := lpiFile.GetValue('Units/Count', 0);
  lpiFile.CloseKey;
  lpiFile.OpenKey('ProjectOptions/Units');
  for i:=0 to fileList.Count-1 do
  begin
    s := 'controller' + DirectorySeparator + StringReplace( fileList[i], ',', '', [rfReplaceAll]) + '.pas';
    lpiFile.SetValue('Unit'+IntToStr(count+i)+'/Filename/Value', s);
    lpiFile.SetValue('Unit'+IntToStr(count+i)+'/IsPartOfProject/Value', True);
  end;
  lpiFile.CloseKey;
  lpiFile.SetValue('ProjectOptions/Units/Count', fileList.Count + count);
  lpiFile.SaveToFile( pathName + 'app.lpi');
  lpiFile.Free;

  Result := True;
end;

function TProjectGenerator.Exec: boolean;
begin
  Result := False;
  Swagger.ProjectName := FProjectName;
  Swagger.DirectoryTarget := FDirectoryTarget;

  fileList.Clear;

  if FProjectType = ptServer then
    Result := serverGenerate
  else
    Result := clientGenerate;
end;

procedure TProjectGenerator.sourceUpdate(AKey, AValue: string);
begin
  sourcecode.Text := StringReplace(sourcecode.Text, AKey, AValue, [rfReplaceAll]);
end;

procedure TProjectGenerator.log(AText: string);
begin
  if FDebug then
    Ftmp_.Add(AText);
end;


function TProjectGenerator.LoadFromFile(AFileName: string): boolean;
begin
  Result := Swagger.LoadFromFile(AFileName);
end;

end.

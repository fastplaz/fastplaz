unit aiplane_tools_fastplaz;

{
  FastPlaz generator tools for the Agent Plane MCP server. They do what the
  FastPlaz menu items do, without the wizard dialogs:

    create_simple_controller  FastPlaz | Create Simple Controller
    create_api_controller     FastPlaz | Create API Controller (JSON)
    create_model              FastPlaz | Database Model Generator

  The new unit is saved in the project directory (or a sub directory of it),
  added to the project and opened in the source editor. Existing files are
  never overwritten.

  The permission card says which file would be created (DisplayFile) and
  with which class, before anything happens.

  The FastPlaz reference for agents (aiplane_reference) is served twice:
  as the MCP resource fastplaz://reference and as the tool
  get_fastplaz_reference, for clients that do not read resources.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes, fpjson, LazFileUtils, LazIDEIntf,
  ProjectIntf, aiplane_mcpserver, aiplane_reference;

type

  { TAIFastPlazTools }

  TAIFastPlazTools = class
  private
    function CheckProject(ACall: TAIMcpToolCall; out AProjectDir: string): boolean;
    function TargetDirectory(ACall: TAIMcpToolCall; const AProjectDir: string;
      out ADirectory: string): boolean;
    function EnsureDirectory(ACall: TAIMcpToolCall; const ADirectory: string): boolean;
    function CheckNewFile(ACall: TAIMcpToolCall; const AFileName: string): boolean;
    function InUnitPath(const ADirectory, AProjectDir: string): boolean;
    procedure AddUnitPathInfo(ARes: TJSONObject; const ADirectory, AProjectDir: string);
    procedure CreateController(ACall: TAIMcpToolCall; AsAPI: boolean);
    procedure ToolSimpleController(ACall: TAIMcpToolCall);
    procedure ToolApiController(ACall: TAIMcpToolCall);
    procedure ToolModel(ACall: TAIMcpToolCall);
    function DisplayFile(ACall: TAIMcpToolCall; const AFileName: string): string;
    function DescribeController(ACall: TAIMcpToolCall): string;
    function DescribeModel(ACall: TAIMcpToolCall): string;
    function ReferenceFile: string;
    function ReadReference(const AUri: string; out AText: string): boolean;
    procedure ToolReference(ACall: TAIMcpToolCall);
  public
    procedure Register(AServer: TAIMcpServer);
  end;

const
  AI_FASTPLAZ_TOOLS_INSTRUCTIONS =
    'The project uses FastPlaz, a web framework for Free Pascal. Before ' +
    'writing or changing FastPlaz code (controllers, routes, themes, ' +
    'TSimpleModel, TJSONUtil, config.json), read the FastPlaz reference: ' +
    'the resource fastplaz://reference, or the tool get_fastplaz_reference ' +
    '(its section argument returns one part). Do not guess FastPlaz APIs. ' +
    'FastPlaz generators: create_simple_controller, create_api_controller and ' +
    'create_model write a new unit from the FastPlaz templates, add it to the ' +
    'project and open it. Prefer them over writing a controller or model unit ' +
    'by hand, then edit the generated file. In FastPlaz a module is a ' +
    'controller: when the user asks to create a module, create a controller ' +
    '(a web page module: create_simple_controller, a JSON/API module: ' +
    'create_api_controller), not a package, folder or plain unit. The ' +
    'generators never overwrite: an existing file is an error, so offer ' +
    'another name or editing the existing unit instead.';

implementation

uses
  fastplaz_tools_register, tools_lib, modsimple_lib, model_lib;

const
  NEW_UNIT_FLAGS = [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc, nfSave];

  // what the generators refuse, so the agent does not offer it
  GENERATOR_NOTES =
    ' Never overwrites: when the file already exists the call fails and ' +
    'nothing changes (use another name, or edit the existing unit). The ' +
    'directory must be inside the project directory; the result says ' +
    'whether it is in the project unit search path (in_unit_path).';
  CONTROLLER_NOTES =
    ' "Create a module" in FastPlaz means create a controller.' + GENERATOR_NOTES;

  SCHEMA_CONTROLLER =
    '{"type":"object","properties":{' +
    '"name":{"type":"string","description":"Module name in words, e.g. \"user group\" ' +
    'gives TUserGroupController (a joined \"UserGroup\" gives TUsergroupController)"},' +
    '"permalink":{"type":"string","description":"Route of the controller, e.g. \"user-group\". ' +
    'Default: the name. The file is <permalink>_controller.pas"},' +
    '"directory":{"type":"string","description":"Directory for the unit, relative to the ' +
    'project directory or absolute inside it. Default: the project directory"}},' +
    '"required":["name"]}';
  SCHEMA_REFERENCE =
    '{"type":"object","properties":{' +
    '"section":{"type":"string","description":"Part of a heading, e.g. \"routes\", ' +
    '\"model\" or \"theme\": only that section is returned. Empty: the whole reference"}}}';
  SCHEMA_MODEL =
    '{"type":"object","properties":{' +
    '"name":{"type":"string","description":"Model name in words, e.g. \"user group\" ' +
    'gives TUserGroupModel in usergroup_model.pas. Optional when table is given"},' +
    '"table":{"type":"string","description":"Database table. Without name the model name ' +
    'comes from it (\"users\" gives TUserModel). Written into the constructor only when ' +
    'TSimpleModel would not derive it from the class name"},' +
    '"directory":{"type":"string","description":"Directory for the unit, relative to the ' +
    'project directory or absolute inside it. Default: the project directory"}}}';

function SplitPaths(const APaths: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := ';';
  Result.StrictDelimiter := True;
  Result.DelimitedText := APaths;
end;

function HasControlChars(const AText: string): boolean;
var
  i: integer;
begin
  for i := 1 to Length(AText) do
    if AText[i] < ' ' then
      Exit(True);
  Result := False;
end;

{ TAIFastPlazTools }

procedure TAIFastPlazTools.Register(AServer: TAIMcpServer);
begin
  if AServer.Instructions <> '' then
    AServer.Instructions := AServer.Instructions + ' ';
  AServer.Instructions := AServer.Instructions + AI_FASTPLAZ_TOOLS_INSTRUCTIONS;
  AServer.RegisterTool('create_simple_controller', 'Create Simple Controller',
    'FastPlaz | Create Simple Controller: a web page controller (TMyCustomController ' +
    'with Get/Post, rendered through the theme, maincontent tag handler). Saves ' +
    '<permalink>_controller.pas, adds it to the project and opens it. The unit ' +
    'registers its route in its initialization section; move that line to ' +
    'routes.pas when the project has one.' + CONTROLLER_NOTES, SCHEMA_CONTROLLER,
    mtpAskSession, @ToolSimpleController);
  AServer.RegisterTool('create_api_controller', 'Create API Controller',
    'FastPlaz | Create API Controller (JSON): a REST controller answering JSON ' +
    '(Get/Post/Options). Saves <permalink>_controller.pas, adds it to the project ' +
    'and opens it. The unit registers its route in its initialization section; ' +
    'move that line to routes.pas when the project has one.' + CONTROLLER_NOTES,
    SCHEMA_CONTROLLER, mtpAskSession, @ToolApiController);
  AServer.RegisterTool('create_model', 'Database Model Generator',
    'FastPlaz | Database Model Generator: a TSimpleModel unit for a database ' +
    'table. Saves <name>_model.pas, adds it to the project and opens it.' +
    GENERATOR_NOTES, SCHEMA_MODEL, mtpAskSession, @ToolModel);
  AServer.SetToolDescriber('create_simple_controller', @DescribeController);
  AServer.SetToolDescriber('create_api_controller', @DescribeController);
  AServer.SetToolDescriber('create_model', @DescribeModel);
  AServer.RegisterTool('get_fastplaz_reference', 'FastPlaz reference',
    'How to write FastPlaz code: project layout, controllers, routes, request ' +
    'and session data, themes and template tags, TSimpleModel, TJSONUtil, ' +
    'config.json, helpers and pitfalls. The same text as the resource ' +
    AI_REFERENCE_URI + '. Read it before writing FastPlaz code.',
    SCHEMA_REFERENCE, mtpAuto, @ToolReference, [mthReadOnly, mthIdempotent]);
  AServer.RegisterResource(AI_REFERENCE_URI, 'fastplaz-reference',
    'FastPlaz reference',
    'How to write FastPlaz code: controllers, routes, themes, TSimpleModel, ' +
    'TJSONUtil, config.json and pitfalls.', 'text/markdown', @ReadReference);
end;

function TAIFastPlazTools.ReferenceFile: string;
begin
  Result := AIReferenceFileName(PackageDirectory(FASTPLAZ_TOOLS_PACKAGE));
end;

function TAIFastPlazTools.ReadReference(const AUri: string; out AText: string): boolean;
begin
  Result := AILoadReference(ReferenceFile, AText);
end;

procedure TAIFastPlazTools.ToolReference(ACall: TAIMcpToolCall);
var
  text, section, part: string;
  found: boolean;
begin
  if not AILoadReference(ReferenceFile, text) then
  begin
    ACall.Fail('The FastPlaz reference cannot be read: ' + text);
    Exit;
  end;
  section := Trim(ACall.Str('section'));
  part := AIReferenceSection(text, section, found);
  if not found then
  begin
    ACall.Fail('No section matches "' + section + '". Sections: ' +
      AIReferenceHeadings(text));
    Exit;
  end;
  ACall.Finish(part);
end;

// The file as the user would look for it: relative to the project directory
// when it is inside it. Nothing is checked here; the tool itself does that.
function TAIFastPlazTools.DisplayFile(ACall: TAIMcpToolCall;
  const AFileName: string): string;
var
  projectDir, directory: string;
begin
  directory := SetDirSeparators(Trim(ACall.Str('directory')));
  Result := AFileName;
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) and
    FilenameIsAbsolute(LazarusIDE.ActiveProject.ProjectInfoFile) then
  begin
    projectDir := AppendPathDelim(ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile));
    if directory = '' then
      directory := projectDir
    else if not FilenameIsAbsolute(directory) then
      directory := projectDir + directory;
    directory := AppendPathDelim(TrimFilename(ExpandFileNameUTF8(directory)));
    Result := directory + AFileName;
    if PathIsInPath(directory, projectDir) then
      Result := CreateRelativePath(Result, projectDir);
  end
  else if directory <> '' then
    Result := AppendPathDelim(directory) + AFileName;
end;

function TAIFastPlazTools.DescribeController(ACall: TAIMcpToolCall): string;
var
  moduleName, link: string;
begin
  Result := '';
  moduleName := Trim(ACall.Str('name'));
  if moduleName = '' then
    Exit;
  link := ControllerPermalink(moduleName, Trim(ACall.Str('permalink')));
  Result := 'Creates ' + DisplayFile(ACall, ControllerFileName(link)) + LineEnding +
    Format('Class %s, route /%s', [ControllerTypeName(moduleName), link]);
end;

function TAIFastPlazTools.DescribeModel(ACall: TAIMcpToolCall): string;
var
  modelText, table, fileName, modelClass, customTable, unused: string;
begin
  Result := '';
  modelText := Trim(ACall.Str('name'));
  table := Trim(ACall.Str('table'));
  if ((modelText = '') and (table = '')) or HasControlChars(table) then
    Exit;
  ResolveModelNames(modelText, table, unused, modelClass, customTable, fileName);
  if customTable = '' then
    customTable := TableNameFromModelClass(modelClass);
  Result := 'Creates ' + DisplayFile(ACall, fileName) + LineEnding +
    Format('Class %s, table %s', [modelClass, customTable]);
end;

function TAIFastPlazTools.CheckProject(ACall: TAIMcpToolCall;
  out AProjectDir: string): boolean;
var
  proj: TLazProject;
begin
  Result := False;
  AProjectDir := '';
  proj := nil;
  if Assigned(LazarusIDE) then
    proj := LazarusIDE.ActiveProject;
  if not Assigned(proj) then
  begin
    ACall.Fail('No project is open.');
    Exit;
  end;
  if not FilenameIsAbsolute(proj.ProjectInfoFile) then
  begin
    ACall.Fail('The project has not been saved yet. Ask the user to save it first.');
    Exit;
  end;
  AProjectDir := AppendPathDelim(ExtractFilePath(proj.ProjectInfoFile));
  Result := True;
end;

// Only the project directory and its sub directories.
function TAIFastPlazTools.TargetDirectory(ACall: TAIMcpToolCall;
  const AProjectDir: string; out ADirectory: string): boolean;
begin
  Result := False;
  ADirectory := SetDirSeparators(Trim(ACall.Str('directory')));
  if ADirectory = '' then
    ADirectory := AProjectDir
  else
  begin
    if not FilenameIsAbsolute(ADirectory) then
      ADirectory := AProjectDir + ADirectory;
    ADirectory := AppendPathDelim(TrimFilename(ExpandFileNameUTF8(ADirectory)));
  end;
  if not PathIsInPath(ADirectory, AProjectDir) then
  begin
    ACall.Fail('directory must be inside the project directory ' + AProjectDir);
    Exit;
  end;
  if FileExistsUTF8(ChompPathDelim(ADirectory)) and
    not DirectoryExistsUTF8(ADirectory) then
  begin
    ACall.Fail('Not a directory: ' + ADirectory);
    Exit;
  end;
  Result := True;
end;

// Creates a missing sub directory, after the other checks passed.
function TAIFastPlazTools.EnsureDirectory(ACall: TAIMcpToolCall;
  const ADirectory: string): boolean;
begin
  Result := DirectoryExistsUTF8(ADirectory) or ForceDirectoriesUTF8(ADirectory);
  if not Result then
    ACall.Fail('Could not create the directory ' + ADirectory);
end;

function TAIFastPlazTools.CheckNewFile(ACall: TAIMcpToolCall;
  const AFileName: string): boolean;
begin
  Result := False;
  if FileExistsUTF8(AFileName) or
    Assigned(LazarusIDE.ActiveProject.FindFile(AFileName, [])) then
  begin
    ACall.Fail('File already exists, nothing was changed: ' + AFileName +
      '. Choose another name or permalink.');
    Exit;
  end;
  Result := True;
end;

function TAIFastPlazTools.InUnitPath(const ADirectory, AProjectDir: string): boolean;
var
  dirs: TStringList;
  i: integer;
  d: string;
begin
  if CompareFilenames(ADirectory, AProjectDir) = 0 then
    Exit(True);
  dirs := SplitPaths(LazarusIDE.ActiveProject.LazCompilerOptions.GetUnitPath(False));
  try
    for i := 0 to dirs.Count - 1 do
    begin
      d := Trim(dirs[i]);
      if d = '' then
        Continue;
      if not FilenameIsAbsolute(d) then
        d := AProjectDir + d;
      d := AppendPathDelim(TrimFilename(d));
      if CompareFilenames(d, ADirectory) = 0 then
        Exit(True);
    end;
  finally
    dirs.Free;
  end;
  Result := False;
end;

procedure TAIFastPlazTools.AddUnitPathInfo(ARes: TJSONObject;
  const ADirectory, AProjectDir: string);
var
  found: boolean;
begin
  found := InUnitPath(ADirectory, AProjectDir);
  ARes.Add('in_unit_path', found);
  if not found then
    ARes.Add('note', 'The directory is not in the project unit search path; ' +
      'add it (project_info shows the paths), or the program cannot find the unit.');
end;

procedure TAIFastPlazTools.CreateController(ACall: TAIMcpToolCall; AsAPI: boolean);
var
  projectDir, directory, moduleName, link, fileName: string;
  r: TModalResult;
  res: TJSONObject;
begin
  moduleName := Trim(ACall.Str('name'));
  link := Trim(ACall.Str('permalink'));
  if moduleName = '' then
  begin
    ACall.Fail('name is required');
    Exit;
  end;
  if not CheckProject(ACall, projectDir) or
    not TargetDirectory(ACall, projectDir, directory) then
    Exit;
  // checked before the IDE creates anything
  fileName := directory + ControllerFileName(ControllerPermalink(moduleName, link));
  if not CheckNewFile(ACall, fileName) or not EnsureDirectory(ACall, directory) then
    Exit;

  r := NewControllerUnit(AsAPI, moduleName, link, directory, NEW_UNIT_FLAGS, fileName);
  if r <> mrOk then
  begin
    ACall.Fail('The IDE did not create the controller (cancelled or failed).');
    Exit;
  end;
  res := TJSONObject.Create;
  res.Add('file', fileName);
  res.Add('class', ModulTypeName);
  res.Add('permalink', Permalink);
  res.Add('route', Format('Route[''%s''] := %s;', [Permalink, ModulTypeName]));
  res.Add('saved', FileExistsUTF8(fileName));
  AddUnitPathInfo(res, directory, projectDir);
  ACall.FinishJSON(res);
end;

procedure TAIFastPlazTools.ToolSimpleController(ACall: TAIMcpToolCall);
begin
  CreateController(ACall, False);
end;

procedure TAIFastPlazTools.ToolApiController(ACall: TAIMcpToolCall);
begin
  CreateController(ACall, True);
end;

procedure TAIFastPlazTools.ToolModel(ACall: TAIMcpToolCall);
var
  projectDir, directory, modelText, table, fileName, modelClass, customTable,
  unused: string;
  r: TModalResult;
  res: TJSONObject;
begin
  modelText := Trim(ACall.Str('name'));
  table := Trim(ACall.Str('table'));
  if (modelText = '') and (table = '') then
  begin
    ACall.Fail('name or table is required');
    Exit;
  end;
  if HasControlChars(table) then
  begin
    ACall.Fail('table must be a single line');
    Exit;
  end;
  if not CheckProject(ACall, projectDir) or
    not TargetDirectory(ACall, projectDir, directory) then
    Exit;
  // checked before the IDE creates anything
  ResolveModelNames(modelText, table, unused, modelClass, customTable, fileName);
  fileName := directory + fileName;
  if not CheckNewFile(ACall, fileName) or not EnsureDirectory(ACall, directory) then
    Exit;

  r := NewModelUnit(modelText, table, directory, NEW_UNIT_FLAGS, fileName);
  if r <> mrOk then
  begin
    ACall.Fail('The IDE did not create the model (cancelled or failed).');
    Exit;
  end;
  res := TJSONObject.Create;
  res.Add('file', fileName);
  res.Add('class', modelClass);
  if customTable <> '' then
    res.Add('table', customTable)
  else
    res.Add('table', TableNameFromModelClass(modelClass));
  res.Add('custom_table_name', customTable <> '');
  res.Add('saved', FileExistsUTF8(fileName));
  AddUnitPathInfo(res, directory, projectDir);
  ACall.FinishJSON(res);
end;

end.

unit aiplane_tools_ide;

{
  IDE tools for the AI Plane MCP server (phase 1 of
  docs/AI_PLANE_TOOL_RUNDOWN.md):

    open_file           open a file in the source editor, optionally at a line
    get_active_editor   file, caret and selection of the active editor
    list_open_files     files open in the source editor
    read_editor         text of a file from its editor buffer (unsaved changes)
    project_info        active project: lpi, main unit, build modes, paths
    list_project_units  files that are part of the project
    compile_project     compile/build the active project, structured messages
    get_messages        messages of the last compile

  Everything runs in the main thread (the server is polled from a timer), so
  IDEIntf and CodeTools are safe to call.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes, fpjson, LazFileUtils, LazIDEIntf, SrcEditorIntf,
  ProjectIntf, CompOptsIntf, IDEMsgIntf, IDEExternToolIntf,
  aiplane_mcpserver;

type

  { TAIIdeTools }

  TAIIdeTools = class
  private
    FBuilding: boolean;
    function ProjectDir: string;
    function ResolvePath(const APath: string): string;
    function PathAllowed(const AFilename: string): boolean;
    function CollectMessages(AMinUrgency: TMessageLineUrgency;
      out ACount: integer): TJSONArray;
    procedure ToolOpenFile(ACall: TAIMcpToolCall);
    procedure ToolActiveEditor(ACall: TAIMcpToolCall);
    procedure ToolOpenFiles(ACall: TAIMcpToolCall);
    procedure ToolReadEditor(ACall: TAIMcpToolCall);
    procedure ToolProjectInfo(ACall: TAIMcpToolCall);
    procedure ToolProjectUnits(ACall: TAIMcpToolCall);
    procedure ToolCompile(ACall: TAIMcpToolCall);
    procedure ToolMessages(ACall: TAIMcpToolCall);
  public
    procedure Register(AServer: TAIMcpServer);
    property Building: boolean read FBuilding;
  end;

const
  { Messages window view of FastPlaz's own lines; get_messages skips it. }
  AI_FASTPLAZ_MESSAGE_VIEW = 'FastPlaz';
  AI_IDE_TOOLS_INSTRUCTIONS =
    'Tools of the Lazarus IDE the user is working in (Free Pascal). Paths are ' +
    'absolute or relative to the project directory. compile_project saves ' +
    'modified editor files first, like the IDE does, and returns the compiler ' +
    'messages with file, line and column; use open_file to show the user a ' +
    'location. read_editor includes unsaved changes.';

function AIUrgencyName(AUrgency: TMessageLineUrgency): string;
function AIUrgencyFromName(const AName: string; ADefault: TMessageLineUrgency): TMessageLineUrgency;

implementation

const
  MAX_MESSAGES = 200;
  MAX_READ_LINES = 2000;

  SCHEMA_NONE = '{"type":"object","properties":{}}';
  SCHEMA_OPEN_FILE =
    '{"type":"object","properties":{' +
    '"path":{"type":"string","description":"File to open, absolute or relative to the project directory"},' +
    '"line":{"type":"integer","minimum":1,"description":"Line to jump to (1-based)"},' +
    '"column":{"type":"integer","minimum":1,"description":"Column (1-based)"}},' +
    '"required":["path"]}';
  SCHEMA_READ_EDITOR =
    '{"type":"object","properties":{' +
    '"path":{"type":"string","description":"File, absolute or relative to the project directory"},' +
    '"start_line":{"type":"integer","minimum":1},' +
    '"end_line":{"type":"integer","minimum":1}},' +
    '"required":["path"]}';
  SCHEMA_COMPILE =
    '{"type":"object","properties":{' +
    '"rebuild":{"type":"boolean","description":"Build all (clean build) instead of compiling changed units"},' +
    '"build_mode":{"type":"string","description":"Build mode to make active first (see project_info)"},' +
    '"min_urgency":{"type":"string","enum":["hint","note","warning","error"],"description":"Lowest message level to return, default warning"}}}';
  SCHEMA_MESSAGES =
    '{"type":"object","properties":{' +
    '"min_urgency":{"type":"string","enum":["hint","note","warning","error"],"description":"Lowest message level to return, default warning"}}}';

function AIUrgencyName(AUrgency: TMessageLineUrgency): string;
begin
  case AUrgency of
    mluHint: Result := 'hint';
    mluNote: Result := 'note';
    mluWarning: Result := 'warning';
    mluImportant: Result := 'important';
    mluError: Result := 'error';
    mluFatal: Result := 'fatal';
    mluPanic: Result := 'panic';
    else
      Result := 'info';
  end;
end;

function AIUrgencyFromName(const AName: string; ADefault: TMessageLineUrgency): TMessageLineUrgency;
begin
  case LowerCase(AName) of
    'hint': Result := mluHint;
    'note': Result := mluNote;
    'warning': Result := mluWarning;
    'error': Result := mluError;
    else
      Result := ADefault;
  end;
end;

function SplitPaths(const APaths: string): TJSONArray;
var
  list: TStringList;
  i: integer;
begin
  Result := TJSONArray.Create;
  list := TStringList.Create;
  try
    list.Delimiter := ';';
    list.StrictDelimiter := True;
    list.DelimitedText := APaths;
    for i := 0 to list.Count - 1 do
      if Trim(list[i]) <> '' then
        Result.Add(Trim(list[i]));
  finally
    list.Free;
  end;
end;

{ TAIIdeTools }

procedure TAIIdeTools.Register(AServer: TAIMcpServer);
begin
  AServer.Instructions := AI_IDE_TOOLS_INSTRUCTIONS;
  AServer.RegisterTool('open_file', 'Open a file in the IDE',
    'Opens a file in the Lazarus source editor and shows it to the user, ' +
    'optionally at a line and column.', SCHEMA_OPEN_FILE, mtpAuto, @ToolOpenFile);
  AServer.RegisterTool('get_active_editor', 'Active editor',
    'File, caret position (1-based line and column), selected text and modified ' +
    'state of the editor the user is looking at.', SCHEMA_NONE, mtpAuto,
    @ToolActiveEditor);
  AServer.RegisterTool('list_open_files', 'Open files',
    'Files open in the Lazarus source editor and whether they have unsaved changes.',
    SCHEMA_NONE, mtpAuto, @ToolOpenFiles);
  AServer.RegisterTool('read_editor', 'Read from the editor',
    'Text of a project file as the user sees it in the editor, unsaved changes ' +
    'included, with line numbers. Falls back to the file on disk when it is not open.',
    SCHEMA_READ_EDITOR, mtpAuto, @ToolReadEditor);
  AServer.RegisterTool('project_info', 'Project information',
    'The active Lazarus project: .lpi file, main unit, build modes, target ' +
    'OS/CPU, unit and include search paths, custom compiler options, output file.',
    SCHEMA_NONE, mtpAuto, @ToolProjectInfo);
  AServer.RegisterTool('list_project_units', 'Project files',
    'Files that are part of the active project.', SCHEMA_NONE, mtpAuto,
    @ToolProjectUnits);
  AServer.RegisterTool('compile_project', 'Compile the project',
    'Compiles (or with rebuild, builds all of) the active project in the IDE and ' +
    'returns success plus the compiler messages with file, line and column. ' +
    'Modified editor files are saved first.', SCHEMA_COMPILE, mtpAskSession,
    @ToolCompile);
  AServer.RegisterTool('get_messages', 'Compiler messages',
    'Messages of the last compile in the IDE Messages window, with file, line and column.',
    SCHEMA_MESSAGES, mtpAuto, @ToolMessages);
end;

function TAIIdeTools.ProjectDir: string;
begin
  Result := '';
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
    Result := ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
  if (Result = '') or not DirectoryExistsUTF8(Result) then
    Result := GetCurrentDirUTF8;
  Result := AppendPathDelim(Result);
end;

// Agents may send / or \ on any OS, absolute or relative paths.
function TAIIdeTools.ResolvePath(const APath: string): string;
begin
  Result := SetDirSeparators(Trim(APath));
  if Result = '' then
    Exit;
  if not FilenameIsAbsolute(Result) then
    Result := ProjectDir + Result;
  Result := TrimFilename(ExpandFileNameUTF8(Result));
end;

// Reading through the IDE is limited to the project, its search paths and
// files the user has open. Symlinks are resolved first (macOS /var ->
// /private/var), names compare as the OS does.
function TAIIdeTools.PathAllowed(const AFilename: string): boolean;
var
  physical: string;
  proj: TLazProject;
  dirs: TJSONArray;
  i: integer;

  function Inside(const ADir: string): boolean;
  var
    d: string;
  begin
    d := Trim(ADir);
    if d = '' then
      Exit(False);
    d := AppendPathDelim(GetPhysicalFilename(TrimFilename(d), pfeOriginal));
    // FileIsInPath also covers sub directories
    Result := FileIsInPath(physical, d);
  end;

begin
  physical := GetPhysicalFilename(AFilename, pfeOriginal);
  if Inside(ProjectDir) then
    Exit(True);
  if Assigned(SourceEditorManagerIntf) and
    Assigned(SourceEditorManagerIntf.SourceEditorIntfWithFilename(AFilename)) then
    Exit(True);
  proj := nil;
  if Assigned(LazarusIDE) then
    proj := LazarusIDE.ActiveProject;
  if not Assigned(proj) then
    Exit(False);
  for i := 0 to proj.FileCount - 1 do
    if CompareFilenames(proj.Files[i].Filename, AFilename) = 0 then
      Exit(True);
  dirs := SplitPaths(proj.LazCompilerOptions.GetUnitPath(False) + ';' +
    proj.LazCompilerOptions.GetIncludePath(False));
  try
    for i := 0 to dirs.Count - 1 do
      if Inside(dirs.Strings[i]) then
        Exit(True);
  finally
    dirs.Free;
  end;
  Result := False;
end;

function TAIIdeTools.CollectMessages(AMinUrgency: TMessageLineUrgency;
  out ACount: integer): TJSONArray;
var
  v, i: integer;
  view: TExtToolView;
  line: TMessageLine;
  item: TJSONObject;
begin
  Result := TJSONArray.Create;
  ACount := 0;
  if not Assigned(IDEMessagesWindow) then
    Exit;
  for v := 0 to IDEMessagesWindow.ViewCount - 1 do
  begin
    view := IDEMessagesWindow.Views[v];
    // FastPlaz's own log (AI Plane, Database Explorer), not build output
    if SameText(view.Caption, AI_FASTPLAZ_MESSAGE_VIEW) then
      Continue;
    for i := 0 to view.Lines.Count - 1 do
    begin
      line := view.Lines[i];
      if line.Urgency < AMinUrgency then
        Continue;
      Inc(ACount);
      if Result.Count >= MAX_MESSAGES then
        Continue;
      item := TJSONObject.Create;
      item.Add('urgency', AIUrgencyName(line.Urgency));
      item.Add('file', line.Filename);
      item.Add('line', line.Line);
      item.Add('column', line.Column);
      item.Add('text', line.Msg);
      Result.Add(item);
    end;
  end;
end;

procedure TAIIdeTools.ToolOpenFile(ACall: TAIMcpToolCall);
var
  fileName: string;
  lineNo, col: integer;
  r: TModalResult;
const
  FLAGS = [ofOnlyIfExists, ofRegularFile, ofAddToRecent];
begin
  fileName := ResolvePath(ACall.Str('path'));
  if fileName = '' then
  begin
    ACall.Fail('path is required');
    Exit;
  end;
  if not FileExistsUTF8(fileName) then
  begin
    ACall.Fail('File not found: ' + fileName);
    Exit;
  end;
  lineNo := ACall.Int('line');
  col := ACall.Int('column', 1);
  if col < 1 then
    col := 1;
  if lineNo > 0 then
    r := LazarusIDE.DoOpenFileAndJumpToPos(fileName, Point(col, lineNo), -1, -1, -1, FLAGS)
  else
    r := LazarusIDE.DoOpenEditorFile(fileName, -1, -1, FLAGS);
  if r = mrOk then
  begin
    if lineNo > 0 then
      ACall.Finish(Format('Opened %s at line %d', [fileName, lineNo]))
    else
      ACall.Finish('Opened ' + fileName);
  end
  else
    ACall.Fail('The IDE could not open ' + fileName);
end;

procedure TAIIdeTools.ToolActiveEditor(ACall: TAIMcpToolCall);
var
  editor: TSourceEditorInterface;
  res: TJSONObject;
  sel: string;
begin
  editor := nil;
  if Assigned(SourceEditorManagerIntf) then
    editor := SourceEditorManagerIntf.ActiveEditor;
  if not Assigned(editor) then
  begin
    ACall.Finish('No file is open in the editor.');
    Exit;
  end;
  res := TJSONObject.Create;
  res.Add('path', editor.FileName);
  res.Add('line', editor.CursorTextXY.Y);
  res.Add('column', editor.CursorTextXY.X);
  res.Add('modified', editor.Modified);
  sel := '';
  if editor.SelectionAvailable then
    sel := editor.Selection;
  res.Add('selection', sel);
  ACall.FinishJSON(res);
end;

procedure TAIIdeTools.ToolOpenFiles(ACall: TAIMcpToolCall);
var
  list: TJSONArray;
  seen: TStringList;
  i: integer;
  editor: TSourceEditorInterface;
begin
  list := TJSONArray.Create;
  seen := TStringList.Create;
  try
    seen.Sorted := True;
    if Assigned(SourceEditorManagerIntf) then
      for i := 0 to SourceEditorManagerIntf.SourceEditorCount - 1 do
      begin
        editor := SourceEditorManagerIntf.SourceEditors[i];
        // the same file can be open in two editor windows
        if seen.IndexOf(editor.FileName) >= 0 then
          Continue;
        seen.Add(editor.FileName);
        list.Add(TJSONObject.Create(['path', editor.FileName,
          'modified', editor.Modified]));
      end;
  finally
    seen.Free;
  end;
  ACall.FinishJSON(list);
end;

procedure TAIIdeTools.ToolReadEditor(ACall: TAIMcpToolCall);
var
  fileName, text, fromWhere: string;
  editor: TSourceEditorInterface;
  lines: TStringList;
  first, last, i: integer;
  sb: TStringBuilder;
begin
  fileName := ResolvePath(ACall.Str('path'));
  if fileName = '' then
  begin
    ACall.Fail('path is required');
    Exit;
  end;
  if not PathAllowed(fileName) then
  begin
    ACall.Fail('Outside the project and its search paths: ' + fileName +
      '. Use your own file tools for other files.');
    Exit;
  end;
  editor := nil;
  if Assigned(SourceEditorManagerIntf) then
    editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(fileName);
  lines := TStringList.Create;
  sb := TStringBuilder.Create;
  try
    if Assigned(editor) then
    begin
      text := editor.GetText(False);
      fromWhere := 'editor buffer';
      if editor.Modified then
        fromWhere := fromWhere + ', unsaved changes';
      lines.Text := text;
    end
    else if FileExistsUTF8(fileName) then
    begin
      lines.LoadFromFile(fileName);
      fromWhere := 'disk';
    end
    else
    begin
      ACall.Fail('File not found: ' + fileName);
      Exit;
    end;
    first := ACall.Int('start_line', 1);
    if first < 1 then
      first := 1;
    last := ACall.Int('end_line', lines.Count);
    if (last > lines.Count) or (last < 1) then
      last := lines.Count;
    if last - first + 1 > MAX_READ_LINES then
      last := first + MAX_READ_LINES - 1;
    sb.Append(Format('%s (%s, lines %d-%d of %d)', [fileName, fromWhere, first,
      last, lines.Count]));
    sb.Append(LineEnding);
    for i := first to last do
    begin
      sb.Append(Format('%6d'#9, [i]));
      sb.Append(lines[i - 1]);
      sb.Append(LineEnding);
    end;
    ACall.Finish(sb.ToString);
  finally
    sb.Free;
    lines.Free;
  end;
end;

procedure TAIIdeTools.ToolProjectInfo(ACall: TAIMcpToolCall);
var
  proj: TLazProject;
  opts: TLazCompilerOptions;
  res: TJSONObject;
  modes: TJSONArray;
  i: integer;
begin
  proj := nil;
  if Assigned(LazarusIDE) then
    proj := LazarusIDE.ActiveProject;
  if not Assigned(proj) then
  begin
    ACall.Fail('No project is open.');
    Exit;
  end;
  opts := proj.LazCompilerOptions;
  res := TJSONObject.Create;
  res.Add('lpi', proj.ProjectInfoFile);
  res.Add('directory', ProjectDir);
  if Assigned(proj.MainFile) then
    res.Add('main_unit', proj.MainFile.Filename)
  else
    res.Add('main_unit', '');
  res.Add('title', proj.Title);
  res.Add('active_build_mode', proj.ActiveBuildModeID);
  modes := TJSONArray.Create;
  for i := 0 to proj.LazBuildModes.Count - 1 do
    modes.Add(proj.LazBuildModes.BuildModes[i].Identifier);
  res.Add('build_modes', modes);
  res.Add('target_os', opts.GetEffectiveTargetOS);
  res.Add('target_cpu', opts.GetEffectiveTargetCPU);
  res.Add('widgetset', opts.GetEffectiveLCLWidgetType);
  res.Add('target_file', opts.CreateTargetFilename);
  res.Add('unit_paths', SplitPaths(opts.GetUnitPath(False)));
  res.Add('include_paths', SplitPaths(opts.GetIncludePath(False)));
  res.Add('custom_options', opts.CustomOptions);
  ACall.FinishJSON(res);
end;

procedure TAIIdeTools.ToolProjectUnits(ACall: TAIMcpToolCall);
var
  proj: TLazProject;
  list: TJSONArray;
  i: integer;
  f: TLazProjectFile;
begin
  proj := nil;
  if Assigned(LazarusIDE) then
    proj := LazarusIDE.ActiveProject;
  if not Assigned(proj) then
  begin
    ACall.Fail('No project is open.');
    Exit;
  end;
  list := TJSONArray.Create;
  for i := 0 to proj.FileCount - 1 do
  begin
    f := proj.Files[i];
    if not f.IsPartOfProject then
      Continue;
    list.Add(TJSONObject.Create(['path', f.Filename,
      'main', f = proj.MainFile]));
  end;
  ACall.FinishJSON(list);
end;

procedure TAIIdeTools.ToolCompile(ACall: TAIMcpToolCall);
var
  proj: TLazProject;
  mode: string;
  reason: TCompileReason;
  started: QWord;
  r: TModalResult;
  res: TJSONObject;
  msgs: TJSONArray;
  total: integer;
begin
  proj := nil;
  if Assigned(LazarusIDE) then
    proj := LazarusIDE.ActiveProject;
  if not Assigned(proj) then
  begin
    ACall.Fail('No project is open.');
    Exit;
  end;
  if FBuilding then
  begin
    ACall.Fail('A compile started by AI Plane is still running.');
    Exit;
  end;
  mode := ACall.Str('build_mode');
  if (mode <> '') and not SameText(mode, proj.ActiveBuildModeID) then
  begin
    if proj.LazBuildModes.IndexOf(mode) < 0 then
    begin
      ACall.Fail('Unknown build mode: ' + mode + '. See project_info.');
      Exit;
    end;
    proj.ActiveBuildModeID := mode;
  end;
  if ACall.Bool('rebuild') then
    reason := crBuild
  else
    reason := crCompile;
  started := GetTickCount64;
  FBuilding := True;
  try
    // Runs the compiler and processes messages until it finished.
    r := LazarusIDE.DoBuildProject(reason, []);
  finally
    FBuilding := False;
  end;
  msgs := CollectMessages(AIUrgencyFromName(ACall.Str('min_urgency'), mluWarning), total);
  res := TJSONObject.Create;
  res.Add('success', r = mrOk);
  res.Add('build_mode', proj.ActiveBuildModeID);
  res.Add('duration_ms', GetTickCount64 - started);
  res.Add('message_count', total);
  res.Add('messages', msgs);
  if r = mrAbort then
    res.Add('note', 'The compile was aborted.');
  ACall.FinishJSON(res);
end;

procedure TAIIdeTools.ToolMessages(ACall: TAIMcpToolCall);
var
  res: TJSONObject;
  total: integer;
begin
  res := TJSONObject.Create;
  res.Add('messages', CollectMessages(AIUrgencyFromName(ACall.Str('min_urgency'),
    mluWarning), total));
  res.Add('message_count', total);
  ACall.FinishJSON(res);
end;

end.

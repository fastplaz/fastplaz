unit aiplane_tools_editor;

{
  Editor, search and unit lookup tools for the Agent Plane MCP server
  (phase 2 of docs/AI_PLANE_TOOL_RUNDOWN.md, ideas from
  lazarus-mcp-assistant):

    edit_text       replace old_string with new_string in the editor buffer
                    (one undo step, Ctrl+Z takes it back), optionally save
    save_file       save an editor buffer to disk
    revert_file     discard unsaved changes, reload from disk
    search_project  text search in the project files (editor buffers for open
                    files, unsaved changes included) or in a directory
    find_unit       which loaded packages contain a unit, whether the project
                    requires them, and where the compiler finds it

  Paths follow TAIIdeTools: absolute or relative to the project directory.
  Reading is allowed where read_editor reads; writing (edit, save, revert)
  only inside the project directory, project files and files the user has
  open, never in other search paths (library sources, FastPlaz itself).

  Everything runs in the main thread (the server is polled from a timer).
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes, fpjson, LazFileUtils, FileUtil,
  LazIDEIntf, SrcEditorIntf, ProjectIntf, PackageIntf, CodeToolManager,
  aiplane_mcpserver, aiplane_tools_ide, aiplane_textedit;

type

  { TAIEditorTools }

  TAIEditorTools = class
  private
    FIde: TAIIdeTools;
    function WriteAllowed(const AFilename: string): boolean;
    function TargetFile(ACall: TAIMcpToolCall; AWrite: boolean;
      out AFileName: string): boolean;
    function OpenEditor(ACall: TAIMcpToolCall; const AFileName: string;
      out AEditor: TSourceEditorInterface): boolean;
    procedure SearchFile(const AFileName, AQuery: string; ACaseSensitive: boolean;
      AMatches: TJSONArray; AMax: integer; var ATotal, AFiles: integer);
    procedure ToolEditText(ACall: TAIMcpToolCall);
    procedure ToolSaveFile(ACall: TAIMcpToolCall);
    procedure ToolRevertFile(ACall: TAIMcpToolCall);
    procedure ToolSearchProject(ACall: TAIMcpToolCall);
    procedure ToolFindUnit(ACall: TAIMcpToolCall);
  public
    constructor Create(AIde: TAIIdeTools);
    procedure Register(AServer: TAIMcpServer);
  end;

const
  AI_EDITOR_TOOLS_INSTRUCTIONS =
    'edit_text changes a file in the IDE editor buffer (one undo step, the ' +
    'user sees it at once); the compiler reads files from disk, so save_file ' +
    '(or edit_text with save) before compile_project. Prefer edit_text over ' +
    'writing a file that is open with unsaved changes. search_project finds ' +
    'text in the project files including unsaved changes. For "Can''t find ' +
    'unit X" use find_unit.';

implementation

const
  DEFAULT_SEARCH_MASK = '*.pas;*.pp;*.p;*.inc;*.lpr;*.lfm;*.html;*.htm;*.json;*.js;*.css';
  DEFAULT_MAX_RESULTS = 200;
  MAX_MAX_RESULTS = 1000;
  MAX_SEARCH_FILES = 3000;
  MAX_SEARCH_FILE_SIZE = 2 * 1024 * 1024;

  SCHEMA_EDIT_TEXT =
    '{"type":"object","properties":{' +
    '"path":{"type":"string","description":"File, absolute or relative to the project directory. Opened in the editor when it is not open yet"},' +
    '"old_string":{"type":"string","description":"Exact text to replace, line breaks as \\n. Must occur exactly once unless replace_all is true"},' +
    '"new_string":{"type":"string","description":"Replacement text (may be empty to delete)"},' +
    '"replace_all":{"type":"boolean","description":"Replace every occurrence. Default false"},' +
    '"save":{"type":"boolean","description":"Save the file afterwards. Default false"}},' +
    '"required":["path","old_string","new_string"]}';
  SCHEMA_PATH =
    '{"type":"object","properties":{' +
    '"path":{"type":"string","description":"File, absolute or relative to the project directory"}},' +
    '"required":["path"]}';
  SCHEMA_SEARCH =
    '{"type":"object","properties":{' +
    '"query":{"type":"string","description":"Text to find (plain text, not a regular expression)"},' +
    '"case_sensitive":{"type":"boolean","description":"Default false (ASCII case folding)"},' +
    '"directory":{"type":"string","description":"Search this directory (recursively) instead of the project files, ' +
    'e.g. \"public_html/themes\". Must be inside the project directory or a search path"},' +
    '"mask":{"type":"string","description":"File masks for directory, separated by ;. Default: Pascal sources, .lfm, .html, .json, .js, .css"},' +
    '"max_results":{"type":"integer","minimum":1,"maximum":1000,"description":"Default 200"}},' +
    '"required":["query"]}';
  SCHEMA_FIND_UNIT =
    '{"type":"object","properties":{' +
    '"unit":{"type":"string","description":"Unit name, e.g. \"fastplaz_handler\""}},' +
    '"required":["unit"]}';

function IsIdentifierPath(const AName: string): boolean;
var
  i: integer;
begin
  Result := AName <> '';
  for i := 1 to Length(AName) do
    if not (AName[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) then
      Exit(False);
end;

// generated and backup copies would double every match
function SkippedDir(const AFileName: string): boolean;
var
  p: string;
begin
  p := LowerCase(SetDirSeparators('/' + AFileName));
  Result := (Pos(PathDelim + 'backup' + PathDelim, p) > 0) or
    (Pos(PathDelim + 'lib' + PathDelim, p) > 0) or
    (Pos(PathDelim + '.git' + PathDelim, p) > 0) or
    (Pos(PathDelim + 'ztemp' + PathDelim, p) > 0);
end;

{ TAIEditorTools }

constructor TAIEditorTools.Create(AIde: TAIIdeTools);
begin
  inherited Create;
  FIde := AIde;
end;

procedure TAIEditorTools.Register(AServer: TAIMcpServer);
begin
  if AServer.Instructions <> '' then
    AServer.Instructions := AServer.Instructions + ' ';
  AServer.Instructions := AServer.Instructions + AI_EDITOR_TOOLS_INSTRUCTIONS;
  AServer.RegisterTool('edit_text', 'Edit text in the IDE editor',
    'Replaces old_string with new_string in the IDE editor buffer of a file, ' +
    'as one undo step (Ctrl+Z takes it back), and opens the file when needed. ' +
    'Fails when old_string is missing or occurs more than once (then add ' +
    'context or set replace_all). Unsaved changes in the editor are kept and ' +
    'edited; nothing is written to disk unless save is true. Project files, ' +
    'the project directory and open files only.', SCHEMA_EDIT_TEXT,
    mtpAskSession, @ToolEditText, []);
  AServer.RegisterTool('save_file', 'Save a file',
    'Saves the IDE editor buffer of a file to disk (a form unit saves its ' +
    '.lfm too). Nothing happens when the file is not open or has no unsaved ' +
    'changes.', SCHEMA_PATH, mtpAskSession, @ToolSaveFile, [mthIdempotent]);
  AServer.RegisterTool('revert_file', 'Revert a file',
    'Discards the unsaved changes of a file in the IDE editor and reloads it ' +
    'from disk. The changes are lost.', SCHEMA_PATH, mtpAsk, @ToolRevertFile,
    [mthDestructive, mthIdempotent]);
  AServer.RegisterTool('search_project', 'Search the project',
    'Finds a text in the files of the active project (open files are searched ' +
    'in the editor, unsaved changes included), or in a directory with a file ' +
    'mask, e.g. the theme templates. Returns file, line and the line text.',
    SCHEMA_SEARCH, mtpAuto, @ToolSearchProject, [mthReadOnly, mthIdempotent]);
  AServer.RegisterTool('find_unit', 'Find a unit',
    'For "Can''t find unit X": the file the compiler would use for a unit ' +
    'from the project directory, whether it is a project file, and which ' +
    'loaded packages contain it, installed or not, and whether the project ' +
    'requires them.', SCHEMA_FIND_UNIT, mtpAuto, @ToolFindUnit,
    [mthReadOnly, mthIdempotent]);
end;

// Writing is narrower than reading: not the other search paths, which hold
// libraries (FastPlaz itself, packages) the user did not open.
function TAIEditorTools.WriteAllowed(const AFilename: string): boolean;
var
  physical, dir: string;
  proj: TLazProject;
  i: integer;
begin
  physical := GetPhysicalFilename(AFilename, pfeOriginal);
  dir := AppendPathDelim(GetPhysicalFilename(TrimFilename(FIde.ProjectDir), pfeOriginal));
  if FileIsInPath(physical, dir) then
    Exit(True);
  if Assigned(SourceEditorManagerIntf) and
    Assigned(SourceEditorManagerIntf.SourceEditorIntfWithFilename(AFilename)) then
    Exit(True);
  proj := nil;
  if Assigned(LazarusIDE) then
    proj := LazarusIDE.ActiveProject;
  if Assigned(proj) then
    for i := 0 to proj.FileCount - 1 do
      if CompareFilenames(proj.Files[i].Filename, AFilename) = 0 then
        Exit(True);
  Result := False;
end;

function TAIEditorTools.TargetFile(ACall: TAIMcpToolCall; AWrite: boolean;
  out AFileName: string): boolean;
begin
  Result := False;
  AFileName := FIde.ResolvePath(ACall.Str('path'));
  if AFileName = '' then
  begin
    ACall.Fail('path is required');
    Exit;
  end;
  if AWrite and not WriteAllowed(AFileName) then
  begin
    ACall.Fail('Only files in the project directory, project files and files ' +
      'open in the editor can be changed through the IDE: ' + AFileName);
    Exit;
  end;
  if not AWrite and not FIde.PathAllowed(AFileName) then
  begin
    ACall.Fail('Outside the project and its search paths: ' + AFileName);
    Exit;
  end;
  Result := True;
end;

function TAIEditorTools.OpenEditor(ACall: TAIMcpToolCall; const AFileName: string;
  out AEditor: TSourceEditorInterface): boolean;
begin
  Result := False;
  AEditor := nil;
  if not Assigned(SourceEditorManagerIntf) or not Assigned(LazarusIDE) then
  begin
    ACall.Fail('The source editor is not available');
    Exit;
  end;
  AEditor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(AFileName);
  if Assigned(AEditor) then
    Exit(True);
  if not FileExistsUTF8(AFileName) then
  begin
    ACall.Fail('File not found: ' + AFileName);
    Exit;
  end;
  if LazarusIDE.DoOpenEditorFile(AFileName, -1, -1,
    [ofOnlyIfExists, ofRegularFile]) <> mrOk then
  begin
    ACall.Fail('The IDE could not open ' + AFileName);
    Exit;
  end;
  AEditor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(AFileName);
  Result := Assigned(AEditor);
  if not Result then
    ACall.Fail('The IDE opened ' + AFileName + ' but no editor was found for it');
end;

procedure TAIEditorTools.ToolEditText(ACall: TAIMcpToolCall);
var
  fileName, text, oldStr, newStr, expected: string;
  editor: TSourceEditorInterface;
  offsets: TAIOffsets;
  i, line, col, endLine, endCol: integer;
  lines: TJSONArray;
  res: TJSONObject;
  replaceAll, saved: boolean;
begin
  if not TargetFile(ACall, True, fileName) then
    Exit;
  oldStr := AINormalizeEOL(ACall.Str('old_string'));
  newStr := AINormalizeEOL(ACall.Str('new_string'));
  replaceAll := ACall.Bool('replace_all');
  if oldStr = '' then
  begin
    ACall.Fail('old_string must not be empty');
    Exit;
  end;
  if oldStr = newStr then
  begin
    ACall.Fail('old_string and new_string are the same; nothing to change');
    Exit;
  end;
  if not OpenEditor(ACall, fileName, editor) then
    Exit;

  text := AINormalizeEOL(editor.GetText(False));
  offsets := AIFindAll(text, oldStr);
  if Length(offsets) = 0 then
  begin
    ACall.Fail('old_string not found in ' + fileName + '. Check spaces and ' +
      'indentation with read_editor (the editor buffer may differ from disk).');
    Exit;
  end;
  if (Length(offsets) > 1) and not replaceAll then
  begin
    lines := TJSONArray.Create;
    try
      for i := 0 to High(offsets) do
      begin
        AIOffsetToLineCol(text, offsets[i], line, col);
        lines.Add(line);
      end;
      ACall.Fail(Format('old_string occurs %d times in %s (lines %s). Add ' +
        'surrounding text so it occurs once, or set replace_all.',
        [Length(offsets), fileName, lines.AsJSON]));
    finally
      lines.Free;
    end;
    Exit;
  end;

  expected := AIReplaceAt(text, offsets, Length(oldStr), newStr);
  lines := TJSONArray.Create;
  editor.BeginUndoBlock;
  try
    // from the end, so the earlier positions stay valid
    for i := High(offsets) downto 0 do
    begin
      AIOffsetToLineCol(text, offsets[i], line, col);
      AIOffsetToLineCol(text, offsets[i] + Length(oldStr), endLine, endCol);
      editor.ReplaceText(Point(col, line), Point(endCol, endLine), newStr);
      lines.Insert(0, TJSONIntegerNumber.Create(line));
    end;
  finally
    editor.EndUndoBlock;
  end;

  if AITrimLineEnds(AINormalizeEOL(editor.GetText(False))) <> AITrimLineEnds(expected) then
  begin
    lines.Free;
    ACall.Fail('The editor text of ' + fileName + ' differs from the expected ' +
      'result (an editor option such as auto indent may have changed it). ' +
      'Check it with read_editor; Ctrl+Z in the IDE takes the edit back.');
    Exit;
  end;

  saved := False;
  if ACall.Bool('save') then
  begin
    saved := LazarusIDE.DoSaveEditorFile(editor, []) = mrOk;
    if not saved then
    begin
      lines.Free;
      ACall.Fail('Edited ' + fileName + ' but saving it failed; the change is ' +
        'in the editor (unsaved).');
      Exit;
    end;
  end;

  res := TJSONObject.Create;
  res.Add('path', fileName);
  res.Add('replaced', Length(offsets));
  res.Add('lines', lines);
  res.Add('saved', saved);
  res.Add('modified', editor.Modified);
  ACall.FinishJSON(res);
end;

procedure TAIEditorTools.ToolSaveFile(ACall: TAIMcpToolCall);
var
  fileName: string;
  editor: TSourceEditorInterface;
  wasModified: boolean;
begin
  if not TargetFile(ACall, True, fileName) then
    Exit;
  editor := nil;
  if Assigned(SourceEditorManagerIntf) then
    editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(fileName);
  if not Assigned(editor) then
  begin
    ACall.FinishJSON(TJSONObject.Create(['path', fileName, 'saved', False,
      'note', 'The file is not open in the editor; it is already on disk as it is.']));
    Exit;
  end;
  wasModified := editor.Modified;
  if LazarusIDE.DoSaveEditorFile(editor, []) <> mrOk then
  begin
    ACall.Fail('Saving ' + fileName + ' failed');
    Exit;
  end;
  ACall.FinishJSON(TJSONObject.Create(['path', fileName, 'saved', True,
    'had_changes', wasModified]));
end;

procedure TAIEditorTools.ToolRevertFile(ACall: TAIMcpToolCall);
var
  fileName: string;
  editor: TSourceEditorInterface;
begin
  if not TargetFile(ACall, True, fileName) then
    Exit;
  editor := nil;
  if Assigned(SourceEditorManagerIntf) then
    editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(fileName);
  if not Assigned(editor) then
  begin
    ACall.FinishJSON(TJSONObject.Create(['path', fileName, 'reverted', False,
      'note', 'The file is not open in the editor; nothing to revert.']));
    Exit;
  end;
  if not editor.Modified then
  begin
    ACall.FinishJSON(TJSONObject.Create(['path', fileName, 'reverted', False,
      'note', 'The editor has no unsaved changes.']));
    Exit;
  end;
  if LazarusIDE.DoRevertEditorFile(fileName) <> mrOk then
  begin
    ACall.Fail('Reverting ' + fileName + ' failed');
    Exit;
  end;
  ACall.FinishJSON(TJSONObject.Create(['path', fileName, 'reverted', True]));
end;

procedure TAIEditorTools.SearchFile(const AFileName, AQuery: string;
  ACaseSensitive: boolean; AMatches: TJSONArray; AMax: integer;
  var ATotal, AFiles: integer);
var
  editor: TSourceEditorInterface;
  text: string;
  lines: TStringList;
begin
  editor := nil;
  if Assigned(SourceEditorManagerIntf) then
    editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(AFileName);
  if Assigned(editor) then
    text := editor.GetText(False)
  else
  begin
    if not FileExistsUTF8(AFileName) or
      (FileSizeUtf8(AFileName) > MAX_SEARCH_FILE_SIZE) then
      Exit;
    lines := TStringList.Create;
    try
      try
        lines.LoadFromFile(AFileName);
      except
        Exit;
      end;
      text := lines.Text;
    finally
      lines.Free;
    end;
  end;
  Inc(AFiles);
  Inc(ATotal, AISearchText(CreateRelativePath(AFileName, FIde.ProjectDir), text,
    AQuery, ACaseSensitive, AMatches, AMax));
end;

procedure TAIEditorTools.ToolSearchProject(ACall: TAIMcpToolCall);
var
  query, dir, mask, scope: string;
  caseSensitive: boolean;
  maxResults, total, filesSearched, i: integer;
  matches: TJSONArray;
  files: TStringList;
  proj: TLazProject;
  res: TJSONObject;
begin
  query := ACall.Str('query');
  if query = '' then
  begin
    ACall.Fail('query is required');
    Exit;
  end;
  caseSensitive := ACall.Bool('case_sensitive');
  maxResults := ACall.Int('max_results', DEFAULT_MAX_RESULTS);
  if maxResults < 1 then
    maxResults := DEFAULT_MAX_RESULTS;
  if maxResults > MAX_MAX_RESULTS then
    maxResults := MAX_MAX_RESULTS;

  files := TStringList.Create;
  matches := TJSONArray.Create;
  try
    dir := Trim(ACall.Str('directory'));
    if dir <> '' then
    begin
      dir := FIde.ResolvePath(dir);
      if not DirectoryExistsUTF8(dir) then
      begin
        ACall.Fail('Directory not found: ' + dir);
        Exit;
      end;
      // a file name inside it: the directory itself is not "in" its own path
      if not FIde.PathAllowed(AppendPathDelim(dir) + 'x') then
      begin
        ACall.Fail('Outside the project and its search paths: ' + dir);
        Exit;
      end;
      mask := Trim(ACall.Str('mask'));
      if mask = '' then
        mask := DEFAULT_SEARCH_MASK;
      FindAllFiles(files, dir, mask, True);
      scope := dir;
    end
    else
    begin
      proj := nil;
      if Assigned(LazarusIDE) then
        proj := LazarusIDE.ActiveProject;
      if not Assigned(proj) then
      begin
        ACall.Fail('No project is open; give a directory');
        Exit;
      end;
      for i := 0 to proj.FileCount - 1 do
        if proj.Files[i].IsPartOfProject and
          FilenameIsAbsolute(proj.Files[i].Filename) then
          files.Add(proj.Files[i].Filename);
      scope := 'project files';
    end;

    total := 0;
    filesSearched := 0;
    for i := 0 to files.Count - 1 do
    begin
      if filesSearched >= MAX_SEARCH_FILES then
        Break;
      if (dir <> '') and SkippedDir(CreateRelativePath(files[i], dir)) then
        Continue;
      SearchFile(files[i], query, caseSensitive, matches, maxResults, total,
        filesSearched);
    end;

    res := TJSONObject.Create;
    res.Add('query', query);
    res.Add('scope', scope);
    res.Add('files_searched', filesSearched);
    res.Add('match_count', total);
    if total > matches.Count then
      res.Add('note', Format('Only the first %d matches are listed.', [matches.Count]));
    res.Add('matches', matches);
    matches := nil;
    ACall.FinishJSON(res);
  finally
    matches.Free;
    files.Free;
  end;
end;

procedure TAIEditorTools.ToolFindUnit(ACall: TAIMcpToolCall);
var
  aUnit, searchName, inFile, resolved, hint: string;
  i, j: integer;
  pkg: TIDEPackage;
  pkgFile: TLazPackageFile;
  proj: TLazProject;
  owner: TObject;
  packages: TJSONArray;
  item, res: TJSONObject;
  inProject, required, anyRequired: boolean;
begin
  aUnit := Trim(ACall.Str('unit'));
  if not IsIdentifierPath(aUnit) then
  begin
    ACall.Fail('unit must be a unit name such as fastplaz_handler');
    Exit;
  end;
  proj := nil;
  if Assigned(LazarusIDE) then
    proj := LazarusIDE.ActiveProject;

  // the file the compiler would take, seen from the project directory
  resolved := '';
  try
    searchName := aUnit;
    inFile := '';
    resolved := CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
      FIde.ProjectDir, searchName, inFile, True);
  except
    resolved := '';
  end;

  inProject := False;
  if Assigned(proj) then
    for i := 0 to proj.FileCount - 1 do
      if proj.Files[i].IsPartOfProject and
        (CompareText(ExtractFileNameOnly(proj.Files[i].Filename), aUnit) = 0) then
        inProject := True;

  packages := TJSONArray.Create;
  anyRequired := False;
  if Assigned(PackageEditingInterface) then
    for i := 0 to PackageEditingInterface.GetPackageCount - 1 do
    begin
      pkg := PackageEditingInterface.GetPackages(i);
      for j := 0 to pkg.FileCount - 1 do
      begin
        pkgFile := pkg.Files[j];
        if not (pkgFile.FileType in PkgFileUnitTypes) then
          Continue;
        if (CompareText(pkgFile.Unit_Name, aUnit) <> 0) and
          (CompareText(ExtractFileNameOnly(pkgFile.Filename), aUnit) <> 0) then
          Continue;
        item := TJSONObject.Create;
        item.Add('package', pkg.Name);
        item.Add('package_file', pkg.Filename);
        item.Add('unit_file', pkgFile.GetFullFilename);
        item.Add('installed', Assigned(PackageEditingInterface.IsPackageInstalled(pkg.Name)));
        required := False;
        if Assigned(proj) then
        begin
          owner := nil;
          required := PackageEditingInterface.IsOwnerDependingOnPkg(proj, pkg.Name, owner);
        end;
        item.Add('project_requires', required);
        anyRequired := anyRequired or required;
        packages.Add(item);
        Break;
      end;
    end;

  if inProject then
    hint := 'The unit is a file of the project.'
  else if anyRequired then
    hint := 'A package the project requires contains the unit. If the compiler ' +
      'still cannot find it, compile that package (or clean and build).'
  else if packages.Count > 0 then
    hint := 'Loaded packages contain the unit but the project does not require ' +
      'them: ask the user to add the package in Project Inspector (Add > New ' +
      'Requirement).'
  else if resolved <> '' then
    hint := 'Found in the search path of the project directory.'
  else
    hint := 'No loaded package contains the unit and the search path of the ' +
      'project directory does not find it: check the spelling, add its ' +
      'directory to the unit search path, or open the package that has it.';

  res := TJSONObject.Create;
  res.Add('unit', aUnit);
  res.Add('resolved_file', resolved);
  res.Add('in_project', inProject);
  res.Add('packages', packages);
  res.Add('hint', hint);
  ACall.FinishJSON(res);
end;

end.

unit aiplane_reference;

{
  The FastPlaz reference for coding agents: a Markdown file shipped in the
  fastplaz_tools package (templates/agent/fastplaz-reference.md), served by
  the MCP server as the resource fastplaz://reference and by the tool
  get_fastplaz_reference.

  The file is read at every request, so it can be edited without rebuilding
  the IDE. No LCL or IDEIntf here.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  AI_REFERENCE_URI = 'fastplaz://reference';
  AI_REFERENCE_FOLDER = 'templates' + DirectorySeparator + 'agent';
  AI_REFERENCE_FILE = 'fastplaz-reference.md';

{ templates/agent/fastplaz-reference.md inside APackageDir ('' when
  APackageDir is empty). }
function AIReferenceFileName(const APackageDir: string): string;
{ Reads a UTF-8 text file. False: AText is the reason. }
function AILoadReference(const AFileName: string; out AText: string): boolean;
{ The level-2 section ("## ...") whose heading contains ASection, ignoring
  case, up to the next level-2 heading. An empty ASection gives the whole
  text. AFound is False when no heading matches. }
function AIReferenceSection(const AText, ASection: string; out AFound: boolean): string;
{ The level-2 headings of AText, comma separated. }
function AIReferenceHeadings(const AText: string): string;

implementation

function AIReferenceFileName(const APackageDir: string): string;
begin
  if APackageDir = '' then
    Exit('');
  Result := IncludeTrailingPathDelimiter(APackageDir) + AI_REFERENCE_FOLDER +
    DirectorySeparator + AI_REFERENCE_FILE;
end;

function AILoadReference(const AFileName: string; out AText: string): boolean;
var
  stream: TFileStream;
begin
  AText := '';
  if AFileName = '' then
  begin
    AText := 'the fastplaz_tools package directory is unknown';
    Exit(False);
  end;
  if not FileExists(AFileName) then
  begin
    AText := 'file not found: ' + AFileName;
    Exit(False);
  end;
  try
    stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(AText, stream.Size);
      if stream.Size > 0 then
        stream.ReadBuffer(AText[1], stream.Size);
    finally
      stream.Free;
    end;
    // UTF-8 BOM
    if Copy(AText, 1, 3) = #$EF#$BB#$BF then
      Delete(AText, 1, 3);
    Result := True;
  except
    on E: Exception do
    begin
      AText := E.Message;
      Result := False;
    end;
  end;
end;

function IsLevel2Heading(const ALine: string): boolean;
begin
  Result := (Copy(ALine, 1, 3) = '## ');
end;

function AIReferenceSection(const AText, ASection: string; out AFound: boolean): string;
var
  lines, part: TStringList;
  i: integer;
  inside, inFence: boolean;
  wanted, line: string;
begin
  wanted := LowerCase(Trim(ASection));
  if wanted = '' then
  begin
    AFound := True;
    Exit(AText);
  end;
  AFound := False;
  Result := '';
  lines := TStringList.Create;
  part := TStringList.Create;
  try
    lines.Text := AText;
    inside := False;
    inFence := False;
    for i := 0 to lines.Count - 1 do
    begin
      line := lines[i];
      // "## " inside a code block is not a heading
      if Copy(TrimLeft(line), 1, 3) = '```' then
        inFence := not inFence;
      if (not inFence) and IsLevel2Heading(line) then
      begin
        if inside then
          Break;
        inside := Pos(wanted, LowerCase(line)) > 0;
      end;
      if inside then
        part.Add(line);
    end;
    AFound := part.Count > 0;
    if AFound then
      Result := TrimRight(part.Text) + LineEnding;
  finally
    part.Free;
    lines.Free;
  end;
end;

function AIReferenceHeadings(const AText: string): string;
var
  lines: TStringList;
  i: integer;
  inFence: boolean;
begin
  Result := '';
  lines := TStringList.Create;
  try
    lines.Text := AText;
    inFence := False;
    for i := 0 to lines.Count - 1 do
    begin
      if Copy(TrimLeft(lines[i]), 1, 3) = '```' then
        inFence := not inFence;
      if (not inFence) and IsLevel2Heading(lines[i]) then
      begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + Trim(Copy(lines[i], 4, MaxInt));
      end;
    end;
  finally
    lines.Free;
  end;
end;

end.

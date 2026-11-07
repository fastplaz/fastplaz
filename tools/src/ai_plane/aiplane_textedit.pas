unit aiplane_textedit;

{
  Text helpers for the Agent Plane editor tools (edit_text, search_project),
  kept free of LCL and IDEIntf so they can be tested on their own.

  Positions: offsets are 1-based byte positions in a string; lines and
  columns are 1-based, columns counted in bytes (the logical column the
  Lazarus source editor uses for ReplaceText).
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, fpjson;

type
  TAIOffsets = array of SizeInt;

{ CRLF and lone CR become LF. }
function AINormalizeEOL(const AText: string): string;
{ AText (LF line ends) without spaces and tabs at the end of each line, for
  comparing editor text when the editor trims trailing spaces. }
function AITrimLineEnds(const AText: string): string;
{ Every non-overlapping occurrence of APattern in AText, left to right. An
  empty APattern gives none. }
function AIFindAll(const AText, APattern: string): TAIOffsets;
{ Line and column (1-based, column in bytes) of AOffset in AText (LF line
  ends). An offset just past the end gives the position after the last
  character. }
procedure AIOffsetToLineCol(const AText: string; AOffset: SizeInt;
  out ALine, ACol: integer);
{ AText with the occurrences at AOffsets (each Length(AOld) bytes long,
  sorted, not overlapping) replaced by ANew. }
function AIReplaceAt(const AText: string; const AOffsets: TAIOffsets;
  AOldLength: SizeInt; const ANew: string): string;
{ Adds {file, line, text} for every line of AText containing AQuery to
  AMatches, at most AMax items in total (AMatches.Count counts). Case
  folding is ASCII only. Lines longer than 300 bytes are cut. Returns the
  number of matching lines in this text, also those not added. }
function AISearchText(const AFileName, AText, AQuery: string;
  ACaseSensitive: boolean; AMatches: TJSONArray; AMax: integer): integer;

implementation

const
  MAX_MATCH_LINE = 300;

// The first AMax bytes of AText, not cutting a UTF-8 sequence.
function CutUTF8(const AText: string; AMax: integer): string;
var
  n: integer;
begin
  if Length(AText) <= AMax then
    Exit(AText);
  n := AMax;
  // do not end before a continuation byte (10xxxxxx)
  while (n > 0) and ((Ord(AText[n + 1]) and $C0) = $80) do
    Dec(n);
  Result := Copy(AText, 1, n);
end;

function AINormalizeEOL(const AText: string): string;
begin
  Result := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function AITrimLineEnds(const AText: string): string;
var
  sb: TStringBuilder;
  i, lineStart, lineEnd: SizeInt;
begin
  sb := TStringBuilder.Create;
  try
    lineStart := 1;
    i := 1;
    while i <= Length(AText) + 1 do
    begin
      if (i > Length(AText)) or (AText[i] = #10) then
      begin
        lineEnd := i - 1;
        while (lineEnd >= lineStart) and (AText[lineEnd] in [' ', #9]) do
          Dec(lineEnd);
        sb.Append(Copy(AText, lineStart, lineEnd - lineStart + 1));
        if i <= Length(AText) then
          sb.Append(#10);
        lineStart := i + 1;
      end;
      Inc(i);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function AIFindAll(const AText, APattern: string): TAIOffsets;
var
  p, n: SizeInt;
begin
  Result := nil;
  if APattern = '' then
    Exit;
  n := 0;
  p := Pos(APattern, AText);
  while p > 0 do
  begin
    if n = Length(Result) then
      SetLength(Result, n * 2 + 4);
    Result[n] := p;
    Inc(n);
    p := PosEx(APattern, AText, p + Length(APattern));
  end;
  SetLength(Result, n);
end;

procedure AIOffsetToLineCol(const AText: string; AOffset: SizeInt;
  out ALine, ACol: integer);
var
  i, lineStart: SizeInt;
begin
  ALine := 1;
  lineStart := 1;
  if AOffset > Length(AText) + 1 then
    AOffset := Length(AText) + 1;
  for i := 1 to AOffset - 1 do
    if AText[i] = #10 then
    begin
      Inc(ALine);
      lineStart := i + 1;
    end;
  ACol := AOffset - lineStart + 1;
end;

function AIReplaceAt(const AText: string; const AOffsets: TAIOffsets;
  AOldLength: SizeInt; const ANew: string): string;
var
  i, last: SizeInt;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    last := 1;
    for i := 0 to High(AOffsets) do
    begin
      sb.Append(Copy(AText, last, AOffsets[i] - last));
      sb.Append(ANew);
      last := AOffsets[i] + AOldLength;
    end;
    sb.Append(Copy(AText, last, MaxInt));
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function AISearchText(const AFileName, AText, AQuery: string;
  ACaseSensitive: boolean; AMatches: TJSONArray; AMax: integer): integer;
var
  lines: TStringList;
  i: integer;
  query, line, shown: string;
  hit: boolean;
begin
  Result := 0;
  if AQuery = '' then
    Exit;
  if ACaseSensitive then
    query := AQuery
  else
    query := LowerCase(AQuery);
  lines := TStringList.Create;
  try
    lines.Text := AText;
    for i := 0 to lines.Count - 1 do
    begin
      line := lines[i];
      if ACaseSensitive then
        hit := Pos(query, line) > 0
      else
        hit := Pos(query, LowerCase(line)) > 0;
      if not hit then
        Continue;
      Inc(Result);
      if AMatches.Count >= AMax then
        Continue;
      shown := Trim(line);
      if Length(shown) > MAX_MATCH_LINE then
        shown := CutUTF8(shown, MAX_MATCH_LINE) + '...';
      AMatches.Add(TJSONObject.Create(['file', AFileName, 'line', i + 1,
        'text', shown]));
    end;
  finally
    lines.Free;
  end;
end;

end.

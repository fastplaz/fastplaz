unit test_aiplane_textedit;

{
  Tests for aiplane_textedit: the text logic behind the Agent Plane tools
  edit_text and search_project (occurrences, line/column positions as the
  Lazarus editor takes them, replacement, line search).
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, aiplane_textedit;

type

  { TAITextEditTest }

  TAITextEditTest = class(TTestCase)
  private
    function ReplaceByLineCol(const AText, AOld, ANew: string): string;
  published
    procedure TestNormalizeEOL;
    procedure TestTrimLineEnds;
    procedure TestFindAll;
    procedure TestFindAllNotOverlapping;
    procedure TestOffsetToLineCol;
    procedure TestOffsetToLineColUTF8;
    procedure TestReplaceAt;
    procedure TestReplaceByPositionsMatchesReplaceAt;
    procedure TestSearchText;
    procedure TestSearchTextCaseSensitive;
    procedure TestSearchTextLimit;
    procedure TestSearchTextCutsLongLines;
  end;

implementation

{ Applies the replacements the way edit_text does in the editor: from the
  last occurrence back, each by (line, column) positions. The text is kept
  as lines, like the editor buffer. }
function TAITextEditTest.ReplaceByLineCol(const AText, AOld, ANew: string): string;
var
  offsets: TAIOffsets;
  lines: TStringList;
  i, line, col, endLine, endCol: integer;
  head, tail: string;
  newLines: TStringList;
  j: integer;
begin
  offsets := AIFindAll(AText, AOld);
  lines := TStringList.Create;
  newLines := TStringList.Create;
  try
    lines.LineBreak := #10;
    newLines.LineBreak := #10;
    lines.Text := AText;
    for i := High(offsets) downto 0 do
    begin
      AIOffsetToLineCol(AText, offsets[i], line, col);
      AIOffsetToLineCol(AText, offsets[i] + Length(AOld), endLine, endCol);
      head := Copy(lines[line - 1], 1, col - 1);
      tail := Copy(lines[endLine - 1], endCol, MaxInt);
      for j := endLine downto line do
        lines.Delete(j - 1);
      newLines.Text := head + ANew + tail;
      if (head + ANew + tail = '') then
        newLines.Text := #10;
      for j := newLines.Count - 1 downto 0 do
        lines.Insert(line - 1, newLines[j]);
    end;
    Result := lines.Text;
  finally
    newLines.Free;
    lines.Free;
  end;
end;

procedure TAITextEditTest.TestNormalizeEOL;
begin
  AssertEquals('a'#10'b'#10'c'#10'd', AINormalizeEOL('a'#13#10'b'#13'c'#10'd'));
  AssertEquals('', AINormalizeEOL(''));
end;

procedure TAITextEditTest.TestTrimLineEnds;
begin
  AssertEquals('a'#10'  b'#10#10'c', AITrimLineEnds('a  '#10'  b'#9#10' '#10'c '));
  AssertEquals('x'#10, AITrimLineEnds('x '#10));
  AssertEquals('', AITrimLineEnds(''));
end;

procedure TAITextEditTest.TestFindAll;
var
  r: TAIOffsets;
begin
  r := AIFindAll('begin end; begin end;', 'begin');
  AssertEquals(2, Length(r));
  AssertEquals(1, r[0]);
  AssertEquals(12, r[1]);
  AssertEquals(0, Length(AIFindAll('abc', 'x')));
  AssertEquals('empty pattern', 0, Length(AIFindAll('abc', '')));
  AssertEquals(0, Length(AIFindAll('', 'a')));
end;

procedure TAITextEditTest.TestFindAllNotOverlapping;
var
  r: TAIOffsets;
begin
  r := AIFindAll('aaaa', 'aa');
  AssertEquals(2, Length(r));
  AssertEquals(1, r[0]);
  AssertEquals(3, r[1]);
end;

procedure TAITextEditTest.TestOffsetToLineCol;
var
  line, col: integer;
  text: string;
begin
  text := 'ab'#10'cde'#10#10'f';
  AIOffsetToLineCol(text, 1, line, col);
  AssertEquals(1, line);
  AssertEquals(1, col);
  AIOffsetToLineCol(text, 3, line, col);
  AssertEquals('the line break itself', 1, line);
  AssertEquals(3, col);
  AIOffsetToLineCol(text, 4, line, col);
  AssertEquals(2, line);
  AssertEquals(1, col);
  AIOffsetToLineCol(text, 8, line, col);
  AssertEquals('empty line', 3, line);
  AssertEquals(1, col);
  AIOffsetToLineCol(text, Length(text) + 1, line, col);
  AssertEquals('past the end', 4, line);
  AssertEquals(2, col);
  AIOffsetToLineCol(text, 1000, line, col);
  AssertEquals('clamped', 4, line);
  AssertEquals(2, col);
end;

procedure TAITextEditTest.TestOffsetToLineColUTF8;
var
  line, col: integer;
  text: string;
begin
  // columns are bytes: the editor's logical column
  text := 'x := ''' + #$C3#$A9 + ''';';
  AIOffsetToLineCol(text, Pos(';', text), line, col);
  AssertEquals(1, line);
  AssertEquals(10, col);
end;

procedure TAITextEditTest.TestReplaceAt;
var
  text: string;
begin
  text := 'a.b.c';
  AssertEquals('a--b--c', AIReplaceAt(text, AIFindAll(text, '.'), 1, '--'));
  AssertEquals('abc', AIReplaceAt(text, AIFindAll(text, '.'), 1, ''));
  AssertEquals('X.b.c', AIReplaceAt(text, AIFindAll(text, 'a'), 1, 'X'));
  AssertEquals('a.b.X', AIReplaceAt(text, AIFindAll(text, 'c'), 1, 'X'));
  AssertEquals('no offsets', text, AIReplaceAt(text, nil, 1, 'X'));
end;

procedure TAITextEditTest.TestReplaceByPositionsMatchesReplaceAt;

  procedure Check(const AText, AOld, ANew: string);
  begin
    AssertEquals(AOld + ' -> ' + ANew,
      AIReplaceAt(AText, AIFindAll(AText, AOld), Length(AOld), ANew) + #10,
      ReplaceByLineCol(AText, AOld, ANew));
  end;

const
  SRC = 'unit x;'#10'begin'#10'  a := 1;'#10'  b := 2;'#10'end.';
begin
  Check(SRC, 'a := 1', 'a := 10');
  Check(SRC, ':=', '=');
  Check(SRC, '1;'#10'  b', '1;'#10'  // new'#10'  c');
  Check(SRC, 'begin'#10'  a := 1;'#10, 'begin'#10);
  Check(SRC, 'end.', 'end.' + ' // ' + #$E2#$9C#$93);
end;

procedure TAITextEditTest.TestSearchText;
var
  matches: TJSONArray;
  n: integer;
begin
  matches := TJSONArray.Create;
  try
    n := AISearchText('a.pas', 'Route[''x''] := TX;'#10'nothing'#10'  route.Add(1);',
      'ROUTE', False, matches, 10);
    AssertEquals(2, n);
    AssertEquals(2, matches.Count);
    AssertEquals('a.pas', matches.Objects[0].Get('file', ''));
    AssertEquals(1, matches.Objects[0].Get('line', 0));
    AssertEquals(3, matches.Objects[1].Get('line', 0));
    AssertEquals('trimmed', 'route.Add(1);', matches.Objects[1].Get('text', ''));
    AssertEquals('empty query', 0, AISearchText('a.pas', 'x', '', False, matches, 10));
  finally
    matches.Free;
  end;
end;

procedure TAITextEditTest.TestSearchTextCaseSensitive;
var
  matches: TJSONArray;
begin
  matches := TJSONArray.Create;
  try
    AssertEquals(1, AISearchText('a.pas', 'Route'#10'route', 'route', True, matches, 10));
    AssertEquals(2, matches.Objects[0].Get('line', 0));
  finally
    matches.Free;
  end;
end;

procedure TAITextEditTest.TestSearchTextLimit;
var
  matches: TJSONArray;
begin
  matches := TJSONArray.Create;
  try
    AssertEquals(3, AISearchText('a.pas', 'x'#10'x'#10'x', 'x', False, matches, 2));
    AssertEquals('only two listed', 2, matches.Count);
    // the limit counts what is already in the array (several files)
    AssertEquals(1, AISearchText('b.pas', 'x', 'x', False, matches, 2));
    AssertEquals(2, matches.Count);
  finally
    matches.Free;
  end;
end;

procedure TAITextEditTest.TestSearchTextCutsLongLines;
var
  matches: TJSONArray;
  line, shown: string;
begin
  // 299 bytes, then a 2-byte character across the cut
  line := 'q' + StringOfChar('a', 298) + #$C3#$A9 + StringOfChar('b', 50);
  matches := TJSONArray.Create;
  try
    AISearchText('a.pas', line, 'q', False, matches, 10);
    shown := matches.Objects[0].Get('text', '');
    AssertEquals('cut before the character', 'q' + StringOfChar('a', 298) + '...', shown);
  finally
    matches.Free;
  end;
end;

initialization
  RegisterTest(TAITextEditTest);

end.

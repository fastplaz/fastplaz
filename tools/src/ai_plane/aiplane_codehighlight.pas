unit aiplane_codehighlight;

{
  Syntax colours for code blocks in the AI Plane chat, using the SynEdit
  highlighters that ship with Lazarus (no extra package).

  The language comes from the code fence info string (```pascal, ```sql,
  ...). Tokens are mapped to a small palette by kind (comment, keyword,
  string, number, directive) instead of the highlighter's own colours, so
  the result reads the same on light and dark themes. Unknown or missing
  languages are drawn plain.

  One highlighter per language is created on first use and kept.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TAICodeToken = record
    Text: string;
    Color: TColor;        // clNone = normal text colour
    Style: TFontStyles;
  end;

  TAICodeLine = array of TAICodeToken;
  TAICodeLines = array of TAICodeLine;

{ Splits ACode into coloured tokens per line. Returns False (and no lines)
  when ALang has no highlighter. }
function AIHighlightCode(const ACode, ALang: string; out ALines: TAICodeLines): boolean;
{ Normalised language name a highlighter exists for, '' otherwise. }
function AICodeLanguage(const ALang: string): string;

implementation

uses
  SynEditHighlighter, SynHighlighterPas, SynHighlighterSQL, SynHighlighterJScript,
  SynHighlighterXML, SynHighlighterHTML, SynHighlighterPython, SynHighlighterPHP,
  SynHighlighterUNIXShellScript, SynHighlighterBat, SynHighlighterIni,
  SynHighlighterCss, SynHighlighterCpp, SynHighlighterJava, SynHighlighterDiff,
  SynHighlighterLFM, SynHighlighterPerl;

type
  // GetDefaultAttribute is protected
  THighlighterAccess = class(TSynCustomHighlighter);

  TTokenKind = (tkPlain, tkComment, tkKeyword, tkString, tkNumber, tkDirective);

var
  Highlighters: TStringList = nil;  // language -> TSynCustomHighlighter

function AICodeLanguage(const ALang: string): string;
var
  s: string;
  p: integer;
begin
  s := LowerCase(Trim(ALang));
  p := Pos(' ', s);
  if p > 0 then
    SetLength(s, p - 1);
  // "{.pascal}" style info strings
  s := StringReplace(StringReplace(StringReplace(s, '{', '', [rfReplaceAll]), '}', '',
    [rfReplaceAll]), '.', '', [rfReplaceAll]);
  case s of
    'pascal', 'pas', 'pp', 'delphi', 'objectpascal', 'object-pascal', 'freepascal',
    'fpc', 'lazarus', 'dpr', 'lpr', 'inc': Result := 'pascal';
    'lfm', 'dfm': Result := 'lfm';
    'sql', 'mysql', 'postgresql', 'postgres', 'sqlite', 'plsql', 'tsql': Result := 'sql';
    'js', 'javascript', 'ts', 'typescript', 'json', 'jsonc', 'json5', 'jsx', 'tsx':
      Result := 'js';
    'xml', 'xaml', 'svg', 'lpi', 'lpk', 'lps', 'xsd': Result := 'xml';
    'html', 'htm', 'xhtml': Result := 'html';
    'python', 'py': Result := 'python';
    'php': Result := 'php';
    'sh', 'bash', 'shell', 'zsh', 'console', 'shellscript': Result := 'shell';
    'bat', 'cmd', 'batch', 'dos': Result := 'bat';
    'ini', 'cfg', 'conf', 'toml', 'properties': Result := 'ini';
    'css', 'scss', 'less': Result := 'css';
    'c', 'cpp', 'c++', 'h', 'hpp', 'cc', 'cs', 'csharp': Result := 'cpp';
    'java', 'kotlin': Result := 'java';
    'diff', 'patch': Result := 'diff';
    'perl', 'pl': Result := 'perl';
    else
      Result := '';
  end;
end;

function GetHighlighter(const ALanguage: string): TSynCustomHighlighter;
var
  i: integer;
begin
  Result := nil;
  if ALanguage = '' then
    Exit;
  if Highlighters = nil then
  begin
    Highlighters := TStringList.Create;
    Highlighters.OwnsObjects := True;
    Highlighters.Sorted := True;
  end;
  i := Highlighters.IndexOf(ALanguage);
  if i >= 0 then
    Exit(TSynCustomHighlighter(Highlighters.Objects[i]));
  case ALanguage of
    'pascal': Result := TSynFreePascalSyn.Create(nil);
    'lfm': Result := TSynLFMSyn.Create(nil);
    'sql': Result := TSynSQLSyn.Create(nil);
    'js': Result := TSynJScriptSyn.Create(nil);
    'xml': Result := TSynXMLSyn.Create(nil);
    'html': Result := TSynHTMLSyn.Create(nil);
    'python': Result := TSynPythonSyn.Create(nil);
    'php': Result := TSynPHPSyn.Create(nil);
    'shell': Result := TSynUNIXShellScriptSyn.Create(nil);
    'bat': Result := TSynBatSyn.Create(nil);
    'ini': Result := TSynIniSyn.Create(nil);
    'css': Result := TSynCssSyn.Create(nil);
    'cpp': Result := TSynCppSyn.Create(nil);
    'java': Result := TSynJavaSyn.Create(nil);
    'diff': Result := TSynDiffSyn.Create(nil);
    'perl': Result := TSynPerlSyn.Create(nil);
  end;
  if Assigned(Result) then
    Highlighters.AddObject(ALanguage, Result);
end;

function KindOf(AHighlighter: TSynCustomHighlighter;
  AAttr: TSynHighlighterAttributes): TTokenKind;
var
  hl: THighlighterAccess;
  name: string;
begin
  Result := tkPlain;
  if AAttr = nil then
    Exit;
  hl := THighlighterAccess(AHighlighter);
  if AAttr = hl.GetDefaultAttribute(SYN_ATTR_COMMENT) then
    Exit(tkComment);
  if AAttr = hl.GetDefaultAttribute(SYN_ATTR_KEYWORD) then
    Exit(tkKeyword);
  if AAttr = hl.GetDefaultAttribute(SYN_ATTR_STRING) then
    Exit(tkString);
  if AAttr = hl.GetDefaultAttribute(SYN_ATTR_NUMBER) then
    Exit(tkNumber);
  if AAttr = hl.GetDefaultAttribute(SYN_ATTR_DIRECTIVE) then
    Exit(tkDirective);
  // highlighters without default attributes: go by the stored name
  name := LowerCase(AAttr.StoredName);
  if Pos('comment', name) > 0 then
    Result := tkComment
  else if (Pos('key', name) > 0) or (Pos('reserved', name) > 0) or
    (Pos('element', name) > 0) or (Pos('tag', name) > 0) then
    Result := tkKeyword
  else if (Pos('string', name) > 0) or (Pos('char', name) > 0) or
    (Pos('value', name) > 0) then
    Result := tkString
  else if (Pos('number', name) > 0) or (Pos('float', name) > 0) or
    (Pos('hex', name) > 0) or (Pos('octal', name) > 0) then
    Result := tkNumber
  else if (Pos('direct', name) > 0) or (Pos('preproc', name) > 0) or
    (Pos('attr', name) > 0) or (Pos('section', name) > 0) or
    (Pos('variable', name) > 0) then
    Result := tkDirective;
end;

// Mid-tone colours that read on light and dark backgrounds.
procedure KindColor(AKind: TTokenKind; out AColor: TColor; out AStyle: TFontStyles);
begin
  AStyle := [];
  case AKind of
    tkComment:
    begin
      AColor := RGBToColor(110, 140, 100);
      AStyle := [fsItalic];
    end;
    tkKeyword:
    begin
      AColor := RGBToColor(45, 100, 210);
      AStyle := [fsBold];
    end;
    tkString: AColor := RGBToColor(190, 85, 40);
    tkNumber: AColor := RGBToColor(150, 70, 180);
    tkDirective: AColor := RGBToColor(0, 140, 140);
    else
      AColor := clNone;
  end;
end;

function AIHighlightCode(const ACode, ALang: string; out ALines: TAICodeLines): boolean;
var
  hl: TSynCustomHighlighter;
  lines: TStringList;
  i, n: integer;
  token: string;
  kind: TTokenKind;
  color: TColor;
  style: TFontStyles;
begin
  ALines := nil;
  hl := GetHighlighter(AICodeLanguage(ALang));
  Result := Assigned(hl);
  if not Result then
    Exit;
  lines := TStringList.Create;
  try
    lines.Text := ACode;
    if lines.Count = 0 then
      lines.Add('');
    SetLength(ALines, lines.Count);
    hl.ResetRange;
    for i := 0 to lines.Count - 1 do
    begin
      // the range (open comment, string, ...) carries over to the next line
      hl.SetLine(lines[i], i);
      while not hl.GetEol do
      begin
        token := hl.GetToken;
        if token <> '' then
        begin
          kind := KindOf(hl, hl.GetTokenAttribute);
          KindColor(kind, color, style);
          n := Length(ALines[i]);
          // merge neighbours that look the same
          if (n > 0) and (ALines[i][n - 1].Color = color) and (ALines[i][n - 1].Style = style) then
            ALines[i][n - 1].Text := ALines[i][n - 1].Text + token
          else
          begin
            SetLength(ALines[i], n + 1);
            ALines[i][n].Text := token;
            ALines[i][n].Color := color;
            ALines[i][n].Style := style;
          end;
        end;
        hl.Next;
      end;
    end;
  finally
    lines.Free;
  end;
end;

finalization
  FreeAndNil(Highlighters);

end.

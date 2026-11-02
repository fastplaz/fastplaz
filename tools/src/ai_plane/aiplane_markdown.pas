unit aiplane_markdown;

{
  Small markdown parser for the AI Plane chat.

  Covers what coding agents write: headings, paragraphs, bullet and numbered
  lists (nested by indentation), block quotes, fenced code, horizontal
  rules, pipe tables, and inline bold, italic, strikethrough, `code`,
  [links](url), bare http(s) URLs and images: ![alt](src "title"), also
  wrapped in a link: [![alt](src)](href).

  Text arrives while it streams, so half-written markup must stay readable:
  an unclosed ``` fence is still a code block, and ** or * only start bold
  or italic when the closing marker is already there; until then the
  markers show as typed.

  Line breaks inside a paragraph are kept (chat answers often rely on them).

  No LCL here: the parser is shared by the chat view and the unit tests.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

type
  TMDInlineStyle = (misBold, misItalic, misStrike, misCode, misLink, misImage);
  TMDInlineStyles = set of TMDInlineStyle;

  TMDRun = record
    Text: string;             // image: the alt text
    Style: TMDInlineStyles;
    Url: string;              // link target; image: the image source
    Title: string;            // link or image title ("tooltip")
    Link: string;             // image wrapped in a link: the link target
  end;

  TMDRuns = array of TMDRun;

  TMDBlockKind = (mbParagraph, mbHeading, mbBullet, mbNumbered, mbQuote, mbCode,
    mbRule, mbTable);

  TMDTableRow = array of TMDRuns;

  TMDBlock = record
    Kind: TMDBlockKind;
    Level: integer;     // heading 1..6, list nesting 0..
    Marker: string;     // numbered list marker, e.g. "2."
    Runs: TMDRuns;      // paragraph, heading, list item, quote
    Code: string;       // code block text
    Lang: string;       // code block info string
    Rows: array of TMDTableRow;  // table; the first row is the header
  end;

  TMDBlocks = array of TMDBlock;

function MDParse(const AText: string): TMDBlocks;
function MDParseInline(const AText: string): TMDRuns;
function MDRunsText(const ARuns: TMDRuns): string;

implementation

const
  LF = #10;
  ESCAPABLE = ['\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.',
    '!', '|', '~', '>'];

function IsWordChar(C: char): boolean; inline;
begin
  // bytes >= $80 belong to UTF-8 letters
  Result := (C in ['A'..'Z', 'a'..'z', '0'..'9']) or (Ord(C) >= $80);
end;

function IsSpace(C: char): boolean; inline;
begin
  Result := C in [' ', #9, #10, #13];
end;

{ ---------- inline ---------- }

// Index of the "]" matching the "[" at AOpen (nested brackets, escapes),
// 0 when there is none.
function MatchBracket(const AText: string; AOpen: integer): integer;
var
  depth, i: integer;
begin
  Result := 0;
  depth := 0;
  i := AOpen;
  while i <= Length(AText) do
  begin
    case AText[i] of
      '\': Inc(i);
      '[': Inc(depth);
      ']':
      begin
        Dec(depth);
        if depth = 0 then
          Exit(i);
      end;
    end;
    Inc(i);
  end;
end;

// Parses a link target that starts after "(" at AStart: url, optional
// "title" (or 'title'), up to the closing ")". AClose gets its index.
function ParseTarget(const AText: string; AStart: integer; out AUrl, ATitle: string;
  out AClose: integer): boolean;
var
  i: integer;
  quote: char;
  inner: string;
begin
  Result := False;
  AUrl := '';
  ATitle := '';
  AClose := 0;
  quote := #0;
  i := AStart;
  while i <= Length(AText) do
  begin
    if quote <> #0 then
    begin
      if AText[i] = quote then
        quote := #0;
    end
    else if AText[i] in ['"', ''''] then
      quote := AText[i]
    else if AText[i] = ')' then
    begin
      AClose := i;
      Break;
    end;
    Inc(i);
  end;
  if AClose = 0 then
    Exit;
  inner := Trim(Copy(AText, AStart, AClose - AStart));
  // <url with spaces> form
  if (inner <> '') and (inner[1] = '<') and (Pos('>', inner) > 0) then
  begin
    i := Pos('>', inner);
    AUrl := Copy(inner, 2, i - 2);
    inner := Trim(Copy(inner, i + 1, MaxInt));
  end
  else
  begin
    i := Pos(' ', inner);
    if i = 0 then
      i := Length(inner) + 1;
    AUrl := Copy(inner, 1, i - 1);
    inner := Trim(Copy(inner, i + 1, MaxInt));
  end;
  // what is left is the optional "title"
  ATitle := inner;
  if (Length(ATitle) >= 2) and (ATitle[1] in ['"', '''']) and
    (ATitle[Length(ATitle)] = ATitle[1]) then
    ATitle := Copy(ATitle, 2, Length(ATitle) - 2);
  Result := True;
end;

function MDParseInline(const AText: string): TMDRuns;
var
  i, n, k, p, q: integer;
  buf, ticks, url, linkText, title: string;
  sub: TMDRuns;
  style: TMDInlineStyles;
  runs: TMDRuns;

  procedure AddRun(const AValue: string; AStyle: TMDInlineStyles; const AUrl: string);
  var
    m: integer;
  begin
    if AValue = '' then
      Exit;
    m := Length(runs);
    // merge with the previous run when nothing differs
    if (m > 0) and (runs[m - 1].Style = AStyle) and (runs[m - 1].Url = AUrl) then
    begin
      runs[m - 1].Text := runs[m - 1].Text + AValue;
      Exit;
    end;
    SetLength(runs, m + 1);
    runs[m].Text := AValue;
    runs[m].Style := AStyle;
    runs[m].Url := AUrl;
  end;

  procedure Flush;
  begin
    AddRun(buf, style, '');
    buf := '';
  end;

  // images are never merged with their neighbours
  procedure AddImage(const AAlt, ASrc, ATitle, ALink: string);
  var
    m: integer;
  begin
    m := Length(runs);
    SetLength(runs, m + 1);
    runs[m].Text := AAlt;
    runs[m].Style := style + [misImage];
    runs[m].Url := ASrc;
    runs[m].Title := ATitle;
    runs[m].Link := ALink;
  end;

  function CharAt(AIndex: integer): char;
  begin
    if (AIndex >= 1) and (AIndex <= n) then
      Result := AText[AIndex]
    else
      Result := ' ';
  end;

  // Toggles AStyle for a delimiter of ALen chars at i. Opening needs a
  // non-space after it and a closing delimiter later; '_' also needs a word
  // boundary, so snake_case stays as it is.
  function Toggle(AStyle: TMDInlineStyle; const ADelim: string): boolean;
  var
    len: integer;
    boundary: boolean;
  begin
    Result := False;
    len := Length(ADelim);
    // '_' and a single '*' need a word boundary (my_var, 2*3*4 stay text)
    boundary := (ADelim[1] = '_') or (ADelim = '*');
    if AStyle in style then
    begin
      if IsSpace(CharAt(i - 1)) then
        Exit;
      if boundary and IsWordChar(CharAt(i + len)) then
        Exit;
      Flush;
      Exclude(style, AStyle);
    end
    else
    begin
      if IsSpace(CharAt(i + len)) then
        Exit;
      if boundary and IsWordChar(CharAt(i - 1)) then
        Exit;
      if PosEx(ADelim, AText, i + len + 1) = 0 then
        Exit;
      Flush;
      Include(style, AStyle);
    end;
    Inc(i, len);
    Result := True;
  end;

begin
  runs := nil;
  buf := '';
  style := [];
  n := Length(AText);
  i := 1;
  while i <= n do
  begin
    case AText[i] of
      '\':
        if (i < n) and (AText[i + 1] in ESCAPABLE) then
        begin
          buf := buf + AText[i + 1];
          Inc(i, 2);
          Continue;
        end;
      '`':
      begin
        k := 0;
        while (i + k <= n) and (AText[i + k] = '`') do
          Inc(k);
        ticks := StringOfChar('`', k);
        p := PosEx(ticks, AText, i + k);
        if p > 0 then
        begin
          Flush;
          AddRun(Trim(Copy(AText, i + k, p - i - k)), style + [misCode], '');
          i := p + k;
        end
        else
        begin
          buf := buf + ticks;
          Inc(i, k);
        end;
        Continue;
      end;
      '*', '_':
      begin
        if (CharAt(i + 1) = AText[i]) then
        begin
          if Toggle(misBold, AText[i] + AText[i]) then
            Continue;
          // not a bold marker: both characters are text
          buf := buf + AText[i] + AText[i];
          Inc(i, 2);
          Continue;
        end;
        if Toggle(misItalic, AText[i]) then
          Continue;
      end;
      '~':
        if CharAt(i + 1) = '~' then
        begin
          if Toggle(misStrike, '~~') then
            Continue;
          buf := buf + '~~';
          Inc(i, 2);
          Continue;
        end;
      '!':
        if CharAt(i + 1) = '[' then
        begin
          p := MatchBracket(AText, i + 1);
          if (p > 0) and (CharAt(p + 1) = '(') and ParseTarget(AText, p + 2, url, title, q) then
          begin
            Flush;
            AddImage(Copy(AText, i + 2, p - i - 2), url, title, '');
            i := q + 1;
            Continue;
          end;
        end;
      '[':
      begin
        p := MatchBracket(AText, i);
        if (p > 0) and (CharAt(p + 1) = '(') and ParseTarget(AText, p + 2, url, title, q) then
        begin
          linkText := Copy(AText, i + 1, p - i - 1);
          Flush;
          sub := nil;
          if Copy(TrimLeft(linkText), 1, 2) = '![' then
            sub := MDParseInline(Trim(linkText));
          if (Length(sub) = 1) and (misImage in sub[0].Style) then
            // a clickable image
            AddImage(sub[0].Text, sub[0].Url, sub[0].Title, url)
          else
          begin
            if Length(sub) > 0 then
              linkText := MDRunsText(sub);
            AddRun(linkText, style + [misLink], url);
            if title <> '' then
              runs[High(runs)].Title := title;
          end;
          i := q + 1;
          Continue;
        end;
      end;
      'h':
        if ((Copy(AText, i, 7) = 'http://') or (Copy(AText, i, 8) = 'https://')) and
          ((i = 1) or IsSpace(AText[i - 1]) or (AText[i - 1] in ['(', '<'])) then
        begin
          p := i;
          while (p <= n) and not IsSpace(AText[p]) and (AText[p] <> '>') do
            Inc(p);
          // trailing punctuation belongs to the sentence
          while (p > i) and (AText[p - 1] in ['.', ',', ';', ':', ')', '!', '?']) do
            Dec(p);
          url := Copy(AText, i, p - i);
          Flush;
          AddRun(url, style + [misLink], url);
          i := p;
          Continue;
        end;
    end;
    buf := buf + AText[i];
    Inc(i);
  end;
  Flush;
  Result := runs;
end;

function MDRunsText(const ARuns: TMDRuns): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to High(ARuns) do
    Result := Result + ARuns[i].Text;
end;

{ ---------- blocks ---------- }

function Indentation(const ALine: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(ALine) do
    case ALine[i] of
      ' ': Inc(Result);
      #9: Inc(Result, 4);
      else
        Break;
    end;
end;

function IsFence(const ATrimmed: string; out AMarker, ALang: string): boolean;
begin
  Result := (Copy(ATrimmed, 1, 3) = '```') or (Copy(ATrimmed, 1, 3) = '~~~');
  if Result then
  begin
    AMarker := Copy(ATrimmed, 1, 3);
    ALang := Trim(Copy(ATrimmed, 4, MaxInt));
  end;
end;

function IsRule(const ATrimmed: string): boolean;
var
  s: string;
  i: integer;
begin
  s := StringReplace(ATrimmed, ' ', '', [rfReplaceAll]);
  Result := (Length(s) >= 3) and (s[1] in ['-', '*', '_']);
  if Result then
    for i := 2 to Length(s) do
      if s[i] <> s[1] then
        Exit(False);
end;

function HeadingLevel(const ATrimmed: string): integer;
begin
  Result := 0;
  while (Result < Length(ATrimmed)) and (ATrimmed[Result + 1] = '#') do
    Inc(Result);
  if (Result = 0) or (Result > 6) or
    ((Result < Length(ATrimmed)) and (ATrimmed[Result + 1] <> ' ')) then
    Result := 0;
end;

function ListItem(const ALine: string; out AIndent: integer; out AOrdered: boolean;
  out AMarker, AContent: string): boolean;
var
  s: string;
  i: integer;
begin
  Result := False;
  AIndent := Indentation(ALine);
  s := TrimLeft(ALine);
  if (Length(s) >= 2) and (s[1] in ['-', '*', '+']) and (s[2] = ' ') then
  begin
    AOrdered := False;
    AMarker := '';
    AContent := Trim(Copy(s, 3, MaxInt));
    Exit(True);
  end;
  i := 1;
  while (i <= Length(s)) and (s[i] in ['0'..'9']) do
    Inc(i);
  if (i > 1) and (i <= 10) and (i < Length(s)) and (s[i] in ['.', ')']) and
    (s[i + 1] = ' ') then
  begin
    AOrdered := True;
    AMarker := Copy(s, 1, i - 1) + '.';
    AContent := Trim(Copy(s, i + 2, MaxInt));
    Result := True;
  end;
end;

function IsTableSeparator(const ATrimmed: string): boolean;
var
  i: integer;
begin
  Result := (Pos('|', ATrimmed) > 0) and (Pos('-', ATrimmed) > 0);
  if Result then
    for i := 1 to Length(ATrimmed) do
      if not (ATrimmed[i] in ['|', '-', ':', ' ']) then
        Exit(False);
end;

function SplitCells(const ATrimmed: string): TMDTableRow;
var
  s, cell: string;
  i, n: integer;
begin
  Result := nil;
  s := ATrimmed;
  if (s <> '') and (s[1] = '|') then
    Delete(s, 1, 1);
  if (s <> '') and (s[Length(s)] = '|') then
    SetLength(s, Length(s) - 1);
  cell := '';
  i := 1;
  while i <= Length(s) do
  begin
    if (s[i] = '\') and (i < Length(s)) and (s[i + 1] = '|') then
    begin
      cell := cell + '|';
      Inc(i, 2);
      Continue;
    end;
    if s[i] = '|' then
    begin
      n := Length(Result);
      SetLength(Result, n + 1);
      Result[n] := MDParseInline(Trim(cell));
      cell := '';
    end
    else
      cell := cell + s[i];
    Inc(i);
  end;
  n := Length(Result);
  SetLength(Result, n + 1);
  Result[n] := MDParseInline(Trim(cell));
end;

function MDParse(const AText: string): TMDBlocks;
var
  lines: TStringList;
  i, indent, n: integer;
  line, t, marker, lang, closeMarker, content, code, para, quote: string;
  ordered: boolean;
  blocks: TMDBlocks;

  function Add(AKind: TMDBlockKind): integer;
  begin
    Result := Length(blocks);
    SetLength(blocks, Result + 1);
    blocks[Result].Kind := AKind;
  end;

  procedure FlushParagraph;
  begin
    if para = '' then
      Exit;
    n := Add(mbParagraph);
    blocks[n].Runs := MDParseInline(para);
    para := '';
  end;

  procedure FlushQuote;
  begin
    if quote = '' then
      Exit;
    n := Add(mbQuote);
    blocks[n].Runs := MDParseInline(quote);
    quote := '';
  end;

  procedure FlushAll;
  begin
    FlushParagraph;
    FlushQuote;
  end;

  function StartsBlock(const ALine: string): boolean;
  var
    tt, m1, m2, c: string;
    ind: integer;
    o: boolean;
  begin
    tt := Trim(ALine);
    Result := (tt = '') or IsFence(tt, m1, m2) or (HeadingLevel(tt) > 0) or
      IsRule(tt) or (Copy(tt, 1, 1) = '>') or (Copy(tt, 1, 1) = '|') or
      ListItem(ALine, ind, o, m1, c);
  end;

begin
  blocks := nil;
  para := '';
  quote := '';
  lines := TStringList.Create;
  try
    lines.Text := StringReplace(AText, #13#10, LF, [rfReplaceAll]);
    i := 0;
    while i < lines.Count do
    begin
      line := lines[i];
      t := Trim(line);

      if IsFence(t, marker, lang) then
      begin
        FlushAll;
        closeMarker := marker;
        code := '';
        Inc(i);
        // an unclosed fence (still streaming) runs to the end
        while (i < lines.Count) and (Copy(Trim(lines[i]), 1, 3) <> closeMarker) do
        begin
          if code <> '' then
            code := code + LF;
          code := code + StringReplace(lines[i], #9, '    ', [rfReplaceAll]);
          Inc(i);
        end;
        Inc(i);
        n := Add(mbCode);
        blocks[n].Code := code;
        blocks[n].Lang := lang;
        Continue;
      end;

      if t = '' then
      begin
        FlushAll;
        Inc(i);
        Continue;
      end;

      if t[1] = '>' then
      begin
        FlushParagraph;
        content := Copy(t, 2, MaxInt);
        if (content <> '') and (content[1] = ' ') then
          Delete(content, 1, 1);
        if quote <> '' then
          quote := quote + LF;
        quote := quote + content;
        Inc(i);
        Continue;
      end;
      FlushQuote;

      if IsRule(t) then
      begin
        FlushParagraph;
        Add(mbRule);
        Inc(i);
        Continue;
      end;

      if HeadingLevel(t) > 0 then
      begin
        FlushParagraph;
        n := Add(mbHeading);
        blocks[n].Level := HeadingLevel(t);
        blocks[n].Runs := MDParseInline(Trim(Copy(t, blocks[n].Level + 1, MaxInt)));
        Inc(i);
        Continue;
      end;

      if (t[1] = '|') and (i + 1 < lines.Count) and IsTableSeparator(Trim(lines[i + 1])) then
      begin
        FlushParagraph;
        n := Add(mbTable);
        SetLength(blocks[n].Rows, 1);
        blocks[n].Rows[0] := SplitCells(t);
        Inc(i, 2);
        while (i < lines.Count) and (Copy(Trim(lines[i]), 1, 1) = '|') do
        begin
          SetLength(blocks[n].Rows, Length(blocks[n].Rows) + 1);
          blocks[n].Rows[High(blocks[n].Rows)] := SplitCells(Trim(lines[i]));
          Inc(i);
        end;
        Continue;
      end;

      if ListItem(line, indent, ordered, marker, content) then
      begin
        FlushParagraph;
        Inc(i);
        // continuation lines of the same item
        while (i < lines.Count) and not StartsBlock(lines[i]) do
        begin
          content := content + LF + Trim(lines[i]);
          Inc(i);
        end;
        if ordered then
          n := Add(mbNumbered)
        else
          n := Add(mbBullet);
        blocks[n].Level := indent div 2;
        blocks[n].Marker := marker;
        blocks[n].Runs := MDParseInline(content);
        Continue;
      end;

      if para <> '' then
        para := para + LF;
      para := para + t;
      Inc(i);
    end;
    FlushAll;
  finally
    lines.Free;
  end;
  Result := blocks;
end;

end.

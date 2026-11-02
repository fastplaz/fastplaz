unit aiplane_chatview;

{
  Conversation view for AI Plane: a scroll box with one owner drawn paint box.

  Every message is a TAIChatItem (user, assistant, thinking, tool call, plan,
  permission, error, info). Assistant and thinking text is markdown
  (aiplane_markdown): headings, lists, quotes, tables, fenced code, and
  inline bold, italic, strikethrough, `code` and links. Other items are
  plain text.

  Layout turns an item into fragments (styled text, code block boxes, rules,
  quote bars, table lines) once per width; painting only draws them. Only
  the item that changed is laid out again, so streaming stays cheap; wrap
  bursts of updates in BeginUpdate/EndUpdate.

  Tool, thinking and plan items fold to their header; a click on the header
  toggles them. Permission items draw their options as buttons and raise
  OnButtonClick. Links open in the browser. Text is copied through the
  context menu.

  LCL only, no IDEIntf: the IDE window and the tester app both use it.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Controls, Forms, Graphics, ExtCtrls, Menus,
  Clipbrd, LCLType, LCLIntf, aiplane_types, aiplane_markdown, aiplane_codehighlight;

type
  TAIChatItemKind = (cikUser, cikAssistant, cikThinking, cikTool, cikPlan,
    cikPermission, cikError, cikInfo);

  TAIChatButton = record
    Id: string;
    Caption: string;
    Kind: string;
    Rect: TRect;  // in paint box coordinates, set while painting
  end;

  TAIChatFragKind = (
    cfText,      // styled text
    cfBox,       // filled rounded box (code block, table header)
    cfLine,      // line from TopLeft to BottomRight (rule, table grid)
    cfBar        // filled rectangle (quote bar)
    );

  { One piece of laid out content. Coordinates are relative to the item's
    content origin (left edge of the text area, first line of content). }
  TAIChatFrag = record
    Kind: TAIChatFragKind;
    R: TRect;
    Text: string;
    Mono: boolean;
    Style: TFontStyles;
    Scale: integer;     // font size in percent
    Color: TColor;      // clNone = the item's text colour
    Back: TColor;       // clNone = no background
    Url: string;
  end;

  { TAIChatItem }

  TAIChatItem = class
  public
    Kind: TAIChatItemKind;
    Key: string;          // tool call id or permission key
    Title: string;
    Text: string;
    Path: string;
    Status: TAIToolStatus;
    Collapsed: boolean;
    Answer: string;       // permission: caption of the chosen option
    Buttons: array of TAIChatButton;
    // layout
    Top: integer;
    Height: integer;
    HeaderHeight: integer;
    ContentY: integer;    // content origin, relative to Top
    ButtonsHeight: integer;
    Frags: array of TAIChatFrag;
    Valid: boolean;
    function Collapsible: boolean;
    function HeaderText: string;
    function IsMarkdown: boolean;
  end;

  TAIChatButtonEvent = procedure(Sender: TObject; AItem: TAIChatItem;
    const AButtonId: string) of object;

  { TAIChatView }

  TAIChatView = class(TScrollBox)
  private
    FBox: TPaintBox;
    FItems: TObjectList;
    FMeasure: TBitmap;
    FCodeFont: TFont;
    FLayoutWidth: integer;
    FOnButtonClick: TAIChatButtonEvent;
    FPopup: TPopupMenu;
    FPopupItem: TAIChatItem;
    FUpdating: integer;
    FDirty: boolean;
    function GetCount: integer;
    function GetItem(AIndex: integer): TAIChatItem;
    function BubbleWidth: integer;
    function TextHeight(const AText: string; AFont: TFont; AWidth: integer): integer;
    procedure MeasureItem(AItem: TAIChatItem);
    procedure Relayout(AFromIndex: integer);
    function AtBottom: boolean;
    procedure ScrollToBottom;
    function ItemAt(Y: integer): TAIChatItem;
    function LinkAt(AItem: TAIChatItem; X, Y: integer): string;
    procedure BoxPaint(Sender: TObject);
    procedure BoxResize(Sender: TObject);
    procedure BoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintItem(ACanvas: TCanvas; AItem: TAIChatItem);
    procedure PopupCopyMessage(Sender: TObject);
    procedure PopupCopyCode(Sender: TObject);
    procedure PopupCopyAll(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(AKind: TAIChatItemKind; const AText: string = ''): TAIChatItem;
    procedure AppendText(AItem: TAIChatItem; const AText: string);
    { Call after changing an item's fields directly. }
    procedure Changed(AItem: TAIChatItem);
    function FindItem(AKind: TAIChatItemKind; const AKey: string): TAIChatItem;
    function LastItem: TAIChatItem;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    function AsText: string;
    property Count: integer read GetCount;
    property Items[AIndex: integer]: TAIChatItem read GetItem;
    property OnButtonClick: TAIChatButtonEvent read FOnButtonClick write FOnButtonClick;
  end;

implementation

const
  MARGIN = 8;
  PAD = 8;
  GAP = 6;
  LIST_GAP = 2;
  LIST_INDENT = 18;
  QUOTE_INDENT = 14;
  CELL_PAD = 6;
  INLINE_CODE_PAD = 3;
  BUTTON_HEIGHT = 24;
  BUTTON_PAD = 12;
  DRAW_FLAGS = DT_WORDBREAK or DT_NOPREFIX or DT_EXPANDTABS or DT_EDITCONTROL;
  HEADING_SCALE: array[1..6] of integer = (150, 130, 115, 105, 100, 100);

function Blend(AColor1, AColor2: TColor; APercent: integer): TColor;
var
  c1, c2: TColor;
begin
  c1 := ColorToRGB(AColor1);
  c2 := ColorToRGB(AColor2);
  Result := RGBToColor(
    (Red(c1) * (100 - APercent) + Red(c2) * APercent) div 100,
    (Green(c1) * (100 - APercent) + Green(c2) * APercent) div 100,
    (Blue(c1) * (100 - APercent) + Blue(c2) * APercent) div 100);
end;

function LinkColor: TColor;
begin
  Result := Blend(RGBToColor(30, 110, 220), clWindowText, 10);
end;

{ ---------- layout ---------- }

type

  { TChatLayout: turns runs and blocks into fragments. }

  TChatLayout = class
  private
    FCanvas: TCanvas;
    FBase: TFont;
    FCode: TFont;
    FBaseSize: integer;
    FMono: boolean;
    FStyle: TFontStyles;
    FScale: integer;
    FFontSet: boolean;
    procedure UseFont(AMono: boolean; AStyle: TFontStyles; AScale: integer);
    function LineHeight(AMono: boolean; AScale: integer): integer;
    function AddFrag(AKind: TAIChatFragKind; const AR: TRect): integer;
  public
    Frags: array of TAIChatFrag;
    constructor Create(ACanvas: TCanvas; ABase, ACode: TFont);
    function Runs(const ARuns: TMDRuns; AX, AWidth, AY: integer;
      AExtraStyle: TFontStyles; AScale: integer; AColor: TColor): integer;
    function Blocks(const ABlocks: TMDBlocks; AWidth: integer): integer;
    function CodeBlock(const ACode, ALang: string; AX, AWidth, AY: integer): integer;
    function Table(const ABlock: TMDBlock; AX, AWidth, AY: integer): integer;
  end;

constructor TChatLayout.Create(ACanvas: TCanvas; ABase, ACode: TFont);
begin
  FCanvas := ACanvas;
  FBase := ABase;
  FCode := ACode;
  FBaseSize := ABase.Size;
  if FBaseSize = 0 then
    FBaseSize := 9;
end;

procedure TChatLayout.UseFont(AMono: boolean; AStyle: TFontStyles; AScale: integer);
begin
  if FFontSet and (AMono = FMono) and (AStyle = FStyle) and (AScale = FScale) then
    Exit;
  if AMono then
    FCanvas.Font.Assign(FCode)
  else
    FCanvas.Font.Assign(FBase);
  FCanvas.Font.Style := AStyle;
  if AScale <> 100 then
  begin
    if AMono then
      FCanvas.Font.Size := FCode.Size * AScale div 100
    else
      FCanvas.Font.Size := FBaseSize * AScale div 100;
  end;
  FMono := AMono;
  FStyle := AStyle;
  FScale := AScale;
  FFontSet := True;
end;

function TChatLayout.LineHeight(AMono: boolean; AScale: integer): integer;
begin
  UseFont(AMono, [], AScale);
  Result := FCanvas.TextHeight('Ag');
end;

function TChatLayout.AddFrag(AKind: TAIChatFragKind; const AR: TRect): integer;
begin
  Result := Length(Frags);
  SetLength(Frags, Result + 1);
  Frags[Result] := Default(TAIChatFrag);
  Frags[Result].Kind := AKind;
  Frags[Result].R := AR;
  Frags[Result].Scale := 100;
  Frags[Result].Color := clNone;
  Frags[Result].Back := clNone;
end;

{ Lays out styled runs with word wrap in [AX, AX + AWidth), starting at AY;
  returns the y below the last line. Line breaks in the text are kept. }
function TChatLayout.Runs(const ARuns: TMDRuns; AX, AWidth, AY: integer;
  AExtraStyle: TFontStyles; AScale: integer; AColor: TColor): integer;
var
  x, y, lineH, lineStart, r, i, n, j, w, fh, extra, k: integer;
  run: TMDRun;
  style: TFontStyles;
  mono: boolean;
  token, part: string;
  isSpace: boolean;
  lastRun: integer;

  procedure EndLine;
  var
    m: integer;
  begin
    // bottom-align the pieces of the line
    for m := lineStart to High(Frags) do
      if Frags[m].Kind = cfText then
      begin
        fh := Frags[m].R.Bottom - Frags[m].R.Top;
        Frags[m].R.Top := y + lineH - fh;
        Frags[m].R.Bottom := y + lineH;
      end;
    Inc(y, lineH);
    x := 0;
    lineH := LineHeight(False, AScale);
    lineStart := Length(Frags);
    lastRun := -1;
  end;

  // AWidthPx is the text width; inline code adds its padding once per box
  procedure Emit(const AText: string; AWidthPx: integer);
  var
    m: integer;
  begin
    fh := FCanvas.TextHeight('Ag');
    m := High(Frags);
    // extend the previous piece when it is the same run on the same line
    if (lastRun = r) and (m >= lineStart) and (Frags[m].Kind = cfText) then
    begin
      Frags[m].Text := Frags[m].Text + AText;
      Inc(Frags[m].R.Right, AWidthPx);
    end
    else
    begin
      Inc(AWidthPx, extra);
      m := AddFrag(cfText, Rect(AX + x, y, AX + x + AWidthPx, y + fh));
      Frags[m].Text := AText;
      Frags[m].Mono := mono;
      Frags[m].Style := style;
      Frags[m].Scale := AScale;
      Frags[m].Color := AColor;
      Frags[m].Url := run.Url;
      if misLink in run.Style then
        Frags[m].Color := LinkColor;
      if misCode in run.Style then
        Frags[m].Back := Blend(clWindow, clWindowText, 9);
    end;
    Inc(x, AWidthPx);
    if fh > lineH then
      lineH := fh;
    lastRun := r;
  end;

begin
  x := 0;
  y := AY;
  lineStart := Length(Frags);
  lastRun := -1;
  lineH := LineHeight(False, AScale);
  if Length(ARuns) = 0 then
    Exit(AY);

  for r := 0 to High(ARuns) do
  begin
    run := ARuns[r];
    style := AExtraStyle;
    if misBold in run.Style then
      Include(style, fsBold);
    if misItalic in run.Style then
      Include(style, fsItalic);
    if misStrike in run.Style then
      Include(style, fsStrikeOut);
    if misLink in run.Style then
      Include(style, fsUnderline);
    mono := misCode in run.Style;
    UseFont(mono, style, AScale);
    extra := 0;

    if mono then
    begin
      // inline code is one padded box per line piece
      extra := 2 * INLINE_CODE_PAD;
    end;

    n := Length(run.Text);
    i := 1;
    while i <= n do
    begin
      if run.Text[i] = #10 then
      begin
        EndLine;
        UseFont(mono, style, AScale);
        Inc(i);
        Continue;
      end;
      j := i;
      isSpace := run.Text[i] = ' ';
      while (j <= n) and (run.Text[j] <> #10) and ((run.Text[j] = ' ') = isSpace) do
        Inc(j);
      token := Copy(run.Text, i, j - i);
      i := j;
      UseFont(mono, style, AScale);
      if isSpace then
      begin
        if x = 0 then
          Continue;  // no spaces at the start of a line
        w := FCanvas.TextWidth(token);
        if x + w > AWidth then
        begin
          EndLine;
          Continue;
        end;
        Emit(token, w);
        Continue;
      end;

      w := FCanvas.TextWidth(token) + extra;
      if (x + w > AWidth) and (x > 0) then
      begin
        EndLine;
        UseFont(mono, style, AScale);
      end;
      // a word wider than the line is broken by characters (UTF-8 safe)
      while w > AWidth do
      begin
        k := 1;
        part := '';
        while k <= Length(token) do
        begin
          j := k + 1;
          while (j <= Length(token)) and ((Ord(token[j]) and $C0) = $80) do
            Inc(j);
          if (part <> '') and (FCanvas.TextWidth(part + Copy(token, k, j - k)) + extra > AWidth - x) then
            Break;
          part := part + Copy(token, k, j - k);
          k := j;
        end;
        Emit(part, FCanvas.TextWidth(part));
        Delete(token, 1, Length(part));
        EndLine;
        UseFont(mono, style, AScale);
        w := FCanvas.TextWidth(token) + extra;
      end;
      if token <> '' then
        Emit(token, w - extra);
    end;
  end;
  if x > 0 then
    EndLine
  else if Length(Frags) > lineStart then
    EndLine;
  Result := y;
end;

{ Monospace text in a shaded box, coloured by the SynEdit highlighter of the
  fence language when there is one; long lines wrap by characters. }
function TChatLayout.CodeBlock(const ACode, ALang: string; AX, AWidth, AY: integer): integer;
var
  tokens: TAICodeLines;
  plain: TStringList;
  i, t, y, x, lineH, box, m, k, j, avail, w: integer;
  rest, part, lang: string;
  color: TColor;
  style: TFontStyles;
begin
  box := AddFrag(cfBox, Rect(AX, AY, AX + AWidth, AY));
  Frags[box].Back := Blend(clWindow, clWindowText, 7);
  lineH := LineHeight(True, 100);
  avail := AWidth - 2 * PAD;
  y := AY + PAD;

  if not AIHighlightCode(ACode, ALang, tokens) then
  begin
    // no highlighter: one plain token per line
    plain := TStringList.Create;
    try
      plain.Text := ACode;
      if plain.Count = 0 then
        plain.Add('');
      SetLength(tokens, plain.Count);
      for i := 0 to plain.Count - 1 do
      begin
        SetLength(tokens[i], 1);
        tokens[i][0].Text := plain[i];
        tokens[i][0].Color := clNone;
        tokens[i][0].Style := [];
      end;
    finally
      plain.Free;
    end;
  end;

  // language label in the top right corner
  // the name as written in the fence ("json", not the highlighter's "js")
  lang := LowerCase(Trim(ALang));
  if Pos(' ', lang) > 0 then
    SetLength(lang, Pos(' ', lang) - 1);
  if lang <> '' then
  begin
    UseFont(False, [], 85);
    w := FCanvas.TextWidth(lang);
    m := AddFrag(cfText, Rect(AX + AWidth - PAD - w, AY + 2, AX + AWidth - PAD,
      AY + 2 + FCanvas.TextHeight('Ag')));
    Frags[m].Text := lang;
    Frags[m].Scale := 85;
    Frags[m].Color := Blend(clWindowText, clWindow, 50);
  end;

  for i := 0 to High(tokens) do
  begin
    x := 0;
    for t := 0 to High(tokens[i]) do
    begin
      rest := tokens[i][t].Text;
      color := tokens[i][t].Color;
      style := tokens[i][t].Style;
      UseFont(True, style, 100);
      while rest <> '' do
      begin
        // the longest prefix that still fits on this line
        part := '';
        k := 1;
        while k <= Length(rest) do
        begin
          j := k + 1;
          while (j <= Length(rest)) and ((Ord(rest[j]) and $C0) = $80) do
            Inc(j);
          if FCanvas.TextWidth(part + Copy(rest, k, j - k)) > avail - x then
            Break;
          part := part + Copy(rest, k, j - k);
          k := j;
        end;
        if (part = '') and (x > 0) then
        begin
          // nothing fits behind the previous token: next line
          Inc(y, lineH);
          x := 0;
          Continue;
        end;
        if part = '' then
        begin
          // narrower than one character: take it anyway
          j := 2;
          while (j <= Length(rest)) and ((Ord(rest[j]) and $C0) = $80) do
            Inc(j);
          part := Copy(rest, 1, j - 1);
        end;
        w := FCanvas.TextWidth(part);
        m := AddFrag(cfText, Rect(AX + PAD + x, y, AX + PAD + x + w, y + lineH));
        Frags[m].Text := part;
        Frags[m].Mono := True;
        Frags[m].Style := style;
        Frags[m].Color := color;
        Inc(x, w);
        Delete(rest, 1, Length(part));
        if rest <> '' then
        begin
          Inc(y, lineH);
          x := 0;
        end;
      end;
    end;
    Inc(y, lineH);
  end;
  Inc(y, PAD);
  Frags[box].R.Bottom := y;
  Result := y;
end;

function TChatLayout.Table(const ABlock: TMDBlock; AX, AWidth, AY: integer): integer;
var
  cols, r, c, total, y, rowH, cellBottom, x, m: integer;
  widths: array of integer;
  grid: TColor;
  extra: TFontStyles;
begin
  cols := 0;
  for r := 0 to High(ABlock.Rows) do
    if Length(ABlock.Rows[r]) > cols then
      cols := Length(ABlock.Rows[r]);
  if cols = 0 then
    Exit(AY);
  // natural widths, then shrink to fit
  SetLength(widths, cols);
  for c := 0 to cols - 1 do
    widths[c] := 40;
  for r := 0 to High(ABlock.Rows) do
    for c := 0 to High(ABlock.Rows[r]) do
    begin
      if r = 0 then
        UseFont(False, [fsBold], 100)
      else
        UseFont(False, [], 100);
      x := FCanvas.TextWidth(MDRunsText(ABlock.Rows[r][c])) + 2 * CELL_PAD + 2;
      if x > widths[c] then
        widths[c] := x;
    end;
  total := 0;
  for c := 0 to cols - 1 do
    Inc(total, widths[c]);
  if total > AWidth then
    for c := 0 to cols - 1 do
      widths[c] := widths[c] * AWidth div total;

  total := 0;
  for c := 0 to cols - 1 do
    Inc(total, widths[c]);
  grid := Blend(clWindow, clWindowText, 22);
  y := AY;
  m := AddFrag(cfLine, Rect(AX, y, AX + total, y));
  Frags[m].Color := grid;
  for r := 0 to High(ABlock.Rows) do
  begin
    if r = 0 then
    begin
      extra := [fsBold];
      m := AddFrag(cfBox, Rect(AX, y, AX + total, y));
      Frags[m].Back := Blend(clWindow, clWindowText, 6);
    end
    else
      extra := [];
    rowH := LineHeight(False, 100);
    x := AX;
    for c := 0 to cols - 1 do
    begin
      if c <= High(ABlock.Rows[r]) then
      begin
        cellBottom := Runs(ABlock.Rows[r][c], x + CELL_PAD, widths[c] - 2 * CELL_PAD,
          y + CELL_PAD, extra, 100, clNone);
        if cellBottom - y - CELL_PAD > rowH then
          rowH := cellBottom - y - CELL_PAD;
      end;
      Inc(x, widths[c]);
    end;
    Inc(rowH, 2 * CELL_PAD);
    if r = 0 then
      Frags[m].R.Bottom := y + rowH;
    // vertical lines of this row
    x := AX;
    for c := 0 to cols do
    begin
      m := AddFrag(cfLine, Rect(x, y, x, y + rowH));
      Frags[m].Color := grid;
      if c < cols then
        Inc(x, widths[c]);
    end;
    Inc(y, rowH);
    m := AddFrag(cfLine, Rect(AX, y, AX + total, y));
    Frags[m].Color := grid;
  end;
  Result := y + 1;
end;

function TChatLayout.Blocks(const ABlocks: TMDBlocks; AWidth: integer): integer;
var
  i, y, x0, m, markerW, top: integer;
  b: TMDBlock;
  prevList, isList: boolean;
  quoteColor: TColor;
begin
  y := 0;
  prevList := False;
  quoteColor := Blend(clWindowText, clWindow, 35);
  for i := 0 to High(ABlocks) do
  begin
    b := ABlocks[i];
    isList := b.Kind in [mbBullet, mbNumbered];
    if i > 0 then
    begin
      if isList and prevList then
        Inc(y, LIST_GAP)
      else if b.Kind = mbHeading then
        Inc(y, GAP + 4)
      else
        Inc(y, GAP);
    end;
    prevList := isList;

    case b.Kind of
      mbParagraph:
        y := Runs(b.Runs, 0, AWidth, y, [], 100, clNone);
      mbHeading:
        y := Runs(b.Runs, 0, AWidth, y, [fsBold], HEADING_SCALE[b.Level], clNone);
      mbBullet, mbNumbered:
      begin
        x0 := b.Level * LIST_INDENT;
        UseFont(False, [], 100);
        if b.Kind = mbBullet then
        begin
          markerW := FCanvas.TextWidth('•') + 8;
          m := AddFrag(cfText, Rect(x0, y, x0 + markerW, y + FCanvas.TextHeight('Ag')));
          Frags[m].Text := '•';
        end
        else
        begin
          markerW := FCanvas.TextWidth('00.') + 6;
          if FCanvas.TextWidth(b.Marker) + 6 > markerW then
            markerW := FCanvas.TextWidth(b.Marker) + 6;
          m := AddFrag(cfText, Rect(x0, y, x0 + markerW, y + FCanvas.TextHeight('Ag')));
          Frags[m].Text := b.Marker;
        end;
        Frags[m].Color := Blend(clWindowText, clWindow, 30);
        y := Runs(b.Runs, x0 + markerW, AWidth - x0 - markerW, y, [], 100, clNone);
      end;
      mbQuote:
      begin
        top := y;
        y := Runs(b.Runs, QUOTE_INDENT, AWidth - QUOTE_INDENT, y, [], 100, quoteColor);
        m := AddFrag(cfBar, Rect(2, top, 5, y));
        Frags[m].Back := Blend(clWindow, clWindowText, 25);
      end;
      mbCode:
        y := CodeBlock(b.Code, b.Lang, 0, AWidth, y);
      mbRule:
      begin
        m := AddFrag(cfLine, Rect(0, y + 4, AWidth, y + 4));
        Frags[m].Color := Blend(clWindow, clWindowText, 25);
        Inc(y, 9);
      end;
      mbTable:
        y := Table(b, 0, AWidth, y);
    end;
  end;
  Result := y;
end;

{ TAIChatItem }

function TAIChatItem.Collapsible: boolean;
begin
  Result := Kind in [cikThinking, cikTool, cikPlan];
end;

function TAIChatItem.IsMarkdown: boolean;
begin
  Result := Kind in [cikAssistant, cikThinking];
end;

function TAIChatItem.HeaderText: string;
const
  STATUS_MARK: array[TAIToolStatus] of string = ('…', '▶', '✓', '✗');
var
  fold: string;
begin
  fold := '';
  if Collapsible then
    if Collapsed then
      fold := '▸ '
    else
      fold := '▾ ';
  case Kind of
    cikThinking: Result := fold + 'Thinking';
    cikTool:
    begin
      Result := fold + STATUS_MARK[Status] + ' ' + Title;
      if (Path <> '') and (Pos(ExtractFileName(Path), Title) = 0) then
        Result := Result + '  ' + ExtractFileName(Path);
    end;
    cikPlan: Result := fold + 'Plan';
    cikPermission: Result := 'Permission: ' + Title;
    cikError: Result := 'Error';
    else
      Result := '';
  end;
end;

{ TAIChatView }

constructor TAIChatView.Create(AOwner: TComponent);
var
  mi: TMenuItem;
begin
  inherited Create(AOwner);
  FItems := TObjectList.Create(True);
  BorderStyle := bsNone;
  HorzScrollBar.Visible := False;
  VertScrollBar.Tracking := True;
  VertScrollBar.Increment := 24;
  Color := clWindow;

  FMeasure := TBitmap.Create;
  FMeasure.SetSize(4, 4);
  FCodeFont := TFont.Create;
  FCodeFont.Name := {$IFDEF WINDOWS}'Consolas'{$ELSE}{$IFDEF DARWIN}'Menlo'{$ELSE}'Monospace'{$ENDIF}{$ENDIF};
  FCodeFont.Size := 9;

  FBox := TPaintBox.Create(Self);
  FBox.Parent := Self;
  FBox.Align := alTop;
  FBox.Height := 10;
  FBox.OnPaint := @BoxPaint;
  FBox.OnResize := @BoxResize;
  FBox.OnMouseUp := @BoxMouseUp;
  FBox.OnMouseMove := @BoxMouseMove;

  FPopup := TPopupMenu.Create(Self);
  FPopup.OnPopup := @PopupPopup;
  mi := TMenuItem.Create(FPopup);
  mi.Caption := 'Copy message';
  mi.OnClick := @PopupCopyMessage;
  FPopup.Items.Add(mi);
  mi := TMenuItem.Create(FPopup);
  mi.Caption := 'Copy code blocks';
  mi.OnClick := @PopupCopyCode;
  FPopup.Items.Add(mi);
  mi := TMenuItem.Create(FPopup);
  mi.Caption := 'Copy conversation';
  mi.OnClick := @PopupCopyAll;
  FPopup.Items.Add(mi);
  FBox.PopupMenu := FPopup;
end;

destructor TAIChatView.Destroy;
begin
  FItems.Free;
  FCodeFont.Free;
  FMeasure.Free;
  inherited Destroy;
end;

function TAIChatView.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TAIChatView.GetItem(AIndex: integer): TAIChatItem;
begin
  Result := TAIChatItem(FItems[AIndex]);
end;

function TAIChatView.LastItem: TAIChatItem;
begin
  if FItems.Count = 0 then
    Result := nil
  else
    Result := TAIChatItem(FItems.Last);
end;

function TAIChatView.BubbleWidth: integer;
begin
  Result := FBox.Width - 2 * MARGIN;
  if Result < 60 then
    Result := 60;
end;

function TAIChatView.TextHeight(const AText: string; AFont: TFont; AWidth: integer): integer;
var
  r: TRect;
begin
  if AText = '' then
    Exit(0);
  FMeasure.Canvas.Font.Assign(AFont);
  r := Rect(0, 0, AWidth, 0);
  DrawText(FMeasure.Canvas.Handle, PChar(AText), Length(AText), r, DRAW_FLAGS or DT_CALCRECT);
  Result := r.Bottom - r.Top;
end;

procedure TAIChatView.MeasureItem(AItem: TAIChatItem);
var
  inner, h, contentH: integer;
  boldFont: TFont;
  layout: TChatLayout;
  plain: TMDRuns;
begin
  inner := BubbleWidth - 2 * PAD;
  boldFont := TFont.Create;
  try
    boldFont.Assign(Font);
    boldFont.Style := [fsBold];
    AItem.HeaderHeight := 0;
    if AItem.HeaderText <> '' then
      AItem.HeaderHeight := TextHeight(AItem.HeaderText, boldFont, inner);
  finally
    boldFont.Free;
  end;

  SetLength(AItem.Frags, 0);
  contentH := 0;
  if not (AItem.Collapsible and AItem.Collapsed) and (Trim(AItem.Text) <> '') then
  begin
    layout := TChatLayout.Create(FMeasure.Canvas, Font, FCodeFont);
    try
      if AItem.IsMarkdown then
        contentH := layout.Blocks(MDParse(AItem.Text), inner)
      else
      begin
        SetLength(plain, 1);
        plain[0].Text := TrimRight(AItem.Text);
        plain[0].Style := [];
        contentH := layout.Runs(plain, 0, inner, 0, [], 100, clNone);
      end;
      AItem.Frags := layout.Frags;
    finally
      layout.Free;
    end;
  end;

  AItem.ContentY := PAD + AItem.HeaderHeight;
  if (AItem.HeaderHeight > 0) and (contentH > 0) then
    Inc(AItem.ContentY, GAP);

  AItem.ButtonsHeight := 0;
  if (AItem.Kind = cikPermission) and (Length(AItem.Buttons) > 0) then
    AItem.ButtonsHeight := BUTTON_HEIGHT + GAP;

  h := AItem.ContentY + contentH + PAD;
  if AItem.ButtonsHeight > 0 then
    Inc(h, AItem.ButtonsHeight);
  AItem.Height := h;
  AItem.Valid := True;
end;

procedure TAIChatView.Relayout(AFromIndex: integer);
var
  i, y: integer;
  item: TAIChatItem;
  stick: boolean;
begin
  if FUpdating > 0 then
  begin
    FDirty := True;
    Exit;
  end;
  FDirty := False;
  stick := AtBottom;
  if FLayoutWidth <> FBox.Width then
  begin
    FLayoutWidth := FBox.Width;
    AFromIndex := 0;
    for i := 0 to FItems.Count - 1 do
      Items[i].Valid := False;
  end;
  if AFromIndex < 0 then
    AFromIndex := 0;
  if AFromIndex = 0 then
    y := MARGIN
  else
    y := Items[AFromIndex - 1].Top + Items[AFromIndex - 1].Height + GAP;
  for i := AFromIndex to FItems.Count - 1 do
  begin
    item := Items[i];
    if not item.Valid then
      MeasureItem(item);
    item.Top := y;
    Inc(y, item.Height + GAP);
  end;
  if FBox.Height <> y + MARGIN then
    FBox.Height := y + MARGIN;
  FBox.Invalidate;
  if stick then
    ScrollToBottom;
end;

function TAIChatView.AtBottom: boolean;
begin
  Result := VertScrollBar.Position + ClientHeight >= FBox.Height - 40;
end;

procedure TAIChatView.ScrollToBottom;
begin
  VertScrollBar.Position := FBox.Height;
end;

function TAIChatView.Add(AKind: TAIChatItemKind; const AText: string): TAIChatItem;
begin
  Result := TAIChatItem.Create;
  Result.Kind := AKind;
  Result.Text := AText;
  Result.Collapsed := AKind in [cikThinking, cikTool];
  FItems.Add(Result);
  Relayout(FItems.Count - 1);
  if FUpdating = 0 then
    ScrollToBottom;
end;

procedure TAIChatView.AppendText(AItem: TAIChatItem; const AText: string);
begin
  AItem.Text := AItem.Text + AText;
  Changed(AItem);
end;

procedure TAIChatView.Changed(AItem: TAIChatItem);
begin
  AItem.Valid := False;
  Relayout(FItems.IndexOf(AItem));
end;

function TAIChatView.FindItem(AKind: TAIChatItemKind; const AKey: string): TAIChatItem;
var
  i: integer;
begin
  for i := FItems.Count - 1 downto 0 do
    if (Items[i].Kind = AKind) and (Items[i].Key = AKey) then
      Exit(Items[i]);
  Result := nil;
end;

procedure TAIChatView.Clear;
begin
  FItems.Clear;
  Relayout(0);
end;

procedure TAIChatView.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TAIChatView.EndUpdate;
begin
  Dec(FUpdating);
  if (FUpdating = 0) and FDirty then
    Relayout(0);
end;

function TAIChatView.AsText: string;
const
  PREFIX: array[TAIChatItemKind] of string =
    ('You', 'Assistant', 'Thinking', 'Tool', 'Plan', 'Permission', 'Error', 'Info');
var
  i: integer;
  item: TAIChatItem;
begin
  Result := '';
  for i := 0 to FItems.Count - 1 do
  begin
    item := Items[i];
    Result := Result + '[' + PREFIX[item.Kind];
    if item.Title <> '' then
      Result := Result + ': ' + item.Title;
    Result := Result + ']' + LineEnding + item.Text + LineEnding + LineEnding;
  end;
end;

function TAIChatView.ItemAt(Y: integer): TAIChatItem;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    if (Y >= Items[i].Top) and (Y < Items[i].Top + Items[i].Height) then
      Exit(Items[i]);
  Result := nil;
end;

function TAIChatView.LinkAt(AItem: TAIChatItem; X, Y: integer): string;
var
  i: integer;
  p: TPoint;
begin
  Result := '';
  if not Assigned(AItem) then
    Exit;
  p := Point(X - MARGIN - PAD, Y - AItem.Top - AItem.ContentY);
  for i := 0 to High(AItem.Frags) do
    if (AItem.Frags[i].Url <> '') and PtInRect(AItem.Frags[i].R, p) then
      Exit(AItem.Frags[i].Url);
end;

procedure TAIChatView.BoxResize(Sender: TObject);
begin
  if FBox.Width <> FLayoutWidth then
    Relayout(0);
end;

procedure TAIChatView.BoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  item: TAIChatItem;
  i: integer;
  hand: boolean;
begin
  hand := False;
  item := ItemAt(Y);
  if Assigned(item) then
  begin
    if item.Collapsible and (Y < item.Top + PAD + item.HeaderHeight) then
      hand := True;
    if item.Answer = '' then
      for i := 0 to High(item.Buttons) do
        if PtInRect(item.Buttons[i].Rect, Point(X, Y)) then
          hand := True;
    if LinkAt(item, X, Y) <> '' then
      hand := True;
  end;
  if hand then
    FBox.Cursor := crHandPoint
  else
    FBox.Cursor := crDefault;
end;

procedure TAIChatView.BoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  item: TAIChatItem;
  i: integer;
  url: string;
begin
  if Button <> mbLeft then
    Exit;
  item := ItemAt(Y);
  if not Assigned(item) then
    Exit;
  if (item.Kind = cikPermission) and (item.Answer = '') then
    for i := 0 to High(item.Buttons) do
      if PtInRect(item.Buttons[i].Rect, Point(X, Y)) then
      begin
        if Assigned(FOnButtonClick) then
          FOnButtonClick(Self, item, item.Buttons[i].Id);
        Exit;
      end;
  url := LinkAt(item, X, Y);
  if url <> '' then
  begin
    OpenURL(url);
    Exit;
  end;
  if item.Collapsible and (Y < item.Top + PAD + item.HeaderHeight) then
  begin
    item.Collapsed := not item.Collapsed;
    Changed(item);
  end;
end;

procedure TAIChatView.BoxPaint(Sender: TObject);
var
  i: integer;
  clip: TRect;
  item: TAIChatItem;
begin
  clip := FBox.Canvas.ClipRect;
  FBox.Canvas.Brush.Color := Color;
  FBox.Canvas.FillRect(clip);
  for i := 0 to FItems.Count - 1 do
  begin
    item := Items[i];
    if item.Top > clip.Bottom then
      Break;
    if item.Top + item.Height < clip.Top then
      Continue;
    PaintItem(FBox.Canvas, item);
  end;
end;

procedure TAIChatView.PaintItem(ACanvas: TCanvas; AItem: TAIChatItem);
var
  bubble, r: TRect;
  back, fore, accent: TColor;
  i, y, x, inner, w, ox, oy, baseSize: integer;
  s: string;
  f: TAIChatFrag;
begin
  fore := clWindowText;
  accent := clHighlight;
  case AItem.Kind of
    cikUser: back := Blend(clWindow, clHighlight, 18);
    cikAssistant: back := clWindow;
    cikError:
    begin
      back := Blend(clWindow, clRed, 15);
      accent := clRed;
    end;
    cikPermission:
    begin
      back := Blend(clWindow, $0080FF, 15);
      accent := $0060C0;
    end;
    cikInfo:
    begin
      back := clWindow;
      fore := clGrayText;
    end;
    else
    begin
      back := Blend(clWindow, clWindowText, 6);
      fore := Blend(clWindowText, clWindow, 25);
    end;
  end;

  bubble := Rect(MARGIN, AItem.Top, MARGIN + BubbleWidth, AItem.Top + AItem.Height);
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := back;
  if AItem.Kind in [cikAssistant, cikInfo] then
    ACanvas.Pen.Color := back
  else
    ACanvas.Pen.Color := Blend(back, clWindowText, 12);
  ACanvas.RoundRect(bubble, 8, 8);

  inner := BubbleWidth - 2 * PAD;
  y := bubble.Top + PAD;
  x := bubble.Left + PAD;

  if AItem.HeaderHeight > 0 then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Assign(Font);
    ACanvas.Font.Style := [fsBold];
    if AItem.Kind = cikTool then
      case AItem.Status of
        atsCompleted: ACanvas.Font.Color := Blend(clWindowText, clGreen, 60);
        atsFailed: ACanvas.Font.Color := clRed;
        else
          ACanvas.Font.Color := fore;
      end
    else if AItem.Kind in [cikError, cikPermission] then
      ACanvas.Font.Color := accent
    else
      ACanvas.Font.Color := fore;
    s := AItem.HeaderText;
    r := Rect(x, y, x + inner, y + AItem.HeaderHeight);
    DrawText(ACanvas.Handle, PChar(s), Length(s), r, DRAW_FLAGS);
  end;

  // content fragments
  ox := x;
  oy := AItem.Top + AItem.ContentY;
  baseSize := Font.Size;
  if baseSize = 0 then
    baseSize := 9;
  for i := 0 to High(AItem.Frags) do
  begin
    f := AItem.Frags[i];
    r := f.R;
    OffsetRect(r, ox, oy);
    case f.Kind of
      cfBox:
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := f.Back;
        ACanvas.Pen.Color := f.Back;
        ACanvas.RoundRect(r, 6, 6);
      end;
      cfBar:
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := f.Back;
        ACanvas.FillRect(r);
      end;
      cfLine:
      begin
        ACanvas.Pen.Color := f.Color;
        ACanvas.Line(r.Left, r.Top, r.Right, r.Bottom);
      end;
      cfText:
      begin
        if f.Back <> clNone then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := f.Back;
          ACanvas.Pen.Color := f.Back;
          ACanvas.RoundRect(r, 4, 4);
        end;
        ACanvas.Brush.Style := bsClear;
        if f.Mono then
          ACanvas.Font.Assign(FCodeFont)
        else
          ACanvas.Font.Assign(Font);
        ACanvas.Font.Style := f.Style;
        if f.Scale <> 100 then
          if f.Mono then
            ACanvas.Font.Size := FCodeFont.Size * f.Scale div 100
          else
            ACanvas.Font.Size := baseSize * f.Scale div 100;
        if f.Color = clNone then
          ACanvas.Font.Color := fore
        else
          ACanvas.Font.Color := f.Color;
        if f.Back <> clNone then
          ACanvas.TextOut(r.Left + INLINE_CODE_PAD, r.Top, f.Text)
        else
          ACanvas.TextOut(r.Left, r.Top, f.Text);
      end;
    end;
  end;

  if AItem.ButtonsHeight > 0 then
  begin
    y := AItem.Top + AItem.Height - PAD - BUTTON_HEIGHT;
    ACanvas.Font.Assign(Font);
    if AItem.Answer <> '' then
    begin
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Color := clGrayText;
      ACanvas.TextOut(x, y + 4, 'Answered: ' + AItem.Answer);
      for i := 0 to High(AItem.Buttons) do
        AItem.Buttons[i].Rect := Rect(0, 0, 0, 0);
    end
    else
      for i := 0 to High(AItem.Buttons) do
      begin
        w := ACanvas.TextWidth(AItem.Buttons[i].Caption) + 2 * BUTTON_PAD;
        r := Rect(x, y, x + w, y + BUTTON_HEIGHT);
        AItem.Buttons[i].Rect := r;
        ACanvas.Brush.Style := bsSolid;
        if Pos('allow', AItem.Buttons[i].Kind) = 1 then
        begin
          ACanvas.Brush.Color := clHighlight;
          ACanvas.Font.Color := clHighlightText;
        end
        else
        begin
          ACanvas.Brush.Color := clBtnFace;
          ACanvas.Font.Color := clBtnText;
        end;
        ACanvas.Pen.Color := Blend(ACanvas.Brush.Color, clWindowText, 25);
        ACanvas.RoundRect(r, 6, 6);
        ACanvas.Brush.Style := bsClear;
        ACanvas.TextOut(r.Left + BUTTON_PAD,
          r.Top + (BUTTON_HEIGHT - ACanvas.TextHeight('Ag')) div 2, AItem.Buttons[i].Caption);
        x := r.Right + GAP;
      end;
  end;
  ACanvas.Brush.Style := bsSolid;
end;

procedure TAIChatView.PopupPopup(Sender: TObject);
var
  p: TPoint;
begin
  p := FBox.ScreenToClient(Mouse.CursorPos);
  FPopupItem := ItemAt(p.Y);
  FPopup.Items[0].Enabled := Assigned(FPopupItem);
  FPopup.Items[1].Enabled := Assigned(FPopupItem) and (Pos('```', FPopupItem.Text) > 0);
end;

procedure TAIChatView.PopupCopyMessage(Sender: TObject);
begin
  if Assigned(FPopupItem) then
    Clipboard.AsText := FPopupItem.Text;
end;

procedure TAIChatView.PopupCopyCode(Sender: TObject);
var
  blocks: TMDBlocks;
  i: integer;
  s: string;
begin
  if not Assigned(FPopupItem) then
    Exit;
  s := '';
  blocks := MDParse(FPopupItem.Text);
  for i := 0 to High(blocks) do
    if blocks[i].Kind = mbCode then
      s := s + blocks[i].Code + LineEnding;
  Clipboard.AsText := s;
end;

procedure TAIChatView.PopupCopyAll(Sender: TObject);
begin
  Clipboard.AsText := AsText;
end;

end.

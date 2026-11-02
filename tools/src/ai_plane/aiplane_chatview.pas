unit aiplane_chatview;

{
  Conversation view for AI Plane: a scroll box with one owner drawn paint box.

  Every message is a TAIChatItem (user, assistant, thinking, tool call, plan,
  permission, error, info). Assistant and thinking text is markdown
  (aiplane_markdown): headings, lists, quotes, tables, fenced code, and
  inline bold, italic, strikethrough, `code` and links. Other items are
  plain text.

  Images: a local file (absolute, file://, or relative to BaseDir) is drawn
  scaled to the text width and at most MAX_IMAGE_HEIGHT high. Remote
  images are not downloaded: they show as a link-styled placeholder that
  opens the URL. Titles become tooltips.

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
    cfBar,       // filled rectangle (quote bar)
    cfImage      // picture from the image cache (Text = resolved path)
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
    Hint: string;       // tooltip: link or image title
    Selectable: boolean;
    // how this piece joins the previous line when copied: 0 new line,
    // 1 a space (word wrap), 2 nothing (a word or code line broken),
    // 3 one line break (next table row)
    WrapJoin: byte;
  end;

  { A place in the text: item index, fragment index, byte offset. }
  TAIChatSelPos = record
    Item: integer;
    Frag: integer;
    Offset: integer;
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
    FSelAnchor: TAIChatSelPos;
    FSelFocus: TAIChatSelPos;
    FHasSel: boolean;
    FMouseIsDown: boolean;
    FDragged: boolean;
    FDblClicked: boolean;
    FDownPt: TPoint;
    FMenuCopy: TMenuItem;
    FMenuCopyCode: TMenuItem;
    FMenuCopyMessage: TMenuItem;
    FBaseDir: string;
    FImages: TStringList;   // resolved path -> TPicture (nil: failed)
    function ResolveLocalImage(const ASrc: string): string;
    function ImagePicture(const APath: string): TPicture;
    function ImageSize(const ASrc: string; out APath: string; out AW, AH: integer): boolean;
    procedure SetBaseDir(const AValue: string);
    procedure OpenTarget(const ATarget: string);
    procedure ApplyFragFont(ACanvas: TCanvas; const AFrag: TAIChatFrag);
    function FragOffsetAt(const AFrag: TAIChatFrag; AX: integer): integer;
    function PosAt(X, Y: integer; out APos: TAIChatSelPos): boolean;
    function SelRange(AItem, AFrag: integer; out AStart, AEnd: integer): boolean;
    procedure OrderedSelection(out ALo, AHi: TAIChatSelPos);
    procedure BoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BoxDblClick(Sender: TObject);
    procedure PopupCopySelection(Sender: TObject);
    procedure PopupSelectAll(Sender: TObject);
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
    procedure PaintItem(ACanvas: TCanvas; AIndex: integer; AItem: TAIChatItem);
    procedure PopupCopyMessage(Sender: TObject);
    procedure PopupCopyCode(Sender: TObject);
    procedure PopupCopyAll(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Selected text as plain text (word-wrapped lines joined again). }
    function SelectedText: string;
    function HasSelection: boolean;
    procedure SelectAll;
    { Selects from (X1, Y1) to (X2, Y2), paint box coordinates, as a mouse
      drag would. }
    procedure SelectRegion(X1, Y1, X2, Y2: integer);
    procedure ClearSelection;
    procedure CopySelection;
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
    { Directory relative image paths are resolved against. }
    property BaseDir: string read FBaseDir write SetBaseDir;
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
  MAX_IMAGE_HEIGHT = 300;
  MAX_IMAGE_FILE_SIZE = 20 * 1024 * 1024;
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
  TImageSizeFunc = function(const ASrc: string; out APath: string;
    out AW, AH: integer): boolean of object;

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
    ImageSize: TImageSizeFunc;
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
  Frags[Result].Selectable := AKind = cfText;
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
  pendingJoin: byte;
  imgPath, target, tip: string;
  iw, ih, m2: integer;

  // AJoin: how the next line joins this one when copied (see WrapJoin)
  procedure EndLine(AJoin: byte);
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
    pendingJoin := AJoin;
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
      if run.Title <> '' then
        Frags[m].Hint := run.Title;
      Frags[m].WrapJoin := pendingJoin;
      pendingJoin := 0;
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
  pendingJoin := 0;
  lineH := LineHeight(False, AScale);
  if Length(ARuns) = 0 then
    Exit(AY);

  for r := 0 to High(ARuns) do
  begin
    run := ARuns[r];

    if misImage in run.Style then
    begin
      // clicking opens the wrapping link, else the image itself
      target := run.Link;
      if target = '' then
        target := run.Url;
      tip := run.Title;
      if tip = '' then
        tip := run.Text;
      if Assigned(ImageSize) and ImageSize(run.Url, imgPath, iw, ih) then
      begin
        // a picture sits on its own line
        if x > 0 then
          EndLine(0);
        if iw > AWidth then
        begin
          ih := ih * AWidth div iw;
          iw := AWidth;
        end;
        if ih > MAX_IMAGE_HEIGHT then
        begin
          iw := iw * MAX_IMAGE_HEIGHT div ih;
          ih := MAX_IMAGE_HEIGHT;
        end;
        if iw < 1 then
          iw := 1;
        if ih < 1 then
          ih := 1;
        m2 := AddFrag(cfImage, Rect(AX, y, AX + iw, y + ih));
        Frags[m2].Text := imgPath;
        Frags[m2].Url := target;
        Frags[m2].Hint := tip;
        Inc(y, ih + 4);
        x := 0;
        lineH := LineHeight(False, AScale);
        lineStart := Length(Frags);
        lastRun := -1;
        pendingJoin := 0;
      end
      else
      begin
        // not drawable here (remote, missing): a link-styled placeholder
        token := '▣ ' + run.Text;
        if run.Text = '' then
          token := '▣ ' + ExtractFileName(StringReplace(run.Url, '/', PathDelim, [rfReplaceAll]));
        style := AExtraStyle + [fsUnderline];
        mono := False;
        extra := 0;
        run.Url := target;
        UseFont(False, style, AScale);
        w := FCanvas.TextWidth(token);
        if (x + w > AWidth) and (x > 0) then
        begin
          EndLine(1);
          UseFont(False, style, AScale);
        end;
        Emit(token, w);
        Frags[High(Frags)].Color := LinkColor;
        if run.Title <> '' then
          Frags[High(Frags)].Hint := run.Title + LineEnding + run.Url
        else
          Frags[High(Frags)].Hint := run.Url;
        lastRun := -1;
      end;
      Continue;
    end;

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
        EndLine(0);
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
          EndLine(1);
          Continue;
        end;
        Emit(token, w);
        Continue;
      end;

      w := FCanvas.TextWidth(token) + extra;
      if (x + w > AWidth) and (x > 0) then
      begin
        EndLine(1);
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
        EndLine(2);
        UseFont(mono, style, AScale);
        w := FCanvas.TextWidth(token) + extra;
      end;
      if token <> '' then
        Emit(token, w - extra);
    end;
  end;
  if x > 0 then
    EndLine(0)
  else if Length(Frags) > lineStart then
    EndLine(0);
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
  join: byte;
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
    Frags[m].Selectable := False;
    Frags[m].Scale := 85;
    Frags[m].Color := Blend(clWindowText, clWindow, 50);
  end;

  for i := 0 to High(tokens) do
  begin
    x := 0;
    join := 0;
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
          join := 2;
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
        Frags[m].WrapJoin := join;
        join := 0;
        Inc(x, w);
        Delete(rest, 1, Length(part));
        if rest <> '' then
        begin
          Inc(y, lineH);
          x := 0;
          join := 2;
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
  cols, r, c, total, y, rowH, cellBottom, x, m, first: integer;
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
        first := Length(Frags);
        cellBottom := Runs(ABlock.Rows[r][c], x + CELL_PAD, widths[c] - 2 * CELL_PAD,
          y + CELL_PAD, extra, 100, clNone);
        if cellBottom - y - CELL_PAD > rowH then
          rowH := cellBottom - y - CELL_PAD;
        // copied rows are separated by one line break, not a blank line
        if (r > 0) and (c = 0) and (first < Length(Frags)) then
          Frags[first].WrapJoin := 3;
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
          Frags[m].Text := '• ';
        end
        else
        begin
          markerW := FCanvas.TextWidth('00.') + 6;
          if FCanvas.TextWidth(b.Marker) + 6 > markerW then
            markerW := FCanvas.TextWidth(b.Marker) + 6;
          m := AddFrag(cfText, Rect(x0, y, x0 + markerW, y + FCanvas.TextHeight('Ag')));
          Frags[m].Text := b.Marker + ' ';
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
  FImages := TStringList.Create;
  FImages.OwnsObjects := True;
  FImages.Sorted := True;
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
  FBox.OnMouseDown := @BoxMouseDown;
  FBox.OnDblClick := @BoxDblClick;
  TabStop := True;

  FPopup := TPopupMenu.Create(Self);
  FPopup.OnPopup := @PopupPopup;
  FMenuCopy := TMenuItem.Create(FPopup);
  FMenuCopy.Caption := 'Copy';
  FMenuCopy.ShortCut := ShortCut(VK_C, [ssCtrl]);
  FMenuCopy.OnClick := @PopupCopySelection;
  FPopup.Items.Add(FMenuCopy);
  mi := TMenuItem.Create(FPopup);
  mi.Caption := 'Select all';
  mi.ShortCut := ShortCut(VK_A, [ssCtrl]);
  mi.OnClick := @PopupSelectAll;
  FPopup.Items.Add(mi);
  mi := TMenuItem.Create(FPopup);
  mi.Caption := '-';
  FPopup.Items.Add(mi);
  FMenuCopyMessage := TMenuItem.Create(FPopup);
  FMenuCopyMessage.Caption := 'Copy message';
  FMenuCopyMessage.OnClick := @PopupCopyMessage;
  FPopup.Items.Add(FMenuCopyMessage);
  FMenuCopyCode := TMenuItem.Create(FPopup);
  FMenuCopyCode.Caption := 'Copy code blocks';
  FMenuCopyCode.OnClick := @PopupCopyCode;
  FPopup.Items.Add(FMenuCopyCode);
  mi := TMenuItem.Create(FPopup);
  mi.Caption := 'Copy conversation';
  mi.OnClick := @PopupCopyAll;
  FPopup.Items.Add(mi);
  FBox.PopupMenu := FPopup;
end;

destructor TAIChatView.Destroy;
begin
  FItems.Free;
  FImages.Free;
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
    layout.ImageSize := @ImageSize;
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
    FHasSel := False;
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
  FHasSel := False;
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
  hand, onText: boolean;
  pos: TAIChatSelPos;
  p: TPoint;
  tip: string;
begin
  if FMouseIsDown and (ssLeft in Shift) then
  begin
    if not FDragged and ((Abs(X - FDownPt.X) > 3) or (Abs(Y - FDownPt.Y) > 3)) then
      FDragged := True;
    if FDragged then
    begin
      // keep the view moving while dragging past its edges
      if Y < VertScrollBar.Position then
        VertScrollBar.Position := VertScrollBar.Position - 20
      else if Y > VertScrollBar.Position + ClientHeight then
        VertScrollBar.Position := VertScrollBar.Position + 20;
      if PosAt(X, Y, pos) then
      begin
        FSelFocus := pos;
        FHasSel := True;
        FBox.Invalidate;
      end;
    end;
    Exit;
  end;

  hand := False;
  onText := False;
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
    p := Point(X - MARGIN - PAD, Y - item.Top - item.ContentY);
    tip := '';
    for i := 0 to High(item.Frags) do
      if PtInRect(item.Frags[i].R, p) then
      begin
        if item.Frags[i].Selectable then
          onText := True;
        if item.Frags[i].Hint <> '' then
          tip := item.Frags[i].Hint;
      end;
    if tip <> FBox.Hint then
    begin
      Application.CancelHint;
      FBox.Hint := tip;
      FBox.ShowHint := tip <> '';
    end;
  end;
  if hand then
    FBox.Cursor := crHandPoint
  else if onText then
    FBox.Cursor := crIBeam
  else
    FBox.Cursor := crDefault;
end;

procedure TAIChatView.BoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  pos: TAIChatSelPos;
begin
  if Button <> mbLeft then
    Exit;
  if CanFocus then
    SetFocus;
  FMouseIsDown := True;
  FDragged := False;
  FDownPt := Point(X, Y);
  if PosAt(X, Y, pos) then
  begin
    FSelAnchor := pos;
    FSelFocus := pos;
  end;
end;

procedure TAIChatView.BoxDblClick(Sender: TObject);
var
  p: TPoint;
  pos: TAIChatSelPos;
  t: string;
  a, b: integer;
begin
  p := FBox.ScreenToClient(Mouse.CursorPos);
  if not PosAt(p.X, p.Y, pos) or (pos.Frag < 0) then
    Exit;
  t := Items[pos.Item].Frags[pos.Frag].Text;
  a := pos.Offset;
  b := pos.Offset;
  while (a > 0) and not (t[a] in [' ', #9]) do
    Dec(a);
  while (b < Length(t)) and not (t[b + 1] in [' ', #9]) do
    Inc(b);
  if b <= a then
    Exit;
  FSelAnchor := pos;
  FSelAnchor.Offset := a;
  FSelFocus := pos;
  FSelFocus.Offset := b;
  FHasSel := True;
  FDblClicked := True;
  FBox.Invalidate;
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
  FMouseIsDown := False;
  if FDblClicked then
  begin
    // the word selected by the double click stays
    FDblClicked := False;
    Exit;
  end;
  if FDragged then
  begin
    FDragged := False;
    Exit;
  end;
  // a plain click drops the selection, then acts as before
  if FHasSel then
  begin
    FHasSel := False;
    FBox.Invalidate;
  end;
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
    OpenTarget(url);
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
    PaintItem(FBox.Canvas, i, item);
  end;
end;

procedure TAIChatView.PaintItem(ACanvas: TCanvas; AIndex: integer; AItem: TAIChatItem);
var
  bubble, r: TRect;
  back, fore, accent: TColor;
  i, y, x, inner, w, ox, oy, baseSize, selStart, selEnd, selPad: integer;
  s: string;
  f: TAIChatFrag;
  pic: TPicture;
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
      cfImage:
      begin
        pic := ImagePicture(f.Text);
        if Assigned(pic) and Assigned(pic.Graphic) then
          ACanvas.StretchDraw(r, pic.Graphic);
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Color := Blend(clWindow, clWindowText, 18);
        ACanvas.Rectangle(r);
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
        ApplyFragFont(ACanvas, f);
        if FHasSel and SelRange(AIndex, i, selStart, selEnd) then
        begin
          // not "pad": that would hide the PAD constant
          selPad := 0;
          if f.Back <> clNone then
            selPad := INLINE_CODE_PAD;
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := Blend(clWindow, clHighlight, 40);
          ACanvas.FillRect(r.Left + selPad + ACanvas.TextWidth(Copy(f.Text, 1, selStart)), r.Top,
            r.Left + selPad + ACanvas.TextWidth(Copy(f.Text, 1, selEnd)), r.Bottom);
        end;
        ACanvas.Brush.Style := bsClear;
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

function TAIChatView.ResolveLocalImage(const ASrc: string): string;
var
  src, ext: string;
begin
  Result := '';
  src := Trim(ASrc);
  if (src = '') or (Pos('://', src) > 0) and (Copy(LowerCase(src), 1, 7) <> 'file://') then
    Exit;  // remote images are never fetched
  if Copy(LowerCase(src), 1, 5) = 'data:' then
    Exit;
  if Copy(LowerCase(src), 1, 7) = 'file://' then
  begin
    Delete(src, 1, 7);
    // file:///C:/x -> C:/x
    if (Length(src) > 2) and (src[1] = '/') and (src[3] = ':') then
      Delete(src, 1, 1);
  end;
  src := StringReplace(src, '/', PathDelim, [rfReplaceAll]);
  if (ExtractFileDrive(src) = '') and ((src = '') or (src[1] <> PathDelim)) then
  begin
    if FBaseDir = '' then
      Exit;
    src := IncludeTrailingPathDelimiter(FBaseDir) + src;
  end;
  src := ExpandFileName(src);
  ext := LowerCase(ExtractFileExt(src));
  if (ext <> '.png') and (ext <> '.jpg') and (ext <> '.jpeg') and (ext <> '.gif') and
    (ext <> '.bmp') and (ext <> '.ico') then
    Exit;
  if FileExists(src) then
    Result := src;
end;

// Cached picture of a resolved path; nil when it cannot be loaded.
function TAIChatView.ImagePicture(const APath: string): TPicture;
var
  i: integer;
  pic: TPicture;
  info: TSearchRec;
begin
  Result := nil;
  if APath = '' then
    Exit;
  i := FImages.IndexOf(APath);
  if i >= 0 then
    Exit(TPicture(FImages.Objects[i]));
  pic := nil;
  if (FindFirst(APath, faAnyFile, info) = 0) then
  begin
    if info.Size <= MAX_IMAGE_FILE_SIZE then
    begin
      pic := TPicture.Create;
      try
        pic.LoadFromFile(APath);
      except
        FreeAndNil(pic);
      end;
    end;
    FindClose(info);
  end;
  // a failed load is cached too, so it is not retried on every layout
  FImages.AddObject(APath, pic);
  Result := pic;
end;

function TAIChatView.ImageSize(const ASrc: string; out APath: string;
  out AW, AH: integer): boolean;
var
  pic: TPicture;
begin
  AW := 0;
  AH := 0;
  APath := ResolveLocalImage(ASrc);
  pic := ImagePicture(APath);
  Result := Assigned(pic) and (pic.Width > 0) and (pic.Height > 0);
  if Result then
  begin
    AW := pic.Width;
    AH := pic.Height;
  end;
end;

procedure TAIChatView.SetBaseDir(const AValue: string);
begin
  if FBaseDir = AValue then
    Exit;
  FBaseDir := AValue;
  // relative image paths may resolve differently now
  FLayoutWidth := -1;
  Relayout(0);
end;

procedure TAIChatView.OpenTarget(const ATarget: string);
var
  path: string;
begin
  if Pos('://', ATarget) > 0 then
  begin
    if Copy(LowerCase(ATarget), 1, 7) = 'file://' then
    begin
      path := ResolveLocalImage(ATarget);
      if path <> '' then
        OpenDocument(path);
    end
    else
      OpenURL(ATarget);
    Exit;
  end;
  if Copy(LowerCase(ATarget), 1, 7) = 'mailto:' then
  begin
    OpenURL(ATarget);
    Exit;
  end;
  // a local file: relative to BaseDir
  path := StringReplace(ATarget, '/', PathDelim, [rfReplaceAll]);
  if (ExtractFileDrive(path) = '') and (FBaseDir <> '') and
    ((path = '') or (path[1] <> PathDelim)) then
    path := IncludeTrailingPathDelimiter(FBaseDir) + path;
  path := ExpandFileName(path);
  if FileExists(path) or DirectoryExists(path) then
    OpenDocument(path);
end;

procedure TAIChatView.ApplyFragFont(ACanvas: TCanvas; const AFrag: TAIChatFrag);
var
  baseSize: integer;
begin
  if AFrag.Mono then
    ACanvas.Font.Assign(FCodeFont)
  else
    ACanvas.Font.Assign(Font);
  ACanvas.Font.Style := AFrag.Style;
  if AFrag.Scale <> 100 then
  begin
    baseSize := Font.Size;
    if baseSize = 0 then
      baseSize := 9;
    if AFrag.Mono then
      ACanvas.Font.Size := FCodeFont.Size * AFrag.Scale div 100
    else
      ACanvas.Font.Size := baseSize * AFrag.Scale div 100;
  end;
end;

// Byte offset of the character boundary nearest to AX (pixels from the
// fragment's left edge).
function TAIChatView.FragOffsetAt(const AFrag: TAIChatFrag; AX: integer): integer;
var
  k, j, w, prevW: integer;
begin
  ApplyFragFont(FMeasure.Canvas, AFrag);
  if AFrag.Back <> clNone then
    Dec(AX, INLINE_CODE_PAD);
  Result := 0;
  prevW := 0;
  k := 1;
  while k <= Length(AFrag.Text) do
  begin
    j := k + 1;
    while (j <= Length(AFrag.Text)) and ((Ord(AFrag.Text[j]) and $C0) = $80) do
      Inc(j);
    w := FMeasure.Canvas.TextWidth(Copy(AFrag.Text, 1, j - 1));
    if AX < (prevW + w) div 2 then
      Exit;
    Result := j - 1;
    prevW := w;
    k := j;
  end;
end;

// Text position under (X, Y) in paint box coordinates, or the nearest one.
function TAIChatView.PosAt(X, Y: integer; out APos: TAIChatSelPos): boolean;
var
  idx, i, j, first, last, leftJ, rightJ: integer;
  item: TAIChatItem;
  p: TPoint;
  f: TAIChatFrag;
begin
  Result := False;
  APos.Item := -1;
  APos.Frag := -1;
  APos.Offset := 0;
  if FItems.Count = 0 then
    Exit;
  idx := FItems.Count - 1;
  for i := 0 to FItems.Count - 1 do
    if Y < Items[i].Top + Items[i].Height then
    begin
      idx := i;
      Break;
    end;
  // items without text (folded cards): look further down, then up
  i := idx;
  while (i < FItems.Count) do
  begin
    first := -1;
    for j := 0 to High(Items[i].Frags) do
      if Items[i].Frags[j].Selectable then
      begin
        first := j;
        Break;
      end;
    if first >= 0 then
      Break;
    Inc(i);
  end;
  if i >= FItems.Count then
  begin
    i := idx;
    while i >= 0 do
    begin
      first := -1;
      for j := 0 to High(Items[i].Frags) do
        if Items[i].Frags[j].Selectable then
        begin
          first := j;
          Break;
        end;
      if first >= 0 then
        Break;
      Dec(i);
    end;
    if i < 0 then
      Exit;
  end;
  if i <> idx then
    // the pointer is on an item without text: snap to the next one's start
    Y := Items[i].Top - 1;
  idx := i;
  item := Items[idx];
  last := first;
  for j := 0 to High(item.Frags) do
    if item.Frags[j].Selectable then
      last := j;

  APos.Item := idx;
  Result := True;
  p := Point(X - MARGIN - PAD, Y - item.Top - item.ContentY);
  if p.Y < item.Frags[first].R.Top then
  begin
    APos.Frag := first;
    Exit;
  end;

  // pieces on the line under the pointer
  leftJ := -1;
  rightJ := -1;
  for j := 0 to High(item.Frags) do
  begin
    f := item.Frags[j];
    if not f.Selectable or (p.Y < f.R.Top) or (p.Y >= f.R.Bottom) then
      Continue;
    if (p.X >= f.R.Left) and (p.X < f.R.Right) then
    begin
      APos.Frag := j;
      APos.Offset := FragOffsetAt(f, p.X - f.R.Left);
      Exit;
    end;
    if (leftJ < 0) or (f.R.Left < item.Frags[leftJ].R.Left) then
      leftJ := j;
    if (rightJ < 0) or (f.R.Right > item.Frags[rightJ].R.Right) then
      rightJ := j;
  end;
  if leftJ >= 0 then
  begin
    if p.X < item.Frags[leftJ].R.Left then
    begin
      APos.Frag := leftJ;
      APos.Offset := 0;
    end
    else
    begin
      APos.Frag := rightJ;
      APos.Offset := Length(item.Frags[rightJ].Text);
    end;
    Exit;
  end;

  // between lines: the start of the next line, else the very end
  for j := 0 to High(item.Frags) do
    if item.Frags[j].Selectable and (item.Frags[j].R.Top > p.Y) then
    begin
      APos.Frag := j;
      APos.Offset := 0;
      Exit;
    end;
  APos.Frag := last;
  APos.Offset := Length(item.Frags[last].Text);
end;

function ComparePos(const A, B: TAIChatSelPos): integer;
begin
  Result := A.Item - B.Item;
  if Result = 0 then
    Result := A.Frag - B.Frag;
  if Result = 0 then
    Result := A.Offset - B.Offset;
end;

procedure TAIChatView.OrderedSelection(out ALo, AHi: TAIChatSelPos);
begin
  if ComparePos(FSelAnchor, FSelFocus) <= 0 then
  begin
    ALo := FSelAnchor;
    AHi := FSelFocus;
  end
  else
  begin
    ALo := FSelFocus;
    AHi := FSelAnchor;
  end;
end;

// Selected byte range [AStart, AEnd) of one fragment.
function TAIChatView.SelRange(AItem, AFrag: integer; out AStart, AEnd: integer): boolean;
var
  lo, hi: TAIChatSelPos;
  len: integer;
begin
  Result := False;
  AStart := 0;
  AEnd := 0;
  if not FHasSel or (AItem < 0) or (AItem >= FItems.Count) or
    (AFrag < 0) or (AFrag > High(Items[AItem].Frags)) then
    Exit;
  OrderedSelection(lo, hi);
  if (AItem < lo.Item) or ((AItem = lo.Item) and (AFrag < lo.Frag)) then
    Exit;
  if (AItem > hi.Item) or ((AItem = hi.Item) and (AFrag > hi.Frag)) then
    Exit;
  len := Length(Items[AItem].Frags[AFrag].Text);
  AEnd := len;
  if (AItem = lo.Item) and (AFrag = lo.Frag) then
    AStart := lo.Offset;
  if (AItem = hi.Item) and (AFrag = hi.Frag) then
    AEnd := hi.Offset;
  if AStart > len then
    AStart := len;
  if AEnd > len then
    AEnd := len;
  Result := AEnd > AStart;
end;

function TAIChatView.SelectedText: string;
var
  lo, hi: TAIChatSelPos;
  i, j, a, b, prevItem: integer;
  prev, f: TAIChatFrag;
  sep: string;
begin
  Result := '';
  if not FHasSel then
    Exit;
  OrderedSelection(lo, hi);
  prevItem := -1;
  prev := Default(TAIChatFrag);
  for i := lo.Item to hi.Item do
  begin
    if (i < 0) or (i >= FItems.Count) then
      Continue;
    for j := 0 to High(Items[i].Frags) do
    begin
      f := Items[i].Frags[j];
      if not f.Selectable or not SelRange(i, j, a, b) then
        Continue;
      sep := '';
      if prevItem >= 0 then
      begin
        if i <> prevItem then
          sep := LineEnding + LineEnding
        else if f.R.Top >= prev.R.Bottom then
        begin
          // a new line
          case f.WrapJoin of
            1:
              if (Result <> '') and (Result[Length(Result)] <> ' ') then
                sep := ' ';
            2: sep := '';
            3: sep := LineEnding;
            else
              if f.R.Top > prev.R.Bottom + 3 then
                sep := LineEnding + LineEnding  // next block
              else
                sep := LineEnding;
          end;
        end
        else if f.R.Left > prev.R.Right + 4 then
          sep := #9;  // table cell
      end;
      Result := Result + sep + Copy(f.Text, a + 1, b - a);
      prev := f;
      prevItem := i;
    end;
  end;
end;

function TAIChatView.HasSelection: boolean;
begin
  Result := FHasSel and (ComparePos(FSelAnchor, FSelFocus) <> 0);
end;

procedure TAIChatView.SelectAll;
var
  i, j: integer;
  found: boolean;
begin
  found := False;
  for i := 0 to FItems.Count - 1 do
    for j := 0 to High(Items[i].Frags) do
      if Items[i].Frags[j].Selectable then
      begin
        if not found then
        begin
          FSelAnchor.Item := i;
          FSelAnchor.Frag := j;
          FSelAnchor.Offset := 0;
          found := True;
        end;
        FSelFocus.Item := i;
        FSelFocus.Frag := j;
        FSelFocus.Offset := Length(Items[i].Frags[j].Text);
      end;
  FHasSel := found;
  FBox.Invalidate;
end;

procedure TAIChatView.SelectRegion(X1, Y1, X2, Y2: integer);
begin
  FHasSel := PosAt(X1, Y1, FSelAnchor) and PosAt(X2, Y2, FSelFocus);
  FBox.Invalidate;
end;

procedure TAIChatView.ClearSelection;
begin
  if not FHasSel then
    Exit;
  FHasSel := False;
  FBox.Invalidate;
end;

procedure TAIChatView.CopySelection;
begin
  if HasSelection then
    Clipboard.AsText := SelectedText;
end;

procedure TAIChatView.KeyDown(var Key: word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and ((Key = VK_C) or (Key = VK_INSERT)) then
  begin
    CopySelection;
    Key := 0;
  end
  else if (ssCtrl in Shift) and (Key = VK_A) then
  begin
    SelectAll;
    Key := 0;
  end
  else if Key = VK_ESCAPE then
    ClearSelection;
  inherited KeyDown(Key, Shift);
end;

procedure TAIChatView.PopupCopySelection(Sender: TObject);
begin
  CopySelection;
end;

procedure TAIChatView.PopupSelectAll(Sender: TObject);
begin
  SelectAll;
end;

procedure TAIChatView.PopupPopup(Sender: TObject);
var
  p: TPoint;
begin
  p := FBox.ScreenToClient(Mouse.CursorPos);
  FPopupItem := ItemAt(p.Y);
  FMenuCopy.Enabled := HasSelection;
  FMenuCopyMessage.Enabled := Assigned(FPopupItem);
  FMenuCopyCode.Enabled := Assigned(FPopupItem) and (Pos('```', FPopupItem.Text) > 0);
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

unit test_aiplane_markdown;

{ Markdown parser used by the AI Plane chat view (aiplane_markdown). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, aiplane_markdown;

type

  { TAIPlaneInlineTest }

  TAIPlaneInlineTest = class(TTestCase)
  published
    procedure TestPlainText;
    procedure TestBold;
    procedure TestItalicStarAndUnderscore;
    procedure TestBoldItalicNested;
    procedure TestSnakeCaseStaysText;
    procedure TestUnclosedMarkersStayText;
    procedure TestInlineCode;
    procedure TestInlineCodeKeepsMarkers;
    procedure TestDoubleBacktickCode;
    procedure TestStrike;
    procedure TestLink;
    procedure TestBareUrl;
    procedure TestBareUrlTrailingDot;
    procedure TestEscapes;
    procedure TestLoneStarIsText;
    procedure TestImage;
    procedure TestImageWithTitle;
    procedure TestLinkedImage;
    procedure TestImageRelativeAndAngle;
    procedure TestBangWithoutImageIsText;
    procedure TestLinkWithBracketsInText;
    procedure TestLinkTitle;
  end;

  { TAIPlaneBlockTest }

  TAIPlaneBlockTest = class(TTestCase)
  published
    procedure TestEmpty;
    procedure TestParagraphsSplitOnBlankLine;
    procedure TestParagraphKeepsLineBreaks;
    procedure TestHeadings;
    procedure TestHashWithoutSpaceIsText;
    procedure TestBulletList;
    procedure TestNestedList;
    procedure TestNumberedList;
    procedure TestListContinuation;
    procedure TestQuote;
    procedure TestCodeBlock;
    procedure TestTildeFence;
    procedure TestUnclosedFence;
    procedure TestCodeKeepsIndentAndMarkup;
    procedure TestRule;
    procedure TestTable;
    procedure TestTableWithoutSeparatorIsText;
    procedure TestMixedDocument;
  end;

implementation

function RunsDump(const ARuns: TMDRuns): string;
const
  NAMES: array[TMDInlineStyle] of char = ('B', 'I', 'S', 'C', 'L', 'G');
var
  i: integer;
  st: TMDInlineStyle;
  flags: string;
begin
  Result := '';
  for i := 0 to High(ARuns) do
  begin
    flags := '';
    for st := Low(TMDInlineStyle) to High(TMDInlineStyle) do
      if st in ARuns[i].Style then
        flags := flags + NAMES[st];
    if i > 0 then
      Result := Result + '|';
    if flags <> '' then
      Result := Result + flags + ':';
    Result := Result + ARuns[i].Text;
  end;
end;

function Inline(const AText: string): string;
begin
  Result := RunsDump(MDParseInline(AText));
end;

{ TAIPlaneInlineTest }

procedure TAIPlaneInlineTest.TestPlainText;
begin
  AssertEquals('hello world', Inline('hello world'));
  AssertEquals('', Inline(''));
end;

procedure TAIPlaneInlineTest.TestBold;
begin
  AssertEquals('a |B:bold| b', Inline('a **bold** b'));
  AssertEquals('a |B:bold| b', Inline('a __bold__ b'));
end;

procedure TAIPlaneInlineTest.TestItalicStarAndUnderscore;
begin
  AssertEquals('an |I:emph| word', Inline('an *emph* word'));
  AssertEquals('an |I:emph| word', Inline('an _emph_ word'));
end;

procedure TAIPlaneInlineTest.TestBoldItalicNested;
begin
  AssertEquals('B:bold |BI:both|B: end', Inline('**bold *both* end**'));
end;

procedure TAIPlaneInlineTest.TestSnakeCaseStaysText;
begin
  AssertEquals('call my_var_name now', Inline('call my_var_name now'));
  AssertEquals('2*3*4', Inline('2*3*4'));
end;

procedure TAIPlaneInlineTest.TestUnclosedMarkersStayText;
begin
  // while streaming the closing marker has not arrived yet
  AssertEquals('this is **bol', Inline('this is **bol'));
  AssertEquals('an *ita', Inline('an *ita'));
end;

procedure TAIPlaneInlineTest.TestInlineCode;
begin
  AssertEquals('run |C:lazbuild -B| now', Inline('run `lazbuild -B` now'));
end;

procedure TAIPlaneInlineTest.TestInlineCodeKeepsMarkers;
begin
  AssertEquals('C:a **b** _c_', Inline('`a **b** _c_`'));
end;

procedure TAIPlaneInlineTest.TestDoubleBacktickCode;
begin
  AssertEquals('C:a ` b', Inline('`` a ` b ``'));
  AssertEquals('open ` tick', Inline('open ` tick'));
end;

procedure TAIPlaneInlineTest.TestStrike;
begin
  AssertEquals('S:old| new', Inline('~~old~~ new'));
end;

procedure TAIPlaneInlineTest.TestLink;
var
  runs: TMDRuns;
begin
  runs := MDParseInline('see [docs](https://example.com/a) here');
  AssertEquals('see |L:docs| here', RunsDump(runs));
  AssertEquals('https://example.com/a', runs[1].Url);
end;

procedure TAIPlaneInlineTest.TestBareUrl;
var
  runs: TMDRuns;
begin
  runs := MDParseInline('go to https://fastplaz.com/docs now');
  AssertEquals('go to |L:https://fastplaz.com/docs| now', RunsDump(runs));
  AssertEquals('https://fastplaz.com/docs', runs[1].Url);
end;

procedure TAIPlaneInlineTest.TestBareUrlTrailingDot;
begin
  AssertEquals('L:http://x.id/a|.', Inline('http://x.id/a.'));
end;

procedure TAIPlaneInlineTest.TestEscapes;
begin
  AssertEquals('*not italic*', Inline('\*not italic\*'));
  AssertEquals('a\b', Inline('a\b'));
end;

procedure TAIPlaneInlineTest.TestLoneStarIsText;
begin
  AssertEquals('a * b', Inline('a * b'));
end;

procedure TAIPlaneInlineTest.TestImage;
var
  runs: TMDRuns;
begin
  runs := MDParseInline('see ![Kanban Hero](https://x.id/a.jpg) here');
  AssertEquals('see |G:Kanban Hero| here', RunsDump(runs));
  AssertEquals('https://x.id/a.jpg', runs[1].Url);
  AssertEquals('', runs[1].Link);
end;

procedure TAIPlaneInlineTest.TestImageWithTitle;
var
  runs: TMDRuns;
begin
  runs := MDParseInline('![Kanban](https://x.id/a b.jpg "The (hero) image")');
  AssertEquals(1, Length(runs));
  AssertEquals('https://x.id/a', runs[0].Url);
  runs := MDParseInline('![Kanban](https://x.id/a.jpg "The (hero) image")');
  AssertEquals('https://x.id/a.jpg', runs[0].Url);
  AssertEquals('quoted ) does not end the target', 'The (hero) image', runs[0].Title);
end;

procedure TAIPlaneInlineTest.TestLinkedImage;
var
  runs: TMDRuns;
begin
  runs := MDParseInline('[![Kanban Hero](https://x.id/a.jpg "Klik")](https://github.com/fastplaz)');
  AssertEquals('G:Kanban Hero', RunsDump(runs));
  AssertEquals('https://x.id/a.jpg', runs[0].Url);
  AssertEquals('Klik', runs[0].Title);
  AssertEquals('https://github.com/fastplaz', runs[0].Link);
end;

procedure TAIPlaneInlineTest.TestImageRelativeAndAngle;
var
  runs: TMDRuns;
begin
  runs := MDParseInline('![logo](./assets/images/logo.png)');
  AssertEquals('./assets/images/logo.png', runs[0].Url);
  runs := MDParseInline('![logo](<docs/my logo.png>)');
  AssertEquals('docs/my logo.png', runs[0].Url);
end;

procedure TAIPlaneInlineTest.TestBangWithoutImageIsText;
begin
  AssertEquals('Hello! [not a link] !', Inline('Hello! [not a link] !'));
  AssertEquals('an ![unclosed](x', Inline('an ![unclosed](x'));
end;

procedure TAIPlaneInlineTest.TestLinkWithBracketsInText;
var
  runs: TMDRuns;
begin
  runs := MDParseInline('[array [0] item](https://x.id/a)');
  AssertEquals('L:array [0] item', RunsDump(runs));
  AssertEquals('https://x.id/a', runs[0].Url);
end;

procedure TAIPlaneInlineTest.TestLinkTitle;
var
  runs: TMDRuns;
begin
  runs := MDParseInline('[docs](https://x.id "Docs page")');
  AssertEquals('https://x.id', runs[0].Url);
  AssertEquals('Docs page', runs[0].Title);
end;

{ TAIPlaneBlockTest }

procedure TAIPlaneBlockTest.TestEmpty;
begin
  AssertEquals(0, Length(MDParse('')));
  AssertEquals(0, Length(MDParse(LineEnding + '  ' + LineEnding)));
end;

procedure TAIPlaneBlockTest.TestParagraphsSplitOnBlankLine;
var
  b: TMDBlocks;
begin
  b := MDParse('one' + #10 + #10 + 'two');
  AssertEquals(2, Length(b));
  AssertTrue(b[0].Kind = mbParagraph);
  AssertEquals('one', MDRunsText(b[0].Runs));
  AssertEquals('two', MDRunsText(b[1].Runs));
end;

procedure TAIPlaneBlockTest.TestParagraphKeepsLineBreaks;
var
  b: TMDBlocks;
begin
  b := MDParse('line one' + #13#10 + 'line two');
  AssertEquals(1, Length(b));
  AssertEquals('line one' + #10 + 'line two', MDRunsText(b[0].Runs));
end;

procedure TAIPlaneBlockTest.TestHeadings;
var
  b: TMDBlocks;
begin
  b := MDParse('# Title' + #10 + '### Sub **x**');
  AssertEquals(2, Length(b));
  AssertTrue(b[0].Kind = mbHeading);
  AssertEquals(1, b[0].Level);
  AssertEquals('Title', MDRunsText(b[0].Runs));
  AssertEquals(3, b[1].Level);
  AssertEquals('Sub |B:x', RunsDump(b[1].Runs));
end;

procedure TAIPlaneBlockTest.TestHashWithoutSpaceIsText;
var
  b: TMDBlocks;
begin
  b := MDParse('#notaheading');
  AssertTrue(b[0].Kind = mbParagraph);
end;

procedure TAIPlaneBlockTest.TestBulletList;
var
  b: TMDBlocks;
begin
  b := MDParse('- one' + #10 + '* two' + #10 + '+ three');
  AssertEquals(3, Length(b));
  AssertTrue(b[0].Kind = mbBullet);
  AssertTrue(b[2].Kind = mbBullet);
  AssertEquals('three', MDRunsText(b[2].Runs));
  AssertEquals(0, b[1].Level);
end;

procedure TAIPlaneBlockTest.TestNestedList;
var
  b: TMDBlocks;
begin
  b := MDParse('- top' + #10 + '  - inner' + #10 + '    - deeper');
  AssertEquals(3, Length(b));
  AssertEquals(0, b[0].Level);
  AssertEquals(1, b[1].Level);
  AssertEquals(2, b[2].Level);
end;

procedure TAIPlaneBlockTest.TestNumberedList;
var
  b: TMDBlocks;
begin
  b := MDParse('1. first' + #10 + '2) second' + #10 + '10. tenth');
  AssertEquals(3, Length(b));
  AssertTrue(b[0].Kind = mbNumbered);
  AssertEquals('1.', b[0].Marker);
  AssertEquals('2.', b[1].Marker);
  AssertEquals('10.', b[2].Marker);
  AssertEquals('tenth', MDRunsText(b[2].Runs));
end;

procedure TAIPlaneBlockTest.TestListContinuation;
var
  b: TMDBlocks;
begin
  b := MDParse('- item' + #10 + '  more text' + #10 + '- next');
  AssertEquals(2, Length(b));
  AssertEquals('item' + #10 + 'more text', MDRunsText(b[0].Runs));
end;

procedure TAIPlaneBlockTest.TestQuote;
var
  b: TMDBlocks;
begin
  b := MDParse('> quoted' + #10 + '> **line**' + #10 + #10 + 'after');
  AssertEquals(2, Length(b));
  AssertTrue(b[0].Kind = mbQuote);
  AssertEquals('quoted' + #10 + '|B:line', RunsDump(b[0].Runs));
  AssertTrue(b[1].Kind = mbParagraph);
end;

procedure TAIPlaneBlockTest.TestCodeBlock;
var
  b: TMDBlocks;
begin
  b := MDParse('Before' + #10 + '```pascal' + #10 + 'begin' + #10 + 'end.' + #10 +
    '```' + #10 + 'After');
  AssertEquals(3, Length(b));
  AssertTrue(b[1].Kind = mbCode);
  AssertEquals('pascal', b[1].Lang);
  AssertEquals('begin' + #10 + 'end.', b[1].Code);
  AssertEquals('After', MDRunsText(b[2].Runs));
end;

procedure TAIPlaneBlockTest.TestTildeFence;
var
  b: TMDBlocks;
begin
  b := MDParse('~~~' + #10 + 'x := 1;' + #10 + '~~~');
  AssertEquals(1, Length(b));
  AssertEquals('x := 1;', b[0].Code);
end;

procedure TAIPlaneBlockTest.TestUnclosedFence;
var
  b: TMDBlocks;
begin
  b := MDParse('Code:' + #10 + '```' + #10 + 'x := 1;' + #10 + 'y := 2;');
  AssertEquals(2, Length(b));
  AssertTrue('still streaming, shown as code', b[1].Kind = mbCode);
  AssertEquals('x := 1;' + #10 + 'y := 2;', b[1].Code);
end;

procedure TAIPlaneBlockTest.TestCodeKeepsIndentAndMarkup;
var
  b: TMDBlocks;
begin
  b := MDParse('```' + #10 + '  if a then' + #10 + #9 + '# not a heading **x**' +
    #10 + '```');
  AssertEquals('  if a then' + #10 + '    # not a heading **x**', b[0].Code);
end;

procedure TAIPlaneBlockTest.TestRule;
var
  b: TMDBlocks;
begin
  b := MDParse('a' + #10 + #10 + '---' + #10 + '* * *' + #10 + 'b');
  AssertEquals(4, Length(b));
  AssertTrue(b[1].Kind = mbRule);
  AssertTrue(b[2].Kind = mbRule);
end;

procedure TAIPlaneBlockTest.TestTable;
var
  b: TMDBlocks;
begin
  b := MDParse('| Name | Value |' + #10 + '|---|:--:|' + #10 + '| a | **1** |' + #10 +
    '| b \| c | 2 |' + #10 + 'after');
  AssertEquals(2, Length(b));
  AssertTrue(b[0].Kind = mbTable);
  AssertEquals(3, Length(b[0].Rows));
  AssertEquals(2, Length(b[0].Rows[0]));
  AssertEquals('Name', MDRunsText(b[0].Rows[0][0]));
  AssertEquals('B:1', RunsDump(b[0].Rows[1][1]));
  AssertEquals('b | c', MDRunsText(b[0].Rows[2][0]));
  AssertTrue(b[1].Kind = mbParagraph);
end;

procedure TAIPlaneBlockTest.TestTableWithoutSeparatorIsText;
var
  b: TMDBlocks;
begin
  // the separator row has not streamed in yet
  b := MDParse('| Name | Value |');
  AssertEquals(1, Length(b));
  AssertTrue(b[0].Kind = mbParagraph);
end;

procedure TAIPlaneBlockTest.TestMixedDocument;
var
  b: TMDBlocks;
begin
  b := MDParse('## Plan' + #10 + 'Steps:' + #10 + '1. Read `tools_lib`' + #10 +
    '2. Fix' + #10 + #10 + '```' + #10 + 'code' + #10 + '```' + #10 + '> note');
  AssertEquals(6, Length(b));
  AssertTrue(b[0].Kind = mbHeading);
  AssertTrue(b[1].Kind = mbParagraph);
  AssertTrue(b[2].Kind = mbNumbered);
  AssertEquals('Read |C:tools_lib', RunsDump(b[2].Runs));
  AssertTrue(b[3].Kind = mbNumbered);
  AssertTrue(b[4].Kind = mbCode);
  AssertTrue(b[5].Kind = mbQuote);
end;

initialization
  RegisterTest(TAIPlaneInlineTest);
  RegisterTest(TAIPlaneBlockTest);

end.

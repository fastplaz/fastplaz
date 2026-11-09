unit test_html_lib;

{$mode objfpc}{$H+}

interface

uses
  html_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { THTMLLibTest }

  THTMLLibTest = class(TTestCase)
  private
    FUtil: THTMLUtil;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReCaptchaAlwaysV2;
    procedure TestHTMLEncode;
    procedure TestStripTagsSimple;
    procedure TestStripTagsGreaterThanInText;
    procedure TestStripTagsUnbalanced;
    procedure TestStripTagsCustom;
    procedure TestStripTagsCustomEmptyMarkers;
    procedure TestMoreLessShortContentUnchanged;
    procedure TestMoreLessCutsAtWord;
    procedure TestMoreLessSingleLongWord;
    procedure TestMoreLessNoSuffix;
    procedure TestMoreLessStripsTags;
    procedure TestInputEscapesValue;
    procedure TestInputHidden;
    procedure TestInputButtonClosed;
    procedure TestLinkEscapes;
    procedure TestImgEscapes;
    procedure TestSelectNilDataset;
    procedure TestSimpleTags;
    procedure TestSecureEquals;
    procedure TestCsrfSessionKey;
  end;

implementation

procedure THTMLLibTest.SetUp;
begin
  FUtil := THTMLUtil.Create;
end;

procedure THTMLLibTest.TearDown;
begin
  FUtil.Free;
end;

procedure THTMLLibTest.TestHTMLEncode;
begin
  AssertEquals('plain', HTMLEncode('plain'));
  AssertEquals('', HTMLEncode(''));
  AssertEquals('&lt;b&gt;&amp;&quot;&#39;', HTMLEncode('<b>&"'''));
  AssertEquals('a &amp;amp; b', HTMLEncode('a &amp; b'));
end;

procedure THTMLLibTest.TestStripTagsSimple;
begin
  AssertEquals('hello world', StripTags('<p>hello <b>world</b></p>'));
  AssertEquals('x', StripTags('<a href="u" title="t">x</a>'));
  AssertEquals('ab', StripTags('a<!-- c -->b'));
  AssertEquals('', StripTags(''));
  AssertEquals('no tags', StripTags('no tags'));
end;

procedure THTMLLibTest.TestStripTagsGreaterThanInText;
begin
  // used to loop forever
  AssertEquals('a > b x', StripTags('a > b <i>x</i>'));
  AssertEquals('5 > 3 ok', StripTags('5 > 3 ok'));
  AssertEquals('a > b', StripTags('a > b'));
end;

procedure THTMLLibTest.TestStripTagsUnbalanced;
begin
  // a "<" without a closing ">" is ordinary text
  AssertEquals('<abc', StripTags('<abc'));
  AssertEquals('x <abc', StripTags('<b>x</b> <abc'));
  AssertEquals('1 < 2 and 3', StripTags('1 < 2 <i>and</i> 3'));
end;

procedure THTMLLibTest.TestStripTagsCustom;
begin
  AssertEquals('ab', StripTagsCustom('a[x]b', '[', ']'));
  AssertEquals('ab', StripTagsCustom('a{{x}}b', '{{', '}}'));
  AssertEquals('a]b', StripTagsCustom('a]b', '[', ']'));
  AssertEquals('a[b', StripTagsCustom('a[b', '[', ']'));
end;

procedure THTMLLibTest.TestStripTagsCustomEmptyMarkers;
begin
  AssertEquals('a<b>', StripTagsCustom('a<b>', '', '>'));
  AssertEquals('a<b>', StripTagsCustom('a<b>', '<', ''));
end;

procedure THTMLLibTest.TestMoreLessShortContentUnchanged;
begin
  AssertEquals('hello world', MoreLess('hello world', 100));
  AssertEquals('hello', MoreLess('hello', 5));
  AssertEquals('', MoreLess('', 10));
end;

procedure THTMLLibTest.TestMoreLessCutsAtWord;
begin
  AssertEquals('one two ...', MoreLess('one two three four', 9));
  AssertEquals('one two ...', MoreLess('one two three four', 10));
  AssertEquals('one two three ...', MoreLess('one two three four', 13));
end;

procedure THTMLLibTest.TestMoreLessSingleLongWord;
begin
  // used to return just " ..."
  AssertEquals('abcde ...', MoreLess('abcdefghij', 5));
end;

procedure THTMLLibTest.TestMoreLessNoSuffix;
begin
  AssertEquals('one two', MoreLess('one two three', 8, ''));
end;

procedure THTMLLibTest.TestMoreLessStripsTags;
begin
  AssertEquals('one two ...', MoreLess('<p>one <b>two</b> three</p>', 8));
  AssertEquals('one two', MoreLess('<p>one <b>two</b></p>', 100));
end;

procedure THTMLLibTest.TestInputEscapesValue;
var
  s: string;
begin
  s := FUtil.AddInputLTE('name', 'text', 'Name <x>', '" autofocus onfocus="alert(1)',
    'a"b', False);
  AssertTrue('attribute not broken', Pos('onfocus="alert', s) = 0);
  AssertTrue('value escaped', Pos('value="&quot; autofocus onfocus=&quot;alert(1)"', s) > 0);
  AssertTrue('label escaped', Pos('Name &lt;x&gt;', s) > 0);
  AssertTrue('placeholder escaped', Pos('placeholder="a&quot;b"', s) > 0);
  AssertTrue('not required', Pos(' required>', s) = 0);
end;

procedure THTMLLibTest.TestInputHidden;
begin
  AssertEquals('<input type="hidden" id="k" name="k" value="v&amp;w" >',
    FUtil.AddInputLTE('k', 'hidden', '', 'v&w'));
end;

procedure THTMLLibTest.TestInputButtonClosed;
var
  s: string;
begin
  s := FUtil.AddInputLTE('q', 'text', 'Q', '', '', True, 'Go Now');
  AssertTrue('button closed', Pos('>Go Now</button></span>', s) > 0);
  AssertTrue('button id without spaces', Pos('id="btn-GoNow"', s) > 0);
  AssertTrue('required', Pos(' required>', s) > 0);
end;

procedure THTMLLibTest.TestLinkEscapes;
begin
  AssertEquals('<a href="u?a=1&amp;b=2" target=_blank>t &lt;b&gt;</a>',
    FUtil.Link('t <b>', 'u?a=1&b=2', ['target=_blank']));
  AssertEquals('<a href="x">x</a>', FUtil.Link('x', []));
end;

procedure THTMLLibTest.TestImgEscapes;
begin
  AssertEquals('<img  src="a&quot;b">', FUtil.img('a"b'));
end;

procedure THTMLLibTest.TestSelectNilDataset;
begin
  AssertEquals('', FUtil.AddSelectLTE('id', 'label', nil));
end;

procedure THTMLLibTest.TestSimpleTags;
begin
  AssertEquals('<H1>t</H1>', H1('t'));
  AssertEquals('<H2 class="c">t</H2>', H2('t', 'c'));
  AssertEquals('<li>t</li>', li('t'));
  AssertEquals('<span class="c">t</span>', Span('t', 'c'));
  AssertEquals('<div>t</div>', Block('t'));
  AssertEquals('<div id="i" >t</div>', Block('t', '', 'i'));
end;

procedure THTMLLibTest.TestSecureEquals;
begin
  AssertTrue(SecureEquals('abc123', 'abc123'));
  AssertFalse(SecureEquals('abc123', 'abc124'));
  AssertFalse(SecureEquals('abc', 'abcd'));
  AssertFalse('empty never matches', SecureEquals('', ''));
  AssertFalse(SecureEquals('', 'x'));
end;

procedure THTMLLibTest.TestCsrfSessionKey;
begin
  AssertEquals(__HTML_CSRF_TOKEN_KEY, CsrfSessionKey(''));
  AssertEquals(__HTML_CSRF_TOKEN_KEY + ':f1', CsrfSessionKey('f1'));
  AssertTrue(CsrfSessionKey('a') <> CsrfSessionKey('b'));
end;

// the v1 API was shut down in 2018: every version renders the v2 widget
procedure THTMLLibTest.TestReCaptchaAlwaysV2;
var
  h: THTMLUtil;
  v1, v2: string;
begin
  h := THTMLUtil.Create;
  try
    v1 := h.ReCaptcha('key"x', 'v1');
    v2 := h.ReCaptcha('key"x', 'v2');
    AssertEquals(v2, v1);
    AssertTrue(Pos('class="g-recaptcha"', v1) > 0);
    AssertTrue(Pos('data-sitekey="key&quot;x"', v1) > 0);
    AssertTrue(Pos('api/challenge', v1) = 0);
    AssertTrue(Pos('recaptcha_version', v1) = 0);
  finally
    h.Free;
  end;
end;

initialization
  RegisterTest(THTMLLibTest);

end.

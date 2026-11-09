unit test_rss_lib;

{$mode objfpc}{$H+}

interface

uses
  rss_lib, fpjson, jsonparser, Classes, SysUtils, fpcunit, testregistry;

type

  { TRSSLibTest }

  TRSSLibTest = class(TTestCase)
  private
    FRss: TRSSReaderLib;
    function ParseJson(const S: string): TJSONObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRss20Channel;
    procedure TestRss20Items;
    procedure TestOnlyItemNodesAreEmitted;
    procedure TestMediaWithoutUrlAttribute;
    procedure TestImageFallbacks;
    procedure TestAtomFeed;
    procedure TestRss10Rdf;
    procedure TestNoChannel;
    procedure TestMalformedXml;
    procedure TestEmptyInput;
    procedure TestLoadTwice;
    procedure TestAsJsonBeforeLoad;
    procedure TestLoadFromUrlRefused;
    procedure TestLoadFromUrlEmpty;
    procedure TestEntityExpansionDoesNotReadFiles;
  end;

implementation

const
  RSS20 =
    '<?xml version="1.0" encoding="UTF-8"?>' +
    '<rss version="2.0" xmlns:media="http://search.yahoo.com/mrss/">' +
    '<channel>' +
    '<title> Feed Title </title>' +
    '<link>http://example.test/</link>' +
    '<language>id</language>' +
    '<image><url>http://example.test/logo.png</url></image>' +
    '<item>' +
    '<title>First &amp; "quoted"</title>' +
    '<link>http://example.test/1</link>' +
    '<description><![CDATA[<p>Desc 1</p>]]></description>' +
    '<media:content url="http://example.test/1.jpg" type="image/jpeg"/>' +
    '</item>' +
    '<lastBuildDate>Mon, 01 Jan 2024 00:00:00 +0000</lastBuildDate>' +
    '<item>' +
    '<title>Second</title>' +
    '<link>http://example.test/2</link>' +
    '<description>Desc 2</description>' +
    '<enclosure url="http://example.test/2.mp3" type="audio/mpeg"/>' +
    '</item>' +
    '</channel>' +
    '</rss>';

  ATOM =
    '<?xml version="1.0" encoding="utf-8"?>' +
    '<feed xmlns="http://www.w3.org/2005/Atom">' +
    '<title>Atom Title</title>' +
    '<link href="http://example.test/atom"/>' +
    '<entry>' +
    '<title>Entry 1</title>' +
    '<link href="http://example.test/e1"/>' +
    '<summary>Sum 1</summary>' +
    '</entry>' +
    '</feed>';

  RDF =
    '<?xml version="1.0"?>' +
    '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://purl.org/rss/1.0/">' +
    '<channel rdf:about="http://example.test/"><title>RDF Title</title><link>http://example.test/</link></channel>' +
    '<item rdf:about="http://example.test/r1"><title>R1</title><link>http://example.test/r1</link><description>D</description></item>' +
    '<item rdf:about="http://example.test/r2"><title>R2</title><link>http://example.test/r2</link></item>' +
    '</rdf:RDF>';

function TRSSLibTest.ParseJson(const S: string): TJSONObject;
var
  d: TJSONData;
begin
  d := GetJSON(S);
  if not (d is TJSONObject) then
  begin
    d.Free;
    Fail('AsJson did not return a JSON object: ' + S);
  end;
  Result := TJSONObject(d);
end;

procedure TRSSLibTest.SetUp;
begin
  FRss := TRSSReaderLib.Create;
end;

procedure TRSSLibTest.TearDown;
begin
  FRss.Free;
end;

procedure TRSSLibTest.TestRss20Channel;
begin
  AssertTrue(FRss.LoadFromString(RSS20));
  AssertEquals('Feed Title', FRss.Title);
  AssertEquals('http://example.test/', FRss.Link);
  AssertEquals('id', FRss.Language);
  AssertEquals('http://example.test/logo.png', FRss.ImageURL);
  AssertEquals(2, FRss.ItemCount);
  AssertNotNull(FRss.Channel);
end;

procedure TRSSLibTest.TestRss20Items;
var
  j: TJSONObject;
  items: TJSONArray;
begin
  AssertTrue(FRss.LoadFromString(RSS20));
  j := ParseJson(FRss.AsJson);
  try
    items := j.Arrays['items'];
    AssertEquals(2, items.Count);
    AssertEquals('First & "quoted"', items.Objects[0].Strings['title']);
    AssertEquals('http://example.test/1', items.Objects[0].Strings['link']);
    AssertEquals('<p>Desc 1</p>', items.Objects[0].Strings['description']);
    AssertEquals('http://example.test/1.jpg', items.Objects[0].Strings['image']);
    AssertEquals('Second', items.Objects[1].Strings['title']);
    AssertEquals('http://example.test/2.mp3', items.Objects[1].Strings['image']);
    AssertEquals('Desc 2', items.Objects[1].Strings['description']);
  finally
    j.Free;
  end;
end;

procedure TRSSLibTest.TestOnlyItemNodesAreEmitted;
var
  j: TJSONObject;
begin
  // <lastBuildDate> sits between the items and used to become an empty entry
  AssertTrue(FRss.LoadFromString(RSS20));
  j := ParseJson(FRss.AsJson);
  try
    AssertEquals(2, j.Arrays['items'].Count);
  finally
    j.Free;
  end;
end;

procedure TRSSLibTest.TestMediaWithoutUrlAttribute;
var
  j: TJSONObject;
begin
  // used to dereference a nil attribute node
  AssertTrue(FRss.LoadFromString(
    '<rss><channel><title>t</title>' +
    '<item><title>a</title><media:content type="image/jpeg"/><enclosure length="1"/></item>' +
    '</channel></rss>'));
  j := ParseJson(FRss.AsJson);
  try
    AssertEquals(1, j.Arrays['items'].Count);
    AssertEquals('', j.Arrays['items'].Objects[0].Strings['image']);
  finally
    j.Free;
  end;
end;

procedure TRSSLibTest.TestImageFallbacks;
var
  j: TJSONObject;
begin
  AssertTrue(FRss.LoadFromString(
    '<rss><channel>' +
    '<item><title>a</title><image><url> http://x/i.png </url></image></item>' +
    '<item><title>b</title><media:thumbnail url="http://x/t.png"/></item>' +
    '</channel></rss>'));
  j := ParseJson(FRss.AsJson);
  try
    AssertEquals('http://x/i.png', j.Arrays['items'].Objects[0].Strings['image']);
    AssertEquals('http://x/t.png', j.Arrays['items'].Objects[1].Strings['image']);
  finally
    j.Free;
  end;
end;

procedure TRSSLibTest.TestAtomFeed;
var
  j: TJSONObject;
begin
  // no <channel>: used to crash with a nil dereference
  AssertTrue(FRss.LoadFromString(ATOM));
  AssertEquals('Atom Title', FRss.Title);
  AssertEquals('http://example.test/atom', FRss.Link);
  AssertEquals(1, FRss.ItemCount);
  j := ParseJson(FRss.AsJson);
  try
    AssertEquals(1, j.Arrays['items'].Count);
    AssertEquals('Entry 1', j.Arrays['items'].Objects[0].Strings['title']);
    AssertEquals('http://example.test/e1', j.Arrays['items'].Objects[0].Strings['link']);
    AssertEquals('Sum 1', j.Arrays['items'].Objects[0].Strings['description']);
  finally
    j.Free;
  end;
end;

procedure TRSSLibTest.TestRss10Rdf;
var
  j: TJSONObject;
begin
  AssertTrue(FRss.LoadFromString(RDF));
  AssertEquals('RDF Title', FRss.Title);
  AssertEquals(2, FRss.ItemCount);
  j := ParseJson(FRss.AsJson);
  try
    AssertEquals(2, j.Arrays['items'].Count);
    AssertEquals('R2', j.Arrays['items'].Objects[1].Strings['title']);
  finally
    j.Free;
  end;
end;

procedure TRSSLibTest.TestNoChannel;
var
  j: TJSONObject;
begin
  AssertTrue(FRss.LoadFromString('<html><body>not a feed</body></html>'));
  AssertEquals('', FRss.Title);
  AssertEquals(0, FRss.ItemCount);
  j := ParseJson(FRss.AsJson);
  try
    AssertEquals(0, j.Arrays['items'].Count);
  finally
    j.Free;
  end;
end;

procedure TRSSLibTest.TestMalformedXml;
begin
  // used to raise EXMLReadError out of LoadFromStream
  AssertFalse(FRss.LoadFromString('<rss><channel><title>x</channel>'));
  AssertEquals('', FRss.AsJson);
  AssertNull(FRss.Channel);
end;

procedure TRSSLibTest.TestEmptyInput;
begin
  AssertFalse(FRss.LoadFromString(''));
  AssertEquals('', FRss.AsJson);
end;

procedure TRSSLibTest.TestLoadTwice;
begin
  // the previous document and stream are released, the state is replaced
  AssertTrue(FRss.LoadFromString(RSS20));
  AssertTrue(FRss.LoadFromString(ATOM));
  AssertEquals('Atom Title', FRss.Title);
  AssertEquals('', FRss.ImageURL);
  AssertEquals(1, FRss.ItemCount);
  AssertFalse(FRss.LoadFromString('<broken'));
  AssertEquals('', FRss.AsJson);
end;

procedure TRSSLibTest.TestAsJsonBeforeLoad;
begin
  AssertEquals('', FRss.AsJson);
  AssertEquals(0, FRss.ItemCount);
  AssertEquals('', FRss.Contents);
end;

procedure TRSSLibTest.TestLoadFromUrlRefused;
begin
  FRss.Timeout := 2000;
  AssertFalse(FRss.LoadFromURL('http://127.0.0.1:1/feed.xml'));
  AssertEquals('http://127.0.0.1:1/feed.xml', FRss.URL);
  AssertEquals('', FRss.AsJson);
  AssertEquals('', FRss.Contents);
end;

procedure TRSSLibTest.TestLoadFromUrlEmpty;
begin
  AssertFalse(FRss.LoadFromURL(''));
end;

procedure TRSSLibTest.TestEntityExpansionDoesNotReadFiles;
var
  j: TJSONObject;
  f: TStringList;
  path: string;
begin
  // an external entity pointing at a local file must not leak its content
  path := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_rss_secret_' +
    IntToStr(GetProcessID) + '.txt';
  f := TStringList.Create;
  try
    f.Text := 'SECRETCONTENT';
    f.SaveToFile(path);
    if FRss.LoadFromString('<?xml version="1.0"?>' +
      '<!DOCTYPE rss [<!ENTITY ext SYSTEM "file:///' +
      StringReplace(path, '\', '/', [rfReplaceAll]) + '">]>' +
      '<rss><channel><title>t</title><item><title>&ext;</title></item></channel></rss>') then
    begin
      j := ParseJson(FRss.AsJson);
      try
        AssertTrue('file content not expanded',
          Pos('SECRETCONTENT', j.AsJSON) = 0);
      finally
        j.Free;
      end;
    end;
  finally
    DeleteFile(path);
    f.Free;
  end;
end;

initialization
  RegisterTest(TRSSLibTest);

end.

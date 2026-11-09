unit rss_lib;
{
  [x] USAGE

  with TRSSReaderLib.Create do
  begin
    if LoadFromURL('http://www/rss') then
      Result := AsJson;
    Free;
  end;

  LoadFromURL / LoadFromStream return False (instead of raising) when the
  feed cannot be fetched or is not well-formed XML. RSS 2.0, RSS 1.0 (RDF)
  and Atom feeds are accepted; for Atom the <feed> element acts as the
  channel and <entry> elements as items.
}

{$mode objfpc}{$H+}

interface

uses
  common,
  fphttpclient, opensslsockets, DOM, XMLRead, xmlreader, fpjson,
  Classes, SysUtils;

const
  RSS_DEFAULT_TIMEOUT = 10000; // ms
  RSS_MAX_CHARS = 32 * 1024 * 1024; // parser limit, guards entity expansion bombs

type

  { THTTPClient }

  THTTPClient = class(TFPHTTPClient)
  public
    // Fetches AURL into a new stream owned by the caller. On failure no
    // stream is returned.
    function GetContent(AURL: string; out AContent: TStream): boolean;
  end;


  { TRSSReaderLib }

  TRSSReaderLib = class(TInterfacedObject)
  private
    FChannel: TDOMNode;
    FImageURL: string;
    FLanguage: string;
    FLink: string;
    FRssStream: TStream;
    FDoc: TXMLDocument;
    FTitle: string;
    FURL: string;
    FUTF8: boolean;
    FTimeout: integer;
    http: THTTPClient;
    function getContents: string;
    function getTagValue(const ANode: TDOMNode; const ATagName: string): string;
    function getAttribute(const ANode: TDOMNode; const AName: string): string;
    function findChild(const ANode: TDOMNode; const ATagName: string): TDOMNode;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromURL(AURL: string): boolean;
    function LoadFromStream(AStream: TStream): boolean;
    function LoadFromString(const AXml: string): boolean;
    function AsJson: string;
    // number of <item>/<entry> elements in the loaded feed
    function ItemCount: integer;
  published
    property Contents: string read getContents;
    property UTF8: boolean read FUTF8 write FUTF8;
    property URL: string read FURL;
    property Timeout: integer read FTimeout write FTimeout;

    property Channel: TDOMNode read FChannel;
    property Title: string read FTitle;
    property Link: string read FLink;
    property Language: string read FLanguage;
    property ImageURL: string read FImageURL;
  end;

implementation

{ THTTPClient }

function THTTPClient.GetContent(AURL: string; out AContent: TStream): boolean;
begin
  Result := False;
  AContent := nil;
  if isEmpty(AURL) then
    Exit;
  AContent := TMemoryStream.Create;
  try
    // no status filter: a 404 body is read instead of raising, and the
    // status is checked below for this request only
    HTTPMethod('GET', AURL, AContent, []);
    Result := ResponseStatusCode = 200;
  except
    Result := False;
  end;
  if not Result then
    FreeAndNil(AContent);
end;

{ TRSSReaderLib }

function TRSSReaderLib.getContents: string;
begin
  Result := '';
  if not Assigned(FRssStream) then
    Exit;
  Result := StreamToString(FRssStream);
end;

function TRSSReaderLib.getTagValue(const ANode: TDOMNode;
  const ATagName: string): string;
var
  _Node: TDOMNode;
begin
  Result := '';
  _Node := findChild(ANode, ATagName);
  if Assigned(_Node) then
  begin
    if FUTF8 then
      Result := UTF8Encode(_Node.TextContent)
    else
      Result := string(_Node.TextContent);
  end;
end;

function TRSSReaderLib.getAttribute(const ANode: TDOMNode; const AName: string): string;
var
  attr: TDOMNode;
begin
  Result := '';
  if not Assigned(ANode) or not Assigned(ANode.Attributes) then
    Exit;
  attr := ANode.Attributes.GetNamedItem(DOMString(AName));
  if Assigned(attr) then
    Result := UTF8Encode(attr.TextContent);
end;

// direct child element with the given name, nil-safe
function TRSSReaderLib.findChild(const ANode: TDOMNode; const ATagName: string): TDOMNode;
begin
  Result := nil;
  if Assigned(ANode) then
    Result := ANode.FindNode(DOMString(ATagName));
end;

procedure TRSSReaderLib.Clear;
begin
  FChannel := nil;
  FreeAndNil(FDoc);
  FreeAndNil(FRssStream);
  FTitle := '';
  FLink := '';
  FLanguage := '';
  FImageURL := '';
end;

constructor TRSSReaderLib.Create;
begin
  inherited Create;
  FUTF8 := True;
  FTimeout := RSS_DEFAULT_TIMEOUT;
  FDoc := nil;
  FRssStream := nil;
  FChannel := nil;
  http := THTTPClient.Create(nil);
  http.AllowRedirect := True;
end;

destructor TRSSReaderLib.Destroy;
begin
  Clear;
  http.Free;
  inherited Destroy;
end;

function TRSSReaderLib.LoadFromURL(AURL: string): boolean;
var
  stream: TStream;
begin
  Result := False;
  if isEmpty(AURL) then
    Exit;

  Clear;
  FURL := AURL;
  http.ConnectTimeout := FTimeout;
  http.IOTimeout := FTimeout;
  if not http.GetContent(AURL, stream) then
    Exit;
  FRssStream := stream;
  FRssStream.Position := 0;
  Result := LoadFromStream(FRssStream);
end;

function TRSSReaderLib.LoadFromStream(AStream: TStream): boolean;
var
  imgNode: TDOMNode;
  root: TDOMNode;
  parser: TDOMParser;
  src: TXMLInputSource;
begin
  Result := False;
  FChannel := nil;
  FreeAndNil(FDoc);
  if AStream <> FRssStream then
    FreeAndNil(FRssStream);
  parser := TDOMParser.Create;
  try
    // feeds never need a DOCTYPE; rejecting it blocks external entities
    // (XXE: <!ENTITY x SYSTEM "file:///...">) which ReadXMLFile expands
    parser.Options.DisallowDoctype := True;
    parser.Options.MaxChars := RSS_MAX_CHARS;
    src := TXMLInputSource.Create(AStream);
    try
      try
        parser.Parse(src, FDoc);
      except
        // malformed or empty XML, or a DOCTYPE
        FreeAndNil(FDoc);
        Exit;
      end;
    finally
      src.Free;
    end;
  finally
    parser.Free;
  end;
  root := FDoc.DocumentElement;
  if not Assigned(root) then
  begin
    FreeAndNil(FDoc);
    Exit;
  end;

  // RSS 2.0 / RSS 1.0: <channel> below the root; Atom: the <feed> root
  FChannel := findChild(root, 'channel');
  if not Assigned(FChannel) then
    FChannel := root;
  FTitle := Trim(getTagValue(FChannel, 'title'));
  FLink := Trim(getTagValue(FChannel, 'link'));
  if (FLink = '') and (FChannel.NodeName = 'feed') then
    FLink := getAttribute(findChild(FChannel, 'link'), 'href');
  FLanguage := Trim(getTagValue(FChannel, 'language'));

  FImageURL := '';
  imgNode := findChild(FChannel, 'image');
  if Assigned(imgNode) then
    FImageURL := Trim(getTagValue(imgNode, 'url'));

  Result := True;
end;

function TRSSReaderLib.LoadFromString(const AXml: string): boolean;
begin
  Clear;
  FRssStream := TStringStream.Create(AXml);
  FRssStream.Position := 0;
  Result := LoadFromStream(FRssStream);
end;

function IsItemNode(const ANode: TDOMNode): boolean;
begin
  Result := Assigned(ANode) and (ANode.NodeType = ELEMENT_NODE) and
    ((ANode.NodeName = 'item') or (ANode.NodeName = 'entry'));
end;

function TRSSReaderLib.ItemCount: integer;
var
  node: TDOMNode;
begin
  Result := 0;
  if not Assigned(FDoc) then
    Exit;
  node := FChannel.FirstChild;
  while Assigned(node) do
  begin
    if IsItemNode(node) then
      Inc(Result);
    node := node.NextSibling;
  end;
  // RSS 1.0 puts the items next to <channel>, under the rdf:RDF root
  if (Result = 0) and (FChannel <> FDoc.DocumentElement) then
  begin
    node := FDoc.DocumentElement.FirstChild;
    while Assigned(node) do
    begin
      if IsItemNode(node) then
        Inc(Result);
      node := node.NextSibling;
    end;
  end;
end;

function TRSSReaderLib.AsJson: string;
var
  item, media, parent: TDOMNode;
  jItem, jRoot: TJSONObject;
  jArray: TJSONArray;
  image, itemLink: string;
begin
  Result := '';
  if not Assigned(FDoc) then
    Exit;

  jRoot := TJSONObject.Create;
  try
    jArray := TJSONArray.Create;
    jRoot.Add('items', jArray); // jRoot owns jArray from here on

    parent := FChannel;
    if (findChild(parent, 'item') = nil) and (findChild(parent, 'entry') = nil) and
      (FChannel <> FDoc.DocumentElement) then
      parent := FDoc.DocumentElement;

    item := parent.FirstChild;
    while Assigned(item) do
    begin
      // only item/entry elements; anything else between items (lastBuildDate,
      // atom:link, ...) used to be emitted as an empty entry
      if not IsItemNode(item) then
      begin
        item := item.NextSibling;
        Continue;
      end;
      jItem := TJSONObject.Create;
      jArray.Add(jItem);
      jItem.Strings['title'] := Trim(getTagValue(item, 'title'));
      itemLink := Trim(getTagValue(item, 'link'));
      if (itemLink = '') and (item.NodeName = 'entry') then
        itemLink := getAttribute(findChild(item, 'link'), 'href');
      jItem.Strings['link'] := itemLink;

      // image: media:content, enclosure, then <image><url>
      image := getAttribute(findChild(item, 'media:content'), 'url');
      if image = '' then
        image := getAttribute(findChild(item, 'media:thumbnail'), 'url');
      if image = '' then
        image := getAttribute(findChild(item, 'enclosure'), 'url');
      if image = '' then
      begin
        media := findChild(item, 'image');
        if Assigned(media) then
          image := Trim(getTagValue(media, 'url'));
      end;
      jItem.Strings['image'] := image;

      if item.NodeName = 'entry' then
      begin
        jItem.Strings['description'] := getTagValue(item, 'summary');
        if jItem.Strings['description'] = '' then
          jItem.Strings['description'] := getTagValue(item, 'content');
      end
      else
        jItem.Strings['description'] := getTagValue(item, 'description');

      item := item.NextSibling;
    end;

    Result := jRoot.AsJSON;
  finally
    jRoot.Free;
  end;
end;

end.

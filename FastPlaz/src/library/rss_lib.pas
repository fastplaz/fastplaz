unit rss_lib;
{
  [x] USAGE

  with TRSSReaderLib.Create do
  begin
    LoadFromURL('http://www/rss');

    Result := AsJson;
    Free;
  end;

}

{$mode objfpc}{$H+}

interface

uses
  common,
  fphttpclient, DOM, XMLRead, fpjson,
  Classes, SysUtils;

type

  { THTTPClient }

  THTTPClient = class(TFPHTTPClient)
  public
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
    http: THTTPClient;
    function getContents: string;
    function getTagValue(const ANode: TDOMNode; const ATagName: string): string;
  public
    constructor Create;
    destructor Destroy;

    function LoadFromURL(AURL: string): boolean;
    function LoadFromStream(AStream: TStream): boolean;
    function AsJson: string;
  published
    property Contents: string read getContents;
    property UTF8: boolean read FUTF8 write FUTF8;
    property URL: string read FURL;

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
  if isEmpty(AURL) then
    Exit;
  try
    AContent := TMemoryStream.Create;
    DoMethod('GET', AURL, AContent, [200]);
  except
  end;
  if ResponseStatusCode = 200 then
  begin
    Result := True;
  end;
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
  if Assigned(ANode) then
  begin
    _Node := ANode.FindNode(DOMString(ATagName));
    if Assigned(_Node) then
    begin
      if FUTF8 then
        Result := UTF8Encode(_Node.TextContent)
      else
        Result := string(_Node.TextContent);
    end;
  end;
end;

constructor TRSSReaderLib.Create;
begin
  FUTF8 := True;
  http := THTTPClient.Create(nil);
end;

destructor TRSSReaderLib.Destroy;
begin
  http.Free;
end;

function TRSSReaderLib.LoadFromURL(AURL: string): boolean;
begin
  Result := False;
  if isEmpty(AURL) then
    Exit;

  FURL := AURL;
  if http.GetContent(AURL, FRssStream) then
  begin
    FRssStream.Position := 0;
    Result := LoadFromStream(FRssStream);
  end;
end;

function TRSSReaderLib.LoadFromStream(AStream: TStream): boolean;
var
  s: string;
  strBytes: TBytes;
  imgNode: TDOMNode;
begin
  Result := False;
  ReadXMLFile(FDoc, AStream);

  FChannel := FDoc.DocumentElement.FindNode('channel');
  FTitle := getTagValue(FChannel, 'title');
  FLink := getTagValue(FChannel, 'link');
  FLanguage := getTagValue(FChannel, 'language');

  imgNode := FChannel.FindNode('image');
  if Assigned(imgNode) then
  begin
    FImageURL := getTagValue(imgNode, 'url');
  end;

  Result := True;
end;

function TRSSReaderLib.AsJson: string;
var
  item, media: TDOMNode;
  jItem: TJSONObject;
  jArray: TJSONArray;
begin
  Result := '';
  if not Assigned(FDoc) then
    Exit;

  jArray:= TJSONArray.Create;
  item := FChannel.FindNode('item');
  while Assigned(item) do
  begin
    jItem := TJSONObject.Create;
    jItem.Strings['title'] := Trim(getTagValue(item, 'title'));
    jItem.Strings['link'] := getTagValue(item, 'link');

    // get media
    media := item.FindNode('media:content');
    if Assigned(media) then
    begin
      jItem.Strings['image'] := media.Attributes.GetNamedItem('url').TextContent;
    end;
    if jItem.Strings['image'] = '' then
    begin
      media := item.FindNode('enclosure');
      if Assigned(media) then
      begin
        jItem.Strings['image'] := media.Attributes.GetNamedItem('url').TextContent;
      end;
    end;
    if jItem.Strings['image'] = '' then
    begin
      media := item.FindNode('image');
      if Assigned(media) then
      begin
        jItem.Strings['image'] := getTagValue(media, 'url');
      end;
    end;
    //-- get media - end

    jItem.Strings['description'] := getTagValue(item, 'description');
    jArray.Add( jItem);

    item := item.NextSibling;
  end;
  jItem := TJSONObject.Create;
  jItem.Add('items', jArray);

  Result := jItem.AsJSON;
  jArray.Free;
end;

end.


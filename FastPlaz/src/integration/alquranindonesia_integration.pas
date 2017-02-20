unit alquranindonesia_integration;

{
  Alquran Indonesia Online
  http://alquran-indonesia.com/index.php?surah=10&page=6

  [x] USAGE
  with TAlquranOnline.Create do
  begin
    Result := FindTerjemahan(1, 1);

    Free;
  end;

}
{$mode objfpc}{$H+}

interface

uses
  fpjson,
  common, http_lib, logutil_lib,
  Classes, SysUtils;

type

  { TAlquranOnline }

  TAlquranOnline = class(TInterfacedObject)
  private
    FArabicText: UnicodeString;
    FAudioURL: UnicodeString;
    FLatin: UnicodeString;
    FSurat: UnicodeString;
    FTerjemahan: UnicodeString;
    function getHTML(ASurat: string; AAyat: string): UnicodeString;
    function getTajwid(AHTML: UnicodeString; AAyat: string): UnicodeString;
    function getLatin(AHTML: UnicodeString; AAyat: string): UnicodeString;
    function getTerjemahan(AHTML: UnicodeString; AAyat: string): UnicodeString;
    function getAudioRL(AHTML: UnicodeString; AAyat: string): UnicodeString;
    function httpGET(AURL: string): UnicodeString;

  public
    constructor Create;
    destructor Destroy;

    property Surat: UnicodeString read FSurat;
    property ArabicText: UnicodeString read FArabicText;
    property Latin: UnicodeString read FLatin;
    property Terjemahan: UnicodeString read FTerjemahan;
    property AudioURL: UnicodeString read FAudioURL;

    function Find(ASurat: integer; AAyat: integer): UnicodeString;
    function Find(ASurat: string; AAyat: string): UnicodeString;
    function FindTerjemahan(ASurat: integer; AAyat: integer): UnicodeString;
    function FindTerjemahan(ASurat: string; AAyat: string): UnicodeString;
    function FindAudioURL(ASurat: string; AAyat: string): UnicodeString;
  end;


implementation

const
  _ALQURANONLINE_URL = 'http://alquran-indonesia.com/index.php?surah=%s&page=%s';
  _ALQURANONLINE_AYAT_PER_PAGE = 10;

{ TAlquranOnline }

function TAlquranOnline.getHTML(ASurat: string; AAyat: string): UnicodeString;
var
  _page, i: integer;
  _url: string;
begin
  Result := '';
  _page := s2i(AAyat) div _ALQURANONLINE_AYAT_PER_PAGE;
  _url := Format(_ALQURANONLINE_URL, [ASurat, i2s(_page)]);
  Result := httpGET(_url);

  i := Pos('id="' + AAyat + '"', Result);
  if i = 0 then
    Exit;

  // html per ayat
  FSurat := Copy(Result, Pos('<title>', Result) + 7);
  FSurat := Copy(FSurat, 0, Pos('- alquran', FSurat) - 2) + ' ' + ASurat + ':' + AAyat;

  Result := Copy(Result, i);
  Result := Copy(Result, Pos('<', Result));
  Result := Copy(Result, 0, Pos('<!-- /panel -->', Result) - 1);
end;

function TAlquranOnline.getTajwid(AHTML: UnicodeString; AAyat: string): UnicodeString;
var
  s: UnicodeString;
begin
  s := 'id="tajwid' + AAyat + '"';
  Result := copy(AHTML, Pos(s, AHTML) + Length(s));
  Result := Copy(Result, 0, Pos('</div>', Result) - 1);
  Result := StripTags(Result);
  ;
  Result := StringReplace(Result, '<br>', #10, [rfReplaceAll]);
  Result := Trim(Result);
end;

function TAlquranOnline.getLatin(AHTML: UnicodeString; AAyat: string): UnicodeString;
var
  s: UnicodeString;
begin
  s := 'id="profile' + AAyat + '">';
  Result := copy(AHTML, Pos(s, AHTML) + Length(s));
  Result := Copy(Result, Pos('<p>', Result) + 3);
  Result := Copy(Result, 0, Pos('</p>', Result) - 1);
  Result := Trim(Result);
  Result := StringReplace(Result, '<b>', '*', [rfReplaceAll]);
  Result := StringReplace(Result, '</b>', '*', [rfReplaceAll]);
  Result := StringReplace(Result, '<u>', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '</u>', '_', [rfReplaceAll]);
end;

function TAlquranOnline.getTerjemahan(AHTML: UnicodeString;
  AAyat: string): UnicodeString;
var
  s: UnicodeString;
begin
  s := '<div class="pull-left m-left-sm m-top-sm ltr ">';
  Result := copy(AHTML, Pos(s, AHTML) + Length(s));
  Result := Copy(Result, Pos('<p>', Result) + 3);
  Result := Copy(Result, 0, Pos('</p>', Result) - 1);
  Result := Trim(Result);
  Result := StringReplace(Result, '<em>', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '</em>', '_', [rfReplaceAll]);
end;

function TAlquranOnline.getAudioRL(AHTML: UnicodeString; AAyat: string): UnicodeString;
var
  i: integer;
  s: UnicodeString;
begin
  Result := '';
  s := 'data-selector="';
  i := Pos(s, AHTML);
  if i = 0 then
    Exit;

  Result := Copy(AHTML, i + Length(s));
  Result := Copy(Result, 0, Pos('"', Result) - 1);
  Result := Trim(Result);
end;

function TAlquranOnline.httpGET(AURL: string): UnicodeString;
var
  _response: IHTTPResponse;
begin
  Result := '';
  if AURL = '' then
    Exit;
  with THTTPLib.Create(AURL) do
  begin
    try
      _response := Get;
      if _response.ResultCode = 200 then
      begin
        Result := _response.ResultText;
      end;
    except
      on E: Exception do
      begin
      end;
    end;
    Free;
  end;

end;

constructor TAlquranOnline.Create;
begin
  FSurat := '';
  FArabicText := '';
  FLatin := '';
  FTerjemahan := '';
  FAudioURL := '';
end;

destructor TAlquranOnline.Destroy;
begin

end;

function TAlquranOnline.Find(ASurat: integer; AAyat: integer): UnicodeString;
begin
  Result := Find(i2s(ASurat), i2s(AAyat));
end;

function TAlquranOnline.Find(ASurat: string; AAyat: string): UnicodeString;
var
  _html: UnicodeString;
begin
  Result := '';
  if s2i(ASurat) > 144 then
    Exit;

  _html := getHTML(ASurat, AAyat);
  FArabicText := getTajwid(_html, AAyat);
  FLatin := getLatin(_html, AAyat);
  FTerjemahan := getTerjemahan(_html, AAyat);
  FAudioURL := getAudioRL(_html, AAyat);

  Result := FArabicText + #10'>> ' + FLatin + #10'>> ' + FTerjemahan;
end;

function TAlquranOnline.FindTerjemahan(ASurat: integer; AAyat: integer): UnicodeString;
begin
  Result := FindTerjemahan(i2s(ASurat), i2s(AAyat));
end;

function TAlquranOnline.FindTerjemahan(ASurat: string; AAyat: string): UnicodeString;
begin
  Result := '';
  if (s2i(ASurat) > 144) or (s2i(ASurat) = 0) then
    Exit;
  if (s2i(AAyat) = 0) then
    Exit;

  Result := Find(ASurat, AAyat);
  Result := FTerjemahan;
end;

function TAlquranOnline.FindAudioURL(ASurat: string; AAyat: string): UnicodeString;
begin
  Result := '';
  if (s2i(ASurat) > 144) or (s2i(ASurat) = 0) then
    Exit;
  if (s2i(AAyat) = 0) then
    Exit;

  Result := Find(ASurat, AAyat);
  Result := FAudioURL;
end;

end.

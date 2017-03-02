unit jobplanet_integration;

{
  JOBPLANET INDONESIA
  https://id.jobplanet.com.

  info [perusahaan]
  review [perusahaan]
  gaji [perusahaan]
  interview [perusahaan]
  loker [perusahaan]

  [x] contoh respon;
  rata2 gaji di rumah123.com adalah xxx.
  dengan gaji terendah xxx dan tertinggi xxx.
  yuk lihat detailnya di https://id.jobplanet.com/companies/48679/info/pt-web-marketing-indonesia-rumah123com

}
{$mode objfpc}{$H+}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson, jsonparser, variants, RegExpr,
  Classes, SysUtils;

type

  { TJobPlanetIntegration }

  TJobPlanetIntegration = class(TInterfacedObject)
  private
    FCompanyCount: integer;
    FCompanyList: TStringList;
    FCountry: string;
    FResultCode: integer;
    FResultText: string;

    function getHTML(AURL: string): string;
    function getContent(AStartString, AStopString: string; AText: string): string;
    function getCompanyList(AHTML: string): string;
    function getInfoFromHTML(AHTML: string): string;
    function getReviewFromHTML(AHTML: string): string;
    function getSalariesFromHTML(AHTML: string): string;
  public
    constructor Create;
    destructor Destroy;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property CompanyCount: integer read FCompanyCount write FCompanyCount;
    property CompanyList: TStringList read FCompanyList write FCompanyList;
    property Country: string read FCountry write FCountry;

    function Info(ACompany: string): string;
    function Review(ACompany: string): string;
    function Salaries(ACompany: string): string;
    function Vacancies(ATitle: string): string;
  end;

implementation

const
  _JOBPLANET_URL = 'https://id.jobplanet.com';
  _JOBPLANET_SEARCH_URL = 'https://id.jobplanet.com/search?category=&query=';
  _JOBPLANET_VACANCYSEARCH_URL =
    'https://id.jobplanet.com/lowongan/search?query=';

  _JOBPLANET_MSG_NOTFOUND = 'Informasi tidak tersedia ...';
  _JOBPLANET_MSG_TINGKATKEPUASAN = #10'Tingkat Kepuasan : ';
  _JOBPLANET_MSG_INFODETIL = #10#10'yuk lihat detailnya di ';

var
  Response: IHTTPResponse;

{ TJobPlanetIntegration }

function TJobPlanetIntegration.getHTML(AURL: string): string;
begin
  Result := '';
  with THTTPLib.Create(AURL) do
  begin
    //AddHeader('Cache-Control', 'no-cache');
    Response := Get;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      Result := FResultText;
    end;
    Free;
  end;
end;

function TJobPlanetIntegration.getContent(AStartString, AStopString: string;
  AText: string): string;
begin
  Result := copy(AText, pos(AStartString, AText) + Length(AStartString));
  Result := Copy(Result, 0, pos(AStopString, Result) - 1);
end;

function TJobPlanetIntegration.getCompanyList(AHTML: string): string;
var
  i: integer;
  s, _companyName, _url, _tmp: string;
begin
  s := '<div class="is_company_card">';
  Result := copy(AHTML, pos(s, AHTML) + Length(s));
  s := '<div class="no_company_card">';
  Result := Copy(Result, 0, pos(s, Result) - 1);

  FCompanyCount := 0;
  try
    with TRegExpr.Create do
    begin
      Expression := '(<div class="result_card ">)';
      if Exec(Result) then
      begin
        FCompanyCount := 1;
        while ExecNext do
        begin
          FCompanyCount := FCompanyCount + 1;
        end;
      end;
      Free;
    end;
  except
  end;

  _tmp := '';
  for i := 0 to FCompanyCount - 1 do
  begin
    s := '<div class="result_card ">';
    Result := Copy(Result, Pos(s, Result) + Length(s));

    // company name
    s := 'class="tit">';
    _companyName := Copy(Result, Pos(s, Result) + Length(s));
    _companyName := copy(_companyName, 0, Pos('</a>', _companyName) - 1);

    // url
    s := '<a href="';
    _url := Copy(Result, Pos(s, Result) + Length(s));
    _url := _JOBPLANET_URL + copy(_url, 0, Pos('?', _url) - 1);

    FCompanyList.Values[_companyName] := _url;
    _tmp := _tmp + '- ' + _companyName + #10;
  end;

  Result := trim(_tmp);
end;

function TJobPlanetIntegration.getInfoFromHTML(AHTML: string): string;
var
  s, _info, _title, _description, _tingkatKepuasan: string;
begin
  _title := getContent('<h2 class="txt_titl_info">', '</h2>', AHTML);
  _title := StripTags(_title);
  _description := getContent('<p>', '</p>', AHTML);

  _tingkatKepuasan := getContent(
    '<span class="val_starmark" style="width:78.0%;"><span class="alt_txt">',
    '</span>', AHTML);
  _tingkatKepuasan := _JOBPLANET_MSG_TINGKATKEPUASAN + _tingkatKepuasan +
    ' dari skala 5';

  _info := _title + #10#10 + _description + #10 + _tingkatKepuasan;

  Result := _info;
end;

function TJobPlanetIntegration.getReviewFromHTML(AHTML: string): string;
var
  s, _label: string;
begin
  Result := getInfoFromHTML(AHTML);
  Result := StringReplace(Result, 'Profil', 'Review', [rfReplaceAll]);

  s := getContent('<div class="content_body_ty1">', '<div class="btn_group">', AHTML);
  s := StripTags(s);
  s := StringReplace(s, 'BEST'#10, '', [rfReplaceAll]);
  s := StringReplace(s, '&quot;', '', [rfReplaceAll]);
  s := StringReplace(s, '    ', '', [rfReplaceAll]);
  s := StringReplace(s, '   ', '', [rfReplaceAll]);
  s := StringReplace(s, '  ', '', [rfReplaceAll]);
  s := StringReplace(s, #10#10#10, #10, [rfReplaceAll]);
  s := StringReplace(s, 'Pro'#10, '*Pro*', [rfReplaceAll]);
  s := StringReplace(s, 'Kontra'#10, '*Kontra*', [rfReplaceAll]);
  s := Trim(s);

  Result := Result + #10#10 + s;
end;

function TJobPlanetIntegration.getSalariesFromHTML(AHTML: string): string;
begin
  Result := getContent('<h1 class="tit">', '</h1>', AHTML);

  Result := Result + #10'rata-rata: ' +
    Trim(StripTags(getContent('<span class="sal_num">', '</span>', AHTML)));
  Result := Result + #10'gaji terendah ' +
    Trim(StripTags(getContent('<span class="min_num">', '</span>', AHTML)));
  Result := Result + ' dan tertinggi ' +
    Trim(StripTags(getContent('<span class="max_num">', '</span>', AHTML)));
end;

constructor TJobPlanetIntegration.Create;
begin
  FCountry := 'id';
  CompanyCount := 0;
  FCompanyList := TStringList.Create;
end;

destructor TJobPlanetIntegration.Destroy;
begin
  FCompanyList.Free;
end;

function TJobPlanetIntegration.Info(ACompany: string): string;
begin
  Result := _JOBPLANET_MSG_NOTFOUND;

  Result := getHTML(_JOBPLANET_SEARCH_URL + UrlEncode(ACompany));
  Result := getCompanyList(Result);
  if FCompanyList.Count <> 1 then
  begin
    if FCompanyList.Count > 1 then
    begin
    end;
    Exit;
  end;

  Result := getHTML(FCompanyList.ValueFromIndex[0]);
  Result := getInfoFromHTML(Result);
  Result := Result + _JOBPLANET_MSG_INFODETIL + FCompanyList.ValueFromIndex[0];
end;

function TJobPlanetIntegration.Review(ACompany: string): string;
begin
  Result := _JOBPLANET_MSG_NOTFOUND;

  Result := getHTML(_JOBPLANET_SEARCH_URL + UrlEncode(ACompany));
  Result := getCompanyList(Result);
  if FCompanyList.Count <> 1 then
  begin
    if FCompanyList.Count > 1 then
    begin
    end;
    Exit;
  end;

  Result := getHTML(FCompanyList.ValueFromIndex[0]);
  Result := getReviewFromHTML(Result);
  Result := Result + _JOBPLANET_MSG_INFODETIL + FCompanyList.ValueFromIndex[0];
end;

function TJobPlanetIntegration.Salaries(ACompany: string): string;
var
  _url: string;
begin
  Result := _JOBPLANET_MSG_NOTFOUND;

  Result := getHTML(_JOBPLANET_SEARCH_URL + UrlEncode(ACompany));
  Result := getCompanyList(Result);
  if FCompanyList.Count <> 1 then
  begin
    if FCompanyList.Count > 1 then
    begin
    end;
    Exit;
  end;

  _url := StringReplace(FCompanyList.ValueFromIndex[0], '/info/',
    '/salaries/', [rfReplaceAll]);

  Result := getHTML(_url);
  Result := getSalariesFromHTML(Result);
  Result := Result + _JOBPLANET_MSG_INFODETIL + _url;
end;

function TJobPlanetIntegration.Vacancies(ATitle: string): string;
var
  i: integer;
  s, html, tmp, _url: string;
begin
  _url := _JOBPLANET_VACANCYSEARCH_URL + UrlEncode(ATitle);
  html := getHTML(_url);
  s := StripTags(getContent(
    '<span class="result_count">Total Lowongan Kerja <span class="num">',
    '</span></span>', html));
  Result := 'Ditemukan ' + s + ' lowongan'#10;

  tmp := '';
  for i := 1 to 5 do
  begin
    s := '<div class="result_unit_info">';
    html := copy(html, pos(s, html) + Length(s));
    s := '<p class="company_name"><button class="btn_open">';
    html := copy(html, pos(s, html) + Length(s));
    tmp := tmp + '- ' + Copy(html, 0, pos('</button>', html) - 1) + #10;
  end;
  tmp := Trim(tmp);

  Result := Result + tmp;
  Result := Result + _JOBPLANET_MSG_INFODETIL + _url;
end;

end.

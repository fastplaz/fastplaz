unit maskofajadwalshalat_integration;

{
  [x] USAGE

  with TMasKofaJadwalShalatIntegration.Create do
  begin
    Username := '';
    Password := '';
    SessionID := '';
    CityList.LoadFromFile('files/shalat-citylist.txt');

    Result := Find('city name');

    Result := Find('city name', Tomorrow);

    Free;
  end;

}

{$mode objfpc}{$H+}

interface

uses
  common, logutil_lib,
  fpjson, jsonparser, dateutils, fphttpclient,
  Classes, SysUtils;

type

  { TMasKofaJadwalShalatIntegration }

  TMasKofaJadwalShalatIntegration = class(TInterfacedObject)
  private
    FCityList: TStringList;
    FIsSuccessfull: boolean;
    FPassword: string;
    FResultCode: integer;
    FResultText: string;
    FSessionID: string;
    FUsername: string;
    jsonData: TJSONData;
    function isPermitted: boolean;
    function getCityID(ACityName: string): integer;
    function getData(ACityID: integer; ADateTime: TDate): boolean;
  public
    constructor Create;
    destructor Destroy;

    property IsSuccessfull: boolean read FIsSuccessfull;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property SessionID: string read FSessionID write FSessionID;
    property CityList: TStringList read FCityList write FCityList;

    function Find(ACityName: string): string;
    function Find(ACityName: string; ADateTime: TDate): string;
    function Find(ACityID: integer; ADateTime: TDate): string;
  published
    property Data: TJSONData read jsonData;
  end;

implementation

const
  //_MASKOFA_JADWALSHALAT_URL = 'http://api.maskofa.com/jadwal-shalat.html';
  _MASKOFA_JADWALSHALAT_BULANAN_URL =
    'http://api.maskofa.com/jadwal-shalat-bulanan.html';
  _MASKOFA_JADWALSHALAT_CITYIDDEFAULT = 310; // jakarta selatan


{ TMasKofaJadwalShalatIntegration }

function TMasKofaJadwalShalatIntegration.isPermitted: boolean;
begin
  Result := False;
  if SessionID <> '' then
    Result := True;
end;

function TMasKofaJadwalShalatIntegration.getCityID(ACityName: string): integer;
var
  i: integer;
begin
  Result := -1;
  if FCityList.Count = 0 then
    Exit;

  for i := 0 to FCityList.Count - 1 do
  begin
    if LowerCase(FCityList.ValueFromIndex[i]) = LowerCase(ACityName) then
    begin
      Result := s2i(FCityList.Names[i]);
      break;
    end;
  end;

  //if Result = -1 then
  //  Result := _MASKOFA_JADWALSHALAT_CITYIDDEFAULT;
end;

function TMasKofaJadwalShalatIntegration.getData(ACityID: integer;
  ADateTime: TDate): boolean;
var
  s: string;
  http: TFPHTTPClient;
begin
  Result := False;

  http := TFPHTTPClient.Create(nil);
  try
    s := 'user=' + FUsername;
    s := s + '&pass=' + FPassword;
    s := s + '&sesid=' + FSessionID;
    s := s + '&kota=' + i2s(ACityID);
    s := s + '&bulan=' + i2s(MonthOf(ADateTime));
    s := s + '&tahun=' + i2s(YearOf(ADateTime));
    s := http.FormPost(_MASKOFA_JADWALSHALAT_BULANAN_URL, s);
    if http.ResponseStatusCode = 200 then
    begin
      jsonData := GetJSON(s);
      if jsonGetData(jsonData, 'status') = 'success' then
      begin
        Result := True;
      end;
    end;
  except
  end;

  http.Free;

  {
  with THTTPLib.Create(_MASKOFA_JADWALSHALAT_URL) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('Content-Type','application/x-www-form-urlencoded');
      FormData['user'] := FUsername;
      FormData['pass'] := FPassword;
      FormData['sesid'] := FSessionID;
      FormData['kota'] := i2s(ACityID);
      FormData['bulan'] := i2s(MonthOf(ADateTime));
      FormData['tahun'] := i2s(YearOf(ADateTime));

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      FIsSuccessfull := IsSuccessfull;
      if FResultCode = 200 then
      begin
        jsonData := GetJSON(FResultText);
        if jsonGetData(jsonData, 'status') = 'success' then
        begin
          Result := True;
        end;
      end;
    except
    end;
    Free;
  end;
  }
end;

constructor TMasKofaJadwalShalatIntegration.Create;
begin
  FUsername := '';
  FPassword := '';
  FSessionID := '';
  FCityList := TStringList.Create;
end;

destructor TMasKofaJadwalShalatIntegration.Destroy;
begin
  FCityList.Free;
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TMasKofaJadwalShalatIntegration.Find(ACityName: string): string;
begin
  Result := Find(ACityName, Today);
end;

function TMasKofaJadwalShalatIntegration.Find(ACityName: string;
  ADateTime: TDate): string;
begin
  Result := Find(getCityID(ACityName), ADateTime);
end;

function TMasKofaJadwalShalatIntegration.Find(ACityID: integer;
  ADateTime: TDate): string;
var
  cityID: integer;
  s: string;
begin
  Result := '';
  if not isPermitted then
    Exit;
  if ACityID = -1 then
    Exit;

  cityID := ACityID;
  if ACityID = 0 then
    cityID := _MASKOFA_JADWALSHALAT_CITYIDDEFAULT;


  if getData(cityID, ADateTime) = False then
    Exit;

  s := FormatDateTime('d', ADateTime);
  Result := Result + #10'Shubuh: ' + jsonGetData(jsonData, 'data/' + s + '/Shubuh');
  Result := Result + #10'Dzuhur: ' + jsonGetData(jsonData, 'data/' + s + '/Dzuhur');
  Result := Result + #10'Ashr: ' + jsonGetData(jsonData, 'data/' + s + '/Ashr');
  Result := Result + #10'Maghrib: ' + jsonGetData(jsonData, 'data/' + s + '/Maghrib');
  Result := Result + #10'Isya: ' + jsonGetData(jsonData, 'data/' + s + '/Isya');

  Result := Trim(Result);
end;

end.

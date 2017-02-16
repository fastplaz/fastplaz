unit kawalpemilu_integration;

{
  ref:
    https://www.kawalpilkada.id/#/tabulasi.html/Provinsi/2017

  Kode Province:
    dki jakarta: 25823

  [x] USAGE

  // textual information
  with TKawalPemiluIntegration.Create do
  begin
    Result := ProvinceRealCountInfo(provinceCode);
    Free;
  end;

  // full json data
  with TKawalPemiluIntegration.Create do
  begin
    Result := ProvinceRealCount(provinceCode);
    Free;
  end;


}

{$mode objfpc}{$H+}


interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson, jsonparser,
  Classes, SysUtils;

type

  { TKawalPemiluIntegration }

  TKawalPemiluIntegration = class(TInterfacedObject)
  private
    FResultCode: integer;
    FResultText: string;
    jsonData: TJSONData;
    function getData(APath: string): string;
    function getDataAsInteger(APath: string): integer;
  public
    constructor Create;
    destructor Destroy;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    function ProvinceRealCount(AProvinceCode: string): string;
    function ProvinceRealCountInfo(AProvinceCode: string): string;
  end;


implementation

const
  KAWALPEMILU_PROVINCE_REALCOUNT_URL =
    'https://www.kawalpilkada.id/kandidat/refreshagregasi/2017/Provinsi/%s';

var
  Response: IHTTPResponse;

{ TKawalPemiluIntegration }

function TKawalPemiluIntegration.getData(APath: string): string;
begin
  Result := '';
  try
    Result := jsonData.GetPath(APath).AsString;
  except
  end;
end;

function TKawalPemiluIntegration.getDataAsInteger(APath: string): integer;
begin
  Result := 0;
  try
    Result := jsonData.GetPath(APath).AsInteger;
  except
  end;
end;

constructor TKawalPemiluIntegration.Create;
begin

end;

destructor TKawalPemiluIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TKawalPemiluIntegration.ProvinceRealCount(AProvinceCode: string): string;
begin
  Result := '';
  if AProvinceCode = '' then
    Exit;

  with THTTPLib.Create(format(KAWALPEMILU_PROVINCE_REALCOUNT_URL, [AProvinceCode])) do
  begin
    try
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      if FResultCode = 200 then
      begin
        Result := FResultText;
        jsonData := GetJSON(Result);
      end;
    except
    end;
    Free;
  end;

end;

function TKawalPemiluIntegration.ProvinceRealCountInfo(AProvinceCode: string): string;
var
  suara1, suara2, suara3, total: integer;
  percent1, percent2, percent3: double;
  s: string;
  paslon1, paslon2, paslon3: string;
begin
  Result := ProvinceRealCount(AProvinceCode);

  if Result = '' then
    Exit;

  if getData('[1].tahun') = '' then
  begin
    Result := 'Maaf, data belum tersedia.';
    Exit;
  end;

  Result := 'Update Real Count: ' + getData('[1].nama');

  Result := Result + #10#10 + 'Calon:```';
  Result := Result + #10 + '1. ' + getData('[1].suaraKandidat.1.nama');
  Result := Result + #10 + '2. ' + getData('[1].suaraKandidat.2.nama');
  Result := Result + #10 + '3. ' + getData('[1].suaraKandidat.3.nama');
  Result := Result + '```';

  Result := Result + #10#10 + getData('[1].jumlahTPSdilockHC') +
    ' dari ' + getData('[1].jumlahTPS') + ' TPS';
  Result := Result + #10 + 'Suara Sah: ' + getData('[1].suarasahHC');
  Result := Result + #10 + 'Suara Tidak Sah: ' + getData('[1].suaratidaksahHC');


  // TPS
  Result := Result + #10 + '\n*Suara TPS:*';
  suara1 := getDataAsInteger('[1].suaraKandidat.1.suaraTPS');
  suara2 := getDataAsInteger('[1].suaraKandidat.2.suaraTPS');
  suara3 := getDataAsInteger('[1].suaraKandidat.3.suaraTPS');

  Result := Result + #10 + '1. ' + FormatFloat('###,##0', suara1);
  Result := Result + #10 + '2. ' + FormatFloat('###,##0', suara2);
  Result := Result + #10 + '3. ' + FormatFloat('###,##0', suara3);

  // C1
  Result := Result + #10 + '\n*Verifikasi C1:*';
  suara1 := getDataAsInteger('[1].suaraKandidat.1.suaraVerifikasiC1');
  suara2 := getDataAsInteger('[1].suaraKandidat.2.suaraVerifikasiC1');
  suara3 := getDataAsInteger('[1].suaraKandidat.3.suaraVerifikasiC1');
  total := suara1 + suara2 + suara3;
  if total > 0 then
  begin
    percent1 := suara1 * 100 / total;
    percent2 := suara2 * 100 / total;
    percent3 := suara3 * 100 / total;
  end;
  s := ' (' + FormatFloat('##,##0.00', percent1) + ')';
  Result := Result + #10 + '1. ' + FormatFloat('##,##0', suara1) + s;
  s := ' (' + FormatFloat('##,##0.00', percent2) + ')';
  Result := Result + #10 + '2. ' + FormatFloat('##,##0', suara2) + s;
  s := ' (' + FormatFloat('##,##0.00', percent3) + ')';
  Result := Result + #10 + '3. ' + FormatFloat('##,##0', suara3) + s;

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

end.

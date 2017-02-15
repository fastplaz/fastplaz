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
begin
  Result := ProvinceRealCount(AProvinceCode);

  if Result = '' then
    Exit;

  Result := 'Update Real Count:';
  Result := Result + #10 + getData('[1].jumlahTPSdilockHC') + ' dari ' +
    getData('[1].jumlahTPS') + ' TPS';
  Result := Result + #10 + 'Suara Sah: ' + getData('[1].suarasahHC');
  Result := Result + #10 + 'Suara Tidak Sah: ' + getData('[1].suaratidaksahHC');


  Result := Result + #10 + '\n*Suara TPS:*';
  Result := Result + #10 + 'Agus-Sylvi: ' + getData('[1].suaraKandidat.1.suaraTPS');
  Result := Result + #10 + 'Ahok-Djarot: ' + getData('[1].suaraKandidat.2.suaraTPS');
  Result := Result + #10 + 'Anies-Sandi: ' + getData('[1].suaraKandidat.3.suaraTPS');

  Result := Result + #10 + '\n*Verifikasi C1:*';
  Result := Result + #10 + 'Agus-Sylvi: ' +
    getData('[1].suaraKandidat.1.suaraVerifikasiC1');
  Result := Result + #10 + 'Ahok-Djarot: ' +
    getData('[1].suaraKandidat.2.suaraVerifikasiC1');
  Result := Result + #10 + 'Anies-Sandi: ' +
    getData('[1].suaraKandidat.3.suaraVerifikasiC1');

  {
  try
    Result := jsonData.GetPath('[1].suaraKandidat').AsJSON;
  except
    on E : Exception do
      die( E.Message);
  end;
  }

  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

end.

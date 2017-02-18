unit ibacorpolicenumber_integration;

{
  // USAGE:
  with TPajakKendaraanIbacorIntegration.Create do
  begin
    Result := Find('policenumber');
    Free;
  end;

}

{$mode objfpc}{$H+}

interface

uses
  fpjson, RegExpr,
  common, http_lib,
  Classes, SysUtils;

type

  { TPajakKendaraanIbacorIntegration }

  TPajakKendaraanIbacorIntegration = class(TInterfacedObject)
  private
    FResultCode: integer;
    FResultText: string;
    jsonData: TJSONData;
    function getData(APath: string): string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    function Find(Text: string): string;
  end;


implementation

const
  _IBACOR_PAJAKKENDARAAN_URL =
    'http://ibacor.com/api/pajak-kendaraan?kode=%s&nomor=%s&seri=%s';

var
  Response: IHTTPResponse;

{ TPajakKendaraanIbacorIntegration }

function TPajakKendaraanIbacorIntegration.getData(APath: string): string;
begin
  Result := '';
  try
    Result := jsonData.GetPath(APath).AsString;
  except
  end;
end;

constructor TPajakKendaraanIbacorIntegration.Create;
begin
end;

destructor TPajakKendaraanIbacorIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TPajakKendaraanIbacorIntegration.Find(Text: string): string;
var
  kode, nomor, seri, url: string;
begin
  Text := UpperCase(StringReplace(Text, ' ', '', [rfReplaceAll]));
  if Text = '' then
    Exit;
  try
    with TRegExpr.Create do
    begin
      Expression := '([A-Z]{1,3})([0-9]{1,4})([A-Z]{1,3})';
      if Exec(Text) then
      begin
        kode := Match[1];
        nomor := Match[2];
        seri := Match[3];

        url := Format(_IBACOR_PAJAKKENDARAAN_URL, [kode, nomor, seri]);
        with THTTPLib.Create(url) do
        begin
          Response := Get;
          FResultCode := Response.ResultCode;
          FResultText := Response.ResultText;
          if Response.ResultCode = 200 then
          begin
            jsonData := GetJSON(Response.ResultText);
            if getData('status') = 'success' then
            begin
              Result := '*' + getData('data.nopol') + '*';
              Result := Result + #10'Jenis:' + getData('data.kendaraan.jenis');
              Result := Result + #10'Merk: ' + getData('data.kendaraan.merk');
              Result := Result + #10'Type: ' + getData('data.kendaraan.type');
              Result := Result + #10'Tahun:' + getData('data.kendaraan.tahun_pembuatan');
              Result := Result + #10'Warna:' + getData('data.kendaraan.warna');
              Result := Result + #10'Wilayah:' + getData('data.kendaraan.wilayah');
              Result := Result + #10'*Jatuh Tempo PKB:' + getData('data.pkb.jatuh_tempo') + '*';
              Result := Result + #10#10'_sumber:' + getData('sumber') + '_';
            end;
          end;
          Free;
        end;
      end;
      Free;
    end;

  except
  end;
end;

end.

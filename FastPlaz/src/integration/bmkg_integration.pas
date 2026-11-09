unit bmkg_integration;

{ DEPRECATED: old BMKG data endpoints no longer exist. The unit stays for existing callers; every request to the
  old endpoint fails. }
{

  DEPRECATED:
    no update from bmkg data
  -----------------------------------

  INFO GEMPA dari BMKG

  [x] USAGE

  with TBMKGIntegration.Create do
  begin
    Result := SimpleInfo;
    Free;
  end;


}
{$mode objfpc}{$H+}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson, jsonparser,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

type

  { TBMKGIntegration }

  TBMKGIntegration = class(TInterfacedObject)
  private
    FResultCode: integer;
    FResultText: string;
  public
    constructor Create;
    destructor Destroy; override;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    function SimpleInfo: string;
    function InfoGempaXML: string;
  end deprecated 'old BMKG data endpoints no longer exist';


implementation

const
  _BMKG_DATA_URL = 'https://data.bmkg.go.id/data.txt';
  _BMKG_INFOGEMPA_URL = 'https://data.bmkg.go.id/autogempa.xml';


{ TBMKGIntegration }

constructor TBMKGIntegration.Create;
begin

end;

destructor TBMKGIntegration.Destroy;
begin
  inherited Destroy;
end;

function TBMKGIntegration.SimpleInfo: string;
var
  Response: IHTTPResponse;
begin
  Result := '';
  with THTTPLib.Create(_BMKG_DATA_URL) do
  try
    //AddHeader('Cache-Control', 'no-cache');
    Response := Get;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      Result := FResultText;
    end;
  finally
    Free;
  end;
end;

function TBMKGIntegration.InfoGempaXML: string;
var
  Response: IHTTPResponse;
begin
  Result := '';
  with THTTPLib.Create(_BMKG_INFOGEMPA_URL) do
  try
    //AddHeader('Cache-Control', 'no-cache');
    Response := Get;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      Result := FResultText;
    end;
  finally
    Free;
  end;
end;

end.

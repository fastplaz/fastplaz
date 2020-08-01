unit bmkg_integration;
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
    destructor Destroy;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    function SimpleInfo: string;
    function InfoGempaXML: string;
  end;


implementation

const
  _BMKG_DATA_URL = 'http://data.bmkg.go.id/data.txt';
  _BMKG_INFOGEMPA_URL = 'http://data.bmkg.go.id/autogempa.xml';

var
  Response: IHTTPResponse;

{ TBMKGIntegration }

constructor TBMKGIntegration.Create;
begin

end;

destructor TBMKGIntegration.Destroy;
begin

end;

function TBMKGIntegration.SimpleInfo: string;
begin
  Result := '';
  with THTTPLib.Create(_BMKG_DATA_URL) do
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

function TBMKGIntegration.InfoGempaXML: string;
begin
  Result := '';
  with THTTPLib.Create(_BMKG_INFOGEMPA_URL) do
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

end.

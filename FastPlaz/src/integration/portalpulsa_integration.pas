{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit portalpulsa_integration;

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, fpjson, logutil_lib, fphttpclient,
  Classes, SysUtils;

type

  { TPortalPulsaIntegration }

  TPortalPulsaIntegration = class
  private
    FCode: string;
    FDebug: boolean;
    FInquiry: string;
    FKey: string;
    FMessage: string;
    FNumber: integer;
    FPhoneNumber: string;
    FResultCode: integer;
    FResultText: string;
    FSecret: string;
    FTransactionID: string;
    FUserID: string;
    jsonData: TJSONData;
    function canAccess: boolean;
    function getSaldo: double;
    function customPost: boolean;
    function getPrefix(APhoneNumber: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function IsiPulsa(APhoneNumber: string; AValue: string): boolean;
    function Status(ATransactionID: string): boolean;
  published
    property Debug: boolean read FDebug write FDebug;
    property UserID: string read FUserID write FUserID;
    property Key: string read FKey write FKey;
    property Secret: string read FSecret write FSecret;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;
    property Message: string read FMessage;

    property Inquiry: string read FInquiry write FInquiry;
    property Code: string read FCode write FCode;
    property PhoneNumber: string read FPhoneNumber write FPhoneNumber;
    property TransactionID: string read FTransactionID write FTransactionID;
    property Number: integer read FNumber write FNumber;

    property Saldo: double read getSaldo;
  end;

implementation

const
  PORTALPULSA_API_URL = 'http://portalpulsa.com/api/connect/';

var
  Response: IHTTPResponse;

{ TPortalPulsaIntegration }

function TPortalPulsaIntegration.canAccess: boolean;
begin
  Result := False;
  if (FUserID = '') or (FKey = '') or (FSecret = '') then
    Exit;
  Result := True;
end;

function TPortalPulsaIntegration.customPost: boolean;
var
  params: TStringList;
  http: TFPHTTPClient;
begin
  Result := False;
  if not canAccess then
    Exit;

  FMessage := '';
  http := TFPHTTPClient.Create(nil);
  params := TStringList.Create;
  try
    http.AddHeader('portal-userid', FUserID);
    http.AddHeader('portal-key', FKey);
    http.AddHeader('portal-secret', FSecret);
    params.Values['inquiry'] := FInquiry;
    params.Values['code'] := FCode;
    params.Values['phone'] := FPhoneNumber;
    params.Values['trxid_api'] := FTransactionID;
    params.Values['no'] := i2s(FNumber);
    FResultText := http.FormPost(PORTALPULSA_API_URL, params);
    FResultCode := http.ResponseStatusCode;
    if FResultCode = 200 then
    begin
      jsonData := GetJSON(FResultText);
      FMessage := jsonGetData(jsonData, 'message');
      Result := True;
    end;
  except
    on E: Exception do
    begin
      if FDebug then
        LogUtil.Add(E.Message, 'portalpulsa');
    end;
  end;
  params.Free;
  http.Free;

end;

function TPortalPulsaIntegration.getPrefix(APhoneNumber: string): string;
var
  s: string;
begin
  Result := '';
  s := Copy(APhoneNumber, 0, 3);
  if s = '089' then
    Result := 'T';

  s := Copy(APhoneNumber, 0, 4);

  // Telkomsel
  if StrInArray(s, ['0811', '0812', '0813', '0821', '0822', '0823', '0851', '0852', '0853']) then
    Result := 'S';

  // XL Axiata
  if StrInArray(s, ['0817','0818','0819','0859','0877','0878']) then
    Result := 'X';

  // Indosat
  if StrInArray(s, ['0855','0856','0857','0858','0814','0815','0816']) then
    Result := 'I';

end;

constructor TPortalPulsaIntegration.Create;
begin
  FDebug := False;
  FInquiry := 'I';
  FTransactionID := '';
  FNumber := 1;
  FMessage := '';
end;

destructor TPortalPulsaIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
  inherited Destroy;
end;

function TPortalPulsaIntegration.IsiPulsa(APhoneNumber: string;
  AValue: string): boolean;
begin
  Result := False;
  FInquiry := 'I';
  FCode := getPrefix(APhoneNumber) + AValue;
  FPhoneNumber := APhoneNumber;
  FNumber := 1;

  if not customPost then
    Exit;

  if jsonGetData(jsonData, 'result') = 'success' then
  begin

    Result := True;
  end;

end;

function TPortalPulsaIntegration.Status(ATransactionID: string): boolean;
begin
  Result := False;
  FInquiry := 'STATUS';
  FTransactionID := ATransactionID;

  if not customPost then
    Exit;

  if jsonGetData(jsonData, 'result') = 'success' then
  begin
    //todo: get status
    FMessage := jsonGetData(jsonData, 'message/note');
    Result := True;
  end;
end;

function TPortalPulsaIntegration.getSaldo: double;
var
  s: string;
begin
  Result := 0;
  if not canAccess then
    Exit;

  FInquiry := 'S';
  if not customPost then
    Exit;

  if jsonGetData(jsonData, 'result') = 'success' then
  begin
    s := jsonGetData(jsonData, 'balance');
    Result := s2f(s);
  end;
end;

end.



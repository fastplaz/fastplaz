{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit googleanalytics_integration;

{
  Google Analytics
  https://analytics.google.com/

  [x] USAGE

  with TGoogleAnalyticsIntegration.Create do
  begin
    TrackingID := 'your-tracking-id';
    ClientID := 'your-client-id';
    HitType := 'event';
    Payloads['an'] := 'CarikBot';
    Payloads['ai'] := 'bot';
    Payloads['av'] := '0';
    Payloads['ec'] := ACategory;
    Payloads['ea'] := AAction;
    Payloads['el'] := ALabel;
    Send;
    Free;
  end;


}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  common,
  http_lib,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

{$ifdef GOOGLEANALYTICS_INTEGRATION}
{$endif}

type

  { TGoogleAnalyticsIntegration }

  TGoogleAnalyticsIntegration = class(TInterfacedObject)
  private
    FClientID: string;
    FHitType: string;
    FPayLoads: TStringList;
    FIsSuccessfull: boolean;
    FTrackingID: string;
    FURL: string;
    FVersion: string;
    function GetPayloads(AName: string): string;
    procedure SetPayloads(AName: string; AValue: string);
  public
    constructor Create;
    destructor Destroy;
    property Payloads[AName: string]: string read GetPayloads write SetPayloads; default;
    function Send: boolean;
  published
    property URL: string read FURL;
    property IsSuccessfull: boolean read FIsSuccessfull;

    property Version: string read FVersion write FVersion;
    property TrackingID: string read FTrackingID write FTrackingID;
    property ClientID: string read FClientID write FClientID;
    property HitType: string read FHitType write FHitType;
  end;

implementation

const
  GOOGLEANALYTICS_BASEURL = 'https://www.google-analytics.com/collect?payload_data&';

var
  Response: IHTTPResponse;

{ TGoogleAnalyticsIntegration }

function TGoogleAnalyticsIntegration.GetPayloads(AName: string): string;
begin
  Result := FPayLoads.Values[AName];
end;

procedure TGoogleAnalyticsIntegration.SetPayloads(AName: string; AValue: string);
begin
  FPayLoads.Values[AName] := AValue;
end;

constructor TGoogleAnalyticsIntegration.Create;
begin
  FIsSuccessfull := False;
  FPayLoads := TStringList.Create;

  FVersion := '1';
  FHitType := 'event';
  Payloads['v'] := FVersion;
  Payloads['tid'] := FTrackingID;
  Payloads['cid'] := FClientID;
  Payloads['t'] := FHitType;
end;

destructor TGoogleAnalyticsIntegration.Destroy;
begin
  FPayLoads.Free;
end;

function TGoogleAnalyticsIntegration.Send: boolean;
var
  i: integer;
  payloadURL: string;
  httpResponse: IHTTPResponse;
begin
  Result := False;
  FIsSuccessfull := False;

  if FTrackingID = '' then
    Exit;
  if FClientID = '' then
    Exit;

  Payloads['v'] := FVersion;
  Payloads['tid'] := FTrackingID;
  Payloads['cid'] := FClientID;
  Payloads['t'] := FHitType;

  FPayLoads.Text := Trim(FPayLoads.Text);
  for i := 0 to FPayLoads.Count - 1 do
  begin
    payloadURL := payloadURL + FPayLoads.Names[i] + '=' +
      FPayLoads.ValueFromIndex[i] + '&';
  end;
  payloadURL := Trim(GOOGLEANALYTICS_BASEURL + payloadURL);
  FURL := payloadURL;

  with THTTPLib.Create(payloadURL) do
  begin
    httpResponse := Post;
    if httpResponse.ResultCode = 200 then
    begin
      FIsSuccessfull := True;
      Result := True;
    end;
    Free;
  end;

end;


end.


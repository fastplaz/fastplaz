unit slack_integration;

{
  USAGE

  with TSlackWebhookIntegration.Create do
  begin
    URL := 'https://hooks.slack.com/services/_________/_________/_________';
    Channel := '#random';
    Username := 'FastPlaz';
    Text := 'Hellooww Word from FastPlaz to Slack';
    Icon := ':ghost:';
    Response := Send;

    log('-- result code: ' + IntToStr(Response.ResultCode));
    log('-- response time: ' + IntToStr(Response.ResponseTime) + 'ms');

    if IsSuccessfull then
    begin
      //
    end;
    Free;
  end;

}

{$mode objfpc}{$H+}
{$include ../../define.inc}


interface

uses
  http_lib,
  Classes, SysUtils;

{$ifdef SLACK_INTEGRATION}

type

  { TSlackWebhookIntegration }

  TSlackWebhookIntegration = class(TInterfacedObject)
  private
    FURL: string;
    FChannel, FUsername, FText, FPayload, FIcon: string;
    FIsSuccessfull: boolean;
    function getChannel: string;
    function getIcon: string;
    function getIsSuccessfull: boolean;
    function getPayload: string;
    function getText: string;
    function getURL: string;
    function getUsername: string;
    procedure setChannel(AValue: string);
    procedure setIcon(AValue: string);
    procedure setPayload(AValue: string);
    procedure setText(AValue: string);
    procedure setURL(AValue: string);
    procedure setUsername(AValue: string);
    procedure generatePayload;
  public
    constructor Create(URL: string);
    constructor Create;
    function Send: IHTTPResponse;
  published
    property URL: string read getURL write setURL;
    property Channel: string read getChannel write setChannel;
    property Username: string read getUsername write setUsername;
    property Text: string read getText write setText;
    property Payload: string read getPayload write setPayload;
    property Icon: string read getIcon write setIcon;
    property IsSuccessfull: boolean read getIsSuccessfull;
  end;

{$endif}

implementation

{$ifdef SLACK_INTEGRATION}

{ TSlackWebhookIntegration }

function TSlackWebhookIntegration.getURL: string;
begin
  Result := FURL;
end;

function TSlackWebhookIntegration.getChannel: string;
begin
  Result := FChannel;
end;

function TSlackWebhookIntegration.getIcon: string;
begin
  Result := FIcon;
end;

function TSlackWebhookIntegration.getIsSuccessfull: boolean;
begin
  Result := FIsSuccessfull;
end;

function TSlackWebhookIntegration.getPayload: string;
begin
  Result := FPayload;
end;

function TSlackWebhookIntegration.getText: string;
begin
  Result := FText;
end;

function TSlackWebhookIntegration.getUsername: string;
begin
  Result := FUsername;
end;

procedure TSlackWebhookIntegration.setChannel(AValue: string);
begin
  FChannel := AValue;
end;

procedure TSlackWebhookIntegration.setIcon(AValue: string);
begin
  FIcon := AValue;
end;

procedure TSlackWebhookIntegration.setPayload(AValue: string);
begin
  FPayload := AValue;
end;

procedure TSlackWebhookIntegration.setText(AValue: string);
begin
  FText := AValue;
end;

procedure TSlackWebhookIntegration.setURL(AValue: string);
begin
  FURL := AValue;
end;

procedure TSlackWebhookIntegration.setUsername(AValue: string);
begin
  FUsername := AValue;
end;

procedure TSlackWebhookIntegration.generatePayload;
var
  s: string;
begin
  s := '{';
  if FChannel <> '' then
    s := s + '"channel":' + '"' + FChannel + '"';
  if FUsername <> '' then
    s := s + ',"username":' + '"' + FUsername + '"';
  if FText <> '' then
    s := s + ',"text":' + '"' + FText + '"';
  if FIcon <> '' then
    s := s + ',"icon_emoji":' + '"' + FIcon + '"';
  s := s + '}';
  Payload := s;
end;

constructor TSlackWebhookIntegration.Create(URL: string);
begin
  setURL(URL);
  FIsSuccessfull := False;
end;

constructor TSlackWebhookIntegration.Create;
begin
  FIsSuccessfull := False;
end;

function TSlackWebhookIntegration.Send: IHTTPResponse;
begin
  if URL = '' then
    Exit;

  generatePayload;
  FIsSuccessfull := False;
  with THTTPLib.Create(URL) do
  begin
    FormData['payload'] := FPayload;
    Result := Post;
    FIsSuccessfull := IsSuccessfull;
    Free;
  end;
end;

{$endif}

end.



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
{  $include ../../define.inc}


interface

uses
  http_lib, fpjson,
  Classes, SysUtils;

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
  public
    constructor Create(URL: string);
    constructor Create;
    // fills Payload from Channel, Username, Text and Icon
    procedure GeneratePayload;
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

implementation

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

procedure TSlackWebhookIntegration.GeneratePayload;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  try
    if FChannel <> '' then
      o.Add('channel', FChannel);
    if FUsername <> '' then
      o.Add('username', FUsername);
    if FText <> '' then
      o.Add('text', FText);
    if FIcon <> '' then
      o.Add('icon_emoji', FIcon);
    Payload := o.AsJSON;
  finally
    o.Free;
  end;
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

  GeneratePayload;
  FIsSuccessfull := False;
  with THTTPLib.Create(URL) do
  try
    FormData['payload'] := FPayload;
    Result := Post;
    FIsSuccessfull := IsSuccessfull;
  finally
    Free;
  end;
end;

end.



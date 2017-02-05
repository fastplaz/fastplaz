unit facebookmessenger_integration;

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson,
  Classes, SysUtils;

type

  { TFacebookMessengerIntegration }

  TFacebookMessengerIntegration = class(TInterfacedObject)
  private
    FBotName: string;
    FIsSuccessfull: boolean;
    FRequestContent: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;

    jsonData: TJSONData;
    function getText: string;
    function getUserID: string;
    procedure setRequestContent(AValue: string);
  public
    constructor Create;
    destructor Destroy;

    property BotName: string read FBotName write FBotName;
    property Token: string read FToken write FToken;
    property RequestContent: string read FRequestContent write setRequestContent;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Text: string read getText;
    property UserID: string read getUserID;

    procedure Send(ATo: string; AMessages: string);

    function isCanSend: boolean;
    function isMessage: boolean;
  end;



implementation

const
  _FACEBOOK_MESSENGER_SEND_URL =
    'https://graph.facebook.com/v2.6/me/messages?access_token=';
  _FACEBOOK_MESSENGER_SEND_JSON =
    '{ "recipient":{"id":"%s" }, "message":{ "text":"%s" }}';

var
  Response: IHTTPResponse;

{ TFacebookMessengerIntegration }

procedure TFacebookMessengerIntegration.setRequestContent(AValue: string);
begin
  if FRequestContent = AValue then
    Exit;
  FRequestContent := AValue;
  jsonData := GetJSON(AValue);
end;

function TFacebookMessengerIntegration.getText: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('entry[0].messaging[0].message.text').AsString;
  except
  end;
end;

function TFacebookMessengerIntegration.getUserID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('entry[0].messaging[0].sender.id').AsString;
  except
  end;
end;

constructor TFacebookMessengerIntegration.Create;
begin

end;

destructor TFacebookMessengerIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

procedure TFacebookMessengerIntegration.Send(ATo: string; AMessages: string);
var
  s: string;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (AMessages = '') then
    Exit;

  with THTTPLib.Create(_FACEBOOK_MESSENGER_SEND_URL + FToken) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      s := Format(_FACEBOOK_MESSENGER_SEND_JSON, [ATo, StringToJSONString(AMessages)]);
      RequestBody := TStringStream.Create(s);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;

      die(FResultText);
    except
    end;

    Free;
  end;

end;

function TFacebookMessengerIntegration.isCanSend: boolean;
begin
  Result := False;
  if FToken = '' then
    Exit;
  Result := True;
end;

function TFacebookMessengerIntegration.isMessage: boolean;
begin
  Result := False;

  // ...

end;

end.

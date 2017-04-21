unit facebookmessenger_integration;

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, logutil_lib, json_lib,
  fpjson, strutils,
  Classes, SysUtils;

type

  { TFacebookTemplateElement }

  TFacebookTemplateElement = class
  private
    FActionURL: string;
    FData: TJSONUtil;
    FImageURL: string;
    FSubTitle: string;
    FTitle: string;
    function getAsJSON: string;
    procedure generateData;
  public
    constructor Create;
    destructor Destroy;
  published
    property Title: string read FTitle write FTitle;
    property SubTitle: string read FSubTitle write FSubTitle;
    property ImageURL: string read FImageURL write FImageURL;
    property ActionURL: string read FActionURL write FActionURL;
    property Data: TJSONUtil read FData write FData;

    property AsJSON: string read getAsJSON;
  end;

  { TFacebookTemplateMessage }

  TFacebookTemplateMessage = class
  private
  public
    constructor Create;
    destructor Destroy;
  published
  end;

  { TFacebookMessengerIntegration }

  TFacebookMessengerIntegration = class(TInterfacedObject)
  private
    FBotName: string;
    FImageCaption: string;
    FImageID: string;
    FImageURL: string;
    FIsSuccessfull: boolean;
    FRequestContent: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;

    jsonData: TJSONData;
    function getIsVoice: boolean;
    function getMessageID: string;
    function getText: string;
    function getUserID: string;
    function getVoiceURL: string;
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
    property MessageID: string read getMessageID;

    procedure Send(ATo: string; AMessages: string);
    procedure SendAudio(ATo: string; AAudioURL: string);
    procedure SendImage(ATo: string; AImageURL: string);
    procedure AskLocation(ATo: string);

    function isCanSend: boolean;
    function isMessage: boolean;
    function isImage(ADetail: boolean = False): boolean;

    property IsVoice: boolean read getIsVoice;
    property VoiceURL: string read getVoiceURL;
    function DownloadVoiceTo(ATargetFile: string): boolean;

  published
    property ImageID: string read FImageID;
    property ImageURL: string read FImageURL;
    property ImageCaption: string read FImageCaption;
  end;



implementation

const
  _FACEBOOK_MSG_MAXLENGTH = 635;
  _FACEBOOK_MSG_SHARE_LOCATION = 'Share lokasi Anda:';
  _FACEBOOK_MESSENGER_SEND_URL =
    'https://graph.facebook.com/v2.6/me/messages?access_token=';
  _FACEBOOK_MESSENGER_SEND_JSON =
    '{ "recipient":{"id":"%s" }, "message":{ "text":"%s" }}';
  _FACEBOOK_MESSENGER_SEND_AUDIO_JSON =
    '{"recipient":{"id":"%s"},"message":{"attachment":{"type":"audio","payload":{"url":"%s"}}}}';
  _FACEBOOK_MESSENGER_SEND_IMAGE_JSON =
    '{"recipient":{"id":"%s"},"message":{"attachment":{"type":"image","payload":{"url":"%s"}}}}';
  _FACEBOOK_MESSENGER_ASK_LOCATION =
    '{"recipient": {"id": "%s"},"message": {"text": "%s","quick_replies": [{"content_type": "location"}]}}';

var
  Response: IHTTPResponse;

{ TFacebookTemplateElement }

function TFacebookTemplateElement.getAsJSON: string;
begin
  generateData;
  Result := FData.AsJSONFormated;
end;

constructor TFacebookTemplateElement.Create;
begin
  FData := TJSONUtil.Create;
  FData['title'] := '';
  FData['subtitle'] := '';
  FData['image_url'] := '';
  FData['default_action/type'] := 'web_url';
  FData['default_action/url'] := '';
  FData['default_action/messenger_extensions'] := true;
  FData['default_action/webview_height_ratio'] := 'tall';
  FData['default_action/fallback_url'] := '';
end;

destructor TFacebookTemplateElement.Destroy;
begin
  FData.Free;
end;

procedure TFacebookTemplateElement.generateData;
begin
  FData['title'] := FTitle;
  FData['subtitle'] := FSubTitle;
  FData['image_url'] := FImageURL;
  FData['default_action/type'] := 'web_url';
  FData['default_action/url'] := FActionURL;
  FData['default_action/messenger_extensions'] := true;
  FData['default_action/webview_height_ratio'] := 'tall';
  FData['default_action/fallback_url'] := '';
end;

{ TFacebookTemplateMessage }

constructor TFacebookTemplateMessage.Create;
begin

end;

destructor TFacebookTemplateMessage.Destroy;
begin

end;

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

function TFacebookMessengerIntegration.getIsVoice: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('entry[0].messaging[0].message.attachments[0].type').AsString =
      'audio' then
      Result := True;
  except
  end;
end;

function TFacebookMessengerIntegration.getMessageID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('entry[0].messaging[0].message.mid').AsString;
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

function TFacebookMessengerIntegration.getVoiceURL: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath(
      'entry[0].messaging[0].message.attachments[0].payload.url').AsString;
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
  inherited;
end;

procedure TFacebookMessengerIntegration.Send(ATo: string; AMessages: string);
var
  posSplit: integer;
  s: string;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (AMessages = '') then
    Exit;

  posSplit := 0;
  s := AMessages;
  if Length(AMessages) > _FACEBOOK_MSG_MAXLENGTH then
  begin
    s := Copy(AMessages, 0, _FACEBOOK_MSG_MAXLENGTH);
    posSplit := RPos(' ', s);
    s := Copy(s, 0, posSplit) + '...';
  end;

  with THTTPLib.Create(_FACEBOOK_MESSENGER_SEND_URL + FToken) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      s := Format(_FACEBOOK_MESSENGER_SEND_JSON, [ATo, StringToJSONString(s)]);
      RequestBody := TStringStream.Create(s);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;

      if FResultCode = 200 then
      begin
        if posSplit > 0 then
        begin
          s := '...' + Copy(AMessages, posSplit);
          if Length(s) > _FACEBOOK_MSG_MAXLENGTH then
            s := Copy(s, 0, _FACEBOOK_MSG_MAXLENGTH) + ' ...';
          Send(ATo, s);
        end;
      end;

    except
    end;

    Free;
  end;
end;

procedure TFacebookMessengerIntegration.SendAudio(ATo: string; AAudioURL: string);
var
  s: string;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (AAudioURL = '') then
    Exit;

  with THTTPLib.Create(_FACEBOOK_MESSENGER_SEND_URL + FToken) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      s := Format(_FACEBOOK_MESSENGER_SEND_AUDIO_JSON,
        [ATo, StringToJSONString(AAudioURL)]);
      RequestBody := TStringStream.Create(s);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;

    Free;
  end;
end;

procedure TFacebookMessengerIntegration.SendImage(ATo: string; AImageURL: string);
var
  s: string;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (AImageURL = '') then
    Exit;

  with THTTPLib.Create(_FACEBOOK_MESSENGER_SEND_URL + FToken) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      s := Format(_FACEBOOK_MESSENGER_SEND_IMAGE_JSON, [ATo, AImageURL]);
      RequestBody := TStringStream.Create(s);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;

    Free;
  end;
end;

procedure TFacebookMessengerIntegration.AskLocation(ATo: string);
var
  s: string;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (FToken = '') then
    Exit;

  with THTTPLib.Create(_FACEBOOK_MESSENGER_SEND_URL + FToken) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      s := Format(_FACEBOOK_MESSENGER_ASK_LOCATION,
        [ATo, _FACEBOOK_MSG_SHARE_LOCATION]);
      RequestBody := TStringStream.Create(s);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
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

function TFacebookMessengerIntegration.isImage(ADetail: boolean): boolean;
begin
  Result := False;
  FImageURL := '';
  try
    if jsonData.GetPath('entry[0].messaging[0].message.attachments[0].type').AsString =
      'image' then
      Result := True;
    FImageURL := jsonData.GetPath(
      'entry[0].messaging[0].message.attachments[0].payload.url').AsString;
    FImageCaption := jsonData.GetPath('entry[0].messaging[0].message.text').AsString;
    FImageID := jsonData.GetPath('entry[0].messaging[0].message.mid').AsString;
  except
  end;
end;

function TFacebookMessengerIntegration.DownloadVoiceTo(ATargetFile: string): boolean;
begin
  Result := False;
  if VoiceURL = '' then
    Exit;

  with THTTPLib.Create(VoiceURL) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FIsSuccessfull := IsSuccessfull;
      if FResultCode = 200 then
      begin
        if FileExists(ATargetFile) then
          DeleteFile(ATargetFile);
        Response.ResultStream.SaveToFile(ATargetFile);
        Result := True;
      end;
    except
      on E: Exception do
      begin
        LogUtil.Add('VOICE-DL: ' + E.Message, 'FACEBOOK');
      end;
    end;
    Free;
  end;

end;

end.

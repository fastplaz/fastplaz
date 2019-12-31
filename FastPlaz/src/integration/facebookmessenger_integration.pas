{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
{
  [x] USAGE

  Facebook := TFacebookMessengerIntegration.Create;
  Facebook.BotName := 'YOUR_BOT_NAME';
  Facebook.Token := 'FACEBOOK_TOKEN';
  Facebook.Send('facebookID, 'text');

  [x] Send PhoneCall Dialog
  Facebook.SendCall('user_id', '+62..........', 'Call Name', 'Your Description');

  [x] Send Button URL
  Facebook.SendButtonURL('user_id', 'title', 'https://your_url', 'Your Description');


  [x] Payload Handler

  function yourpayloadHandler(const APayload, ATitle: String): String;


  Facebook.PayloadHandler['YOUR_PAYLOAD'] := @yourpayloadHandler;
  Text := Facebook.PayloadHandling;

  [x] Quick Replay

  Facebook.QuickReply.AddLocation;
  Facebook.QuickReply.AddEmail;
  Facebook.QuickReply.AddPhone;
  Facebook.QuickReply.AddText('THE TEXT', 'YOUR_PAYLOAD', 'https://your_image.png');
  Facebook.SendQuickReply(Facebook.UserID);


}
unit facebookmessenger_integration;

{$mode objfpc}{$H+}


interface

uses
  common, http_lib, logutil_lib, json_lib,
  fpjson, strutils, fgl, Classes, SysUtils;

type

  generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
  TPayloadHandlerCallback = function(const APayload, ATitle: string): string of object;
  TPayloadHandlerCallbackMap = specialize TStringHashMap<TPayloadHandlerCallback>;

  { TFacebookQuickReply }

  TFacebookQuickReply = class
  private
    FItem: TJSONArray;
    function getCount: Integer;
  public
    constructor Create;
    destructor Destroy;

    procedure AddText( ATitle, APayload, AImageURL: string);
    procedure AddLocation;
    procedure AddEmail;
    procedure AddPhone;

  published
    property Count: Integer read getCount;
    property Data: TJSONArray read FItem;
  end;

  { TFacebookTemplateCardElement }
  TFacebookTemplateCardElement = class
    private
      FCards: TJSONArray;
      function getCount: Integer;
    public
      constructor Create;
      destructor Destroy;
    published
      property Count: Integer read getCount;
      property Cards: TJSONArray read FCards;
  end;

  { TFacebookTemplateCard }
  TFacebookTemplateCard = class
  private
    FElement: TFacebookTemplateCardElement;
    function getCount: Integer;
  public
    constructor Create;
    destructor Destroy;
  published
    property Count: Integer read getCount;
    property Element: TFacebookTemplateCardElement read FElement;
  end;

  { TFacebookMessengerIntegration }

  TFacebookMessengerIntegration = class(TInterfacedObject)
  private
    FBotName: string;
    FImageCaption: string;
    FImageID: string;
    FImageURL: string;
    FIsSuccessfull: boolean;
    FLocationLatitude: double;
    FLocationLongitude: double;
    FLocationName: string;
    FQuickReply: TFacebookQuickReply;
    FRequestContent: string;
    FResultCode: integer;
    FResultText: string;
    FTemplateCard: TFacebookTemplateCard;
    FToken: string;

    jsonData: TJSONData;
    function getIsLocation: boolean;
    function getIsVoice: boolean;
    function getMessageID: string;
    function getPayload: string;
    function getPayloadHandler(const TagName: string): TPayloadHandlerCallback;
    function getPayloadTitle: string;
    function getPostbackTitle: string;
    function getText: string;
    function getUserID: string;
    function getVoiceURL: string;
    procedure setPayloadHandler(const TagName: string;
      AValue: TPayloadHandlerCallback);
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
    procedure SendCall(ATo: string; APhoneNumber:string; ATitle: string = 'Call'; ADescription: string = '');
    procedure SendButtonURL(ATo: string; ATitle, AURL: string; ADescription: string);
    procedure SendQuickReply(ATo: string; ACaption: string = 'Quick Reply');
    procedure SendTemplateCard(ATo: string; AContent: TJSONArray);
    procedure AskLocation(ATo: string);

    function isCanSend: boolean;
    function isMessage: boolean;
    function isImage(ADetail: boolean = False): boolean;
    function isPostback: boolean;

    property IsVoice: boolean read getIsVoice;
    property IsLocation: boolean read getIsLocation;
    property VoiceURL: string read getVoiceURL;
    function DownloadVoiceTo(ATargetFile: string): boolean;

    // Postback
    property Payload: string read getPayload;
    property PayloadTitle: string read getPayloadTitle;
    property PostbackTitle: string read getPostbackTitle; // = PayloadTitle
    property PayloadHandler[const TagName: string]: TPayloadHandlerCallback
      read getPayloadHandler write setPayloadHandler;
    function PayloadHandling: String;

    //
    property QuickReply: TFacebookQuickReply read FQuickReply write FQuickReply;
    property TemplateCard: TFacebookTemplateCard read FTemplateCard write FTemplateCard;

  published
    property ImageID: string read FImageID;
    property ImageURL: string read FImageURL;
    property ImageCaption: string read FImageCaption;

    property LocationLatitude: double read FLocationLatitude;
    property LocationLongitude: double read FLocationLongitude;
    property LocationName: string read FLocationName;
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
    '{"recipient":{"id":"%s"},"message":{"attachment":{"type":"image","payload":{"url":"%s","is_reusable":true}}}}';
  _FACEBOOK_MESSENGER_ASK_LOCATION =
    '{"recipient": {"id": "%s"},"message": {"text": "%s","quick_replies": [{"content_type": "location"}]}}';
  _FACEBOOK_MESSENGER_SEND_CALL =
    '{"recipient":{"id":"%id%"},"message":{"attachment":{"type":"template","payload":{"template_type":"button","text":"%text%","buttons":[{"type":"phone_number","title":"%title%","payload":"%number%"}]}}}}';
  _FACEBOOK_MESSENGER_SEND_BUTTON_URL =
    '{"recipient":{"id":"%id%"},"message":{"attachment":{"type":"template","payload":{"template_type":"button","text":"%text%","buttons":[{"type":"web_url","url":"%url%","title":"%title%","messenger_extensions": true,"webview_share_button":"hide","webview_height_ratio":"full"}]}}}}';

var
  Response: IHTTPResponse;
  ___PayloadHandlerCallbackMap: TPayloadHandlerCallbackMap;

{ TFacebookTemplateCardElement }

function TFacebookTemplateCardElement.getCount: Integer;
begin
  Result := FCards.Count;
end;

constructor TFacebookTemplateCardElement.Create;
begin
  FCards := TJSONArray.Create;
end;

destructor TFacebookTemplateCardElement.Destroy;
begin
  FCards.Free;
end;

{ TFacebookTemplateCard }

function TFacebookTemplateCard.getCount: Integer;
begin
  Result := FElement.Count;
end;

constructor TFacebookTemplateCard.Create;
begin
  FElement := TFacebookTemplateCardElement.Create;
end;

destructor TFacebookTemplateCard.Destroy;
begin
  FElement.Free;
end;

{ TFacebookQuickReply }

function TFacebookQuickReply.getCount: Integer;
begin
  Result := FItem.Count;
end;

constructor TFacebookQuickReply.Create;
begin
  FItem := TJSONArray.Create;
end;

destructor TFacebookQuickReply.Destroy;
begin
  FItem.Free;
end;

procedure TFacebookQuickReply.AddText(ATitle, APayload, AImageURL: string);
var
  o: TJSONObject;
begin
  if ATitle.IsEmpty or AImageURL.IsEmpty or APayload.IsEmpty then
    Exit;
  o := TJSONObject.Create;
  o.Add('content_type', 'text');
  o.Add('title', ATitle);
  o.Add('image_url', AImageURL);
  o.Add('payload', APayload);
  FItem.Add(o);
end;

procedure TFacebookQuickReply.AddLocation;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  o.Add('content_type', 'location');
  FItem.Add(o);
end;

procedure TFacebookQuickReply.AddEmail;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  o.Add('content_type', 'user_email');
  FItem.Add(o);
end;

procedure TFacebookQuickReply.AddPhone;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  o.Add('content_type', 'user_phone_number');
  FItem.Add(o);
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

function TFacebookMessengerIntegration.getIsLocation: boolean;
begin
  Result := False;
  try
    if not (jsonData.GetPath(
      'entry[0].messaging[0].message.attachments[0].type').AsString
      = 'location') then
      Exit;

    FLocationLatitude := jsonData.GetPath(
      'entry[0].messaging[0].message.attachments[0].payload.coordinates.lat').AsFloat;
    FLocationLongitude := jsonData.GetPath(
      'entry[0].messaging[0].message.attachments[0].payload.coordinates.long').AsFloat;
    Result := True;
    FLocationName := jsonData.GetPath(
      'entry[0].messaging[0].message.attachments[0].title').AsString;
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

function TFacebookMessengerIntegration.getPayload: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('entry[0].messaging[0].postback.payload').AsString;
  except
  end;
end;

function TFacebookMessengerIntegration.getPayloadHandler(
  const TagName: string): TPayloadHandlerCallback;
begin
  Result := ___PayloadHandlerCallbackMap[TagName];
end;

function TFacebookMessengerIntegration.getPayloadTitle: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('entry[0].messaging[0].postback.title').AsString;
  except
  end;
end;

function TFacebookMessengerIntegration.getPostbackTitle: string;
begin
  Result := getPayloadTitle;
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

procedure TFacebookMessengerIntegration.setPayloadHandler(const TagName: string;
  AValue: TPayloadHandlerCallback);
begin
  ___PayloadHandlerCallbackMap[TagName] := AValue;
end;

constructor TFacebookMessengerIntegration.Create;
begin
  ___PayloadHandlerCallbackMap := TPayloadHandlerCallbackMap.Create;
  FQuickReply := TFacebookQuickReply.Create;
  FTemplateCard := TFacebookTemplateCard.Create;
end;

destructor TFacebookMessengerIntegration.Destroy;
begin
  FTemplateCard.Free;
  FQuickReply.Free;
  ___PayloadHandlerCallbackMap.Free;
  if Assigned(jsonData) then
    jsonData.Free;
  inherited;
end;

procedure TFacebookMessengerIntegration.Send(ATo: string; AMessages: string);
var
  posSplit: integer;
  s: string;
begin
  FIsSuccessfull := False;
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

      if FResultCode = 200 then
      begin
        if posSplit > 0 then
        begin
          s := '...' + Copy(AMessages, posSplit);
          if Length(s) > _FACEBOOK_MSG_MAXLENGTH then
            s := Copy(s, 0, _FACEBOOK_MSG_MAXLENGTH) + ' ...';
          Send(ATo, s);
        end;
        FIsSuccessfull := True;
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

procedure TFacebookMessengerIntegration.SendCall(ATo: string;
  APhoneNumber: string; ATitle: string; ADescription: string);
var
  s: String;
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
      //s := Format(_FACEBOOK_MESSENGER_SEND_CALL, [ATo, AImageURL]);
      s := _FACEBOOK_MESSENGER_SEND_CALL;
      s := s.Replace('%id%', ATo);
      s := s.Replace('%number%', APhoneNumber);
      s := s.Replace('%title%', ATitle);
      s := s.Replace('%text%', ADescription);
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

procedure TFacebookMessengerIntegration.SendButtonURL(ATo: string; ATitle,
  AURL: string; ADescription: string);
var
  s: String;
begin
  if not isCanSend then
    Exit;
  if ATo.IsEmpty or FToken.IsEmpty or ADescription.IsEmpty then
    Exit;

  with THTTPLib.Create(_FACEBOOK_MESSENGER_SEND_URL + FToken) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      s := _FACEBOOK_MESSENGER_SEND_BUTTON_URL;
      s := s.Replace('%id%', ATo);
      s := s.Replace('%url%', AURL);
      s := s.Replace('%title%', ATitle);
      s := s.Replace('%text%', ADescription);
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

procedure TFacebookMessengerIntegration.SendQuickReply(ATo: string;
  ACaption: string);
var
  s : string;
  o : TJSONUtil;
begin
  if not isCanSend then
    Exit;
  if ATo.IsEmpty or FToken.IsEmpty or ACaption.IsEmpty then
    Exit;

  o := TJSONUtil.Create;
  o['recipient/id'] := ATo;
  o['message/text'] := ACaption;
  o.ValueArray['message/quick_replies'] := QuickReply.Data;

  s := o.AsJSONFormated;

  with THTTPLib.Create(_FACEBOOK_MESSENGER_SEND_URL + FToken) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(s);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;

    Free;
  end;

  o.Free;
end;

procedure TFacebookMessengerIntegration.SendTemplateCard(ATo: string;
  AContent: TJSONArray);
var
  i: Integer;
  s: string;
  o : TJSONUtil;
  aElements, aButtons: TJSONArray;
  oItem, oDefaultAction, oButton: TJSONObject;
begin
  if ATo.IsEmpty or FToken.IsEmpty then
    Exit;

  aElements := TJSONArray.Create;
  for i := 0 to AContent.Count-1 do
  begin
    oItem := TJSONObject.Create;
    oItem.Add('title', jsonGetData(AContent.Items[i], 'title'));
    oItem.Add('subtitle', jsonGetData(AContent.Items[i], 'subtitle'));
    oItem.Add('image_url', jsonGetData(AContent.Items[i], 'image_url'));

    oDefaultAction := TJSONObject.Create;
    oDefaultAction.Add('type', 'web_url');
    oDefaultAction.Add('url', jsonGetData(AContent.Items[i], 'url'));
    oDefaultAction.Add('webview_height_ratio', 'FULL');
    //oItem.Add('default_action', oDefaultAction);

    aButtons := TJSONArray.Create;
    oButton := TJSONObject.Create;
    oButton.Add('type', 'web_url');
    oButton.Add('title', 'Tampilkan Peta');
    oButton.Add('url', jsonGetData(AContent.Items[i], 'url'));
    aButtons.Add(oButton);
    oItem.Add('buttons', aButtons);

    aElements.Add(oItem);
  end;

  o := TJSONUtil.Create;
  o['recipient/id'] := ATo;
  o['message/attachment/type'] := 'template';
  o['message/attachment/payload/template_type'] := 'generic';
  o.ValueArray['message/attachment/payload/elements'] := aElements;

  s := o.AsJSON;

  with THTTPLib.Create(_FACEBOOK_MESSENGER_SEND_URL + FToken) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(s);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  aElements.Free;
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

function TFacebookMessengerIntegration.isPostback: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('entry[0].messaging[0].postback.payload').AsString <> '' then
      Result := True;
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

function TFacebookMessengerIntegration.PayloadHandling: String;
var
  i: Integer;
  h: TPayloadHandlerCallback;
begin
  Result := '';
  i := ___PayloadHandlerCallbackMap.IndexOf(getPayload);
  if i = -1 then
    Exit;
  h := ___PayloadHandlerCallbackMap.Data[i];
  Result := h( getPayload, getPayloadTitle);
end;

end.

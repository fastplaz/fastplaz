unit line_integration;

{

  [x] SEND MESSAGE
  LINE.Send(LINE.UserID, 'message')

  [x] REPLY
  LINE.Reply(ReplyToken, 'message');

  [x] Buttons
  with TLineTemplateMessage.Create('buttons') do
  begin
    AltText := 'Restoran Rekomendasi';
    Title := 'Restoran Rekomendasi';
    Text := 'Silahkan Pilih:';
    ThumbnailImageURL:= 'https://www/images/imagemap.jpg';
    AddActionPostBack( 'Pilih ini', 'asdf?asfd=2&a=1');
    AddActionURI( 'Detail', 'http://url');
    AddActionMessage( 'Bantuan', 'help');
    template:= AsJSON;

    // send to user
    LINE.Push( line.UserID, template, true);

    Free;
  end;


  [x] Carousel
  with TLineTemplateMessage.Create('carousel') do
  begin
    AltText := 'Restoran Rekomendasi';
    Title := 'Restoran Rekomendasi';
    Text := 'Silahkan Pilih:';

    template := generateButtons;

    AddColumnAsJson(template);
    AddColumnAsJson(template);
    AddColumnAsJson(template);
    AddColumnAsJson(template);

    template := AsJSON;
    Free;
  end;



}
{$mode objfpc}{$H+}

interface

uses
  common, http_lib, logutil_lib, json_lib,
  fpjson, strutils,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

type

  { TLineTemplateMessage }

  TLineTemplateMessage = class
  private
    jsonObject: TJSONObject;
    jsonTemplate: TJSONObject;
    jsonActions: TJSONArray;
    jsonColumns: TJSONArray;

    FTemplateType: string;
    function getAltText: string;
    function getText: string;
    function getThumbnailImageURL: string;
    function getTitle: string;
    procedure setAltText(AValue: string);
    procedure setText(AValue: string);
    procedure setThumbnailImageURL(AValue: string);
    procedure setTitle(AValue: string);
  public
    constructor Create(ATemplateType: string);
    destructor Destroy; override;

    function AsJSON: string;
    function AsJSONFormated: string;

    // buttons
    procedure AddActionPostBack(ALabel, AData: string);
    procedure AddActionMessage(ALabel, AData: string);
    procedure AddActionURI(ALabel, AData: string);
    procedure AddCustomAction(AType, ALabel, AData: string);

    // carouse
    procedure AddColumnAsJson(AJsonString: string);

    function GetTemplateAsJsonString: string;
  published
    property AltText: string read getAltText write setAltText;
    property Title: string read getTitle write setTitle;
    property Text: string read getText write setText;
    property ThumbnailImageURL: string read getThumbnailImageURL
      write setThumbnailImageURL;
  end;

  { TLineIntegration }

  TLineIntegration = class(TInterfacedObject)
  private
    FBotName: string;
    FDebug: boolean;
    FIsSuccessfull: boolean;
    FLocationLatitude: double;
    FLocationLongitude: double;
    FLocationName: string;
    FPostbackData: TStrings;
    FRequestContent: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
    FUserProfile: TJSONUtil;
    jsonData: TJSONData;
    function getGroupID: string;
    function getGroupName: string;
    function getMessageID: string;
    function getReplyToken: string;
    function getText: string;
    function getUserID: string;
    procedure setRequestContent(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    property BotName: string read FBotName write FBotName;
    property Token: string read FToken write FToken;
    property RequestContent: string read FRequestContent write setRequestContent;

    property IsSuccessfull: boolean read FIsSuccessfull;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property ReplyToken: string read getReplyToken;
    property Text: string read getText;
    property UserID: string read getUserID;
    property GroupID: string read getGroupID;
    property GroupName: string read getGroupName;

    procedure Reply(AReplyToken: string; AMessages: string);
    procedure Push(ATo: string; AMessages: string; AISJSON: boolean = False);
    procedure Send(ATo: string; AMessages: string);
    procedure SendAudio(AUserID: string; AAudioURL: string);
    procedure SendImage(AUserID: string; AImageURL: string);
    procedure SendSticker(ATo: string; APackageID: string; AStickerID: string);
    procedure SendLocation(AUserID: string; AName: string; AAddress: string;
      ALatitude: double; ALongitude: double);
    function GetContent(AMessageID: string; ATargetFile: string): boolean;
    function GetContent(AMessageID: string): boolean;
    function GetProfile(AUserId: string): boolean;

    function isCanSend: boolean;
    function isJoinToGroup: boolean;
    function isMessage: boolean;
    function isLocation: boolean;
    function isAudio: boolean;
    function isImage: boolean;
    function isPostback: boolean;
    function isVoice: boolean;
    function isSticker: boolean;
    function isGroup: boolean;
    function isMentioned: boolean;

    function DownloadFile(AMessageID: string; ATargetPath: string): boolean;
  published
    property Debug:boolean read FDebug write FDebug;
    property MessageID: string read getMessageID;
    property LocationLatitude: double read FLocationLatitude;
    property LocationLongitude: double read FLocationLongitude;
    property LocationName: string read FLocationName;
    property PostbackData: TStrings read FPostbackData;
    property UserProfile: TJSONUtil read FUserProfile;
  end;


implementation

const
  _LINE_REPLY_URL = 'https://api.line.me/v2/bot/message/reply';
  _LINE_PUSH_URL = 'https://api.line.me/v2/bot/message/push';
  _LINE_GETCONTENT_URL = 'https://api.line.me/v2/bot/message/%s/content';
  _LINE_GETPROFILE_URL = 'https://api.line.me/v2/bot/profile/%s';
  _LINE_PATH_TEMP_DEFAULT = 'ztemp/cache/';


// JSON numbers always use a dot
function floatToJson(AValue: double): string;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;
  Result := FormatFloat('0.######', AValue, fs);
end;

{ TLineTemplateMessage }

function TLineTemplateMessage.getAltText: string;
begin
  //Result := jsonGetData( jsonObject, 'altText');
  Result := jsonObject['altText'].AsString;
end;

function TLineTemplateMessage.getText: string;
begin
  Result := jsonTemplate['text'].AsString;
end;

function TLineTemplateMessage.getThumbnailImageURL: string;
begin
  Result := jsonTemplate['thumbnailImageUrl'].AsString;
end;

function TLineTemplateMessage.getTitle: string;
begin
  Result := jsonTemplate['title'].AsString;
end;

procedure TLineTemplateMessage.setAltText(AValue: string);
begin
  jsonObject.Strings['altText'] := AValue;
end;

procedure TLineTemplateMessage.setText(AValue: string);
begin
  jsonTemplate.Strings['text'] := AValue;
end;

procedure TLineTemplateMessage.setThumbnailImageURL(AValue: string);
begin
  if AValue.Trim.IsEmpty then Exit;
  jsonTemplate.Strings['thumbnailImageUrl'] := AValue;
end;

procedure TLineTemplateMessage.setTitle(AValue: string);
begin
  jsonTemplate.Strings['title'] := AValue;
end;

constructor TLineTemplateMessage.Create(ATemplateType: string);
begin
  if ATemplateType = '' then
    ATemplateType := 'buttons';
  FTemplateType := ATemplateType;

  jsonObject := TJSONObject.Create;
  jsonObject.Strings['type'] := 'template';
  jsonObject.Strings['altText'] := '';

  jsonTemplate := TJSONObject.Create;
  jsonTemplate.Strings['type'] := ATemplateType;
  //if ATemplateType = 'buttons' then
  //  jsonTemplate.Strings['thumbnailImageUrl'] := '';
  jsonTemplate.Strings['title'] := 'this is title';
  jsonTemplate.Strings['text'] := 'this is text';

  jsonActions := TJSONArray.Create;
  jsonColumns := TJSONArray.Create;

end;

destructor TLineTemplateMessage.Destroy;
begin
  jsonColumns.Free;
  jsonActions.Free;
  jsonTemplate.Free;
  jsonObject.Free;
  inherited Destroy;
end;

// built from copies: the parts stay owned by this object, so AsJSON can be
// called more than once and Destroy frees each part exactly once
function TLineTemplateMessage.AsJSON: string;
var
  o, t: TJSONObject;
begin
  o := TJSONObject(jsonObject.Clone);
  try
    t := TJSONObject(jsonTemplate.Clone);
    if FTemplateType = 'buttons' then
      t.Arrays['actions'] := TJSONArray(jsonActions.Clone);
    if FTemplateType = 'carousel' then
      t.Arrays['columns'] := TJSONArray(jsonColumns.Clone);
    o.Objects['template'] := t;
    Result := o.AsJSON;
  finally
    o.Free;
  end;
end;

function TLineTemplateMessage.AsJSONFormated: string;
begin
  Result := AsJSON;
  Result := JsonFormatter(Result);
end;

function TLineTemplateMessage.GetTemplateAsJsonString: string;
var
  t: TJSONObject;
begin
  t := TJSONObject(jsonTemplate.Clone);
  try
    if t.IndexOfName('type') <> -1 then
      t.Delete('type');
    t.Arrays['actions'] := TJSONArray(jsonActions.Clone);
    Result := t.AsJSON;
  finally
    t.Free;
  end;
end;

procedure TLineTemplateMessage.AddActionPostBack(ALabel, AData: string);
begin
  AddCustomAction('postback', Copy(ALabel, 0, 20), AData);
end;

procedure TLineTemplateMessage.AddActionMessage(ALabel, AData: string);
begin
  AddCustomAction('message', Copy(ALabel, 0, 20), AData);
end;

procedure TLineTemplateMessage.AddActionURI(ALabel, AData: string);
begin
  AddCustomAction('uri', Copy(ALabel, 0, 20), AData);
end;

procedure TLineTemplateMessage.AddCustomAction(AType, ALabel, AData: string);
var
  json: TJSONObject;
begin
  json := TJSONObject.Create;
  json.Strings['type'] := AType;
  json.Strings['label'] := ALabel;
  if AType = 'postback' then
    json.Strings['data'] := AData;
  if AType = 'uri' then
    json.Strings['uri'] := AData;
  if AType = 'message' then
    json.Strings['text'] := AData;

  jsonActions.Add(json);
end;

procedure TLineTemplateMessage.AddColumnAsJson(AJsonString: string);
var
  json: TJSONData;
begin
  try
    json := GetJSON(AJsonString, False);
  except
    json := nil;
  end;
  if json is TJSONObject then
    jsonColumns.Add(json)
  else
    json.Free;
end;


{ TLineIntegration }

procedure TLineIntegration.setRequestContent(AValue: string);
begin
  if FRequestContent = AValue then
    Exit;
  FRequestContent := AValue;
  FreeAndNil(jsonData);
  try
    jsonData := GetJSON(AValue, False);
  except
  end;
end;

function TLineIntegration.getReplyToken: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('events[0].replyToken').AsString;
  except
  end;
end;

function TLineIntegration.getGroupID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('events[0].source.groupId').AsString;
  except
  end;
end;

function TLineIntegration.getGroupName: string;
begin
  Result := getGroupID;

  //TODO: get group real group name
end;

function TLineIntegration.getMessageID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('events[0].message.id').AsString;
  except
  end;
end;

function TLineIntegration.getText: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('events[0].message.text').AsString;
  except
  end;
end;

function TLineIntegration.getUserID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('events[0].source.userId').AsString;
  except
  end;
end;

constructor TLineIntegration.Create;
begin
  FUserProfile := TJSONUtil.Create;
  FDebug := False;
end;

destructor TLineIntegration.Destroy;
begin
  FUserProfile.Free;
  if Assigned(FPostbackData) then
    FPostbackData.Free;
  if Assigned(jsonData) then
    jsonData.Free;
  inherited Destroy;
end;

procedure TLineIntegration.Reply(AReplyToken: string; AMessages: string);
var
  Response: IHTTPResponse;
  _jsonString: TStringList;
begin
  if not isCanSend then
    Exit;
  if (AReplyToken = '') or (AMessages = '') then
    Exit;

  _jsonString := TStringList.Create;
  with THTTPLib.Create(_LINE_REPLY_URL) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('Authorization', 'Bearer ' + FToken);

      _jsonString.Add('{');
      _jsonString.Add('"replyToken":"' + StringToJSONString(AReplyToken, False) + '",');
      _jsonString.Add('"messages":[');
      _jsonString.add('{');
      _jsonString.add('"type":"text",');
      _jsonString.add('"text":"' + StringToJSONString(AMessages, False) + '"');
      _jsonString.add('}');
      _jsonString.Add(']');
      _jsonString.Add('}');

      RequestBody := TStringStream.Create(_jsonString.Text);

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
      on E: Exception do
      begin
        LogUtil.Add('reply: ' + E.Message, 'LINE');
      end;
    end;
  finally
    Free;
  end;

  _jsonString.Free;
end;

procedure TLineIntegration.Push(ATo: string; AMessages: string; AISJSON: boolean);
var
  Response: IHTTPResponse;
  _jsonString: TStringList;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (AMessages = '') then
    Exit;

  _jsonString := TStringList.Create;
  with THTTPLib.Create(_LINE_PUSH_URL) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      AddHeader('Authorization', 'Bearer ' + FToken);

      _jsonString.Add('{');
      _jsonString.Add('"to":"' + StringToJSONString(ATo, False) + '",');
      if AISJSON then
      begin
        _jsonString.Add('"messages":[' + AMessages);
      end
      else
      begin
        _jsonString.Add('"messages":[');
        _jsonString.add('{');
        _jsonString.add('"type":"text",');
        _jsonString.add('"text":"' + StringToJSONString(AMessages, False) + '"');
        _jsonString.add('}');
      end;
      _jsonString.Add(']');
      _jsonString.Add('}');

      RequestBody := TStringStream.Create(_jsonString.Text);

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add('push: ' + E.Message, 'LINE');
      end;
    end;
  finally
    Free;
  end;

  _jsonString.Free;
end;

procedure TLineIntegration.Send(ATo: string; AMessages: string);
begin
  Push(ATo, AMessages);
end;

procedure TLineIntegration.SendAudio(AUserID: string; AAudioURL: string);
var
  Response: IHTTPResponse;
  urlAudio: string;
  _jsonString: TStringList;
begin
  if not isCanSend then
    Exit;
  if (AUserID = '') or (AAudioURL = '') then
    Exit;

  //try force https
  AAudioURL := StringReplace(AAudioURL, 'http://', 'https://', [rfReplaceAll]);

  _jsonString := TStringList.Create;
  _jsonString.Text := '';
  with THTTPLib.Create(_LINE_PUSH_URL) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      AddHeader('Authorization', 'Bearer ' + FToken);

      urlAudio := ReplaceAll(AAudioURL, ['*', '$', '#', '>', '<', ''''], '');
      urlAudio := StringReplace(urlAudio, '\n', '._', [rfReplaceAll]);
      urlAudio := StringReplace(urlAudio, #10, '._', [rfReplaceAll]);

      urlAudio := StringToJSONString(Trim(urlAudio), False);
      if Length(urlAudio) > 999 then
      begin
        urlAudio := copy(urlAudio, 0, 999);
        urlAudio := copy(urlAudio, 0, RPos('_', urlAudio) - 1);
      end;
      if FDebug then
        LogUtil.Add( urlAudio, 'LINE_AUDIO');

      _jsonString.Add('{');
      _jsonString.Add('"to":"' + StringToJSONString(AUserID, False) + '",');
      _jsonString.Add('"messages":[');
      _jsonString.Add('{"type": "audio", "originalContentUrl": "' +
        urlAudio + '", "duration": ' + i2s(Length(urlAudio) * 120) + '}');
      _jsonString.Add(']');
      _jsonString.Add('}');

      RequestBody := TStringStream.Create(_jsonString.Text);

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
  finally
    Free;

  end;

  _jsonString.Free;
end;

procedure TLineIntegration.SendImage(AUserID: string; AImageURL: string);
var
  Response: IHTTPResponse;
  urlImage: string;
  _jsonString: TStringList;
begin
  if not isCanSend then
    Exit;
  if (AUserID = '') or (AImageURL = '') then
    Exit;

  //try force https
  AImageURL := StringReplace(AImageURL, 'http://', 'https://', [rfReplaceAll]);

  _jsonString := TStringList.Create;
  _jsonString.Text := '';
  with THTTPLib.Create(_LINE_PUSH_URL) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      AddHeader('Authorization', 'Bearer ' + FToken);

      urlImage := ReplaceAll(AImageURL, ['*', '$', '#', '>', '<', ''''], '');
      urlImage := StringReplace(urlImage, '\n', '._', [rfReplaceAll]);
      urlImage := StringReplace(urlImage, #10, '._', [rfReplaceAll]);

      urlImage := StringToJSONString(Trim(urlImage), False);
      if Length(urlImage) > 999 then
      begin
        urlImage := copy(urlImage, 0, 999);
        urlImage := copy(urlImage, 0, RPos('_', urlImage) - 1);
      end;
      if FDebug then
        LogUtil.Add( urlImage, 'LINE_IMAGE');

      _jsonString.Add('{');
      _jsonString.Add('"to":"' + StringToJSONString(AUserID, False) + '",');
      _jsonString.Add('"messages":[');
      _jsonString.Add('{"type": "image", "originalContentUrl": "' +
        urlImage + '", "previewImageUrl": "' + urlImage + '"}');
      _jsonString.Add(']');
      _jsonString.Add('}');

      RequestBody := TStringStream.Create(_jsonString.Text);

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add( E.Message, 'LINE_IMAGE');
      end;
    end;
  finally
    Free;
  end;

  _jsonString.Free;
end;

// packageID: 2000000
// stickerID: 48000 47976 48076 48080
procedure TLineIntegration.SendSticker(ATo: string; APackageID: string;
  AStickerID: string);
var
  Response: IHTTPResponse;
  _jsonString: TStringList;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (APackageID = '') or (AStickerID = '') then
    Exit;

  _jsonString := TStringList.Create;
  with THTTPLib.Create(_LINE_PUSH_URL) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      AddHeader('Authorization', 'Bearer ' + FToken);


      _jsonString.Add('{');
      // the recipient is the parameter, not the sender of the incoming event
      _jsonString.Add('"to":"' + StringToJSONString(ATo, False) + '",');
      _jsonString.Add('"messages":[');
      _jsonString.add('{');
      _jsonString.add('"type":"sticker",');
      _jsonString.add('"packageId":"' + StringToJSONString(APackageID, False) + '",');
      _jsonString.add('"stickerId":"' + StringToJSONString(AStickerID, False) + '"');
      _jsonString.add('}');

      //--
      _jsonString.Add(']');
      _jsonString.Add('}');

      RequestBody := TStringStream.Create(_jsonString.Text);

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
  finally
    Free;
  end;

  _jsonString.Free;
end;

procedure TLineIntegration.SendLocation(AUserID: string; AName: string;
  AAddress: string; ALatitude: double; ALongitude: double);
var
  Response: IHTTPResponse;
  _jsonString: TStringList;
begin
  if not isCanSend then
    Exit;
  if (AUserID = '') or (ALatitude = 0) or (ALongitude = 0) then
    Exit;

  _jsonString := TStringList.Create;
  _jsonString.Text := '';
  with THTTPLib.Create(_LINE_PUSH_URL) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      AddHeader('Authorization', 'Bearer ' + FToken);

      _jsonString.Add('{');
      _jsonString.Add('"to":"' + StringToJSONString(AUserID, False) + '",');
      _jsonString.Add('"messages":[');
      _jsonString.Add('{"type": "location",');
      _jsonString.Add(' "title": "' + StringToJSONString(AName, False) + '",');
      _jsonString.Add(' "address": "' + StringToJSONString(AAddress, False) + '",');
      _jsonString.Add(' "latitude": ' + floatToJson(ALatitude) + ',');
      _jsonString.Add(' "longitude": ' + floatToJson(ALongitude) + ',');
      _jsonString.Add(' "z": 0}');
      _jsonString.Add(']');
      _jsonString.Add('}');

      RequestBody := TStringStream.Create(_jsonString.Text);

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
  finally
    Free;

  end;

  _jsonString.Free;
end;

function TLineIntegration.GetContent(AMessageID: string; ATargetFile: string): boolean;
var
  Response: IHTTPResponse;
  urlTarget: string;
begin
  Result := False;
  if (AMessageID = '') or (FToken = '') then
    Exit;

  urlTarget := Format(_LINE_GETCONTENT_URL, [AMessageID]);
  with THTTPLib.Create(urlTarget) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Authorization', 'Bearer ' + FToken);
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
        LogUtil.Add('download: ' + E.Message, 'LINE');
      end;
    end;
  finally
    Free;
  end;
end;

function TLineIntegration.GetContent(AMessageID: string): boolean;
begin
  Result := GetContent(AMessageID, _LINE_PATH_TEMP_DEFAULT + trim(AMessageID) + '.msg');
end;

function TLineIntegration.GetProfile(AUserId: string): boolean;
var
  Response: IHTTPResponse;
  urlTarget: string;
begin
  Result := False;
  if FToken.IsEmpty or AUserId.IsEmpty then Exit;

  urlTarget := Format(_LINE_GETPROFILE_URL, [AUserId]);
  with THTTPLib.Create(urlTarget) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Authorization', 'Bearer ' + FToken);
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
      if FResultCode = 200 then
      begin
        FUserProfile.LoadFromJsonString(Response.ResultText, False);
        Result := True;
      end;
    except
      on E: Exception do
      begin
        LogUtil.Add('get profile: ' + E.Message, 'LINE');
      end;
    end;
  finally
    Free;
  end;
end;

function TLineIntegration.isCanSend: boolean;
begin
  Result := False;
  if FToken = '' then
    Exit;
  Result := True;
end;

function TLineIntegration.isJoinToGroup: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].type').AsString = 'join' then
      Result := True;
  except
  end;
end;

function TLineIntegration.isMessage: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].type').AsString = 'message' then
      Result := True;
  except
  end;
end;

function TLineIntegration.isLocation: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].message.type').AsString = 'location' then
    begin
      Result := True;
      FLocationLatitude := jsonData.GetPath('events[0].message.latitude').AsFloat;
      FLocationLongitude := jsonData.GetPath('events[0].message.longitude').AsFloat;
      FLocationName := jsonData.GetPath('events[0].message.title').AsString;
    end;
  except
  end;
end;

function TLineIntegration.isAudio: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].message.type').AsString = 'audio' then
      Result := True;
  except
  end;
end;

function TLineIntegration.isImage: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].message.type').AsString = 'image' then
      Result := True;
  except
  end;
end;

function TLineIntegration.isPostback: boolean;
var
  s: string;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].type').AsString = 'postback' then
      Result := True;
    s := jsonData.GetPath('events[0].postback.data').AsString;
    FreeAndNil(FPostbackData);
    FPostbackData := Explode(s, '&');
  except
  end;
end;

function TLineIntegration.isVoice: boolean;
begin
  Result := isAudio;
end;

function TLineIntegration.isSticker: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].message.type').AsString = 'sticker' then
      Result := True;
  except
  end;
end;

function TLineIntegration.isGroup: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].source.type').AsString = 'group' then
      Result := True;
  except
  end;
end;

function TLineIntegration.isMentioned: boolean;
begin
  Result := False;
  if pos('@' + LowerCase(FBotName), Text) > 0 then
    Result := True;
  if pos('Bot', Text) > 0 then    // force dectect as Bot  (____Bot)
    Result := True;
end;

function TLineIntegration.DownloadFile(AMessageID: string;
  ATargetPath: string): boolean;
begin
  Result := GetContent(AMessageID, ATargetPath);
end;

end.

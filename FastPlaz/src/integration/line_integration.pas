unit line_integration;

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson,
  Classes, SysUtils;

type

  { TLineIntegration }

  TLineIntegration = class(TInterfacedObject)
  private
    FBotName: string;
    FIsSuccessfull: boolean;
    FRequestContent: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
    jsonData: TJSONData;
    function getGroupID: string;
    function getGroupName: string;
    function getReplyToken: string;
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

    property ReplyToken: string read getReplyToken;
    property Text: string read getText;
    property UserID: string read getUserID;
    property GroupID: string read getGroupID;
    property GroupName: string read getGroupName;

    procedure Reply(AReplyToken: string; AMessages: string);
    procedure Push(ATo: string; AMessages: string);
    procedure Send(ATo: string; AMessages: string);
    procedure SendAudio(AUserID: string; AAudioURL: string);
    procedure SendSticker(ATo: string; APackageID: string; AStickerID: string);

    function isCanSend: boolean;
    function isJoinToGroup: boolean;
    function isMessage: boolean;
    function isSticker: boolean;
    function isGroup: boolean;
    function isMentioned: boolean;
  end;


implementation

const
  _LINE_REPLY_URL = 'https://api.line.me/v2/bot/message/reply';
  _LINE_PUSH_URL = 'https://api.line.me/v2/bot/message/push';

var
  Response: IHTTPResponse;

{ TLineIntegration }

procedure TLineIntegration.setRequestContent(AValue: string);
begin
  if FRequestContent = AValue then
    Exit;
  FRequestContent := AValue;
  jsonData := GetJSON(AValue);
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

end;

destructor TLineIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

procedure TLineIntegration.Reply(AReplyToken: string; AMessages: string);
var
  i: integer;
  _jsonString: TStringList;
  lst: TStringList;
begin
  if not isCanSend then
    Exit;
  if (AReplyToken = '') or (AMessages = '') then
    Exit;

  _jsonString := TStringList.Create;
  lst := TStringList.Create;
  lst.Text := AMessages;
  with THTTPLib.Create(_LINE_REPLY_URL) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      AddHeader('Authorization', 'Bearer ' + FToken);


      _jsonString.Add('{');
      _jsonString.Add('"replyToken":"' + AReplyToken + '",');
      _jsonString.Add('"messages":[');

      {
      for i := 0 to lst.Count - 1 do
      begin
        _jsonString.add('{');
        _jsonString.add('"type":"text",');
        _jsonString.add('"text":"' + StringToJSONString(lst[i]) + '"');
        _jsonString.add('}');
        if i <> lst.Count - 1 then
          _jsonString.Add(',');
      end;
      }
      _jsonString.add('{');
      _jsonString.add('"type":"text",');
      _jsonString.add('"text":"' + StringToJSONString(lst.Text) + '"');
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
      on E: Exception do
      begin
        LogUtil.Add('reply: ' + E.Message, 'LINE');
      end;
    end;
    Free;
  end;

  _jsonString.Free;
  lst.Free;
end;

procedure TLineIntegration.Push(ATo: string; AMessages: string);
var
  i: integer;
  _jsonString: TStringList;
  lst: TStringList;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (AMessages = '') then
    Exit;

  _jsonString := TStringList.Create;
  lst := TStringList.Create;
  lst.Text := AMessages;
  with THTTPLib.Create(_LINE_PUSH_URL) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      AddHeader('Authorization', 'Bearer ' + FToken);


      _jsonString.Add('{');
      _jsonString.Add('"to":"' + ATo + '",');
      _jsonString.Add('"messages":[');
      {
      for i := 0 to lst.Count - 1 do
      begin
        _jsonString.add('{');
        _jsonString.add('"type":"text",');
        _jsonString.add('"text":"' + StringToJSONString(lst[i]) + '"');
        _jsonString.add('}');
        if i <> lst.Count - 1 then
          _jsonString.Add(',');
      end;
      }
      _jsonString.add('{');
      _jsonString.add('"type":"text",');
      _jsonString.add('"text":"' + StringToJSONString(lst.Text) + '"');
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
    Free;
  end;

  _jsonString.Free;
  lst.Free;
end;

procedure TLineIntegration.Send(ATo: string; AMessages: string);
begin
  Push(ATo, AMessages);
end;

procedure TLineIntegration.SendAudio(AUserID: string; AAudioURL: string);
var
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
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      AddHeader('Authorization', 'Bearer ' + FToken);

      _jsonString.Add('{');
      _jsonString.Add('"to":"' + AUserID + '",');
      _jsonString.Add('"messages":[');
      _jsonString.Add('{"type": "audio", "originalContentUrl": "' +
        AAudioURL + '", "duration": 240000}');
      _jsonString.Add(']');
      _jsonString.Add('}');

      RequestBody := TStringStream.Create(_jsonString.Text);

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;

  end;

  _jsonString.Free;
end;

// packageID: 2000000
// stickerID: 48000 47976 48076 48080
procedure TLineIntegration.SendSticker(ATo: string; APackageID: string;
  AStickerID: string);
var
  _jsonString: TStringList;
begin
  if not isCanSend then
    Exit;
  if (ATo = '') or (APackageID = '') or (AStickerID = '') then
    Exit;

  _jsonString := TStringList.Create;
  with THTTPLib.Create(_LINE_PUSH_URL) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      AddHeader('Authorization', 'Bearer ' + FToken);


      _jsonString.Add('{');
      _jsonString.Add('"to":"' + UserID + '",');
      _jsonString.Add('"messages":[');
      _jsonString.add('{');
      _jsonString.add('"type":"sticker",');
      _jsonString.add('"packageId":"' + APackageID + '",');
      _jsonString.add('"stickerId":"' + AStickerID + '"');
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
    Free;
  end;

  _jsonString.Free;
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

end.



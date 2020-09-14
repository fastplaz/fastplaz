unit telegram_integration;

{
  // USAGE:
  with TTelegramIntegration.Create do
  begin
    Token := 'your telegram token id';

    // your code
    Telegram.SendMessage( 1234567, 'your text');

    if IsSuccessfull then
    begin
      // ....
    end;
    Free;
  end;

  // Send Message
  // syntax:
  //  Telegram.SendMessage( ChatID, Text);
  Telegram.SendMessage( 1234567, 'your text');

  // Reply Message
  Telegram.SendMessage( 1234567, 'your text', 178);

  // Send Photo
  if Telegram.SendPhoto( 1234567, '/your/path/filename.jpg', 'this is photo caption') then
  begin
    ...
  end

  // Send Video
  if Telegram.SendVideo( 1234567, '/your/path/video.mp4', 'this is video') then
  begin
    ...
  end

  // Send Contact
  Telegram.SendContact( 1234567, 'FirstName', 'LastName', '+62876543210');


  // json body content from telegram
  {
  "message" : {
    "message_id" : 123456,
    "chat" : {
      "id" : 2222647
      },
    "text" : "text message from sender"
    }
  }

}

{$mode objfpc}{$H+}
{ $include ../../define.inc}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  fpjson, jsonparser,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils, json_helpers;

{$ifdef TELEGRAM_INTEGRATION}
{$endif}

const
  TELEGRAM_FILEURL = 'https://api.telegram.org/file/bot%s/';

type

  TShowStatusEvent = procedure(Status: String) of Object;
  TExecuteHandlerEvent = procedure of Object;
  TMessageHandler = procedure(AMessage: String; var AReply: String; var AHandled: Boolean) of Object;
  TSendMessageEvent = procedure(const ChatID: string; const Text: string; const ReplyToMessageID: string) of Object;

  { TTelegramSimpleThread }
  TTelegramSimpleThread = class(TThread)
  private
    contentResult: String;
    FOnExecuteHandler: TExecuteHandlerEvent;
    FOnShowStatus: TShowStatusEvent;
    procedure syncUpdate;
  protected
    procedure Execute; override;
  public
    property OnExecuteHandler: TExecuteHandlerEvent read FOnExecuteHandler write FOnExecuteHandler;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

  { TTelegramSimpleSendThread }
  TTelegramSimpleSendThread = class(TThread)
  private
    contentResult: String;
    FOnExecuteHandler: TSendMessageEvent;
  protected
    procedure Execute; override;
  public
    ThreadChatID, ThreadChatText, ThreadReplyTo: string;
    property OnExecuteHandler: TSendMessageEvent read FOnExecuteHandler write FOnExecuteHandler;
  end;

  { TTelegramIntegration }

  TTelegramIntegration = class(TInterfacedObject)
  private
    FCallbackData: TStrings;
    FCallbackInlineKeyboard: TJSONUtil;
    FCallbackInstance: string;
    FDebug: boolean;
    FFileName: string;
    FImageID: string;
    FImagePath: string;
    FImageURL: string;
    FInvitedFullName: string;
    FInvitedUserName: string;
    FInvitedUserId: string;
    FLocationLatitude: double;
    FLocationLongitude: double;
    FLocationName: string;
    FOnMessage: TMessageHandler;
    FResultMessageID: string;
    FVoiceDuration: integer;
    FVoiceID: string;
    FVoiceSize: integer;
    FVoiceType: string;
    jsonData: TJSONData;

    FIsSuccessfull: boolean;
    FLastUpdateID: integer;
    FParseMode: string;
    FRequestContent: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
    FURL: string;
    FGetUpdatesInProcess: Boolean;
    FGetUpdatesContent: string;
    function getChatID: string;
    function getChatType: string;
    function getFullName: string;
    function getGroupName: string;
    function getImageCaption: string;
    function getIsAdmin: boolean;
    function getIsBot: boolean;
    function getIsCallbackQuery: boolean;
    function getIsDocument: boolean;
    function getIsGroup: boolean;
    function getIsInvitation: boolean;
    function getIsLocation: boolean;
    function getIsPicture: boolean;
    function getIsSticker: boolean;
    function getIsTextMentionExists: boolean;
    function getIsUserLeft: boolean;
    function getIsVoice: boolean;
    function getLeftUserID: string;
    function getMessageID: string;
    function getReplyFromID: string;
    function getText: string;
    function getUpdateID: integer;
    function getUserID: string;
    function getUserName: string;
    procedure setRequestContent(AValue: string);
    procedure setToken(AValue: string);
    procedure onGetUpdatesExecuteHandler;
    procedure onGetUpdatesHandler( ATelegramResponse: String);
  public
    constructor Create;
    destructor Destroy;

    function getUpdates(const UpdateID: integer = 0): string;

    function getUpdatesDynamic(const UpdateID: integer = 0): Boolean;

    function GetMe: string;
    function SendMessage(const ChatID: string = '0'; const Text: string = '';
      const ReplyToMessageID: string = ''): boolean;
    function SendMessage(const ChatID: integer = 0; const Text: string = '';
      const ReplyToMessageID: integer = 0): boolean;
    function SendMessageAsThread(const ChatID: string = '0'; const Text: string = '';
      const ReplyToMessageID: string = ''): boolean;
    procedure SendMessageAsThreadExecute(const ChatID: string; const Text: string; const ReplyToMessageID: string);
    function EditMessage(const ChatID: string; MessageID: string;
      Text: string = ''): boolean;
    function EditMessage(const AChatID: string; AMessageID: string;
      AText: string; AData: TJSONUtil): boolean;
    function DeleteMessage(const AChatID: string; AMessageID: string): boolean;
    function SendAudio(const ChatID: string = '0'; const AAudioURL: string = '';
      const ACaption: string = ''; const ReplyToMessageID: string = ''): boolean;
    function SendPhoto(const ChatID: string; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendPhoto(const ChatID: integer; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendPhotoFromURL(const ChatID: string; const AImageURL: string;
      const Caption: string = ''; const ReplyToMessageID: string = ''): boolean;
    function SendVideo(const ChatID: string; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendVenue(const ChatID: string; const AName: string;
      const AAddress: string; ALatitude, ALongitude:double; const ReplyToMessageID: string = ''): boolean;
    function SendContact(const ChatID: integer;
      FirstName, LastName, PhoneNumber: string): boolean;
    function SendContact(const ChatID: string;
      FirstName, LastName, PhoneNumber: string): boolean;
    function SendDocument(const ChatID: string; const AFile: string;
      const ACaption: string = ''; const ReplyToMessageID: string = ''): boolean;
    function SendInlineKeyboard(const ChatId: string; const AText: string;
      const AData: TJSONUtil): boolean;
    function SendKeyboard(const ChatId: string; const AText: string;
      const AData: TJSONUtil): boolean;
    function RemoveKeyboard(const ChatId: string; const AText: string): boolean;
    function GetFilePath(FileID: string): string;
    function GetFullFileURL(FileID: string): string;
    function DownloadFile(FilePath: string; TargetFile: string): boolean;
    function GroupMemberCount(AGroupID: string): integer;
    function GroupAdminList(AGroupID: string; Formated: boolean = True): string;

    function isImage(ADetail: boolean = False): boolean;
    function KickUser(AChatID: string; AUserID: string; AReason: string; AUntilDate: Integer = 0):boolean;
    function RestrictUser(AChatID: string; AUserID: string;
      AUntilDate: integer = 0;
      ASendMessage: boolean = false;
      ASendMedia: boolean = false;
      ASendOther: boolean = false;
      ASendWebPreview: boolean = false
      ):boolean;

    procedure SetWebHook(AValue: string);
    function DeleteWebHook: boolean;
    property OnMessage: TMessageHandler read FOnMessage write FOnMessage;
  published
    property FileName: string read FFileName;
    property Debug: boolean read FDebug write FDebug;
    property RequestContent: string read FRequestContent write setRequestContent;
    property Text: string read getText;
    property UpdateID: integer read getUpdateID;
    property MessageID: string read getMessageID;
    property ChatID: string read getChatID;
    property ChatType: string read getChatType;
    property UserID: string read getUserID;
    property LeftUserID: string read getLeftUserID;
    property UserName: string read getUserName;
    property FullName: string read getFullName;
    property GroupName: string read getGroupName;
    property ReplyFromID: string read getReplyFromID;
    property IsGroup: boolean read getIsGroup;
    property IsAdmin: boolean read getIsAdmin;
    property IsBot: boolean read getIsBot;
    property IsInvitation: boolean read getIsInvitation;
    property IsUserLeft: boolean read getIsUserLeft;
    property IsCallbackQuery: boolean read getIsCallbackQuery;
    property IsTextMentionExists: boolean read getIsTextMentionExists;

    property InvitedUserId: string read FInvitedUserId;
    property InvitedUserName: string read FInvitedUserName;
    property InvitedFullName: string read FInvitedFullName;

    property LastUpdateID: integer read FLastUpdateID;
    property URL: string read FURL;
    property Token: string read FToken write setToken;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property ParseMode: string read FParseMode write FParseMode;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;
    property ResultMessageID: string read FResultMessageID;

    property LocationLatitude: double read FLocationLatitude;
    property LocationLongitude: double read FLocationLongitude;
    property LocationName: string read FLocationName;

    property IsVoice: boolean read getIsVoice;
    property IsLocation: boolean read getIsLocation;
    property IsSticker: boolean read getIsSticker;
    property IsPicture: boolean read getIsPicture;
    property IsDocument: boolean read getIsDocument;
    property VoiceDuration: integer read FVoiceDuration;
    property VoiceType: string read FVoiceType;
    property VoiceID: string read FVoiceID;
    property VoiceSize: integer read FVoiceSize;

    property ImageID: string read FImageID;
    property ImageURL: string read FImageURL;
    property ImagePath: string read FImagePath;
    property ImageCaption: string read getImageCaption;

    property WebHook: string write SetWebHook;
    property GetUpdatesContent: string read FGetUpdatesContent;

    property CallbackInstance: string read FCallbackInstance;
    property CallbackData: TStrings read FCallbackData;
    property CallbackInlineKeyboard: TJSONUtil read FCallbackInlineKeyboard;
  end;

implementation

const
  TELEGRAM_BASEURL = 'https://api.telegram.org/bot%s/';
  TELEGRAM_COMMAND_GETUPDATES = 'getUpdates';
  TELEGRAM_COMMAND_GETME = 'getMe';
  TELEGRAM_COMMAND_SENDMESSAGE =
    'sendMessage?chat_id=%s&reply_to_message_id=%s&parse_mode=%s&disable_web_page_preview=false&text=%s';
  TELEGRAM_COMMAND_DELETEMESSAGE = 'deleteMessage?chat_id=%s&message_id=%s';
  TELEGRAM_COMMAND_SENDPHOTO = 'sendPhoto?chat_id=%s&caption=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDVIDEO = 'sendVideo?chat_id=%s&caption=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDAUDIO = 'sendAudio?chat_id=%s&caption=%s&audio=%s';
  TELEGRAM_COMMAND_SENDDOCUMENT = 'sendDocument?chat_id=%s&caption=%s&parse_mode=%s';
  //https://api.telegram.org/bot307478661:AAF9DGtgoASYsVF6KwHm7qhimXq8cHGIxTk/sendVenue?chat_id=2222647&latitude=-6.228018&longitude=106.82453&title=suatu%20tempat&address=alamatnya
  TELEGRAM_COMMAND_SENDVENUE = 'sendVenue?chat_id=%s&title=%s&address=%s&latitude=%f&longitude=%f&parse_mode=%s';
  TELEGRAM_COMMAND_SENDCONTACT =
    'sendContact?chat_id=%d&phone_number=%s&first_name=%s&last_name=%s';
  TELEGRAM_COMMAND_GETFILE = 'getFile?file_id=';
  TELEGRAM_COMMAND_GETGROUPADMINISTRATOR = 'getChatAdministrators?chat_id=';
  TELEGRAM_COMMAND_GETGROUPMEMBERCOUNT = 'getChatMembersCount?chat_id=';

  TELEGRAM_COMMAND_KICKUSER = 'kickChatMember?chat_id=%s&user_id=%s&until_date=%d';
  TELEGRAM_COMMAND_RESTRICTUSER = 'restrictChatMember?chat_id=%s&user_id=%s&until_date=%d'
    + '&can_send_messages=%s&can_send_media_messages=%s'
    + '&can_send_other_messages=%s&can_add_web_page_previews=%s';

var
  Response: IHTTPResponse;

{ TTelegramSimpleSendThread }

procedure TTelegramSimpleSendThread.Execute;
begin
  if Assigned(FOnExecuteHandler) then
  begin
    FOnExecuteHandler(ThreadChatID,ThreadChatText,ThreadReplyTo);
  end;
end;

{ TTelegramSimpleThread }

procedure TTelegramSimpleThread.syncUpdate;
begin
  if Assigned(FOnShowStatus) then
  begin
    FOnShowStatus(contentResult);
  end;
end;

procedure TTelegramSimpleThread.Execute;
begin
  if Assigned(FOnExecuteHandler) then
  begin
    FOnExecuteHandler();
  end;
  Synchronize(@syncUpdate);
end;


{ TTelegramIntegration }

procedure TTelegramIntegration.setToken(AValue: string);
begin
  if FToken = AValue then
    Exit;
  FToken := AValue;
  FURL := format(TELEGRAM_BASEURL, [FToken]);
end;

procedure TTelegramIntegration.SetWebHook(AValue: string);
var
  urlTarget: string;
begin
  if FURL.IsEmpty then Exit;
  urlTarget := FURL + 'setWebhook?url=' + AValue;
  file_get_contents(urlTarget);
end;

function TTelegramIntegration.DeleteWebHook: boolean;
var
  urlTarget: string;
begin
  Result := False;
  if FURL.IsEmpty then Exit;
  urlTarget := FURL + 'setWebhook?url=';
  file_get_contents(urlTarget);
  //TODO: check if successfull
end;

procedure TTelegramIntegration.setRequestContent(AValue: string);
begin
  if FRequestContent = AValue then
    Exit;
  FRequestContent := AValue;
  try
    jsonData := GetJSON(AValue);
  except
  end;
end;

function TTelegramIntegration.getText: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.text').AsString;
    if Result = 'False' then
      Result := '';
  except
    try
      Result := jsonData.GetPath('edited_message.text').AsString;
    except
    end;
  end;
  if IsCallbackQuery then
  begin
    try
      Result := jsonData.GetPath('callback_query.message.text').AsString;
    except
    end;
  end;
end;

function TTelegramIntegration.getChatID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.chat.id').AsString;
  except
    try
      Result := jsonData.GetPath('edited_message.chat.id').AsString;
    except
    end;
  end;
  if IsCallbackQuery then
  begin
    try
      Result := jsonData.GetPath('callback_query.message.chat.id').AsString;
    except
    end;
  end;
end;

function TTelegramIntegration.getChatType: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.chat.type').AsString;
  except
    try
      Result := jsonData.GetPath('edited_message.chat.type').AsString;
    except
    end;
  end;
  if IsCallbackQuery then
  begin
    try
      Result := jsonData.GetPath('callback_query.message.chat.type').AsString;
    except
    end;
  end;
end;

function TTelegramIntegration.getFullName: string;
begin
  Result := '';
  try
    Result := trim(jsonData.GetPath('message.from.first_name').AsString +
      ' ' + jsonData.GetPath('message.from.last_name').AsString);
    if Result = '' then
      UserName;
  except
  end;
  if IsCallbackQuery then
  begin
    try
      Result := jsonData.GetPath('callback_query.from.first_name').AsString
        + ' ' + jsonData.GetPath('callback_query.from.last_name').AsString;
    except
    end;
  end;
end;

function TTelegramIntegration.getGroupName: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.chat.title').AsString;
  except
  end;
end;

function TTelegramIntegration.getImageCaption: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.caption').AsString;
  except
  end;
end;

function TTelegramIntegration.getIsAdmin: boolean;
begin
  Result := False;

  //TODO: check is admin
end;

function TTelegramIntegration.getIsBot: boolean;
begin
  Result := False;
  try
    Result := jsonData.GetPath('message.from.is_bot').AsBoolean;
  except
  end;
end;

{
  if IsCallbackQuery then
    stringValue = CallbackData.Values['key'];
}
function TTelegramIntegration.getIsCallbackQuery: boolean;
var
  s: string;
begin
  Result := False;
  try
    s := jsonData.GetPath('callback_query').AsJSON;
    if not s.IsEmpty then
    begin
      FCallbackInstance:= jsonData.GetPath('callback_query.chat_instance').AsString;
      s := jsonData.GetPath('callback_query.data').AsString;
      FCallbackData := Explode(s, '&');

      // get inline keyboard data
      s := jsonData.GetPath('callback_query.message.reply_markup.inline_keyboard').AsJSON;
      FCallbackInlineKeyboard := TJSONUtil.Create;
      FCallbackInlineKeyboard.LoadFromJsonString(s, False);

      Result := True;
    end;
  except
  end;

end;

function TTelegramIntegration.getIsDocument: boolean;
begin
  Result := False;
  FFileName := '';
  try
    FFileName := jsonData.GetPath('message.document.file_name').AsString;
    //mime_type
    Result := True;
  except
  end;
end;

function TTelegramIntegration.getIsGroup: boolean;
var
  s: string;
begin
  Result := False;
  s := ChatType;
  if ((s = 'group') or (s = 'supergroup')) then
    Result := True;
end;

function TTelegramIntegration.getIsInvitation: boolean;
begin
  Result := False;
  try
    FInvitedFullName := jsonData.GetPath('message.new_chat_member.first_name').AsString;
    FInvitedFullName := FInvitedFullName + ' ' + jsonData.GetPath('message.new_chat_member.last_name').AsString;
    FInvitedFullName := trim(FInvitedFullName);
  except
  end;
  try
    FInvitedUserName := '+' + jsonData.GetPath('message.new_chat_member.id').AsString;
    FInvitedUserId := jsonData.GetPath('message.new_chat_member.id').AsString;
    FInvitedUserName := jsonData.GetPath('message.new_chat_member.username').AsString;
  except
  end;

  if FInvitedUserName <> '' then
    Result := True;
end;

function TTelegramIntegration.getIsUserLeft: boolean;
var
  s: String;
begin
  Result := False;
  try
    s := jsonData.GetPath('message.left_chat_member.id').AsString;
    Result := True;
  except
  end;
end;


function TTelegramIntegration.getIsLocation: boolean;
begin
  Result := False;
  try
    FLocationLatitude := jsonData.GetPath('message.location.latitude').AsFloat;
    FLocationLongitude := jsonData.GetPath('message.location.longitude').AsFloat;
    Result := True;
    FLocationName := jsonData.GetPath('message.venue.title').AsString +
      ', ' + jsonData.GetPath('message.venue.address').AsString;
  except
  end;
end;

function TTelegramIntegration.getIsPicture: boolean;
begin
  Result := isImage(False);
end;

function TTelegramIntegration.getIsSticker: boolean;
var
  s: string;
begin
  Result := False;
  try
    s := jsonData.GetPath('message.sticker.file_id').AsString;
    if not s.IsEmpty then
      Result := True;
  except
  end;
end;

function TTelegramIntegration.getIsTextMentionExists: boolean;
var
  i: integer;
  entityType: string;
  entities: TJSONArray;
  item: TJSONData;
begin
  Result := False;

  if IsCallbackQuery then
  begin
    try
      entities := TJSONArray(jsonData.GetPath('callback_query.message.entities'));
      if entities.Count > 0 then
      begin
        for i:=0 to entities.Count-1 do
        begin
          item := entities.Items[i];
          entityType := item.ValueOfNameAsString('type');
          if entityType = 'text_mention' then
          begin
            Result := True;
            exit;
          end;
        end;
      end;
    except
      on e: Exception do
      begin
        die('x:'+e.Message);
      end;
    end;
  end;//IsCallbackQuery
end;

function TTelegramIntegration.getIsVoice: boolean;
begin
  Result := False;
  try
    FVoiceDuration := jsonData.GetPath('message.voice.duration').AsInteger;
    FVoiceID := jsonData.GetPath('message.voice.file_id').AsString;
    FVoiceType := jsonData.GetPath('message.voice.mime_type').AsString;
    FVoiceSize := jsonData.GetPath('message.voice.file_size').AsInteger;
    Result := True;
  except
  end;
end;

function TTelegramIntegration.getMessageID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.message_id').AsString;
  except
  end;
  if IsCallbackQuery then
  begin
    try
      Result := jsonData.GetPath('callback_query.message.message_id').AsString;
    except
    end;
  end;
end;

function TTelegramIntegration.getReplyFromID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.reply_to_message.from.id').AsString;
  except
  end;
end;

function TTelegramIntegration.getUpdateID: integer;
begin
  Result := 0;
  try
    Result := jsonData.GetPath('update_id').AsInteger;
  except
  end;
end;

function TTelegramIntegration.getUserID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.from.id').AsString;
  except
    try
      Result := jsonData.GetPath('edited_message.from.id').AsString;
    except
    end;
  end;
  if IsCallbackQuery then
  begin
    try
      Result := jsonData.GetPath('callback_query.from.id').AsString;
    except
    end;
  end;
end;

function TTelegramIntegration.getLeftUserID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.left_chat_member.id').AsString;
  except
  end;
end;



function TTelegramIntegration.getUserName: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.from.username').AsString;
  except
    try
      Result := jsonData.GetPath('edited_message.from.username').AsString;
    except
    end;
  end;
  if IsCallbackQuery then
  begin
    try
      Result := jsonData.GetPath('callback_query.from.username').AsString;
    except
    end;
  end;
end;


constructor TTelegramIntegration.Create;
begin
  FURL := '';
  FParseMode := 'Markdown';
  FLastUpdateID := 0;
  FIsSuccessfull := False;
  FDebug := True;
  FGetUpdatesInProcess := False;
  FResultMessageID := '0';
  FCallbackInstance := '';
  FFileName := '';
end;

destructor TTelegramIntegration.Destroy;
begin
  if Assigned(FCallbackInlineKeyboard) then
    FCallbackInlineKeyboard.Free;
  if Assigned(FCallbackData) then
    FCallbackData.Free;
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TTelegramIntegration.getUpdates(const UpdateID: integer): string;
var
  urlTarget: string;
  j: TJSONData;
  a: TJSONArray;
  i: integer;
begin
  FIsSuccessfull := False;
  Result := '{"ok":false}';
  FLastUpdateID := 0;
  if FURL = '' then
    Exit;

  //-- todo: manual get update from telegram
  urlTarget := URL + TELEGRAM_COMMAND_GETUPDATES + '?offset=' + IntToStr(UpdateID);
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      Result := FResultText;
      FIsSuccessfull := IsSuccessfull;

      j := GetJSON(FResultText);
      i := TJSONObject(j).IndexOfName('result');
      if i <> -1 then
      begin
        a := TJSONArray(j.Items[i]);
        if a.Count>1 then a := TJSONArray(a.Items[0]);
        if a.Count>1 then
        begin
          a := TJSONArray(a.Items[0]);
          FLastUpdateID := s2i(a.AsJSON) + 1;
        end;
      end;

    except
      on E:Exception do
      begin
      end;
    end;
    Free;
  end;
end;

function TTelegramIntegration.getUpdatesDynamic(const UpdateID: integer
  ): Boolean;
begin
  Result := False;
  if FGetUpdatesInProcess then Exit;
  FGetUpdatesInProcess := True;
  FGetUpdatesContent := '';
  with TTelegramSimpleThread.Create(True) do
  begin
    FreeOnTerminate := True;
    OnExecuteHandler := @onGetUpdatesExecuteHandler;
    OnShowStatus := @onGetUpdatesHandler;
    Start;
  end;
  Result := True;
end;

procedure TTelegramIntegration.onGetUpdatesExecuteHandler;
begin
  FGetUpdatesContent:= getUpdates(FLastUpdateID);
end;

procedure TTelegramIntegration.onGetUpdatesHandler(ATelegramResponse: String
  );
var
  i, indexResult: integer;
  s: string;
  jData: TJSONData;
  aData, item: TJSONArray;
  aReply: string;
  aHandled: boolean;
begin
  // get result from FGetUpdatesContent
  if not Assigned(FOnMessage) then
    Exit;

  try
    jData := GetJSON(FGetUpdatesContent);
    if jData.GetPath('ok').AsBoolean then
    begin
      indexResult := TJSONObject(jData).IndexOfName('result');
      if indexResult <> -1 then
      begin
        aData := TJSONArray(jData.Items[indexResult]);
        i := aData.Count;
        for i:= 0 to aData.count - 1 do
        begin
          s := jData.GetPath('result['+i2s(i)+']').AsJSON;
          RequestContent := s;
          FLastUpdateID := UpdateID+1;

          aReply := '';
          aHandled := false;
          FOnMessage(Text, aReply, aHandled);
          if aHandled then
          begin
            SendMessageAsThread(ChatID, aReply);
          end;
        end;
      end;
    end;
  except
  end;

  FGetUpdatesInProcess := False;
end;

function TTelegramIntegration.GetMe: string;
var
  urlTarget: string;
begin
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  Result := '';
  if (FURL = '') then
    Exit;
  urlTarget := URL + TELEGRAM_COMMAND_GETME;

  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      Result := FResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

end;

function TTelegramIntegration.SendMessage(const ChatID: string;
  const Text: string; const ReplyToMessageID: string): boolean;
var
  s, urlTarget, payloadAsString: string;
  json: TJSONUtil;
begin
  Result := False;
  FResultCode := 0;
  FResultText := '';
  FResultMessageID := '';
  FIsSuccessfull := False;
  if (ChatID = '') or (ChatID = '0') or (Text = '') or (FURL = '') then
    Exit;

  s := StringReplace(Text, '\n', #10, [rfReplaceAll]);

  json := TJSONUtil.Create;
  json['chat_id'] := ChatId;
  json['text'] := s;
  json['parse_mode'] := FParseMode;
  json['reply_to_message_id'] := ReplyToMessageID;
  json['disable_web_page_preview'] := 'false';
  json['reply_markup/remove_keyboard'] := True;
  payloadAsString := json.AsJSON;
  json.Free;

  urlTarget := URL + 'sendMessage';
  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      RequestBody.Free;
      RequestBody := nil;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
      if FIsSuccessfull then
      begin
        json := TJSONUtil.Create;
        json.LoadFromJsonString(FResultText);
        FResultMessageID := json['result/message_id'];
        json.Free;
        Result := True;
      end;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendMessage(const ChatID: integer;
  const Text: string; const ReplyToMessageID: integer): boolean;
begin
  try
    SendMessage(i2s(ChatID), Text, i2s(ReplyToMessageID));
  except
  end;
end;

function TTelegramIntegration.SendMessageAsThread(const ChatID: string;
  const Text: string; const ReplyToMessageID: string): boolean;
begin
  if ChatID.IsEmpty then Exit;
  if Text.IsEmpty then Exit;
  with TTelegramSimpleSendThread.Create(True) do
  begin
    ThreadChatID := ChatID;
    ThreadChatText := Text;
    ThreadReplyTo := ReplyToMessageID;
    FreeOnTerminate := True;
    OnExecuteHandler := @SendMessageAsThreadExecute;
    Start;
  end;
end;

procedure TTelegramIntegration.SendMessageAsThreadExecute(const ChatID: string;
  const Text: string; const ReplyToMessageID: string);
begin
  SendMessage(ChatID, Text, ReplyToMessageID);
end;

function TTelegramIntegration.EditMessage(const ChatID: string;
  MessageID: string; Text: string): boolean;
var
  s, urlTarget, payloadAsString: string;
  json: TJSONUtil;
begin
  Result := False;
  FResultCode := 0;
  FResultText := '';
  FResultMessageID := '';
  FIsSuccessfull := False;
  if (ChatID = '') or (ChatID = '0') or (Text = '') or (FURL = '') then
    Exit;

  s := StringReplace(Text, '\n', #10, [rfReplaceAll]);
  json := TJSONUtil.Create;
  json['chat_id'] := ChatId;
  json['message_id'] := MessageID;
  json['parse_mode'] := FParseMode;
  json['disable_web_page_preview'] := 'false';
  json['text'] := s;
  payloadAsString := json.AsJSON;
  json.Free;

  urlTarget := URL + 'editMessageText';
  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.EditMessage(const AChatID: string;
  AMessageID: string; AText: string; AData: TJSONUtil): boolean;
var
  urlTarget, payloadAsString: string;
  json: TJSONUtil;
begin
  Result := False;
  if (AChatId = '') or (AChatId = '0') or (AText = '') or (FURL = '')
    or (AData.Data.Count = 0) then
    Exit;

  json := TJSONUtil.Create;
  json['chat_id'] := AChatId;
  json['message_id'] := AMessageID;
  json['text'] := AText;
  json['parse_mode'] := FParseMode;
  json.ValueArray['reply_markup/inline_keyboard'] := TJSONArray(AData.Data);

  payloadAsString := json.AsJSON;
  json.Free;

  urlTarget := URL + 'editMessageText';
  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      if Response.ResultCode = 200 then
        Result := True;
    except
    end;
    Free;
  end;
end;

function TTelegramIntegration.DeleteMessage(const AChatID: string;
  AMessageID: string): boolean;
var
  s, urlTarget: string;
begin
  Result := False;

  if AChatID.IsEmpty or AMessageID.IsEmpty then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_DELETEMESSAGE,
    [AChatID, AMessageID]);
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
      Result := False;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
    Free;
  end;
  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendAudio(const ChatID: string;
  const AAudioURL: string; const ACaption: string;
  const ReplyToMessageID: string): boolean;
var
  urlTarget: string;
  json: TJSONUtil;
  errorCode: integer;
begin
  Result := False;
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  if (ChatID = '') or (AAudioURL = '') then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_SENDAUDIO,
    [ChatID, ACaption, AAudioURL]);
  if ReplyToMessageID <> '' then
    urlTarget := urlTarget + '&reply_to_message_id=' + ReplyToMessageID;

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/x-www-form-urlencoded';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      json := TJSONUtil.Create;
      json.LoadFromJsonString(FResultText);
      errorCode := json['error_code'];
      json.Free;

      if errorCode <> 200 then
      begin
        LogUtil.Add(urlTarget, 'TAUDIO');
      end;

      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendPhoto(const ChatID: string;
  const FileName: string; const Caption: string;
  const ReplyToMessageID: integer): boolean;
begin
  try
    SendPhoto(StrToInt(ChatID), FileName, Caption, ReplyToMessageID);
  except
  end;
end;

function TTelegramIntegration.SendPhoto(const ChatID: integer;
  const FileName: string; const Caption: string;
  const ReplyToMessageID: integer): boolean;
var
  urlTarget: string;
begin
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  Result := False;
  if (ChatID = 0) or (FileName = '') or (FURL = '') then
    Exit;
  if not FileExists(FileName) then
    Exit;
  urlTarget := URL + format(TELEGRAM_COMMAND_SENDPHOTO, [i2s(ChatID), UrlEncode(Caption), FParseMode]);
  if ReplyToMessageID <> 0 then
    urlTarget := urlTarget + '&reply_to_message_id=' + IntToStr(ReplyToMessageID);

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/x-www-form-urlencoded';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      FormData['chat_id'] := IntToStr(ChatID);
      //FormData['caption'] := Caption;
      AddFile(FileName, 'photo');
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendPhotoFromURL(const ChatID: string;
  const AImageURL: string; const Caption: string;
  const ReplyToMessageID: string): boolean;
var
  urlTarget: string;
begin
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  Result := False;
  if (ChatID = '') or (AImageURL = '') or (FURL = '') then
    Exit;
  urlTarget := URL + format(TELEGRAM_COMMAND_SENDPHOTO,
    [ChatID, UrlEncode(Caption), FParseMode]);
  if ReplyToMessageID <> '' then
    urlTarget := urlTarget + '&reply_to_message_id=' + ReplyToMessageID;

  urlTarget := urlTarget + '&chat_id=' + ChatID;
  urlTarget := urlTarget + '&photo=' + AImageURL;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/x-www-form-urlencoded';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendVideo(const ChatID: string;
  const FileName: string; const Caption: string;
  const ReplyToMessageID: integer): boolean;
var
  urlTarget: string;
begin
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  Result := False;
  if (ChatID = '') or (FileName = '') or (FURL = '') then
    Exit;
  if not FileExists(FileName) then
    Exit;
  urlTarget := URL + format(TELEGRAM_COMMAND_SENDVIDEO, [ChatID, Caption, FParseMode]);
  if ReplyToMessageID <> 0 then
    urlTarget := urlTarget + '&reply_to_message_id=' + IntToStr(ReplyToMessageID);

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/x-www-form-urlencoded';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      FormData['chat_id'] := ChatID;
      FormData['caption'] := Caption;
      FormData['parse_mode'] := FParseMode;
      AddFile(FileName, 'video');
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  Result := FIsSuccessfull;

end;

function TTelegramIntegration.SendVenue(const ChatID: string;
  const AName: string; const AAddress: string; ALatitude, ALongitude: double;
  const ReplyToMessageID: string): boolean;
var
  urlTarget: string;
begin
  Result := False;
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;

  if (ChatID = '') or (ALatitude = 0) or ( ALongitude=0) then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_SENDVENUE,
    [ChatID, UrlEncode(AName), UrlEncode(AAddress), ALatitude, ALongitude, FParseMode]);

  if ReplyToMessageID <> '' then
    urlTarget := urlTarget + '&reply_to_message_id=' + ReplyToMessageID;

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/x-www-form-urlencoded';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      if FResultCode = 200 then
        FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  Result := FIsSuccessfull;
end;


function TTelegramIntegration.SendContact(const ChatID: integer;
  FirstName, LastName, PhoneNumber: string): boolean;
var
  urlTarget: string;
begin
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  Result := False;
  if (ChatID = 0) or (PhoneNumber = '') or (FURL = '') then
    Exit;
  urlTarget := URL + format(TELEGRAM_COMMAND_SENDCONTACT,
    [ChatID, PhoneNumber, FirstName, LastName]);
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  Result := FIsSuccessfull;

end;

function TTelegramIntegration.SendContact(const ChatID: string;
  FirstName, LastName, PhoneNumber: string): boolean;
begin
  try
    SendContact(StrToInt(ChatID), FirstName, LastName, PhoneNumber);
  except
  end;
end;

function TTelegramIntegration.SendDocument(const ChatID: string;
  const AFile: string; const ACaption: string; const ReplyToMessageID: string): boolean;
var
  urlTarget: string;
begin
  Result := False;
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  if (ChatID = '') or (AFile = '') then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_SENDDOCUMENT,
    [ChatID, ACaption, FParseMode]);
  if ReplyToMessageID <> '' then
    urlTarget := urlTarget + '&reply_to_message_id=' + ReplyToMessageID;

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/x-www-form-urlencoded';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      //FormData['caption'] := Caption;
      AddFile(AFile, 'document');
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendInlineKeyboard(const ChatId: string;
  const AText: string; const AData: TJSONUtil): boolean;
var
  payloadAsString, urlTarget: string;
  json: TJSONUtil;
begin
  Result := False;
  if (ChatID = '') or (ChatID = '0') or (AText = '') or (FURL = '')
    or (AData.Data.Count = 0) then
    Exit;

  json := TJSONUtil.Create;
  json['chat_id'] := ChatId;
  json['text'] := AText;
  json['parse_mode'] := FParseMode;
  json.ValueArray['reply_markup/inline_keyboard'] := TJSONArray(AData.Data);

  payloadAsString := json.AsJSON;
  json.Free;

  urlTarget := URL + 'sendMessage?parse_mode=' + FParseMode;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      if Response.ResultCode = 200 then
        Result := True;
    except
    end;
    Free;
  end;
end;

function TTelegramIntegration.SendKeyboard(const ChatId: string;
  const AText: string; const AData: TJSONUtil): boolean;
var
  payloadAsString, urlTarget: string;
  json: TJSONUtil;
begin
  Result := False;
  if (ChatID = '') or (ChatID = '0') or (AText = '') or (FURL = '')
    or (AData.Data.Count = 0) then
    Exit;

  json := TJSONUtil.Create;
  json['chat_id'] := ChatId;
  json['text'] := AText;
  json['parse_mode'] := FParseMode;
  json.ValueArray['reply_markup/keyboard'] := TJSONArray(AData.Data);

  payloadAsString := json.AsJSON;
  json.Free;

  urlTarget := URL + 'sendMessage?parse_mode=' + FParseMode;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      if Response.ResultCode = 200 then
        Result := True;
    except
    end;
    Free;
  end;
end;

function TTelegramIntegration.RemoveKeyboard(const ChatId: string;
  const AText: string): boolean;
var
  payloadAsString, urlTarget: string;
  json: TJSONUtil;
begin
  Result := False;
  if (ChatID = '') or (ChatID = '0') or (AText = '') or (FURL = '') then
    Exit;

  json := TJSONUtil.Create;
  json['chat_id'] := ChatId;
  json['text'] := AText;
  json['parse_mode'] := FParseMode;
  json['reply_markup/remove_keyboard'] := True;

  payloadAsString := json.AsJSON;
  json.Free;

  urlTarget := URL + 'sendMessage?parse_mode=' + FParseMode;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      if Response.ResultCode = 200 then
        Result := True;
    except
    end;
    Free;
  end;

end;

// example result: "photo/file_2"
function TTelegramIntegration.GetFilePath(FileID: string): string;
var
  urlTarget: string;
  json: TJSONUtil;
begin
  Result := '';
  urlTarget := URL + TELEGRAM_COMMAND_GETFILE + FileID;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
      if FResultCode <> 200 then
        Exit;
    except
    end;
    Free;
  end;

  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(FResultText);
    Result := json['result/file_path'];
  except
  end;
  json.Free;
end;

function TTelegramIntegration.GetFullFileURL(FileID: string): string;
begin
  Result := GetFilePath(FileID);
  Result := format(TELEGRAM_FILEURL, [Token]) + Result;
end;

function TTelegramIntegration.DownloadFile(FilePath: string;
  TargetFile: string): boolean;
var
  urlTarget: string;
begin
  Result := False;
  urlTarget := format(TELEGRAM_FILEURL, [FToken]) + FilePath;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FIsSuccessfull := IsSuccessfull;
      if FResultCode = 200 then
      begin
        if FileExists(TargetFile) then
          DeleteFile(TargetFile);
        Response.ResultStream.SaveToFile(TargetFile);
        Result := True;
      end;
    except
      on E: Exception do
      begin
        LogUtil.Add('download: ' + E.Message, 'telegram');
      end;
    end;
    Free;
  end;
end;

function TTelegramIntegration.GroupMemberCount(AGroupID: string): integer;
var
  s, urlTarget: string;
begin
  Result := 0;
  urlTarget := URL + TELEGRAM_COMMAND_GETGROUPMEMBERCOUNT + AGroupID;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  if FResultCode <> 200 then
    Exit;

  jsonData := GetJSON(FResultText);
  s := jsonGetData(jsonData, 'result');
  Result := s2i(s);
end;

function TTelegramIntegration.GroupAdminList(AGroupID: string; Formated: boolean
  ): string;
var
  i: integer;
  s, firstName, lastName, urlTarget: string;
  json: TJSONData;
begin
  Result := '';
  urlTarget := URL + TELEGRAM_COMMAND_GETGROUPADMINISTRATOR + AGroupID;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
    Free;
  end;

  if FResultCode <> 200 then
    Exit;

  try
    json := GetJSON(FResultText, False);
    i := 0;
    s := jsonGetData(json, 'result[0]/user/id');
    firstName := trim(jsonGetData(json, 'result[0]/user/first_name'));
    lastName := trim(jsonGetData(json, 'result[0]/user/last_name'));
    firstName := firstName+' '+lastName;
    firstName := trim(SafeText(firstName, ' '));
    s := '['+firstName+'](tg://user?id='+ s + ')';
    repeat
      if s <> '' then
        Result := Result + s + ', ';
      i := i + 1;
      s := jsonGetData(json, 'result[' + i2s(i) + ']/user/id');
      if not s.IsEmpty then begin
        firstName := trim(jsonGetData(json, 'result[' + i2s(i) + ']/user/first_name'));
        lastName := trim(jsonGetData(json, 'result[' + i2s(i) + ']/user/last_name'));
        firstName := firstName+' '+lastName;
        firstName := trim(SafeText(firstName, ' '));
        if firstName.IsEmpty then
          firstName := trim(jsonGetData(json, 'result[' + i2s(i) + ']/user/username'));
        if firstName.IsEmpty then
          firstName := 'Admin';
        s := '['+firstName+'](tg://user?id='+ s + ')';
      end;
    until s = '';
    json.Free;
  except
  end;
  Result := Result.Trim;
  Result := copy(Result, 0, length(Result) - 1);
end;

function TTelegramIntegration.isImage(ADetail: boolean): boolean;
begin
  Result := False;

  FImageID := '';
  FImageURL := '';
  FImagePath := '';
  try
    FImageID := jsonData.GetPath('message.photo[2].file_id').AsString;
  except
    try
      FImageID := jsonData.GetPath('message.photo[1].file_id').AsString;
    except
      try
        FImageID := jsonData.GetPath('message.photo[0].file_id').AsString;
      except
        on e: Exception do
        begin
        end;
      end;
    end;
  end;

  if FImageID = '' then
    Exit;

  Result := True;
  if not ADetail then
    Exit;

  FImagePath := GetFilePath(FImageID);
  if FImagePath = '' then
    Exit;

  FImageURL := format(TELEGRAM_FILEURL, [FToken]) + FImagePath;
end;

function TTelegramIntegration.KickUser(AChatID: string; AUserID: string;
  AReason: string; AUntilDate: Integer): boolean;
var
  urlTarget: string;
begin
  Result := False;
  if AChatID.IsEmpty then
    Exit;
  if AUserID.IsEmpty then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_RESTRICTUSER,
    [ChatID, AUserID, AUntilDate]);
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;

      Result := True;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
    Free;
  end;

end;

function TTelegramIntegration.RestrictUser(AChatID: string; AUserID: string;
  AUntilDate: integer; ASendMessage: boolean; ASendMedia: boolean;
  ASendOther: boolean; ASendWebPreview: boolean): boolean;
var
  urlTarget: string;
begin
  Result := False;
  if AChatID.IsEmpty then
    Exit;
  if AUserID.IsEmpty then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_RESTRICTUSER,
    [ChatID, AUserID, AUntilDate, b2s(ASendMessage), b2s(ASendMedia), b2s(ASendOther), b2s(ASendWebPreview)]);

  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;

      Result := True;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
    Free;
  end;
end;



end.
{
  FORMAT MESSAGE FROM TELEGRAM

  {
    "ok":true,
    "result":[
      {
        "update_id":00000000001,
        "message":{
          "message_id":263,
          "from":{
            "id":2222647,
            "first_name":"firstname",
            "last_name":"lastname",
            "username":"username"
          },
          "chat":{
            "id":123456,
            "first_name":"firstname",
            "last_name":"lastname",
            "username":"username",
            "type":"private"
          },
          "date":1462275476,
          "text":"text message"
        }
      }
    ]
  }


}

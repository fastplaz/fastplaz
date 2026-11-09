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
  opensslsockets, fpopenssl,
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
    FSynchronizeStatus: boolean;
    procedure syncUpdate;
  protected
    procedure Execute; override;
  public
    property OnExecuteHandler: TExecuteHandlerEvent read FOnExecuteHandler write FOnExecuteHandler;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
    // True: OnShowStatus runs on the main thread through Synchronize, which
    // needs CheckSynchronize there (GUI applications). False: it runs on
    // this thread.
    property SynchronizeStatus: boolean read FSynchronizeStatus write FSynchronizeStatus;
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
    FAdminListAsJson: TJSONData;
    FAdminListAsString: string;
    FCallbackData: TStrings;
    FChatInfo,
    FCallbackInlineKeyboard: TJSONUtil;
    FCallbackInstance: string;
    FDebug: boolean;
    FFileName: string;
    FForwardStoryFirstName:string;
    FForwardStoryID:string;
    FForwardStoryUserName:string;
    FImageID: string;
    FImagePath: string;
    FImageURL: string;
    FInvitedBy: string;
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
    FCallbackParsed: boolean;
    FIsCallbackQuery: boolean;
    FUpdatesSynchronize: boolean;
    function sendMediaFile(const ACommand, AFieldName, AChatID, AFilePath,
      ACaption, AReplyToMessageID, AThreadId: string): boolean;
    function getCallbackData: TStrings;
    function getCallbackInlineKeyboard: TJSONUtil;
    function getCallbackInstance: string;
    function getChatID: string;
    function getChatType: string;
    function getFullName: string;
    function getStoryChatId:string;
    function getStoryChatUsername:string;
    function getGroupName: string;
    function getImageCaption: string;
    function getIsBot: boolean;
    function getIsCallbackQuery: boolean;
    function getIsDocument: boolean;
    function getIsForward: boolean;
    function getIsForwardFromDeletedAccount: boolean;
    function getIsForwardFromStory: boolean;
    function getIsGroup: boolean;
    function getIsInvitation: boolean;
    function getIsLocation: boolean;
    function getIsPicture: boolean;
    function getIsReply: boolean;
    function getIsSticker: boolean;
    function getIsTextMentionExists: boolean;
    function getIsTopic: boolean;
    function getIsUserLeft: boolean;
    function getIsVoice: boolean;
    function getLeftUserID: string;
    function getMessageID: string;
    function getReplyFromFullName: string;
    function getReplyFromID: string;
    function getReplyFromMessageID: string;
    function getReplyFromText: string;
    function getReplyFromUserID: string;
    function getReplyFromUserName: string;
    function getText: string;
    function getThreadID: integer; deprecated;
    function getTopicId: integer;
    function getTopicName: string;
    function getUpdateID: integer;
    function getUserID: string;
    function getUserName: string;
    procedure setRequestContent(AValue: string);
    procedure setToken(AValue: string);
    procedure onGetUpdatesExecuteHandler;
    procedure onGetUpdatesHandler( ATelegramResponse: String);
  public
    constructor Create;
    destructor Destroy; override;

    function getUpdates(const UpdateID: integer = 0): string;

    // sendMessage/editMessageText body with the keyboard rows of AData
    // (an array document); '' when there are none. AData is only read.
    function KeyboardPayload(const AChatId, AText, AMarkupPath: string;
      const AData: TJSONUtil; const AThreadId: string; const AMessageID: string = ''): string;

    function getUpdatesDynamic(const UpdateID: integer = 0): Boolean;

    function GetMe: string;
    function SendMessage(const ChatID: string = '0'; const Text: string = '';
      const ReplyToMessageID: string = ''; const AThreadId: string = ''): boolean;
    function SendMessage(const ChatID: integer = 0; const Text: string = '';
      const ReplyToMessageID: integer = 0; const AThreadId: integer = 0): boolean;
    function SendMessageAsThread(const ChatID: string = '0'; const Text: string = '';
      const ReplyToMessageID: string = ''): boolean;
    procedure SendMessageAsThreadExecute(const ChatID: string; const Text: string; const ReplyToMessageID: string);
    function EditMessage(const ChatID: string; MessageID: string;
      Text: string = ''): boolean;
    function EditMessage(const AChatID: string; AMessageID: string;
      AText: string; AData: TJSONUtil): boolean;
    function DeleteMessage(const AChatID: string; AMessageID: string): boolean;
    function SendAudio(const ChatID: string = '0'; const AAudioPath: string = '';
      const ACaption: string = ''; const ReplyToMessageID: string = ''; const AThreadId: string = ''): boolean;
    function SendVoice(const ChatID: string = '0'; const AAudioPath: string = '';
      const ACaption: string = ''; const ReplyToMessageID: string = ''; const AThreadId: string = ''): boolean;
    function SendPhoto(const ChatID: string; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendPhoto(const ChatID: integer; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendPhotoFromURL(const ChatID: string; const AImageURL: string;
      const Caption: string = ''; const ReplyToMessageID: string = ''; const AThreadId: string = ''): boolean;
    function SendVideo(const ChatID: string; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendVenue(const ChatID: string; const AName: string;
      const AAddress: string; ALatitude, ALongitude:double; const ReplyToMessageID: string = ''; const AThreadId: string = ''): boolean;
    function SendContact(const ChatID: integer;
      FirstName, LastName, PhoneNumber: string): boolean;
    function SendContact(const ChatID: string;
      FirstName, LastName, PhoneNumber: string): boolean;
    function SendDocument(const ChatID: string; const AFile: string;
      const ACaption: string = ''; const ReplyToMessageID: string = ''): boolean;
    function SendInlineKeyboard(const ChatId: string; const AText: string;
      const AData: TJSONUtil; const AThreadId: string = ''): boolean;
    function SendKeyboard(const ChatId: string; const AText: string;
      const AData: TJSONUtil; const AThreadId: string = ''): boolean;
    function RemoveKeyboard(const ChatId: string; const AText: string): boolean;
    function GetFilePath(FileID: string): string;
    function GetFullFileURL(FileID: string): string;
    function DownloadFile(FilePath: string; TargetFile: string): boolean;
    function GroupMemberCount(AGroupID: string): integer;
    function GroupAdminList(AGroupID: string; Formated: boolean = True): string;
    function GetChatInfo(AChatId: string): string;

    function IsAdmin(AUserId: string = ''; Force: boolean = True): boolean;
    function IsAdminFromUsername(AUserName: string = ''; Force: boolean = True): boolean;
    function IsImage(ADetail: boolean = False): boolean;
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
    property ThreadId: integer read getThreadID;
    property IsTopic: boolean read getIsTopic;
    property TopicId: integer read getTopicId;
    property TopicName: string read getTopicName;
    property MessageID: string read getMessageID;
    property ChatID: string read getChatID;
    property ChatType: string read getChatType;
    property ChatInfo: TJSONUtil read FChatInfo write FChatInfo;
    property UserID: string read getUserID;
    property LeftUserID: string read getLeftUserID;
    property UserName: string read getUserName;
    property FullName: string read getFullName;
    property StoryChatId: string read getStoryChatId;
    property StoryChatUsername: string read getStoryChatUsername;
    property GroupName: string read getGroupName;
    property AdminListAsJson: TJSONData read FAdminListAsJson;
    property AdminListAsString: string read FAdminListAsString;

    property ReplyFromID: string read getReplyFromID;
    property ReplyFromUserID: string read getReplyFromUserID;
    property ReplyFromUserName: string read getReplyFromUserName;
    property ReplyFromFullName: string read getReplyFromFullName;
    property ReplyFromMessageID: string read getReplyFromMessageID;
    property ReplyFromText: string read getReplyFromText;
    property IsGroup: boolean read getIsGroup;
    property IsBot: boolean read getIsBot;
    property IsInvitation: boolean read getIsInvitation;
    property IsUserLeft: boolean read getIsUserLeft;
    property IsCallbackQuery: boolean read getIsCallbackQuery;
    property IsTextMentionExists: boolean read getIsTextMentionExists;
    property IsReply: boolean read getIsReply;

    property InvitedUserId: string read FInvitedUserId;
    property InvitedUserName: string read FInvitedUserName;
    property InvitedFullName: string read FInvitedFullName;
    property InvitedBy: string read FInvitedBy;

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
    property IsForward: boolean read getIsForward;
    property IsForwardFromDeletedAccount: boolean read getIsForwardFromDeletedAccount;
    property IsForwardFromStory: boolean read getIsForwardFromStory;

    property ForwardStoryID: string read FForwardStoryID;
    property ForwardStoryFirstName: string read FForwardStoryFirstName;
    property ForwardStoryUsername: string read FForwardStoryUserName;

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
    // getUpdatesDynamic: deliver OnMessage on the main thread (needs
    // CheckSynchronize there); default False, delivered on the worker thread
    property UpdatesSynchronize: boolean read FUpdatesSynchronize write FUpdatesSynchronize;

    // parsed on first use for the current RequestContent
    property CallbackInstance: string read getCallbackInstance;
    property CallbackData: TStrings read getCallbackData;
    property CallbackInlineKeyboard: TJSONUtil read getCallbackInlineKeyboard;
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
  // uploads: every parameter travels in the multipart body
  TELEGRAM_COMMAND_SENDPHOTO_UPLOAD = 'sendPhoto';
  TELEGRAM_COMMAND_SENDVIDEO = 'sendVideo';
  TELEGRAM_COMMAND_SENDAUDIO = 'sendAudio';
  TELEGRAM_COMMAND_SENDVOICE = 'sendVoice';
  TELEGRAM_COMMAND_SENDDOCUMENT = 'sendDocument';
  TELEGRAM_COMMAND_SENDVENUE = 'sendVenue?chat_id=%s&title=%s&address=%s&latitude=%s&longitude=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDCONTACT =
    'sendContact?chat_id=%s&phone_number=%s&first_name=%s&last_name=%s';
  TELEGRAM_COMMAND_GETFILE = 'getFile?file_id=';
  TELEGRAM_COMMAND_GETGROUPADMINISTRATOR = 'getChatAdministrators?chat_id=';
  TELEGRAM_COMMAND_GETGROUPMEMBERCOUNT = 'getChatMembersCount?chat_id=';
  TELEGRAM_COMMAND_GETCHATINFO = 'getChat?chat_id=';

  TELEGRAM_COMMAND_KICKUSER = 'kickChatMember?chat_id=%s&user_id=%s&until_date=%d';
  TELEGRAM_COMMAND_RESTRICTUSER = 'restrictChatMember?chat_id=%s&user_id=%s&until_date=%d'
    + '&can_send_messages=%s&can_send_media_messages=%s'
    + '&can_send_other_messages=%s&can_add_web_page_previews=%s';


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
  // Synchronize never returns without CheckSynchronize on the main thread
  // (a CGI or daemon has none), which left GetUpdatesInProcess stuck
  if FSynchronizeStatus then
    Synchronize(@syncUpdate)
  else
    syncUpdate;
end;


// coordinates for a query string: always a dot, whatever the locale
function floatToDot(AValue: double): string;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  fs.ThousandSeparator := #0;
  Result := FormatFloat('0.######', AValue, fs);
end;

{ TTelegramIntegration }

// chat_id, caption, parse_mode, reply and thread go in the multipart body
// together with the file; the old code put some of them in the URL
// (unencoded, and after a path without "?")
function TTelegramIntegration.sendMediaFile(const ACommand, AFieldName,
  AChatID, AFilePath, ACaption, AReplyToMessageID, AThreadId: string): boolean;
var
  Response: IHTTPResponse;
  json: TJSONUtil;
begin
  Result := False;
  FResultCode := 0;
  FResultText := '';
  FResultMessageID := '';
  FIsSuccessfull := False;
  if (AChatID = '') or (AChatID = '0') or (AFilePath = '') or (FURL = '') then
    Exit;
  if not FileExists(AFilePath) then
    Exit;

  with THTTPLib.Create(URL + ACommand) do
  try
    try
      AddHeader('Cache-Control', 'no-cache');
      FormData['chat_id'] := AChatID;
      if ACaption <> '' then
      begin
        FormData['caption'] := ACaption;
        FormData['parse_mode'] := FParseMode;
      end;
      if (AReplyToMessageID <> '') and (AReplyToMessageID <> '0') then
        FormData['reply_to_message_id'] := AReplyToMessageID;
      if (AThreadId <> '') and (AThreadId <> '0') then
        FormData['message_thread_id'] := AThreadId;
      AddFile(AFilePath, AFieldName);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
      if FIsSuccessfull then
      begin
        json := TJSONUtil.Create;
        try
          json.LoadFromJsonString(FResultText, False);
          FResultMessageID := json['result/message_id'];
        finally
          json.Free;
        end;
      end
      else if FDebug then
        LogUtil.Add(ACommand + ': ' + FResultText, 'TELEGRAM');
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
    Free;
  end;

  Result := FIsSuccessfull;
end;

// sendMessage/editMessageText body with a keyboard; the rows are copied so
// the caller keeps ownership of AData. '' when AData holds no array.
function TTelegramIntegration.KeyboardPayload(const AChatId, AText,
  AMarkupPath: string; const AData: TJSONUtil; const AThreadId: string;
  const AMessageID: string): string;
var
  json: TJSONUtil;
  rows: TJSONArray;
begin
  Result := '';
  if not Assigned(AData) then
    Exit;
  rows := AData.AsArray;
  if (not Assigned(rows)) or (rows.Count = 0) then
    Exit;
  json := TJSONUtil.Create;
  try
    json['chat_id'] := AChatId;
    if AMessageID <> '' then
      json['message_id'] := AMessageID;
    json['text'] := AText;
    json['parse_mode'] := FParseMode;
    if ((AThreadId <> '') AND (AThreadId <> '0')) then
      json['message_thread_id'] := AThreadId;
    json.ValueArray[AMarkupPath] := TJSONArray(rows.Clone);
    Result := json.AsJSON;
  finally
    json.Free;
  end;
end;

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
  urlTarget := FURL + 'setWebhook?url=' + UrlEncode(AValue);
  file_get_contents(urlTarget);
end;

function TTelegramIntegration.DeleteWebHook: boolean;
var
  urlTarget: string;
begin
  Result := False;
  if FURL.IsEmpty then Exit;
  urlTarget := FURL + 'deleteWebhook';
  Result := Pos('"ok":true', file_get_contents(urlTarget)) > 0;
end;

procedure TTelegramIntegration.setRequestContent(AValue: string);
begin
  if FRequestContent = AValue then
    Exit;
  FRequestContent := AValue;
  FCallbackParsed := False;
  FIsCallbackQuery := False;
  FreeAndNil(jsonData);
  try
    jsonData := GetJSON(AValue, False);
  except
  end;
end;

function TTelegramIntegration.getCallbackData: TStrings;
begin
  getIsCallbackQuery;
  Result := FCallbackData;
end;

function TTelegramIntegration.getCallbackInlineKeyboard: TJSONUtil;
begin
  getIsCallbackQuery;
  Result := FCallbackInlineKeyboard;
end;

function TTelegramIntegration.getCallbackInstance: string;
begin
  getIsCallbackQuery;
  Result := FCallbackInstance;
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

function TTelegramIntegration.getThreadID: integer;
var
  s: string;
begin
  Result := 0;
  try
    Result := jsonData.GetPath('message.message_thread_id').AsInteger;
  except
  end;
  if Result = 0 then
  begin
    try
      s := jsonData.GetPath('callback_query.message.message_thread_id').AsString;
      Result := s2i(s);
    except
    end;
  end;
end;

function TTelegramIntegration.getTopicId: integer;
var
  s: string;
begin
  Result := 0;
  try
    Result := jsonData.GetPath('message.message_thread_id').AsInteger;
  except
  end;
  if Result = 0 then
  begin
    try
      s := jsonData.GetPath('callback_query.message.message_thread_id').AsString;
      Result := s2i(s);
    except
    end;
  end;
end;

function TTelegramIntegration.getTopicName: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.reply_to_message.forum_topic_created.name').AsString;
  except
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
    Result := jsonGetData(jsonData, 'message/from/first_name')
      + ' ' + jsonGetData(jsonData, 'message/from/last_name');
    Result := Result.Trim;
    if Result = '' then
      Result := UserName;
  except
  end;
  if IsCallbackQuery then
  begin
    try
      Result := jsonGetData(jsonData, 'callback_query/from/first_name')
        + ' ' + jsonGetData(jsonData, 'callback_query/from/last_name');
      Result := Result.Trim;
    except
    end;
  end;
  Result := ReplaceAll(Result,['.','-','_',',','|','*'],'').Replace('  ',' ').Trim;
end;

function TTelegramIntegration.getStoryChatId:string;
begin
  Result := '';
  try
    Result := jsonGetData(jsonData, 'message/story/chat/id');
    Result := Result.Trim;
  except
  end;
end;

function TTelegramIntegration.getStoryChatUsername:string;
begin
  Result := '';
  try
    Result := jsonGetData(jsonData, 'message/story/chat/username');
  except
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
// parsed once per RequestContent: the callback data and keyboard objects
// stay valid for the whole request
function TTelegramIntegration.getIsCallbackQuery: boolean;
var
  s: string;
begin
  if FCallbackParsed then
  begin
    Result := FIsCallbackQuery;
    Exit;
  end;
  FCallbackParsed := True;
  FIsCallbackQuery := False;
  FreeAndNil(FCallbackData);
  FreeAndNil(FCallbackInlineKeyboard);
  Result := False;
  if not Assigned(jsonData) then
    Exit;
  try
    s := jsonData.GetPath('callback_query').AsJSON;
    if not s.IsEmpty then
    begin
      FCallbackInstance:= jsonData.GetPath('callback_query.chat_instance').AsString;
      s := jsonData.GetPath('callback_query.data').AsString;
      FCallbackData := Explode(s, '&');

      // get inline keyboard data
      FCallbackInlineKeyboard := TJSONUtil.Create;
      try
        s := jsonData.GetPath('callback_query.message.reply_markup.inline_keyboard').AsJSON;
        FCallbackInlineKeyboard.LoadFromJsonString(s, False);
      except
      end;

      Result := True;
    end;
  except
  end;
  FIsCallbackQuery := Result;
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

function TTelegramIntegration.getIsForward: boolean;
begin
  Result := False;
  try
    jsonData.GetPath('message.forward_origin').AsJSON;
    Result := True;
  except
  end;
end;

function TTelegramIntegration.getIsForwardFromDeletedAccount: boolean;
var
  originSenderName: string;
begin
  Result := False;
  try
    originSenderName := jsonData.GetPath('message.forward_origin.sender_user_name').AsString;
    if originSenderName = 'Deleted Account' then
      Result := True;
  except
  end;
end;

function TTelegramIntegration.getIsForwardFromStory:boolean;
begin
  Result := false;
  try
    FForwardStoryID := jsonData.GetPath('message.story.chat.id').AsString;
    FForwardStoryFirstName := jsonData.GetPath('message.story.chat.first_name').AsString;
    FForwardStoryUserName := jsonData.GetPath('message.story.chat.username').AsString;
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
  FInvitedBy := '';
  FInvitedFullName := jsonGetData(jsonData, 'message/new_chat_member/first_name')
    + ' ' + jsonGetData(jsonData, 'message/new_chat_member/last_name');
  FInvitedFullName := FInvitedFullName.Trim;

  FInvitedUserId := jsonGetData(jsonData, 'message/new_chat_member/id');
  FInvitedUserName := jsonGetData(jsonData, 'message/new_chat_member/username');
  if FInvitedUserName.IsEmpty then
    FInvitedUserName := '+' + jsonGetData(jsonData, 'message/new_chat_member/id');

  try
    FInvitedBy := jsonGetData(jsonData, 'message/from/id');
    if (InvitedBy = FInvitedUserId) then
      FInvitedBy := '';
  except
  end;

  if FInvitedUserId <> '' then
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

function TTelegramIntegration.getIsReply: boolean;
begin
  Result := False;
  if not getReplyFromID.IsEmpty then
    Result := True;
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
        if FDebug then
          LogUtil.Add('entities: ' + e.Message, 'TELEGRAM');
      end;
    end;
  end;//IsCallbackQuery
end;

function TTelegramIntegration.getIsTopic: boolean;
begin
  Result := False;
  try
    Result := jsonData.GetPath('message.is_topic_message').AsBoolean;
  except
  end;
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
  Result := jsonGetData(jsonData, 'message.message_id');
  if Result.IsEmpty then
    Result := jsonGetData(jsonData, 'edited_message.message_id');

  if IsCallbackQuery then
  begin
    try
      Result := jsonData.GetPath('callback_query.message.message_id').AsString;
    except
    end;
  end;
end;

function TTelegramIntegration.getReplyFromFullName: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.reply_to_message.from.first_name').AsString;
    Result += ' ';
    Result += jsonData.GetPath('message.reply_to_message.from.last_name').AsString;
  except
  end;
  Result := Result.Trim;
end;

function TTelegramIntegration.getReplyFromID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.reply_to_message.from.id').AsString;
  except
  end;
end;

function TTelegramIntegration.getReplyFromMessageID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.reply_to_message.message_id').AsString;
  except
  end;
end;

function TTelegramIntegration.getReplyFromText: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.reply_to_message.text').AsString;
  except
  end;
end;

function TTelegramIntegration.getReplyFromUserID: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.reply_to_message.from.id').AsString;
  except
  end;
end;

function TTelegramIntegration.getReplyFromUserName: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('message.reply_to_message.from.username').AsString;
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
  FAdminListAsString := '';
end;

destructor TTelegramIntegration.Destroy;
begin
  if Assigned(FCallbackInlineKeyboard) then
    FCallbackInlineKeyboard.Free;
  if Assigned(FCallbackData) then
    FCallbackData.Free;
  if Assigned(jsonData) then
    jsonData.Free;
  if Assigned(FAdminListAsJson) then
    FAdminListAsJson.Free;
  if Assigned(FChatInfo) then
    FChatInfo.Free;
  inherited Destroy;
end;

function TTelegramIntegration.getUpdates(const UpdateID: integer): string;
var
  Response: IHTTPResponse;
  urlTarget: string;
  j, a: TJSONData;
begin
  FIsSuccessfull := False;
  Result := '{"ok":false}';
  FLastUpdateID := 0;
  if FURL = '' then
    Exit;

  urlTarget := URL + TELEGRAM_COMMAND_GETUPDATES + '?offset=' + IntToStr(UpdateID);
  with THTTPLib.Create(urlTarget) do
  try
    try
      AddHeader('Cache-Control', 'no-cache');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      Result := FResultText;
      FIsSuccessfull := IsSuccessfull;
    except
      on E:Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
    Free;
  end;

  // the next offset is the last update id + 1
  j := nil;
  try
    j := GetJSON(FResultText, False);
    a := j.FindPath('result');
    if Assigned(a) and (a.JSONType = jtArray) and (a.Count > 0) then
      FLastUpdateID := a.Items[a.Count - 1].GetPath('update_id').AsInteger + 1;
  except
  end;
  j.Free;
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
    SynchronizeStatus := FUpdatesSynchronize;
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
  i: integer;
  jData, aData: TJSONData;
  aReply: string;
  aHandled: boolean;
begin
  // get result from FGetUpdatesContent
  if not Assigned(FOnMessage) then
  begin
    FGetUpdatesInProcess := False;
    Exit;
  end;

  jData := nil;
  try
    try
      jData := GetJSON(FGetUpdatesContent, False);
      if jData.GetPath('ok').AsBoolean then
      begin
        aData := jData.FindPath('result');
        if Assigned(aData) and (aData.JSONType = jtArray) then
        begin
          for i := 0 to aData.Count - 1 do
          begin
            RequestContent := aData.Items[i].AsJSON;
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
  finally
    jData.Free;
  end;

  FGetUpdatesInProcess := False;
end;

function TTelegramIntegration.GetMe: string;
var
  Response: IHTTPResponse;
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
  try
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
  finally
    Free;
  end;

end;

function TTelegramIntegration.SendMessage(const ChatID: string;
  const Text: string; const ReplyToMessageID: string; const AThreadId: string
  ): boolean;
var
  Response: IHTTPResponse;
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
  if (ReplyToMessageID <> '') and (ReplyToMessageID <> '0') then
    json['reply_to_message_id'] := ReplyToMessageID;
  json['disable_web_page_preview'] := 'false';
  json['reply_markup/remove_keyboard'] := True;
  if ((AThreadId <> '') AND (AThreadId <> '0')) then
  begin
    json['message_thread_id'] := AThreadId;
  end;
  payloadAsString := json.AsJSON;
  json.Free;

  urlTarget := URL + 'sendMessage';
  with THTTPLib.Create(urlTarget) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      if IsSuccessfull then
      begin
        json := TJSONUtil.Create;
        json.LoadFromJsonString(FResultText, False);
        FResultMessageID := json['result/message_id'];
        s := json['ok'];
        if not s.ToLower.Equals('false') then
        begin
          FIsSuccessfull := True;
        end;
        json.Free;
      end;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendMessage(const ChatID: integer;
  const Text: string; const ReplyToMessageID: integer; const AThreadId: integer
  ): boolean;
begin
  Result := SendMessage(i2s(ChatID), Text, i2s(ReplyToMessageID), i2s(AThreadId));
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
  Response: IHTTPResponse;
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
  try
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
  finally
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.EditMessage(const AChatID: string;
  AMessageID: string; AText: string; AData: TJSONUtil): boolean;
var
  Response: IHTTPResponse;
  urlTarget, payloadAsString: string;
begin
  Result := False;
  if (AChatId = '') or (AChatId = '0') or (AText = '') or (FURL = '') then
    Exit;
  payloadAsString := KeyboardPayload(AChatId, AText, 'reply_markup/inline_keyboard',
    AData, '', AMessageID);
  if payloadAsString = '' then
    Exit;

  urlTarget := URL + 'editMessageText';
  with THTTPLib.Create(urlTarget) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
      Result := FIsSuccessfull;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
    Free;
  end;
end;

function TTelegramIntegration.DeleteMessage(const AChatID: string;
  AMessageID: string): boolean;
var
  Response: IHTTPResponse;
  s, urlTarget: string;
begin
  Result := False;

  if AChatID.IsEmpty or AMessageID.IsEmpty then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_DELETEMESSAGE,
    [AChatID, AMessageID]);
  with THTTPLib.Create(urlTarget) do
  try
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
  finally
    Free;
  end;
  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendAudio(const ChatID:string;const AAudioPath:
  string;const ACaption:string;const ReplyToMessageID:string;const AThreadId:
  string):boolean;
begin
  Result := sendMediaFile(TELEGRAM_COMMAND_SENDAUDIO, 'audio', ChatID,
    AAudioPath, ACaption, ReplyToMessageID, AThreadId);
end;

function TTelegramIntegration.SendVoice(const ChatID:string;const AAudioPath:
  string;const ACaption:string;const ReplyToMessageID:string;const AThreadId:
  string):boolean;
begin
  Result := sendMediaFile(TELEGRAM_COMMAND_SENDVOICE, 'voice', ChatID,
    AAudioPath, ACaption, ReplyToMessageID, AThreadId);
end;

// the string overload is the real one: supergroup ids do not fit an integer
function TTelegramIntegration.SendPhoto(const ChatID: string;
  const FileName: string; const Caption: string;
  const ReplyToMessageID: integer): boolean;
begin
  Result := sendMediaFile(TELEGRAM_COMMAND_SENDPHOTO_UPLOAD, 'photo', ChatID,
    FileName, Caption, i2s(ReplyToMessageID), '');
end;

function TTelegramIntegration.SendPhoto(const ChatID: integer;
  const FileName: string; const Caption: string;
  const ReplyToMessageID: integer): boolean;
begin
  Result := SendPhoto(i2s(ChatID), FileName, Caption, ReplyToMessageID);
end;

function TTelegramIntegration.SendPhotoFromURL(const ChatID: string;
  const AImageURL: string; const Caption: string;
  const ReplyToMessageID: string; const AThreadId: string): boolean;
var
  Response: IHTTPResponse;
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
  urlTarget := urlTarget + '&photo=' + UrlEncode(AImageURL);

  //TODO: change to post method and add
  if ((AThreadId <> '') AND (AThreadId <> '0')) then
  begin
    //json['message_thread_id'] := AThreadId;
    urlTarget := urlTarget + '&message_thread_id=' + AThreadId;
  end;

  with THTTPLib.Create(urlTarget) do
  try
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
  finally
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendVideo(const ChatID: string;
  const FileName: string; const Caption: string;
  const ReplyToMessageID: integer): boolean;
begin
  Result := sendMediaFile(TELEGRAM_COMMAND_SENDVIDEO, 'video', ChatID,
    FileName, Caption, i2s(ReplyToMessageID), '');
end;

function TTelegramIntegration.SendVenue(const ChatID: string;
  const AName: string; const AAddress: string; ALatitude, ALongitude: double;
  const ReplyToMessageID: string; const AThreadId: string): boolean;
var
  Response: IHTTPResponse;
  urlTarget: string;
begin
  Result := False;
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;

  if (ChatID = '') or (ALatitude = 0) or ( ALongitude=0) then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_SENDVENUE,
    [ChatID, UrlEncode(AName), UrlEncode(AAddress), floatToDot(ALatitude),
    floatToDot(ALongitude), FParseMode]);

  if ReplyToMessageID <> '' then
    urlTarget := urlTarget + '&reply_to_message_id=' + ReplyToMessageID;

  //TODO: change to post method and add
  if ((AThreadId <> '') AND (AThreadId <> '0')) then
  begin
    //json['message_thread_id'] := AThreadId;
    urlTarget := urlTarget + '&message_thread_id=' + AThreadId;
  end;

  with THTTPLib.Create(urlTarget) do
  try
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
  finally
    Free;
  end;

  Result := FIsSuccessfull;
end;


function TTelegramIntegration.SendContact(const ChatID: integer;
  FirstName, LastName, PhoneNumber: string): boolean;
begin
  Result := SendContact(i2s(ChatID), FirstName, LastName, PhoneNumber);
end;

function TTelegramIntegration.SendContact(const ChatID: string;
  FirstName, LastName, PhoneNumber: string): boolean;
var
  Response: IHTTPResponse;
  urlTarget: string;
begin
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  Result := False;
  if (ChatID = '') or (ChatID = '0') or (PhoneNumber = '') or (FURL = '') then
    Exit;
  urlTarget := URL + format(TELEGRAM_COMMAND_SENDCONTACT,
    [ChatID, UrlEncode(PhoneNumber), UrlEncode(FirstName), UrlEncode(LastName)]);
  with THTTPLib.Create(urlTarget) do
  try
    try
      AddHeader('Cache-Control', 'no-cache');
      Response := Get;
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
  finally
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TTelegramIntegration.SendDocument(const ChatID: string;
  const AFile: string; const ACaption: string; const ReplyToMessageID: string): boolean;
begin
  Result := sendMediaFile(TELEGRAM_COMMAND_SENDDOCUMENT, 'document', ChatID,
    AFile, ACaption, ReplyToMessageID, '');
end;

function TTelegramIntegration.SendInlineKeyboard(const ChatId: string;
  const AText: string; const AData: TJSONUtil; const AThreadId: string
  ): boolean;
var
  Response: IHTTPResponse;
  payloadAsString, urlTarget: string;
  json: TJSONUtil;
begin
  Result := False;
  FResultMessageID := '0';
  if (ChatID = '') or (ChatID = '0') or (AText = '') or (FURL = '') then
    Exit;
  payloadAsString := KeyboardPayload(ChatId, AText, 'reply_markup/inline_keyboard',
    AData, AThreadId);
  if payloadAsString = '' then
    Exit;

  urlTarget := URL + 'sendMessage';
  with THTTPLib.Create(urlTarget) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
      if Response.ResultCode = 200 then
      begin
        Result := True;
        json := TJSONUtil.Create;
        json.LoadFromJsonString(FResultText, False);
        FResultMessageID := json['result/message_id'];
        json.Free;
      end;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
    Free;
  end;
end;

function TTelegramIntegration.SendKeyboard(const ChatId: string;
  const AText: string; const AData: TJSONUtil; const AThreadId: string
  ): boolean;
var
  Response: IHTTPResponse;
  payloadAsString, urlTarget: string;
begin
  Result := False;
  if (ChatID = '') or (ChatID = '0') or (AText = '') or (FURL = '') then
    Exit;
  payloadAsString := KeyboardPayload(ChatId, AText, 'reply_markup/keyboard',
    AData, AThreadId);
  if payloadAsString = '' then
    Exit;

  urlTarget := URL + 'sendMessage';
  with THTTPLib.Create(urlTarget) do
  try
    try
      ContentType := 'application/json';
      AddHeader('Cache-Control', 'no-cache');
      RequestBody := TStringStream.Create(payloadAsString);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
      if Response.ResultCode = 200 then
        Result := True;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
    Free;
  end;
end;

function TTelegramIntegration.RemoveKeyboard(const ChatId: string;
  const AText: string): boolean;
var
  Response: IHTTPResponse;
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
  try
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
  finally
    Free;
  end;

end;

// example result: "photo/file_2"
function TTelegramIntegration.GetFilePath(FileID: string): string;
var
  Response: IHTTPResponse;
  urlTarget: string;
  json: TJSONUtil;
begin
  Result := '';
  urlTarget := URL + TELEGRAM_COMMAND_GETFILE + FileID;
  with THTTPLib.Create(urlTarget) do
  try
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
  finally
    Free;
  end;

  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(FResultText, False);
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
  Response: IHTTPResponse;
  urlTarget: string;
begin
  Result := False;
  urlTarget := format(TELEGRAM_FILEURL, [FToken]) + FilePath;
  with THTTPLib.Create(urlTarget) do
  try
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
  finally
    Free;
  end;
end;

function TTelegramIntegration.GroupMemberCount(AGroupID: string): integer;
var
  Response: IHTTPResponse;
  s, urlTarget: string;
  jsonMember: TJSONData;
begin
  Result := 0;
  urlTarget := URL + TELEGRAM_COMMAND_GETGROUPMEMBERCOUNT + AGroupID;
  with THTTPLib.Create(urlTarget) do
  try
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
  finally
    Free;
  end;

  if FResultCode <> 200 then
    Exit;

  jsonMember := GetJSON(FResultText, False);
  s := jsonGetData(jsonMember, 'result');
  jsonMember.Free;
  Result := s2i(s);
end;

// markdown mentions of the administrators; the list is cached in
// AdminListAsJson for IsAdmin. A failed request clears the cache so a stale
// list of another group is never consulted.
function TTelegramIntegration.GroupAdminList(AGroupID: string; Formated: boolean
  ): string;
var
  Response: IHTTPResponse;
  i: integer;
  s, firstName, lastName, urlTarget: string;
  jsonAdmin, adminList, item: TJSONData;
begin
  Result := '';
  FResultCode := -1;
  FreeAndNil(FAdminListAsJson);
  if (AGroupID = '') or (FURL = '') then
    Exit;
  urlTarget := URL + TELEGRAM_COMMAND_GETGROUPADMINISTRATOR + AGroupID;
  with THTTPLib.Create(urlTarget) do
  try
    try
      AddHeader('Cache-Control', 'no-cache');
      Response := Get;
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
  finally
    Free;
  end;

  if FResultCode <> 200 then
    Exit;

  jsonAdmin := nil;
  try
    try
      jsonAdmin := GetJSON(FResultText, False);
      adminList := jsonAdmin.FindPath('result');
      if (not Assigned(adminList)) or (adminList.JSONType <> jtArray) then
        Exit;
      FAdminListAsJson := adminList.Clone;
      for i := 0 to adminList.Count - 1 do
      begin
        item := adminList.Items[i];
        s := jsonGetData(item, 'user/id');
        if s.IsEmpty then
          Continue;
        firstName := trim(jsonGetData(item, 'user/first_name'));
        lastName := trim(jsonGetData(item, 'user/last_name'));
        firstName := trim(SafeText(trim(firstName + ' ' + lastName), ' '));
        if firstName.IsEmpty then
          firstName := trim(jsonGetData(item, 'user/username'));
        if firstName.IsEmpty then
          firstName := 'Admin';
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + '[' + firstName + '](tg://user?id=' + s + ')';
      end;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
    jsonAdmin.Free;
  end;
end;

function TTelegramIntegration.GetChatInfo(AChatId: string): string;
var
  Response: IHTTPResponse;
  urlTarget: string;
begin
  Result := '';
  FResultCode := -1;
  urlTarget := URL + TELEGRAM_COMMAND_GETCHATINFO + AChatId;
  with THTTPLib.Create(urlTarget) do
  try
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;
    except
    end;
  finally
    Free;
  end;

  if FResultCode <> 200 then
    Exit;

  FreeAndNil(FChatInfo);
  FChatInfo := TJSONUtil.Create;
  FChatInfo.LoadFromJsonString(FResultText);

  Result := FResultText;
end;

function TTelegramIntegration.IsAdmin(AUserId: string; Force: boolean): boolean;
var
  i: integer;
  s, sid: string;
  entities: TJSONArray;
  item: TJSONData;
begin
  Result := False;
  if AUserId.IsEmpty then
    AUserId := getUserID;

  if Force then
  begin
    FAdminListAsString := GroupAdminList(getChatID, False);
    if (not Assigned(FAdminListAsJson)) then
      Exit;
  end;

  if (not Assigned(FAdminListAsJson)) or (FAdminListAsJson.JSONType <> jtArray) then
    Exit;
  try
    entities := TJSONArray(FAdminListAsJson);
    for i:=0 to entities.Count-1 do
    begin
      item := entities.Items[i];
      sid := jsonGetData(item,'user/id');
      if sid = AUserId then
      begin
        Result := True;
        Break;
      end;
    end;
  except
  end;
end;

function TTelegramIntegration.IsAdminFromUsername(AUserName: string;
  Force: boolean): boolean;
var
  i: integer;
  s, sid: string;
  entities: TJSONArray;
  item: TJSONData;
begin
  Result := False;
  if AUserName.IsEmpty then
    AUserName := getUserName;

  if Force then
  begin
    FAdminListAsString := GroupAdminList(getChatID, False);
    if (not Assigned(FAdminListAsJson)) then
      Exit;
  end;

  if (not Assigned(FAdminListAsJson)) or (FAdminListAsJson.JSONType <> jtArray) then
    Exit;
  try
    entities := TJSONArray(FAdminListAsJson);
    for i:=0 to entities.Count-1 do
    begin
      item := entities.Items[i];
      sid := jsonGetData(item,'user/username');
      if sid = AUserName then
      begin
        Result := True;
        Break;
      end;
    end;
  except
  end;
end;

function TTelegramIntegration.IsImage(ADetail: boolean): boolean;
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
  Response: IHTTPResponse;
  urlTarget: string;
begin
  Result := False;
  if AChatID.IsEmpty then
    Exit;
  if AUserID.IsEmpty then
    Exit;

  // the chat is the parameter, not the chat of the incoming message
  if AUntilDate = 0 then
  begin
    urlTarget := TELEGRAM_COMMAND_KICKUSER;
    urlTarget := urlTarget.Replace('&until_date=%d','');
    urlTarget := URL + Format(urlTarget, [AChatID, AUserID]);
  end
  else
  begin
    urlTarget := URL + Format(TELEGRAM_COMMAND_KICKUSER,
      [AChatID, AUserID, AUntilDate]);
  end;

  with THTTPLib.Create(urlTarget) do
  try
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;

      Result := FIsSuccessfull;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
    Free;
  end;

end;

function TTelegramIntegration.RestrictUser(AChatID: string; AUserID: string;
  AUntilDate: integer; ASendMessage: boolean; ASendMedia: boolean;
  ASendOther: boolean; ASendWebPreview: boolean): boolean;
var
  Response: IHTTPResponse;
  urlTarget: string;
begin
  Result := False;
  if AChatID.IsEmpty then
    Exit;
  if AUserID.IsEmpty then
    Exit;

  urlTarget := URL + format(TELEGRAM_COMMAND_RESTRICTUSER,
    [AChatID, AUserID, AUntilDate, b2s(ASendMessage), b2s(ASendMedia), b2s(ASendOther), b2s(ASendWebPreview)]);

  with THTTPLib.Create(urlTarget) do
  try
    try
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;

      Result := FIsSuccessfull;
    except
      on E: Exception do
      begin
        if FDebug then
          LogUtil.Add(E.Message, 'TELEGRAM');
      end;
    end;
  finally
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

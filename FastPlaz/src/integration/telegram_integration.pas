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
  Classes, SysUtils;

{$ifdef TELEGRAM_INTEGRATION}
{$endif}

const
  TELEGRAM_FILEURL = 'https://api.telegram.org/file/bot%s/';


type

  { TTelegramIntegration }

  TTelegramIntegration = class(TInterfacedObject)
  private
    FIsSuccessfull: boolean;
    FLastUpdateID: integer;
    FParseMode: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
    FURL: string;
    procedure setToken(AValue: string);
  public
    constructor Create;

    function getUpdates(const UpdateID: integer = 0): string;
    function GetMe: string;
    function SendMessage(const ChatID: integer = 0; const Text: string = '';
      const ReplyToMessageID: integer = 0): boolean;
    function SendMessage(const ChatID: string = '0'; const Text: string = '';
      const ReplyToMessageID: integer = 0): boolean;
    function SendPhoto(const ChatID: string; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendPhoto(const ChatID: integer; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendVideo(const ChatID: string; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendVideo(const ChatID: integer; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendContact(const ChatID: integer;
      FirstName, LastName, PhoneNumber: string): boolean;
    function SendContact(const ChatID: string;
      FirstName, LastName, PhoneNumber: string): boolean;
    function GetFileURL(FileID: string): string;
    function DownloadFile(FilePath: string; TargetFile: string): boolean;
  published
    property LastUpdateID: integer read FLastUpdateID;
    property URL: string read FURL;
    property Token: string read FToken write setToken;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property ParseMode: string read FParseMode write FParseMode;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

  end;

implementation

const
  TELEGRAM_BASEURL = 'https://api.telegram.org/bot%s/';
  TELEGRAM_COMMAND_GETUPDATES = 'getUpdates';
  TELEGRAM_COMMAND_GETME = 'getMe';
  TELEGRAM_COMMAND_SENDMESSAGE = 'sendMessage?chat_id=%d&text=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDPHOTO = 'sendPhoto?chat_id=%d&caption=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDVIDEO = 'sendVideo?chat_id=%d&caption=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDCONTACT =
    'sendContact?chat_id=%d&phone_number=%s&first_name=%s&last_name=%s';
  TELEGRAM_COMMAND_GETFILE = 'getFile?file_id=';

var
  Response: IHTTPResponse;


{ TTelegramIntegration }

procedure TTelegramIntegration.setToken(AValue: string);
begin
  if FToken = AValue then
    Exit;
  FToken := AValue;
  FURL := format(TELEGRAM_BASEURL, [FToken]);
end;

constructor TTelegramIntegration.Create;
begin
  FURL := '';
  FParseMode := 'Markdown';
  FLastUpdateID := 0;
  FIsSuccessfull := False;
end;

function TTelegramIntegration.getUpdates(const UpdateID: integer): string;
var
  urlTarget, s: string;
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
        s := j.Items[i].AsJSON;
        a := TJSONArray(j.Items[i]);
        a := TJSONArray(a.Items[0]);
        a := TJSONArray(a.Items[0]);
        FLastUpdateID := StrToInt64(a.AsJSON) + 1;
      end;

    except
    end;
    Free;
  end;

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

function TTelegramIntegration.SendMessage(const ChatID: integer;
  const Text: string; const ReplyToMessageID: integer): boolean;
var
  urlTarget: string;
begin
  FResultCode := 0;
  FResultText := '';
  FIsSuccessfull := False;
  Result := False;
  if (ChatID = 0) or (Text = '') or (FURL = '') then
    Exit;
  urlTarget := URL + format(TELEGRAM_COMMAND_SENDMESSAGE, [ChatID, Text, FParseMode]);
  if ReplyToMessageID <> 0 then
    urlTarget := urlTarget + '&parse_mode=Markdown&reply_to_message_id=' +
      IntToStr(ReplyToMessageID);
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

function TTelegramIntegration.SendMessage(const ChatID: string;
  const Text: string; const ReplyToMessageID: integer): boolean;
begin
  try
    SendMessage(StrToInt(ChatID), Text, ReplyToMessageID);
  except
  end;
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
  urlTarget := URL + format(TELEGRAM_COMMAND_SENDPHOTO, [ChatID, Caption, FParseMode]);
  if ReplyToMessageID <> 0 then
    urlTarget := urlTarget + '&reply_to_message_id=' + IntToStr(ReplyToMessageID);

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/x-www-form-urlencoded';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      FormData['chat_id'] := IntToStr(ChatID);
      FormData['caption'] := Caption;
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

function TTelegramIntegration.SendVideo(const ChatID: string;
  const FileName: string; const Caption: string;
  const ReplyToMessageID: integer): boolean;
begin
  try
    SendVideo(StrToInt(ChatID), FileName, Caption, ReplyToMessageID);
  except
  end;
end;

function TTelegramIntegration.SendVideo(const ChatID: integer;
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
  urlTarget := URL + format(TELEGRAM_COMMAND_SENDVIDEO, [ChatID, Caption, FParseMode]);
  if ReplyToMessageID <> 0 then
    urlTarget := urlTarget + '&reply_to_message_id=' + IntToStr(ReplyToMessageID);

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/x-www-form-urlencoded';
      AddHeader('Cache-Control', 'no-cache');
      //AddHeader('Accept', '*/*');
      FormData['chat_id'] := IntToStr(ChatID);
      FormData['caption'] := Caption;
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

// example result: "photo/file_2"
function TTelegramIntegration.GetFileURL(FileID: string): string;
var
  s, urlTarget: string;
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
    s := json['ok'];
    Result := json['result/file_path'];
  except
  end;
  json.Free;
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



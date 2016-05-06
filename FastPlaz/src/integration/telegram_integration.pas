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
  http_lib,
  Classes, SysUtils;

{$ifdef TELEGRAM_INTEGRATION}
{$endif}

type

  { TTelegramIntegration }

  TTelegramIntegration = class(TInterfacedObject)
  private
    FIsSuccessfull: boolean;
    FParseMode: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
    FUpdateID: integer;
    FURL: string;
    procedure setToken(AValue: string);
  public
    constructor Create;

    function getUpdates(const LastUpdateID: integer = 0): boolean;
    function GetMe: string;
    function SendMessage(const ChatID: integer = 0; const Text: string = '';
      const ReplyToMessageID: integer = 0): boolean;
    function SendPhoto(const ChatID: integer; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendVideo(const ChatID: integer; const FileName: string;
      const Caption: string = ''; const ReplyToMessageID: integer = 0): boolean;
    function SendContact(const ChatID: integer;
      FirstName, LastName, PhoneNumber: string): boolean;
  published
    property URL: string read FURL;
    property UpdateID: integer read FUpdateID;
    property Token: string read FToken write setToken;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property ParseMode: string read FParseMode write FParseMode;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

  end;

implementation

const
  TELEGRAM_BASEURL = 'https://api.telegram.org/bot%s/';
  TELEGRAM_COMMAND_GETME = 'getMe';
  TELEGRAM_COMMAND_SENDMESSAGE = 'sendMessage?chat_id=%d&text=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDPHOTO = 'sendPhoto?chat_id=%d&caption=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDVIDEO = 'sendVideo?chat_id=%d&caption=%s&parse_mode=%s';
  TELEGRAM_COMMAND_SENDCONTACT =
    'sendContact?chat_id=%d&phone_number=%s&first_name=%s&last_name=%s';

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
  FUpdateID := 0;
  FIsSuccessfull := False;
end;

function TTelegramIntegration.getUpdates(const LastUpdateID: integer): boolean;
begin
  FIsSuccessfull := False;
  Result := False;
  if FURL = '' then
    Exit;

  //-- todo: manual get update from telegram

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
    urlTarget := urlTarget + '&reply_to_message_id=' + IntToStr(ReplyToMessageID);
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


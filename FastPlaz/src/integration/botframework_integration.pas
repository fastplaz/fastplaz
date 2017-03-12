unit botframework_integration;
{

  [x] USAGE:
  with TMSBotFrameworkIntegration.Create do
  begin
    ClientID := '...';
    ClientSecret := '....';
    Token := '...';
    ReplyMessage( 'your message');

    Free;
  end;

}

{$mode objfpc}{$H+}

interface

uses
  fpjson,
  common, http_lib, logutil_lib, json_lib,
  Classes, SysUtils;

type

  { TMSBotFrameworkIntegration }

  TMSBotFrameworkIntegration = class(TInterfacedObject)
  private
    FClientID: string;
    FClientSecret: string;
    FIsSuccessfull: boolean;
    FMessageType: string;
    FRequestContent: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
    FTokenExpiresIn: integer;
    FTokenType: string;
    jsonData: TJSONData;

    function getChannelID: string;
    function getConversationID: string;
    function getMessageID: string;
    function getMessageType: string;
    function getRecipientID: string;
    function getRecipientName: string;
    function getServiceURL: string;
    function getText: string;
    function getUserID: string;
    function getUserName: string;
    procedure setRequestContent(AValue: string);
  public
    constructor Create;
    destructor Destroy;

    function GetToken: string;
    function GetTokenAsJson: string;
    function isPing: boolean;

    function ReplyMessage( AText:string):boolean;
  published
    property RequestContent: string read FRequestContent write setRequestContent;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property ClientID: string read FClientID write FClientID;
    property ClientSecret: string read FClientSecret write FClientSecret;

    property Token: string read FToken write FToken;
    property TokenType: string read FTokenType;
    property TokenExpiresIn: integer read FTokenExpiresIn;

    property Text:string read getText;
    property MessageType: string read getMessageType;
    property MessageID:string read getMessageID;
    property ChannelID:string read getChannelID;
    property UserID:string read getUserID;
    property UserName:string read getUserName;
    property RecipientID:string read getRecipientID;
    property RecipientName:string read getRecipientName;
    property ConversationID:string read getConversationID;
    property ServiceURL:string read getServiceURL;
  end;


implementation

const
  _BOTFRAMEWORK_URL_TOKEN = 'https://login.microsoftonline.com/common/oauth2/v2.0/token';
  _BOTFRAMEWORK_CONVERSATION_URL = 'https://api.botframework.com/v3/conversations';

var
  Response: IHTTPResponse;

{ TMSBotFrameworkIntegration }

function TMSBotFrameworkIntegration.getMessageType: string;
begin
  FMessageType := jsonGetData(jsonData, 'type');
  Result := FMessageType;
end;

function TMSBotFrameworkIntegration.getRecipientID: string;
begin
  Result := jsonGetData(jsonData, 'recipient/id');
end;

function TMSBotFrameworkIntegration.getRecipientName: string;
begin
  Result := jsonGetData(jsonData, 'recipient/name');
end;

function TMSBotFrameworkIntegration.getServiceURL: string;
begin
  Result := jsonGetData(jsonData, 'serviceUrl');
end;

function TMSBotFrameworkIntegration.getText: string;
begin
  Result := jsonGetData(jsonData, 'text');
end;

function TMSBotFrameworkIntegration.getUserID: string;
begin
  Result := jsonGetData(jsonData, 'from/id');
end;

function TMSBotFrameworkIntegration.getUserName: string;
begin
  Result := jsonGetData(jsonData, 'from/name');
end;

function TMSBotFrameworkIntegration.getChannelID: string;
begin
  Result := jsonGetData(jsonData, 'channelId');
end;

function TMSBotFrameworkIntegration.getConversationID: string;
begin
  Result := jsonGetData(jsonData, 'conversation/id');
end;

function TMSBotFrameworkIntegration.getMessageID: string;
begin
  Result := jsonGetData(jsonData, 'id');
end;

procedure TMSBotFrameworkIntegration.setRequestContent(AValue: string);
begin
  if FRequestContent = AValue then
    Exit;
  FRequestContent := trim(AValue);
  try
    jsonData := GetJSON(FRequestContent);
  except
  end;
end;

constructor TMSBotFrameworkIntegration.Create;
begin
  FIsSuccessfull := False;
  FResultCode := 0;
  FResultText := '';
  FTokenExpiresIn := 0;
  FTokenType := '';
  FMessageType := '';
end;

destructor TMSBotFrameworkIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TMSBotFrameworkIntegration.GetToken: string;
var
  _jsonData: TJSONData;
begin
  Result := GetTokenAsJson;
  if Result = '' then
    Exit;
  try
    _jsonData := GetJSON(Result);
    FToken := jsonGetData(_jsonData, 'access_token');
    Result := FToken;
    FTokenType := jsonGetData(_jsonData, 'token_type');
    FTokenExpiresIn := _jsonData.GetPath('expires_in').AsInteger;
    _jsonData.Free;
  except
  end;
end;

function TMSBotFrameworkIntegration.GetTokenAsJson: string;
begin
  Result := '';
  if (FClientID = '') or (FClientSecret = '') then
    Exit;

  with THTTPLib.Create(_BOTFRAMEWORK_URL_TOKEN) do
  begin
    try
      FormData['client_id'] := FClientID;
      FormData['client_secret'] := FClientSecret;
      FormData['grant_type'] := 'client_credentials';
      FormData['scope'] := 'https://graph.microsoft.com/.default';
      FormData['scopex'] := 'https%3A%2F%2Fgraph.microsoft.com%2F.default';
      FormData['_'] := '_';
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;

      if FResultCode = 200 then
      begin

        Result := FResultText;
      end;

    except
    end;
    Free;
  end;

end;

function TMSBotFrameworkIntegration.isPing: boolean;
begin
  Result := False;
  if MessageType = 'ping' then
    Result := True;
end;

function TMSBotFrameworkIntegration.ReplyMessage(AText: string): boolean;
var
  urlTarget: string;
  jsonParameter: TJSONUtil;
begin
  Result := False;

  // https://directline.botframework.com/v3/conversations/{conversationsID}/activities/{activitesID}
  urlTarget:= ServiceURL + 'v3/conversations/'+ConversationID+'/activities/' + MessageID;

  jsonParameter := TJSONUtil.Create;
  jsonParameter['type'] := 'message';
  jsonParameter['from/id'] := RecipientID;
  jsonParameter['from/name'] := RecipientName;
  jsonParameter['conversation/id'] := ConversationID;
  jsonParameter['recipient/id'] := UserID;
  jsonParameter['recipient/name'] := UserName;
  jsonParameter['text'] := AText;
  jsonParameter['replyToId'] := MessageID;

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Authorization', 'Bearer ' + FToken);
      RequestBody := TStringStream.Create(jsonParameter.AsJSON);
      Response := Post;

      Die(Response.ResultText);

    except
    end;

    Free;
  end;


  die( jsonParameter.AsJSONFormated);
end;

end.



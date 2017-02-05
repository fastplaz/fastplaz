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
    FIsSuccessfull: boolean;
    FRequestContent: string;
    FResultCode: integer;
    FResultText: string;
    FToken: string;
    jsonData: TJSONData;
    function getReplyToken: string;
    function getText: string;
    function getUserID: string;
    procedure setRequestContent(AValue: string);
  public
    constructor Create;
    destructor Destroy;

    property Token: string read FToken write FToken;
    property RequestContent: string read FRequestContent write setRequestContent;

    property IsSuccessfull: boolean read FIsSuccessfull;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property ReplyToken: string read getReplyToken;
    property Text: string read getText;
    property UserID: string read getUserID;

    procedure Reply(AReplyToken: string; AMessages: string);
    procedure Push(ATo: string; AMessages: string);
    procedure Send(ATo: string; AMessages: string);

    function isMessage:boolean;
    function isGroup:boolean;
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
  if FRequestContent=AValue then Exit;
  FRequestContent:=AValue;
  jsonData := GetJSON( AValue);
end;

function TLineIntegration.getReplyToken: string;
begin
  Result := '';
  try
    Result := jsonData.GetPath('events[0].replyToken').AsString;
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
  if FToken = '' then
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
      for i := 0 to lst.Count - 1 do
      begin
        _jsonString.add('{');
        _jsonString.add('"type":"text",');
        _jsonString.add('"text":"' + StringToJSONString(lst[i]) + '"');
        _jsonString.add('}');
        if i <> lst.Count - 1 then
          _jsonString.Add(',');
      end;
      _jsonString.Add(']');
      _jsonString.Add('}');

      RequestBody := TStringStream.Create(_jsonString.Text);

      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;

      die( FResultText);
    except
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
  if FToken = '' then
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
      for i := 0 to lst.Count - 1 do
      begin
        _jsonString.add('{');
        _jsonString.add('"type":"text",');
        _jsonString.add('"text":"' + StringToJSONString(lst[i]) + '"');
        _jsonString.add('}');
        if i <> lst.Count - 1 then
          _jsonString.Add(',');
      end;
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

function TLineIntegration.isMessage: boolean;
begin
  Result := False;
  try
    if jsonData.GetPath('events[0].type').AsString = 'message' then
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

end.


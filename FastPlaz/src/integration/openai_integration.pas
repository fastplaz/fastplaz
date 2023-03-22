unit openai_integration;

{
  // USAGE:

  // Chat Completion
  with TOpenAIIntegration.Create do
  begin
    Key :=  'your openai key';
    if ChatCompletion('cari informasi tentang fastplaz framework') then
    begin
      varResult := Response.AsJSON;

    end;

    .
    .
    .

    Free;
  end;

}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  http_lib, fpjson, Classes, SysUtils;

{$ifdef OPENAI_INTEGRATION}
{$endif}

type

  { TOpenAIIntegration }

  TOpenAIIntegration = class(TInterfacedObject)
  private
    FBaseURL: string;
    FIsSuccessfull: boolean;
    FKey: string;
    FMaxToken: integer;
    FMessages: TJSONArray;
    FModel: string;
    FOnSyncStatus: TNotifyEvent;
    FTemperature: double;
    function isPermitted(): boolean;
  public
    Payload: TJSONObject;
    Response: TJSONData;
    HttpResult: IHTTPResponse;
    HttpResultAsText: string;
    constructor Create;
    destructor Destroy;
    function ChatCompletion(APrompt: string; ARole: string = 'user'): boolean;
  published
    property Messages: TJSONArray read FMessages write FMessages;
    property BaseURL: string read FBaseURL write FBaseURL;
    property Key: string read FKey write FKey;
    property Model: string read FModel write FModel;
    property MaxToken: integer read FMaxToken write FMaxToken;
    property Temperature: double read FTemperature write FTemperature;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property OnSyncStatus: TNotifyEvent read FOnSyncStatus write FOnSyncStatus;
  end;

implementation

const
  OPENAI_BASEURL = 'https://api.openai.com/v1/';
  OPENAI_MODEL_DEFAULT = 'gpt-3.5-turbo';
  OPENAI_TEMPERATURE_DEFAULT = 0;
  OPENAI_MAXTOKEN_DEFAULT = 250;

var
  Response: IHTTPResponse;


{ TOpenAIIntegration }

function TOpenAIIntegration.isPermitted: boolean;
begin
  Result := False;
  if FKey.IsEmpty then Exit;

  Result := True
end;

constructor TOpenAIIntegration.Create;
begin
  FIsSuccessfull := False;
  FBaseURL := OPENAI_BASEURL;
  FModel := OPENAI_MODEL_DEFAULT;
  FMaxToken := OPENAI_MAXTOKEN_DEFAULT;
  FTemperature := OPENAI_TEMPERATURE_DEFAULT;
  FMessages := TJSONArray.Create;
  Payload := TJSONObject.Create;
  Response := TJSONObject.Create;
end;

destructor TOpenAIIntegration.Destroy;
begin
  Payload.Free;
  Response.Free;
end;

function TOpenAIIntegration.ChatCompletion(APrompt: string; ARole: string
  ): boolean;
var
  url: string;
  msg: TJSONObject;
begin
  Result := False;
  Response.Clear;
  if not isPermitted() then Exit;
  if APrompt.IsEmpty then Exit;
  url := FBaseURL + 'chat/completions';

  // current message
  msg := TJSONObject.Create;
  msg.Add('role', ARole);
  msg.Add('content', APrompt.Trim);
  FMessages.Add(msg);

  //TODO: Add previouse messages

  // Payload
  Payload.Add('model', FModel);
  Payload.Add('max_tokens', FMaxToken);
  Payload.Add('temperature', FTemperature);
  Payload.Add('messages', FMessages);

  with THTTPLib.Create(url) do
  begin
    OnSyncStatus := FOnSyncStatus;
    AddHeader('_source', 'carik');
    AddHeader('Authorization', 'Bearer ' + FKey);
    ContentType := 'application/json';
    RequestBody := TStringStream.Create(Payload.AsJSON);
    HttpResult := Post;
    HttpResultAsText:= HttpResult.ResultText;
    if HttpResult.ResultCode = 200 then
    begin
      try
        Response := GetJSON(HttpResultAsText);
        Result := True;
      except
      end;
    end;
    Free;
  end;


end;


end.

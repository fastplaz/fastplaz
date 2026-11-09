unit test_integration;

{
  The parts of src/integration that work without a network: webhook
  parsing, request bodies, URL building and object lifetime.
}

{$mode objfpc}{$H+}

interface

uses
  telegram_integration, facebookmessenger_integration, line_integration,
  whois_integration, graphql_integration, slack_integration,
  botframework_integration, elasticsearch_integration, openai_integration,
  firebase_integration, googleplacesearch_integration,
  json_lib, fpjson, Classes, SysUtils, fpcunit, testregistry;

type

  { TTelegramIntegrationTest }

  TTelegramIntegrationTest = class(TTestCase)
  private
    FTelegram: TTelegramIntegration;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTokenBuildsURL;
    procedure TestParseGroupMessage;
    procedure TestFullNameFallsBackToUsername;
    procedure TestParseCallbackQuery;
    procedure TestCallbackObjectsStableWithinRequest;
    procedure TestKeyboardPayloadCopiesRows;
    procedure TestKeyboardPayloadNeedsArray;
    procedure TestSendWithoutTokenOrFileFails;
    procedure TestSupergroupIdThroughStringOverloads;
  end;

  { TFacebookIntegrationTest }

  TFacebookIntegrationTest = class(TTestCase)
  private
    FFacebook: TFacebookMessengerIntegration;
    FSeenPayload, FSeenTitle: string;
    function menuHandler(const APayload, ATitle: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParseMessage;
    procedure TestParsePostbackAndHandler;
    procedure TestPayloadHandlingWithoutPayload;
    procedure TestHandlersArePerInstance;
    procedure TestInvalidContentIsIgnored;
    procedure TestQuickReplyRows;
  end;

  { TLineIntegrationTest }

  TLineIntegrationTest = class(TTestCase)
  published
    procedure TestTemplateButtonsJsonIsRepeatable;
    procedure TestTemplateCarouselColumns;
    procedure TestTemplateAsJsonString;
    procedure TestParseMessageEvent;
    procedure TestParsePostbackTwice;
  end;

  { TSmallIntegrationsTest }

  TSmallIntegrationsTest = class(TTestCase)
  published
    procedure TestWhoisExtensionOf;
    procedure TestGraphQLRequestBodyIsEscaped;
    procedure TestSlackPayloadIsEscaped;
    procedure TestBotFrameworkServiceURLSlash;
    procedure TestElasticsearchURL;
    procedure TestOpenAIPayloadRebuiltPerCall;
    procedure TestCreateAndFree;
  end;

implementation

const
  TG_GROUP_MESSAGE =
    '{"update_id":100,"message":{"message_id":263,'
    + '"from":{"id":2222647,"is_bot":false,"first_name":"Luri","last_name":"D.","username":"luri"},'
    + '"chat":{"id":-1001234567890,"title":"Grup Uji","type":"supergroup"},'
    + '"date":1462275476,"text":"halo semua",'
    + '"reply_to_message":{"message_id":200,"from":{"id":5,"first_name":"Budi","username":"budi"},"text":"asal"}}}';
  TG_NO_NAME =
    '{"update_id":1,"message":{"message_id":2,"from":{"id":3,"username":"onlyuser"},'
    + '"chat":{"id":3,"type":"private"},"text":"x"}}';
  TG_CALLBACK =
    '{"update_id":101,"callback_query":{"id":"1","from":{"id":77,"first_name":"A","username":"ua"},'
    + '"chat_instance":"ci-1","data":"_=1&text=halo",'
    + '"message":{"message_id":9,"chat":{"id":123,"type":"private"},"text":"pilih",'
    + '"reply_markup":{"inline_keyboard":[[{"text":"Ya","callback_data":"y=1"}]]}}}}';

  FB_MESSAGE =
    '{"object":"page","entry":[{"id":"1","time":1,"messaging":[{"sender":{"id":"u-1"},'
    + '"recipient":{"id":"p-1"},"timestamp":1,"message":{"mid":"m-1","text":"hai"}}]}]}';
  FB_POSTBACK =
    '{"object":"page","entry":[{"id":"1","time":1,"messaging":[{"sender":{"id":"u-2"},'
    + '"recipient":{"id":"p-1"},"timestamp":1,"postback":{"payload":"MENU:3","title":"Menu Tiga"}}]}]}';

  LINE_MESSAGE =
    '{"events":[{"type":"message","replyToken":"rt-1","source":{"type":"group","groupId":"g-1","userId":"u-1"},'
    + '"message":{"id":"m-1","type":"text","text":"halo"}}]}';
  LINE_POSTBACK =
    '{"events":[{"type":"postback","replyToken":"rt-2","source":{"type":"user","userId":"u-2"},'
    + '"postback":{"data":"x=1&y=2"}}]}';

{ TTelegramIntegrationTest }

procedure TTelegramIntegrationTest.SetUp;
begin
  FTelegram := TTelegramIntegration.Create;
  FTelegram.Debug := False;
end;

procedure TTelegramIntegrationTest.TearDown;
begin
  FTelegram.Free;
end;

procedure TTelegramIntegrationTest.TestTokenBuildsURL;
begin
  AssertEquals('', FTelegram.URL);
  FTelegram.Token := '123:abc';
  AssertEquals('https://api.telegram.org/bot123:abc/', FTelegram.URL);
end;

procedure TTelegramIntegrationTest.TestParseGroupMessage;
begin
  FTelegram.RequestContent := TG_GROUP_MESSAGE;
  AssertEquals(100, FTelegram.UpdateID);
  AssertEquals('halo semua', FTelegram.Text);
  AssertEquals('263', FTelegram.MessageID);
  AssertEquals('-1001234567890', FTelegram.ChatID);
  AssertEquals('supergroup', FTelegram.ChatType);
  AssertTrue(FTelegram.IsGroup);
  AssertEquals('Grup Uji', FTelegram.GroupName);
  AssertEquals('2222647', FTelegram.UserID);
  AssertEquals('luri', FTelegram.UserName);
  AssertEquals('Luri D', FTelegram.FullName);
  AssertFalse(FTelegram.IsBot);
  AssertFalse(FTelegram.IsCallbackQuery);
  AssertNull(FTelegram.CallbackData);
  AssertTrue(FTelegram.IsReply);
  AssertEquals('5', FTelegram.ReplyFromID);
  AssertEquals('200', FTelegram.ReplyFromMessageID);
  AssertEquals('asal', FTelegram.ReplyFromText);
  AssertEquals('budi', FTelegram.ReplyFromUserName);
  AssertFalse(FTelegram.IsLocation);
  AssertFalse(FTelegram.IsPicture);
  AssertFalse(FTelegram.IsDocument);
end;

procedure TTelegramIntegrationTest.TestFullNameFallsBackToUsername;
begin
  FTelegram.RequestContent := TG_NO_NAME;
  AssertEquals('onlyuser', FTelegram.FullName);
end;

procedure TTelegramIntegrationTest.TestParseCallbackQuery;
begin
  FTelegram.RequestContent := TG_CALLBACK;
  AssertTrue(FTelegram.IsCallbackQuery);
  AssertEquals('ci-1', FTelegram.CallbackInstance);
  AssertEquals('pilih', FTelegram.Text);
  AssertEquals('123', FTelegram.ChatID);
  AssertEquals('9', FTelegram.MessageID);
  AssertEquals('77', FTelegram.UserID);
  AssertEquals('ua', FTelegram.UserName);
  AssertNotNull(FTelegram.CallbackData);
  AssertEquals('halo', FTelegram.CallbackData.Values['text']);
  AssertNotNull(FTelegram.CallbackInlineKeyboard);
  AssertTrue(FTelegram.CallbackInlineKeyboard.IsArray);
  AssertEquals('Ya', FTelegram.CallbackInlineKeyboard.AsArray.Items[0].Items[0].GetPath('text').AsString);
end;

// the getters all call IsCallbackQuery; the objects it hands out must not
// be replaced in between (Carik passes CallbackInlineKeyboard to EditMessage)
procedure TTelegramIntegrationTest.TestCallbackObjectsStableWithinRequest;
var
  keyboard: TJSONUtil;
  data: TStrings;
begin
  FTelegram.RequestContent := TG_CALLBACK;
  keyboard := FTelegram.CallbackInlineKeyboard;
  data := FTelegram.CallbackData;
  FTelegram.ChatID;
  FTelegram.UserID;
  FTelegram.Text;
  AssertSame(keyboard, FTelegram.CallbackInlineKeyboard);
  AssertSame(data, FTelegram.CallbackData);
  // a new request parses again
  FTelegram.RequestContent := TG_GROUP_MESSAGE;
  AssertFalse(FTelegram.IsCallbackQuery);
  AssertNull(FTelegram.CallbackInlineKeyboard);
end;

procedure TTelegramIntegrationTest.TestKeyboardPayloadCopiesRows;
var
  rows: TJSONUtil;
  payload: string;
  j: TJSONData;
begin
  rows := TJSONUtil.Create;
  try
    AssertTrue(rows.LoadFromJsonString('[[{"text":"Ya","callback_data":"y=1"}],[{"text":"Tidak","callback_data":"y=0"}]]'));
    payload := FTelegram.KeyboardPayload('123', 'pilih "satu"', 'reply_markup/inline_keyboard', rows, '7', '9');
    j := GetJSON(payload);
    try
      AssertEquals('123', j.GetPath('chat_id').AsString);
      AssertEquals('pilih "satu"', j.GetPath('text').AsString);
      AssertEquals('7', j.GetPath('message_thread_id').AsString);
      AssertEquals('9', j.GetPath('message_id').AsString);
      AssertEquals(2, j.GetPath('reply_markup.inline_keyboard').Count);
      AssertEquals('Tidak', j.GetPath('reply_markup.inline_keyboard[1][0].text').AsString);
    finally
      j.Free;
    end;
    // the caller's rows are untouched and still its own
    AssertEquals(2, rows.AsArray.Count);
    payload := FTelegram.KeyboardPayload('123', 'x', 'reply_markup/keyboard', rows, '');
    AssertTrue(Pos('"keyboard"', payload) > 0);
    AssertTrue(Pos('message_thread_id', payload) = 0);
  finally
    rows.Free;
  end;
end;

procedure TTelegramIntegrationTest.TestKeyboardPayloadNeedsArray;
var
  obj: TJSONUtil;
begin
  AssertEquals('', FTelegram.KeyboardPayload('1', 'x', 'reply_markup/inline_keyboard', nil, ''));
  obj := TJSONUtil.Create;
  try
    obj['a'] := 1;
    AssertEquals('', FTelegram.KeyboardPayload('1', 'x', 'reply_markup/inline_keyboard', obj, ''));
    obj.LoadFromJsonString('[]');
    AssertEquals('', FTelegram.KeyboardPayload('1', 'x', 'reply_markup/inline_keyboard', obj, ''));
  finally
    obj.Free;
  end;
end;

procedure TTelegramIntegrationTest.TestSendWithoutTokenOrFileFails;
begin
  // no token: nothing is sent and every call reports failure
  AssertFalse(FTelegram.SendMessage('1', 'x'));
  AssertFalse(FTelegram.SendMessage(1, 'x'));
  AssertFalse(FTelegram.SendPhoto('1', 'nofile.jpg'));
  AssertFalse(FTelegram.SendContact('1', 'a', 'b', '+62'));
  AssertFalse(FTelegram.KickUser('', '1', ''));
  AssertFalse(FTelegram.RestrictUser('1', '', 0));
  AssertFalse(FTelegram.DeleteWebHook);
  AssertEquals('', FTelegram.GroupAdminList('-100', True));
  AssertFalse(FTelegram.IsAdmin('1', False));
  FTelegram.Token := '123:abc';
  // a missing file is rejected before any request
  AssertFalse(FTelegram.SendPhoto('1', 'does-not-exist.jpg'));
  AssertFalse(FTelegram.SendVideo('1', 'does-not-exist.mp4'));
  AssertFalse(FTelegram.SendAudio('1', 'does-not-exist.mp3'));
  AssertFalse(FTelegram.SendVoice('1', 'does-not-exist.ogg'));
  AssertFalse(FTelegram.SendDocument('1', 'does-not-exist.pdf'));
  AssertFalse(FTelegram.IsSuccessfull);
end;

// supergroup ids do not fit a 32 bit integer; the string overloads must not
// go through StrToInt
procedure TTelegramIntegrationTest.TestSupergroupIdThroughStringOverloads;
begin
  AssertFalse(FTelegram.SendPhoto('-1001234567890', 'does-not-exist.jpg'));
  AssertFalse(FTelegram.SendContact('-1001234567890', 'a', 'b', ''));
  AssertFalse(FTelegram.SendMessage('-1001234567890', ''));
end;

{ TFacebookIntegrationTest }

function TFacebookIntegrationTest.menuHandler(const APayload, ATitle: string): string;
begin
  FSeenPayload := APayload;
  FSeenTitle := ATitle;
  Result := 'handled';
end;

procedure TFacebookIntegrationTest.SetUp;
begin
  FFacebook := TFacebookMessengerIntegration.Create;
  FSeenPayload := '';
  FSeenTitle := '';
end;

procedure TFacebookIntegrationTest.TearDown;
begin
  FFacebook.Free;
end;

procedure TFacebookIntegrationTest.TestParseMessage;
begin
  FFacebook.RequestContent := FB_MESSAGE;
  AssertEquals('hai', FFacebook.Text);
  AssertEquals('u-1', FFacebook.UserID);
  AssertEquals('m-1', FFacebook.MessageID);
  AssertEquals('u-1', FFacebook.FullName);
  AssertFalse(FFacebook.isPostback);
  AssertFalse(FFacebook.isQuickReply);
  AssertFalse(FFacebook.isImage);
  AssertFalse(FFacebook.IsVoice);
  AssertFalse(FFacebook.IsLocation);
  AssertFalse(FFacebook.isCanSend);
end;

procedure TFacebookIntegrationTest.TestParsePostbackAndHandler;
begin
  FFacebook.RequestContent := FB_POSTBACK;
  AssertTrue(FFacebook.isPostback);
  AssertEquals('MENU:3', FFacebook.Payload);
  AssertEquals('Menu Tiga', FFacebook.PayloadTitle);
  // no handler registered for MENU
  AssertEquals('', FFacebook.PayloadHandling);
  FFacebook.PayloadHandler['MENU'] := @menuHandler;
  AssertEquals('handled', FFacebook.PayloadHandling);
  AssertEquals('MENU:3', FSeenPayload);
  AssertEquals('Menu Tiga', FSeenTitle);
end;

procedure TFacebookIntegrationTest.TestPayloadHandlingWithoutPayload;
begin
  FFacebook.PayloadHandler['MENU'] := @menuHandler;
  FFacebook.RequestContent := FB_MESSAGE;
  AssertEquals('', FFacebook.PayloadHandling);
  AssertEquals('', FSeenPayload);
end;

procedure TFacebookIntegrationTest.TestHandlersArePerInstance;
var
  other: TFacebookMessengerIntegration;
begin
  FFacebook.PayloadHandler['MENU'] := @menuHandler;
  other := TFacebookMessengerIntegration.Create;
  try
    other.RequestContent := FB_POSTBACK;
    AssertEquals('', other.PayloadHandling);
  finally
    other.Free;
  end;
  // the first instance keeps its handler
  FFacebook.RequestContent := FB_POSTBACK;
  AssertEquals('handled', FFacebook.PayloadHandling);
end;

procedure TFacebookIntegrationTest.TestInvalidContentIsIgnored;
begin
  FFacebook.RequestContent := '{"entry":';
  AssertEquals('', FFacebook.Text);
  AssertEquals('', FFacebook.UserID);
  AssertFalse(FFacebook.isPostback);
  FFacebook.RequestContent := FB_MESSAGE;
  AssertEquals('hai', FFacebook.Text);
end;

procedure TFacebookIntegrationTest.TestQuickReplyRows;
begin
  AssertEquals(0, FFacebook.QuickReply.Count);
  FFacebook.QuickReply.AddText('Satu', 'P1', 'https://img/1.png');
  FFacebook.QuickReply.AddText('', 'P2', 'https://img/2.png');
  FFacebook.QuickReply.AddEmail;
  FFacebook.QuickReply.AddPhone;
  AssertEquals(3, FFacebook.QuickReply.Count);
  AssertEquals('Satu', FFacebook.QuickReply.Data.Items[0].GetPath('title').AsString);
  AssertEquals('user_email', FFacebook.QuickReply.Data.Items[1].GetPath('content_type').AsString);
end;

{ TLineIntegrationTest }

// AsJSON used to move the parts into the root object; a second call freed
// the template while it was still in use
procedure TLineIntegrationTest.TestTemplateButtonsJsonIsRepeatable;
var
  t: TLineTemplateMessage;
  first, second: string;
  j: TJSONData;
begin
  t := TLineTemplateMessage.Create('buttons');
  try
    t.AltText := 'alt';
    t.Title := 'Judul';
    t.Text := 'Pilih:';
    t.ThumbnailImageURL := 'https://x/y.jpg';
    t.AddActionPostBack('Label yang sangat panjang sekali', 'a=1');
    t.AddActionURI('Detail', 'https://x');
    t.AddActionMessage('Bantuan', 'help');
    first := t.AsJSON;
    second := t.AsJSON;
    AssertEquals(first, second);
    j := GetJSON(first);
    try
      AssertEquals('template', j.GetPath('type').AsString);
      AssertEquals('alt', j.GetPath('altText').AsString);
      AssertEquals('buttons', j.GetPath('template.type').AsString);
      AssertEquals('Judul', j.GetPath('template.title').AsString);
      AssertEquals('https://x/y.jpg', j.GetPath('template.thumbnailImageUrl').AsString);
      AssertEquals(3, j.GetPath('template.actions').Count);
      AssertEquals('postback', j.GetPath('template.actions[0].type').AsString);
      // labels are cut at 20 characters
      AssertEquals(20, Length(j.GetPath('template.actions[0].label').AsString));
      AssertEquals('a=1', j.GetPath('template.actions[0].data').AsString);
      AssertEquals('https://x', j.GetPath('template.actions[1].uri').AsString);
      AssertEquals('help', j.GetPath('template.actions[2].text').AsString);
    finally
      j.Free;
    end;
    AssertEquals('alt', t.AltText);
    AssertEquals('Judul', t.Title);
  finally
    t.Free;
  end;
end;

procedure TLineIntegrationTest.TestTemplateCarouselColumns;
var
  t: TLineTemplateMessage;
  j: TJSONData;
begin
  t := TLineTemplateMessage.Create('carousel');
  try
    t.AddColumnAsJson('{"title":"a"}');
    t.AddColumnAsJson('not json');
    t.AddColumnAsJson('[1]');
    t.AddColumnAsJson('{"title":"b"}');
    j := GetJSON(t.AsJSON);
    try
      AssertEquals(2, j.GetPath('template.columns').Count);
      AssertEquals('b', j.GetPath('template.columns[1].title').AsString);
      AssertNull(j.FindPath('template.actions'));
    finally
      j.Free;
    end;
  finally
    t.Free;
  end;
end;

procedure TLineIntegrationTest.TestTemplateAsJsonString;
var
  t: TLineTemplateMessage;
  j: TJSONData;
begin
  t := TLineTemplateMessage.Create('');
  try
    t.AddActionMessage('x', 'y');
    j := GetJSON(t.GetTemplateAsJsonString);
    try
      AssertNull(j.FindPath('type'));
      AssertEquals(1, j.GetPath('actions').Count);
    finally
      j.Free;
    end;
    // the full message still carries the type
    AssertTrue(Pos('"type"', t.AsJSON) > 0);
  finally
    t.Free;
  end;
end;

procedure TLineIntegrationTest.TestParseMessageEvent;
var
  line: TLineIntegration;
begin
  line := TLineIntegration.Create;
  try
    line.RequestContent := LINE_MESSAGE;
    AssertEquals('rt-1', line.ReplyToken);
    AssertEquals('halo', line.Text);
    AssertEquals('u-1', line.UserID);
    AssertEquals('g-1', line.GroupID);
    AssertEquals('m-1', line.MessageID);
    AssertTrue(line.isMessage);
    AssertTrue(line.isGroup);
    AssertFalse(line.isPostback);
    AssertFalse(line.isImage);
    AssertFalse(line.isSticker);
    AssertFalse(line.isLocation);
    AssertFalse(line.isCanSend);
    line.RequestContent := '{"events":';
    AssertEquals('', line.Text);
  finally
    line.Free;
  end;
end;

procedure TLineIntegrationTest.TestParsePostbackTwice;
var
  line: TLineIntegration;
begin
  line := TLineIntegration.Create;
  try
    line.RequestContent := LINE_POSTBACK;
    AssertTrue(line.isPostback);
    AssertTrue(line.isPostback);
    AssertEquals('1', line.PostbackData.Values['x']);
    AssertEquals('2', line.PostbackData.Values['y']);
    AssertFalse(line.isGroup);
  finally
    line.Free;
  end;
end;

{ TSmallIntegrationsTest }

procedure TSmallIntegrationsTest.TestWhoisExtensionOf;
begin
  AssertEquals('com', TWhoisIntegration.ExtensionOf('example.com'));
  AssertEquals('id', TWhoisIntegration.ExtensionOf('Example.CO.ID'));
  AssertEquals('', TWhoisIntegration.ExtensionOf('localhost'));
  AssertEquals('', TWhoisIntegration.ExtensionOf(''));
end;

procedure TSmallIntegrationsTest.TestGraphQLRequestBodyIsEscaped;
var
  q, body: string;
  j: TJSONData;
begin
  q := 'query { repo(name: "fast"'#10'plaz") { id } }';
  body := TGraphQLIntegration.BuildRequestBody(q, '');
  j := GetJSON(body);
  try
    AssertEquals(q, j.GetPath('query').AsString);
    AssertNull(j.FindPath('variables'));
  finally
    j.Free;
  end;
  body := TGraphQLIntegration.BuildRequestBody(q, '{"n":3}');
  j := GetJSON(body);
  try
    AssertEquals(q, j.GetPath('query').AsString);
    AssertEquals(3, j.GetPath('variables.n').AsInteger);
  finally
    j.Free;
  end;
end;

procedure TSmallIntegrationsTest.TestSlackPayloadIsEscaped;
var
  slack: TSlackWebhookIntegration;
  j: TJSONData;
begin
  slack := TSlackWebhookIntegration.Create;
  try
    slack.Channel := '#random';
    slack.Text := 'say "hi"'#10'twice';
    slack.Icon := ':ghost:';
    slack.GeneratePayload;
    j := GetJSON(slack.Payload);
    try
      AssertEquals('#random', j.GetPath('channel').AsString);
      AssertEquals('say "hi"'#10'twice', j.GetPath('text').AsString);
      AssertEquals(':ghost:', j.GetPath('icon_emoji').AsString);
      AssertNull(j.FindPath('username'));
    finally
      j.Free;
    end;
  finally
    slack.Free;
  end;
end;

procedure TSmallIntegrationsTest.TestBotFrameworkServiceURLSlash;
var
  bot: TMSBotFrameworkIntegration;
begin
  bot := TMSBotFrameworkIntegration.Create;
  try
    bot.RequestContent := '{"type":"message","serviceUrl":"https://smba.trafficmanager.net/apis","text":" hi ","conversation":{"id":"c1"},"id":"m1"}';
    AssertEquals('https://smba.trafficmanager.net/apis/', bot.ServiceURL);
    AssertEquals('hi', bot.Text);
    AssertEquals('c1', bot.ConversationID);
    bot.RequestContent := '{"serviceUrl":"https://x/apis/"}';
    AssertEquals('https://x/apis/', bot.ServiceURL);
    bot.RequestContent := '{}';
    AssertEquals('', bot.ServiceURL);
  finally
    bot.Free;
  end;
end;

procedure TSmallIntegrationsTest.TestElasticsearchURL;
var
  es: TElasticSearchIntegration;
begin
  es := TElasticSearchIntegration.Create;
  try
    es.Endpoint := 'http://localhost:9200/';
    es.IndexName := 'logs/';
    es.TypeName := 'doc';
    AssertEquals('http://localhost:9200/logs/doc', es.URL);
    // nothing is sent without an endpoint, and URL is never altered
    es.Endpoint := '';
    AssertFalse(es.Query);
    AssertEquals('/logs/doc', es.URL);
  finally
    es.Free;
  end;
end;

// Add on an existing key raises in fpjson: the payload must be rebuilt for
// every call. Port 9 refuses the connection, so no real request is made.
procedure TSmallIntegrationsTest.TestOpenAIPayloadRebuiltPerCall;
var
  ai: TOpenAIIntegration;
begin
  ai := TOpenAIIntegration.Create;
  try
    ai.BaseURL := 'http://127.0.0.1:9/';
    AssertFalse(ai.ChatCompletion('satu'));
    AssertFalse(ai.ChatCompletion('dua'));
    AssertNotNull(ai.Response);
    AssertEquals(2, ai.Messages.Count);
    AssertEquals(1, ai.Payload.Count - 3);
    AssertEquals(2, ai.Payload.Arrays['messages'].Count);
    AssertEquals('dua', ai.Payload.GetPath('messages[1].content').AsString);
    // without a key against the public API nothing is attempted
    ai.BaseURL := OPENAI_BASEURL;
    AssertFalse(ai.ChatCompletion('tiga'));
  finally
    ai.Free;
  end;
end;

// every class frees what it created (the destructors used to be hidden
// instead of overridden, so they never ran)
procedure TSmallIntegrationsTest.TestCreateAndFree;
var
  i: integer;
begin
  for i := 1 to 2 do
  begin
    TTelegramIntegration.Create.Free;
    TFacebookMessengerIntegration.Create.Free;
    TLineIntegration.Create.Free;
    TLineTemplateMessage.Create('buttons').Free;
    TWhoisIntegration.Create.Free;
    TGraphQLIntegration.Create.Free;
    TMSBotFrameworkIntegration.Create.Free;
    TElasticSearchIntegration.Create.Free;
    TOpenAIIntegration.Create.Free;
    TFirebaseIntegration.Create.Free;
    TGooglePlaceIntegration.Create.Free;
  end;
  AssertTrue(True);
end;

initialization
  RegisterTest(TTelegramIntegrationTest);
  RegisterTest(TFacebookIntegrationTest);
  RegisterTest(TLineIntegrationTest);
  RegisterTest(TSmallIntegrationsTest);

end.

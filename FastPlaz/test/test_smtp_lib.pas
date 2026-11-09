unit test_smtp_lib;

{$mode objfpc}{$H+}

interface

uses
  smtp_lib, sockets, ssockets, base64, Classes, SysUtils, fpcunit, testregistry;

type

  { TFakeSMTPServer
    Minimal scripted SMTP server on 127.0.0.1 that accepts one session,
    records everything the client sent and answers like a real server. }

  TFakeSMTPServer = class(TThread)
  private
    FServer: TInetServer;
    FConn: TSocketStream;
    FPending: string;
    FPort: word;
    FReady: PRTLEvent;
    FOfferAuth: boolean;
    FRejectRcpt: string;
    FFailAuth: boolean;
    FDropAfterGreeting: boolean;
    procedure HandleConnect(Sender: TObject; Data: TSocketStream);
    procedure Reply(const S: string);
    function NextLine(out ALine: string): boolean;
    procedure Session;
  public
    Commands: TStringList;   // every command line received
    Data: string;            // raw DATA payload (without the final CRLF.CRLF)
    AuthPlain: string;       // decoded AUTH PLAIN payload (#0user#0pass)
    constructor Create(APort: word);
    destructor Destroy; override;
    procedure Execute; override;
    procedure WaitReady;
    property OfferAuth: boolean read FOfferAuth write FOfferAuth;
    property RejectRcpt: string read FRejectRcpt write FRejectRcpt;
    property FailAuth: boolean read FFailAuth write FFailAuth;
    property DropAfterGreeting: boolean read FDropAfterGreeting write FDropAfterGreeting;
  end;

  { TSMTPLibTest }

  TSMTPLibTest = class(TTestCase)
  private
    FServer: TFakeSMTPServer;
    FPort: word;
    function StartServer: TFakeSMTPServer;
    function NewClient: TSMTPClient;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // helpers
    procedure TestExtractAddress;
    procedure TestEncodeHeaderAscii;
    procedure TestEncodeHeaderUtf8;
    procedure TestEncodeAddress;
    procedure TestDotStuff;
    procedure TestDateTimeFormat;
    procedure TestBase64Lines;
    procedure TestBuildMessageHeaders;
    procedure TestBuildMessageBodyDecodes;
    procedure TestBuildMessageHtml;
    procedure TestBuildMessageNoBccHeader;
    // protocol against the fake server
    procedure TestSendPlain;
    procedure TestSendWithAuthPlain;
    procedure TestAuthFailure;
    procedure TestRecipientRejected;
    procedure TestStartTLSNotOffered;
    procedure TestConnectRefused;
    procedure TestServerDrops;
    procedure TestNoRecipients;
    procedure TestLogMasksCredentials;
  end;

implementation

const
  CRLF = #13#10;
  BASE_PORT = 25250;

{ TFakeSMTPServer }

constructor TFakeSMTPServer.Create(APort: word);
begin
  inherited Create(True);
  FPort := APort;
  FOfferAuth := True;
  FReady := RTLEventCreate;
  Commands := TStringList.Create;
  FreeOnTerminate := False;
end;

destructor TFakeSMTPServer.Destroy;
begin
  RTLEventDestroy(FReady);
  Commands.Free;
  inherited Destroy;
end;

procedure TFakeSMTPServer.WaitReady;
begin
  RTLEventWaitFor(FReady, 5000);
end;

procedure TFakeSMTPServer.Reply(const S: string);
var
  r: string;
  sent: integer;
begin
  r := S + CRLF;
  sent := FConn.Write(r[1], Length(r));
  if sent <> Length(r) then
    raise Exception.CreateFmt('reply failed: wrote %d of %d bytes, socket error %d',
      [sent, Length(r), SocketError]);
end;

function TFakeSMTPServer.NextLine(out ALine: string): boolean;
var
  buf: array[0..4095] of char;
  n, p: integer;
  chunk: string;
begin
  repeat
    p := Pos(CRLF, FPending);
    if p > 0 then
    begin
      ALine := Copy(FPending, 1, p - 1);
      Delete(FPending, 1, p + 1);
      Exit(True);
    end;
    n := FConn.Read(buf, SizeOf(buf));
    if n <= 0 then
      Exit(False);
    SetLength(chunk, n);
    Move(buf, chunk[1], n);
    FPending := FPending + chunk;
  until False;
end;

procedure TFakeSMTPServer.Session;
var
  line, cmd, upper: string;
  inData: boolean;
begin
  FPending := '';
  inData := False;
  Reply('220 fake.test ESMTP ready');
  if FDropAfterGreeting then
    Exit;
  while NextLine(line) do
  begin
    if inData then
    begin
      if line = '.' then
      begin
        inData := False;
        Reply('250 2.0.0 queued');
      end
      else
      begin
        if Data <> '' then
          Data := Data + CRLF;
        Data := Data + line;
      end;
      Continue;
    end;
    Commands.Add(line);
    upper := UpperCase(line);
    cmd := Copy(upper, 1, 4);
    if cmd = 'EHLO' then
    begin
      Reply('250-fake.test');
      Reply('250-SIZE 10240000');
      if FOfferAuth then
        Reply('250-AUTH PLAIN LOGIN');
      Reply('250 8BITMIME');
    end
    else if cmd = 'HELO' then
      Reply('250 fake.test')
    else if cmd = 'AUTH' then
    begin
      AuthPlain := DecodeStringBase64(Copy(line, 12, MaxInt));
      if FFailAuth then
        Reply('535 5.7.8 authentication failed')
      else
        Reply('235 2.7.0 authentication successful');
    end
    else if cmd = 'MAIL' then
      Reply('250 2.1.0 ok')
    else if cmd = 'RCPT' then
    begin
      if (FRejectRcpt <> '') and (Pos(FRejectRcpt, line) > 0) then
        Reply('550 5.1.1 no such user')
      else
        Reply('250 2.1.5 ok');
    end
    else if cmd = 'DATA' then
    begin
      inData := True;
      Reply('354 end data with <CR><LF>.<CR><LF>');
    end
    else if cmd = 'QUIT' then
    begin
      Reply('221 2.0.0 bye');
      Break;
    end
    else if cmd = 'STAR' then
      Reply('454 TLS not available')
    else
      Reply('500 unknown command');
  end;
end;

// Called from TInetServer.StartAccepting for the one accepted connection.
// Accept must go through StartAccepting: called directly it closes the
// socket again because the server is not in accepting state.
procedure TFakeSMTPServer.HandleConnect(Sender: TObject; Data: TSocketStream);
begin
  FConn := Data;
  try
    Session;
  finally
    FreeAndNil(FConn);
    FServer.StopAccepting(False);
  end;
end;

procedure TFakeSMTPServer.Execute;
begin
  FServer := TInetServer.Create('127.0.0.1', FPort);
  try
    FServer.ReuseAddress := True;
    FServer.MaxConnections := 1;
    FServer.OnConnect := @HandleConnect;
    FServer.Bind;
    FServer.Listen;
    RTLEventSetEvent(FReady);
    FServer.StartAccepting;
  finally
    FServer.Free;
  end;
end;

{ TSMTPLibTest }

procedure TSMTPLibTest.SetUp;
begin
  FServer := nil;
  // spread ports a little so parallel runs do not collide
  FPort := BASE_PORT + (GetProcessID mod 200);
end;

procedure TSMTPLibTest.TearDown;
var
  s: TInetSocket;
begin
  if Assigned(FServer) then
  begin
    // a failed test may leave the server blocked in Accept: poke it
    if not FServer.Finished then
    begin
      try
        s := TInetSocket.Create('127.0.0.1', FPort, 1000);
        s.Free;
      except
      end;
    end;
    FServer.WaitFor;
    FServer.Free;
  end;
end;

function TSMTPLibTest.StartServer: TFakeSMTPServer;
begin
  Result := TFakeSMTPServer.Create(FPort);
  FServer := Result;
end;

function TSMTPLibTest.NewClient: TSMTPClient;
begin
  Result := TSMTPClient.Create;
  Result.Host := '127.0.0.1';
  Result.Port := FPort;
  Result.Timeout := 5000;
  Result.HeloName := 'unit.test';
end;

procedure TSMTPLibTest.TestExtractAddress;
begin
  AssertEquals('a@b.c', SMTPExtractAddress('a@b.c'));
  AssertEquals('a@b.c', SMTPExtractAddress('  a@b.c '));
  AssertEquals('a@b.c', SMTPExtractAddress('"Some One" <a@b.c>'));
  AssertEquals('a@b.c', SMTPExtractAddress('Some One<a@b.c>'));
  AssertEquals('', SMTPExtractAddress('<>'));
end;

procedure TSMTPLibTest.TestEncodeHeaderAscii;
begin
  AssertEquals('Hello World', SMTPEncodeHeader('Hello World'));
  AssertEquals('', SMTPEncodeHeader(''));
end;

procedure TSMTPLibTest.TestEncodeHeaderUtf8;
var
  s: string;
begin
  s := SMTPEncodeHeader('Halo ' + #$C3#$A9);
  AssertEquals('=?UTF-8?B?' + EncodeStringBase64('Halo ' + #$C3#$A9) + '?=', s);
end;

procedure TSMTPLibTest.TestEncodeAddress;
begin
  AssertEquals('a@b.c', SMTPEncodeAddress('a@b.c'));
  AssertEquals('"Budi" <a@b.c>', SMTPEncodeAddress('"Budi" <a@b.c>'));
  AssertEquals('"Budi" <a@b.c>', SMTPEncodeAddress('Budi <a@b.c>'));
  AssertEquals('=?UTF-8?B?' + EncodeStringBase64('Bud' + #$C3#$AD) + '?= <a@b.c>',
    SMTPEncodeAddress('"Bud' + #$C3#$AD + '" <a@b.c>'));
end;

procedure TSMTPLibTest.TestDotStuff;
begin
  AssertEquals('..hidden', SMTPDotStuff('.hidden'));
  AssertEquals('a' + CRLF + '..b' + CRLF + 'c', SMTPDotStuff('a' + CRLF + '.b' + CRLF + 'c'));
  AssertEquals('a.b', SMTPDotStuff('a.b'));
  AssertEquals('', SMTPDotStuff(''));
end;

procedure TSMTPLibTest.TestDateTimeFormat;
var
  s: string;
begin
  // 2024-02-29 13:05:09 is a Thursday
  s := SMTPDateTime(EncodeDate(2024, 2, 29) + EncodeTime(13, 5, 9, 0));
  AssertEquals('Thu, 29 Feb 2024 13:05:09 ', Copy(s, 1, 26));
  AssertEquals(31, Length(s));
  AssertTrue('zone sign', s[27] in ['+', '-']);
end;

procedure TSMTPLibTest.TestBase64Lines;
var
  s, raw: string;
  i: integer;
begin
  raw := '';
  for i := 1 to 200 do
    raw := raw + 'x';
  s := SMTPBase64Lines(raw);
  AssertEquals(76, Pos(CRLF, s) - 1);
  AssertEquals(CRLF, Copy(s, Length(s) - 1, 2));
  AssertEquals(raw, DecodeStringBase64(StringReplace(s, CRLF, '', [rfReplaceAll])));
  AssertEquals('', SMTPBase64Lines(''));
end;

procedure TSMTPLibTest.TestBuildMessageHeaders;
var
  msg: string;
begin
  msg := SMTPBuildMessage('"Sender" <s@x.y>', 'a@x.y, "B" <b@x.y>', 'c@x.y',
    'Subject here', 'body', False, 'X-Mailer: test');
  AssertTrue('Date', Pos('Date: ', msg) = 1);
  AssertTrue('From', Pos(CRLF + 'From: "Sender" <s@x.y>' + CRLF, msg) > 0);
  AssertTrue('To', Pos(CRLF + 'To: a@x.y, "B" <b@x.y>' + CRLF, msg) > 0);
  AssertTrue('Cc', Pos(CRLF + 'Cc: c@x.y' + CRLF, msg) > 0);
  AssertTrue('Subject', Pos(CRLF + 'Subject: Subject here' + CRLF, msg) > 0);
  AssertTrue('Message-ID domain', Pos('@x.y>' + CRLF, msg) > 0);
  AssertTrue('MIME', Pos(CRLF + 'MIME-Version: 1.0' + CRLF, msg) > 0);
  AssertTrue('text/plain', Pos(CRLF + 'Content-Type: text/plain; charset=UTF-8' + CRLF, msg) > 0);
  AssertTrue('base64', Pos(CRLF + 'Content-Transfer-Encoding: base64' + CRLF, msg) > 0);
  AssertTrue('extra header', Pos(CRLF + 'X-Mailer: test' + CRLF, msg) > 0);
  AssertTrue('blank line before body', Pos(CRLF + CRLF, msg) > 0);
  AssertTrue('no bare LF', Pos(#10, StringReplace(msg, CRLF, '', [rfReplaceAll])) = 0);
end;

procedure TSMTPLibTest.TestBuildMessageBodyDecodes;
var
  msg, body: string;
  p: integer;
begin
  msg := SMTPBuildMessage('s@x.y', 'a@x.y', '', 'S', 'line1' + #10 + 'line2', False);
  p := Pos(CRLF + CRLF, msg);
  body := Copy(msg, p + 4, MaxInt);
  AssertEquals('line1' + CRLF + 'line2',
    DecodeStringBase64(StringReplace(body, CRLF, '', [rfReplaceAll])));
end;

procedure TSMTPLibTest.TestBuildMessageHtml;
var
  msg: string;
begin
  msg := SMTPBuildMessage('s@x.y', 'a@x.y', '', 'S', '<b>hi</b>', True);
  AssertTrue(Pos(CRLF + 'Content-Type: text/html; charset=UTF-8' + CRLF, msg) > 0);
end;

procedure TSMTPLibTest.TestBuildMessageNoBccHeader;
var
  msg: string;
begin
  msg := SMTPBuildMessage('s@x.y', 'a@x.y', '', 'S', 'x', False);
  AssertTrue(Pos('Bcc', msg) = 0);
  AssertTrue(Pos('Cc:', msg) = 0);
end;

procedure TSMTPLibTest.TestSendPlain;
var
  srv: TFakeSMTPServer;
  c: TSMTPClient;
  rcpt: TStringList;
  ok: boolean;
begin
  srv := StartServer;
  srv.OfferAuth := False;
  srv.Start;
  srv.WaitReady;
  c := NewClient;
  rcpt := TStringList.Create;
  try
    rcpt.Add('"A" <a@x.y>');
    rcpt.Add('b@x.y');
    ok := c.Send('"S" <s@x.y>', rcpt, 'Subject: t' + CRLF + CRLF + 'hello' + CRLF + '.dot');
    AssertTrue(c.ErrorMessage, ok);
    AssertEquals(250, c.LastCode);
  finally
    rcpt.Free;
    c.Free;
  end;
  srv.WaitFor;
  AssertEquals('EHLO unit.test', srv.Commands[0]);
  AssertEquals('MAIL FROM:<s@x.y>', srv.Commands[1]);
  AssertEquals('RCPT TO:<a@x.y>', srv.Commands[2]);
  AssertEquals('RCPT TO:<b@x.y>', srv.Commands[3]);
  AssertEquals('DATA', srv.Commands[4]);
  AssertEquals('QUIT', srv.Commands[5]);
  AssertEquals('dot stuffed data', 'Subject: t' + CRLF + CRLF + 'hello' + CRLF + '..dot', srv.Data);
  AssertTrue('no AUTH sent without username', Pos('AUTH', srv.Commands.Text) = 0);
end;

procedure TSMTPLibTest.TestSendWithAuthPlain;
var
  srv: TFakeSMTPServer;
  c: TSMTPClient;
  rcpt: TStringList;
begin
  srv := StartServer;
  srv.Start;
  srv.WaitReady;
  c := NewClient;
  rcpt := TStringList.Create;
  try
    c.UserName := 'user@x.y';
    c.Password := 'p4ss';
    rcpt.Add('a@x.y');
    AssertTrue(c.ErrorMessage, c.Send('s@x.y', rcpt, 'hi'));
    AssertTrue('extension parsed', c.HasExtension('AUTH'));
    AssertTrue('extension parsed', c.HasExtension('SIZE'));
    AssertFalse(c.HasExtension('STARTTLS'));
  finally
    rcpt.Free;
    c.Free;
  end;
  srv.WaitFor;
  AssertEquals(#0'user@x.y'#0'p4ss', srv.AuthPlain);
  AssertEquals('AUTH PLAIN ' + EncodeStringBase64(#0'user@x.y'#0'p4ss'), srv.Commands[1]);
end;

procedure TSMTPLibTest.TestAuthFailure;
var
  srv: TFakeSMTPServer;
  c: TSMTPClient;
  rcpt: TStringList;
begin
  srv := StartServer;
  srv.FailAuth := True;
  srv.Start;
  srv.WaitReady;
  c := NewClient;
  rcpt := TStringList.Create;
  try
    c.UserName := 'u';
    c.Password := 'p';
    rcpt.Add('a@x.y');
    AssertFalse(c.Send('s@x.y', rcpt, 'hi'));
    AssertEquals(535, c.LastCode);
    AssertTrue(c.ErrorMessage, Pos('AUTH failed: 535', c.ErrorMessage) > 0);
  finally
    rcpt.Free;
    c.Free;
  end;
  srv.WaitFor;
  AssertTrue('no MAIL FROM after failed auth', Pos('MAIL FROM', srv.Commands.Text) = 0);
  AssertEquals('QUIT', srv.Commands[srv.Commands.Count - 1]);
end;

procedure TSMTPLibTest.TestRecipientRejected;
var
  srv: TFakeSMTPServer;
  c: TSMTPClient;
  rcpt: TStringList;
begin
  srv := StartServer;
  srv.OfferAuth := False;
  srv.RejectRcpt := 'bad@';
  srv.Start;
  srv.WaitReady;
  c := NewClient;
  rcpt := TStringList.Create;
  try
    rcpt.Add('good@x.y');
    rcpt.Add('bad@x.y');
    AssertFalse(c.Send('s@x.y', rcpt, 'hi'));
    AssertEquals(550, c.LastCode);
    AssertTrue(c.ErrorMessage, Pos('RCPT failed: 550', c.ErrorMessage) > 0);
  finally
    rcpt.Free;
    c.Free;
  end;
  srv.WaitFor;
  AssertTrue('no DATA after rejected recipient', Pos('DATA', srv.Commands.Text) = 0);
end;

procedure TSMTPLibTest.TestStartTLSNotOffered;
var
  srv: TFakeSMTPServer;
  c: TSMTPClient;
  rcpt: TStringList;
begin
  srv := StartServer;
  srv.Start;
  srv.WaitReady;
  c := NewClient;
  rcpt := TStringList.Create;
  try
    c.Security := ssStartTLS;
    rcpt.Add('a@x.y');
    AssertFalse(c.Send('s@x.y', rcpt, 'hi'));
    AssertEquals('server does not offer STARTTLS', c.ErrorMessage);
  finally
    rcpt.Free;
    c.Free;
  end;
  srv.WaitFor;
  AssertTrue('nothing sent in clear', Pos('MAIL FROM', srv.Commands.Text) = 0);
end;

procedure TSMTPLibTest.TestConnectRefused;
var
  c: TSMTPClient;
  rcpt: TStringList;
begin
  c := NewClient;
  rcpt := TStringList.Create;
  try
    c.Port := 1;
    rcpt.Add('a@x.y');
    AssertFalse(c.Send('s@x.y', rcpt, 'hi'));
    AssertTrue(c.ErrorMessage, Pos('connect failed', c.ErrorMessage) = 1);
    AssertFalse(c.Connected);
  finally
    rcpt.Free;
    c.Free;
  end;
end;

procedure TSMTPLibTest.TestServerDrops;
var
  srv: TFakeSMTPServer;
  c: TSMTPClient;
  rcpt: TStringList;
begin
  srv := StartServer;
  srv.DropAfterGreeting := True;
  srv.Start;
  srv.WaitReady;
  c := NewClient;
  rcpt := TStringList.Create;
  try
    rcpt.Add('a@x.y');
    AssertFalse(c.Send('s@x.y', rcpt, 'hi'));
    AssertTrue(c.ErrorMessage, Pos('connection closed', c.ErrorMessage) > 0);
  finally
    rcpt.Free;
    c.Free;
  end;
end;

procedure TSMTPLibTest.TestNoRecipients;
var
  srv: TFakeSMTPServer;
  c: TSMTPClient;
  rcpt: TStringList;
begin
  srv := StartServer;
  srv.OfferAuth := False;
  srv.Start;
  srv.WaitReady;
  c := NewClient;
  rcpt := TStringList.Create;
  try
    AssertFalse(c.Send('s@x.y', rcpt, 'hi'));
    AssertEquals('no recipients', c.ErrorMessage);
  finally
    rcpt.Free;
    c.Free;
  end;
end;

procedure TSMTPLibTest.TestLogMasksCredentials;
var
  srv: TFakeSMTPServer;
  c: TSMTPClient;
  rcpt: TStringList;
begin
  srv := StartServer;
  srv.Start;
  srv.WaitReady;
  c := NewClient;
  rcpt := TStringList.Create;
  try
    c.UserName := 'secretuser';
    c.Password := 'secretpass';
    rcpt.Add('a@x.y');
    AssertTrue(c.ErrorMessage, c.Send('s@x.y', rcpt, 'hi'));
    AssertTrue('password not in log', Pos('secretpass', c.Log.Text) = 0);
    AssertTrue('encoded credentials not in log',
      Pos(EncodeStringBase64(#0'secretuser'#0'secretpass'), c.Log.Text) = 0);
    AssertTrue('masked marker present', Pos('AUTH PLAIN ****', c.Log.Text) > 0);
    AssertTrue('replies logged', Pos('< 235', c.Log.Text) > 0);
  finally
    rcpt.Free;
    c.Free;
  end;
  srv.WaitFor;
end;

initialization
  RegisterTest(TSMTPLibTest);

end.

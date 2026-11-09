unit smtp_lib;

{
  SMTP client without third-party dependencies.

  Built on the FPC units ssockets / sslsockets / opensslsockets, so it needs
  the OpenSSL shared library at runtime for STARTTLS and implicit SSL:
    Windows : libssl-1_1-x64.dll + libcrypto-1_1-x64.dll (or ssleay32/libeay32)
    Linux   : libssl.so / libcrypto.so (1.0 or 1.1)
  Plain connections (ssNone) work without OpenSSL.

  USAGE

  var
    Client: TSMTPClient;
    Recipients: TStringList;
    Data: string;
  begin
    Recipients := TStringList.Create;
    Recipients.Add('someone@example.com');
    Data := SMTPBuildMessage('"Sender" <noreply@example.com>',
      '"Someone" <someone@example.com>', '', 'Subject', 'Hello', False);

    Client := TSMTPClient.Create;
    Client.Host := 'smtp.example.com';
    Client.Port := 587;
    Client.Security := ssStartTLS;     // ssNone, ssStartTLS, ssSSL (implicit, 465)
    Client.UserName := 'user';
    Client.Password := 'secret';
    if not Client.Send('noreply@example.com', Recipients, Data) then
      WriteLn(Client.ErrorMessage);
    WriteLn(Client.Log.Text);
    Client.Free;
    Recipients.Free;
  end;
}

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  Classes, SysUtils, sockets, ssockets, sslsockets, opensslsockets;

type
  TSMTPSecurity = (ssNone, ssStartTLS, ssSSL);

  { TSMTPSocketHandler
    OpenSSL handler that stays in plain mode until UpgradeToTLS is called,
    so one connection can do EHLO, STARTTLS and then the TLS handshake. }

  TSMTPSocketHandler = class(TOpenSSLSocketHandler)
  private
    FWantTLS: boolean;
  public
    function Connect: boolean; override;
    function Close: boolean; override;
    function Shutdown(BiDirectional: boolean): boolean; override;
    function Send(const Buffer; Count: integer): integer; override;
    function Recv(const Buffer; Count: integer): integer; override;
    function UpgradeToTLS: boolean;
    property WantTLS: boolean read FWantTLS write FWantTLS;
  end;

  { TSMTPClient }

  TSMTPClient = class
  private
    FHost: string;
    FPort: word;
    FSecurity: TSMTPSecurity;
    FUserName: string;
    FPassword: string;
    FHeloName: string;
    FTimeout: integer;
    FVerifyPeerCert: boolean;
    FSocket: TInetSocket;
    FHandler: TSMTPSocketHandler;
    FBuffer: string;
    FLog: TStringList;
    FExtensions: TStringList;
    FLastCode: integer;
    FLastReply: string;
    FErrorMessage: string;
    FConnected: boolean;
    function ReadLine(out ALine: string): boolean;
    function ReadReply: boolean;
    function SendRaw(const AData: string): boolean;
    function SendLine(const ALine: string; const ALogAs: string = ''): boolean;
    function Command(const ACmd: string; const AExpected: array of integer;
      const ALogAs: string = ''): boolean;
    function Ehlo: boolean;
    function DoStartTLS: boolean;
    function Fail(const AMessage: string): boolean;
    procedure AddLog(const ALine: string);
  public
    constructor Create;
    destructor Destroy; override;
    // Step by step
    function Connect: boolean;
    function Login: boolean;
    function SendMail(const AFrom: string; ARecipients: TStrings;
      const AData: string): boolean;
    procedure Disconnect;
    // All in one: connect, login when UserName is set, send, quit
    function Send(const AFrom: string; ARecipients: TStrings;
      const AData: string): boolean;
    function HasExtension(const AName: string): boolean;
    property Host: string read FHost write FHost;
    property Port: word read FPort write FPort;
    property Security: TSMTPSecurity read FSecurity write FSecurity;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property HeloName: string read FHeloName write FHeloName;
    property Timeout: integer read FTimeout write FTimeout;  // ms, default 30000
    property VerifyPeerCert: boolean read FVerifyPeerCert write FVerifyPeerCert;
    property Connected: boolean read FConnected;
    property LastCode: integer read FLastCode;
    property LastReply: string read FLastReply;
    property ErrorMessage: string read FErrorMessage;
    property Log: TStringList read FLog;
    property Extensions: TStringList read FExtensions;
  end;

// "Name" <a@b> or Name <a@b> or a@b  ->  a@b
function SMTPExtractAddress(const S: string): string;
// RFC 2047 B-encoding of a header value; ASCII input is returned unchanged
function SMTPEncodeHeader(const S: string): string;
// Encode the display name of one address, keep the address itself
function SMTPEncodeAddress(const S: string): string;
// Escape lines that start with '.', as required inside DATA
function SMTPDotStuff(const AData: string): string;
// RFC 5322 date header value for ADateTime (local time with zone offset)
function SMTPDateTime(ADateTime: TDateTime): string;
// Build a complete message (headers + base64 body) with CRLF line endings.
// ATo and ACc are comma separated address lists; ABcc is never written.
function SMTPBuildMessage(const AFrom, ATo, ACc, ASubject, ABody: string;
  AHtml: boolean; const AExtraHeaders: string = ''): string;
function SMTPBase64Lines(const S: string): string;

implementation

uses
  base64;

const
  CRLF = #13#10;
  {$if defined(linux) or defined(android) or defined(freebsd) or defined(openbsd) or defined(netbsd)}
  SMTP_SEND_FLAGS = MSG_NOSIGNAL; // do not raise SIGPIPE when the server went away
  {$else}
  SMTP_SEND_FLAGS = 0;
  {$endif}

{ ---------------------------------------------------------------------
  helpers
  --------------------------------------------------------------------- }

function IsAscii(const S: string): boolean;
var
  i: integer;
begin
  for i := 1 to Length(S) do
    if (S[i] < #32) or (S[i] > #126) then
      Exit(False);
  Result := True;
end;

function SMTPExtractAddress(const S: string): string;
var
  p1, p2: integer;
begin
  p1 := Pos('<', S);
  p2 := Pos('>', S);
  if (p1 > 0) and (p2 > p1) then
    Result := Copy(S, p1 + 1, p2 - p1 - 1)
  else
    Result := S;
  Result := Trim(Result);
end;

function SMTPEncodeHeader(const S: string): string;
begin
  if IsAscii(S) then
    Result := S
  else
    Result := '=?UTF-8?B?' + EncodeStringBase64(S) + '?=';
end;

function SMTPEncodeAddress(const S: string): string;
var
  addr, name: string;
  p: integer;
begin
  addr := SMTPExtractAddress(S);
  p := Pos('<', S);
  if p > 0 then
    name := Trim(Copy(S, 1, p - 1))
  else
    name := '';
  if (Length(name) >= 2) and (name[1] = '"') and (name[Length(name)] = '"') then
    name := Copy(name, 2, Length(name) - 2);
  if name = '' then
    Exit(addr);
  if IsAscii(name) then
    Result := '"' + name + '" <' + addr + '>'
  else
    Result := SMTPEncodeHeader(name) + ' <' + addr + '>';
end;

function SMTPDotStuff(const AData: string): string;
begin
  Result := AData;
  if (Result <> '') and (Result[1] = '.') then
    Result := '.' + Result;
  Result := StringReplace(Result, CRLF + '.', CRLF + '..', [rfReplaceAll]);
end;

function SMTPDateTime(ADateTime: TDateTime): string;
const
  Days: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  Months: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  y, m, d, h, n, s, ms: word;
  ofs: integer;
  sign: char;
begin
  DecodeDate(ADateTime, y, m, d);
  DecodeTime(ADateTime, h, n, s, ms);
  // GetLocalTimeOffset is minutes west of UTC
  ofs := -GetLocalTimeOffset;
  if ofs < 0 then
    sign := '-'
  else
    sign := '+';
  ofs := Abs(ofs);
  Result := Format('%s, %d %s %d %.2d:%.2d:%.2d %s%.2d%.2d',
    [Days[DayOfWeek(ADateTime)], d, Months[m], y, h, n, s, sign,
    ofs div 60, ofs mod 60]);
end;

function SMTPBase64Lines(const S: string): string;
var
  enc: string;
  i: integer;
begin
  enc := EncodeStringBase64(S);
  Result := '';
  i := 1;
  while i <= Length(enc) do
  begin
    Result := Result + Copy(enc, i, 76) + CRLF;
    Inc(i, 76);
  end;
end;

function NormalizeLineEndings(const S: string): string;
begin
  Result := StringReplace(S, CRLF, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #10, CRLF, [rfReplaceAll]);
end;

function EncodeAddressList(const S: string): string;
var
  parts: TStringList;
  i: integer;
begin
  Result := '';
  parts := TStringList.Create;
  try
    parts.StrictDelimiter := True;
    parts.Delimiter := ',';
    parts.DelimitedText := S;
    for i := 0 to parts.Count - 1 do
    begin
      if Trim(parts[i]) = '' then
        Continue;
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + SMTPEncodeAddress(Trim(parts[i]));
    end;
  finally
    parts.Free;
  end;
end;

function SMTPBuildMessage(const AFrom, ATo, ACc, ASubject, ABody: string;
  AHtml: boolean; const AExtraHeaders: string): string;
var
  guid: TGuid;
  domain: string;
begin
  domain := SMTPExtractAddress(AFrom);
  if Pos('@', domain) > 0 then
    domain := Copy(domain, Pos('@', domain) + 1, MaxInt)
  else
    domain := 'localhost';
  CreateGUID(guid);
  Result := 'Date: ' + SMTPDateTime(Now) + CRLF +
    'From: ' + SMTPEncodeAddress(AFrom) + CRLF +
    'To: ' + EncodeAddressList(ATo) + CRLF;
  if Trim(ACc) <> '' then
    Result := Result + 'Cc: ' + EncodeAddressList(ACc) + CRLF;
  Result := Result + 'Subject: ' + SMTPEncodeHeader(ASubject) + CRLF +
    'Message-ID: <' + LowerCase(StringReplace(StringReplace(StringReplace(
    GUIDToString(guid), '{', '', []), '}', '', []), '-', '', [rfReplaceAll])) +
    '@' + domain + '>' + CRLF +
    'MIME-Version: 1.0' + CRLF;
  if AHtml then
    Result := Result + 'Content-Type: text/html; charset=UTF-8' + CRLF
  else
    Result := Result + 'Content-Type: text/plain; charset=UTF-8' + CRLF;
  Result := Result + 'Content-Transfer-Encoding: base64' + CRLF;
  if Trim(AExtraHeaders) <> '' then
    Result := Result + NormalizeLineEndings(Trim(AExtraHeaders)) + CRLF;
  Result := Result + CRLF + SMTPBase64Lines(NormalizeLineEndings(ABody));
end;

{ ---------------------------------------------------------------------
  TSMTPSocketHandler
  --------------------------------------------------------------------- }

function TSMTPSocketHandler.Connect: boolean;
begin
  if FWantTLS then
    Result := inherited Connect
  else
    Result := True;
end;

function TSMTPSocketHandler.Close: boolean;
begin
  if SSLActive then
    Result := inherited Close
  else
    Result := True;
end;

function TSMTPSocketHandler.Shutdown(BiDirectional: boolean): boolean;
begin
  if SSLActive then
    Result := inherited Shutdown(BiDirectional)
  else
    Result := True;
end;

function TSMTPSocketHandler.Send(const Buffer; Count: integer): integer;
begin
  if SSLActive then
    Result := inherited Send(Buffer, Count)
  else
    Result := fpsend(Socket.Handle, @Buffer, Count, SMTP_SEND_FLAGS);
end;

function TSMTPSocketHandler.Recv(const Buffer; Count: integer): integer;
begin
  if SSLActive then
    Result := inherited Recv(Buffer, Count)
  else
    Result := fprecv(Socket.Handle, @Buffer, Count, 0);
end;

function TSMTPSocketHandler.UpgradeToTLS: boolean;
begin
  FWantTLS := True;
  Result := Connect;
end;

{ ---------------------------------------------------------------------
  TSMTPClient
  --------------------------------------------------------------------- }

constructor TSMTPClient.Create;
begin
  inherited Create;
  FPort := 25;
  FSecurity := ssNone;
  FTimeout := 30000;
  FHeloName := 'localhost';
  FLog := TStringList.Create;
  FExtensions := TStringList.Create;
end;

destructor TSMTPClient.Destroy;
begin
  Disconnect;
  FExtensions.Free;
  FLog.Free;
  inherited Destroy;
end;

procedure TSMTPClient.AddLog(const ALine: string);
begin
  FLog.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + ALine);
end;

function TSMTPClient.Fail(const AMessage: string): boolean;
begin
  FErrorMessage := AMessage;
  AddLog('!! ' + AMessage);
  Result := False;
end;

function TSMTPClient.ReadLine(out ALine: string): boolean;
var
  buf: array[0..4095] of char;
  n, p: integer;
begin
  ALine := '';
  Result := False;
  repeat
    p := Pos(CRLF, FBuffer);
    if p > 0 then
    begin
      ALine := Copy(FBuffer, 1, p - 1);
      Delete(FBuffer, 1, p + 1);
      Exit(True);
    end;
    n := FSocket.Read(buf, SizeOf(buf));
    if n <= 0 then
      Exit(Fail('connection closed or timed out while waiting for the server'));
    SetLength(ALine, n);
    Move(buf, ALine[1], n);
    FBuffer := FBuffer + ALine;
    ALine := '';
  until False;
end;

// Reads one (possibly multi-line) reply into FLastCode / FLastReply.
function TSMTPClient.ReadReply: boolean;
var
  line: string;
  code: integer;
begin
  FLastReply := '';
  FLastCode := 0;
  repeat
    if not ReadLine(line) then
      Exit(False);
    AddLog('< ' + line);
    if (Length(line) < 3) or not TryStrToInt(Copy(line, 1, 3), code) then
      Exit(Fail('malformed reply: ' + line));
    FLastCode := code;
    if FLastReply <> '' then
      FLastReply := FLastReply + #10;
    FLastReply := FLastReply + Copy(line, 5, MaxInt);
  until (Length(line) < 4) or (line[4] <> '-');
  Result := True;
end;

function TSMTPClient.SendRaw(const AData: string): boolean;
var
  sent, total: integer;
begin
  total := 0;
  while total < Length(AData) do
  begin
    sent := FSocket.Write(AData[total + 1], Length(AData) - total);
    if sent <= 0 then
      Exit(Fail('connection closed while sending'));
    Inc(total, sent);
  end;
  Result := True;
end;

function TSMTPClient.SendLine(const ALine: string; const ALogAs: string): boolean;
begin
  if ALogAs = '' then
    AddLog('> ' + ALine)
  else
    AddLog('> ' + ALogAs);
  Result := SendRaw(ALine + CRLF);
end;

function TSMTPClient.Command(const ACmd: string;
  const AExpected: array of integer; const ALogAs: string): boolean;
var
  i: integer;
begin
  if not SendLine(ACmd, ALogAs) then
    Exit(False);
  if not ReadReply then
    Exit(False);
  for i := Low(AExpected) to High(AExpected) do
    if FLastCode = AExpected[i] then
      Exit(True);
  Result := Fail(Format('%s failed: %d %s', [Copy(ACmd, 1, Pos(' ', ACmd + ' ') - 1),
    FLastCode, StringReplace(FLastReply, #10, ' / ', [rfReplaceAll])]));
end;

function TSMTPClient.HasExtension(const AName: string): boolean;
var
  i: integer;
begin
  for i := 0 to FExtensions.Count - 1 do
    if SameText(Copy(FExtensions[i], 1, Length(AName)), AName) and
      ((Length(FExtensions[i]) = Length(AName)) or
      (FExtensions[i][Length(AName) + 1] = ' ')) then
      Exit(True);
  Result := False;
end;

function TSMTPClient.Ehlo: boolean;
var
  lines: TStringList;
  i: integer;
begin
  FExtensions.Clear;
  if not Command('EHLO ' + FHeloName, [250]) then
  begin
    // old server: fall back to HELO
    if (FLastCode >= 500) and Command('HELO ' + FHeloName, [250]) then
      Exit(True);
    Exit(False);
  end;
  lines := TStringList.Create;
  try
    lines.Text := FLastReply;
    for i := 1 to lines.Count - 1 do
      FExtensions.Add(Trim(lines[i]));
  finally
    lines.Free;
  end;
  Result := True;
end;

function TSMTPClient.DoStartTLS: boolean;
begin
  if not HasExtension('STARTTLS') then
    Exit(Fail('server does not offer STARTTLS'));
  if not Command('STARTTLS', [220]) then
    Exit(False);
  FBuffer := '';
  if not FHandler.UpgradeToTLS then
    Exit(Fail('TLS handshake failed: ' + FHandler.SSLLastErrorString));
  AddLog('** TLS established');
  // the server state is reset after STARTTLS
  Result := Ehlo;
end;

function TSMTPClient.Connect: boolean;
var
  handler: TSocketHandler;
  {$ifdef darwin}
  one: integer;
  {$endif}
begin
  Disconnect;
  FErrorMessage := '';
  FBuffer := '';
  try
    // the OpenSSL handler loads the library on creation, so a plain
    // connection uses the default handler and never needs OpenSSL
    if FSecurity = ssNone then
      handler := TSocketHandler.Create
    else
    begin
      FHandler := TSMTPSocketHandler.Create;
      FHandler.VerifyPeerCert := FVerifyPeerCert;
      FHandler.WantTLS := FSecurity = ssSSL;
      handler := FHandler;
    end;
    AddLog(Format('** connecting to %s:%d', [FHost, FPort]));
    // with a handler given, TInetSocket.Create does not connect by itself
    FSocket := TInetSocket.Create(FHost, FPort, FTimeout, handler);
    FSocket.WriteFlags := SMTP_SEND_FLAGS;
    FSocket.Connect;
    {$ifdef darwin}
    one := 1;
    fpsetsockopt(FSocket.Handle, SOL_SOCKET, SO_NOSIGPIPE, @one, SizeOf(one));
    {$endif}
    FSocket.IOTimeout := FTimeout;
  except
    on E: Exception do
    begin
      // the socket owns the handler; drop both on a failed connect
      FreeAndNil(FSocket);
      FHandler := nil;
      Exit(Fail('connect failed: ' + E.Message));
    end;
  end;
  if (FSecurity = ssSSL) and not FHandler.SSLActive then
    Exit(Fail('TLS handshake failed: ' + FHandler.SSLLastErrorString));
  FConnected := True;
  if not ReadReply then
    Exit(False);
  if FLastCode <> 220 then
    Exit(Fail('unexpected greeting: ' + IntToStr(FLastCode) + ' ' + FLastReply));
  if not Ehlo then
    Exit(False);
  if FSecurity = ssStartTLS then
    Result := DoStartTLS
  else
    Result := True;
end;

function TSMTPClient.Login: boolean;
var
  auth: string;
begin
  if FUserName = '' then
    Exit(True);
  if not FConnected then
    Exit(Fail('not connected'));
  // choose PLAIN when offered (or when the server lists nothing), else LOGIN
  auth := '';
  if HasExtension('AUTH') then
  begin
    if Pos('PLAIN', UpperCase(FExtensions.Text)) > 0 then
      auth := 'PLAIN'
    else if Pos('LOGIN', UpperCase(FExtensions.Text)) > 0 then
      auth := 'LOGIN';
  end;
  if auth = '' then
    auth := 'PLAIN';
  if auth = 'PLAIN' then
    Result := Command('AUTH PLAIN ' + EncodeStringBase64(#0 + FUserName + #0 + FPassword),
      [235], 'AUTH PLAIN ****')
  else
  begin
    Result := Command('AUTH LOGIN', [334]) and
      Command(EncodeStringBase64(FUserName), [334], '**** (username)') and
      Command(EncodeStringBase64(FPassword), [235], '**** (password)');
  end;
end;

function TSMTPClient.SendMail(const AFrom: string; ARecipients: TStrings;
  const AData: string): boolean;
var
  i: integer;
  addr, data: string;
begin
  if not FConnected then
    Exit(Fail('not connected'));
  if not Assigned(ARecipients) or (ARecipients.Count = 0) then
    Exit(Fail('no recipients'));
  if not Command('MAIL FROM:<' + SMTPExtractAddress(AFrom) + '>', [250]) then
    Exit(False);
  for i := 0 to ARecipients.Count - 1 do
  begin
    addr := SMTPExtractAddress(ARecipients[i]);
    if addr = '' then
      Continue;
    if not Command('RCPT TO:<' + addr + '>', [250, 251]) then
      Exit(False);
  end;
  if not Command('DATA', [354]) then
    Exit(False);
  data := SMTPDotStuff(NormalizeLineEndings(AData));
  if (data <> '') and (Copy(data, Length(data) - 1, 2) <> CRLF) then
    data := data + CRLF;
  AddLog('> (' + IntToStr(Length(data)) + ' bytes of message data)');
  if not SendRaw(data) then
    Exit(False);
  Result := Command('.', [250]);
end;

procedure TSMTPClient.Disconnect;
var
  savedError, savedReply: string;
  savedCode: integer;
begin
  if FConnected then
  begin
    // QUIT is best effort; it must not replace the error, code and reply
    // of the command that matters (the last one before QUIT)
    savedError := FErrorMessage;
    savedCode := FLastCode;
    savedReply := FLastReply;
    try
      Command('QUIT', [221]);
    except
    end;
    FErrorMessage := savedError;
    FLastCode := savedCode;
    FLastReply := savedReply;
  end;
  FConnected := False;
  FreeAndNil(FSocket);   // also frees FHandler
  FHandler := nil;
  FBuffer := '';
end;

function TSMTPClient.Send(const AFrom: string; ARecipients: TStrings;
  const AData: string): boolean;
begin
  Result := Connect and Login and SendMail(AFrom, ARecipients, AData);
  Disconnect;
end;

end.

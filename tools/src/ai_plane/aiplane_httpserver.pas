unit aiplane_httpserver;

{
  Minimal HTTP/1.1 server for the Agent Plane MCP endpoint.

  - Listens on 127.0.0.1 only.
  - Non-blocking sockets, polled from the owner's timer (Poll), no threads:
    OnRequest is raised in the caller's thread, so handlers may use IDEIntf.
  - Keep-alive, pipelined requests, Content-Length and chunked bodies.
  - A request may be answered later (Respond after OnRequest returned), for
    example after the user answered a permission card; the connection waits.
  - Windows, macOS and Linux: socket differences are kept in this unit.

  This unit must not depend on LCL or IDEIntf.
}

{$mode objfpc}{$H+}

interface

uses
  // WinSock2 first, so the portable Sockets names win where both declare one
  {$ifdef MSWINDOWS}WinSock2,{$endif}
  {$ifdef UNIX}BaseUnix,{$endif}
  Classes, SysUtils, Sockets;

const
  AI_HTTP_MAX_BODY = 4 * 1024 * 1024;
  AI_HTTP_MAX_HEADER = 64 * 1024;
  AI_HTTP_IDLE_TIMEOUT_MS = 5 * 60 * 1000;

type
  TAIHttpServer = class;
  TAIHttpConnection = class;

  { TAIHttpRequest }

  TAIHttpRequest = class
  private
    FConnection: TAIHttpConnection;
    FHeaders: TStringList;
    FAnswered: boolean;
  public
    Method: string;
    Path: string;
    Body: string;
    constructor Create;
    destructor Destroy; override;
    { Header value by name (case-insensitive), '' when missing. }
    function Header(const AName: string): string;
    property Headers: TStringList read FHeaders;
    { False once the client closed the connection. }
    function Connected: boolean;
    property Answered: boolean read FAnswered;
  end;

  TAIHttpRequestEvent = procedure(Sender: TObject; ARequest: TAIHttpRequest) of object;

  TAIHttpConnState = (hcsReading, hcsWaiting, hcsClosing);

  { TAIHttpConnection }

  TAIHttpConnection = class
  private
    FServer: TAIHttpServer;
    FSocket: TSocket;
    FIn: string;
    FOut: string;
    FState: TAIHttpConnState;
    FRequest: TAIHttpRequest;
    FKeepAlive: boolean;
    FLastActivity: QWord;
    function ReadAvailable: boolean;
    function WritePending: boolean;
    function TryParseRequest: boolean;
    procedure SendError(AStatus: integer; const AMessage: string);
  public
    constructor Create(AServer: TAIHttpServer; ASocket: TSocket);
    destructor Destroy; override;
  end;

  { TAIHttpServer }

  TAIHttpServer = class
  private
    FListener: TSocket;
    FPort: word;
    FConnections: TList;
    FOnRequest: TAIHttpRequestEvent;
    FPolling: boolean;
    FLastError: string;
    procedure AcceptConnections;
    procedure CloseConnection(AConn: TAIHttpConnection);
    procedure DispatchRequest(AConn: TAIHttpConnection);
  public
    constructor Create;
    destructor Destroy; override;
    { Binds 127.0.0.1:APort, or the first free port up to APort + ATries - 1.
      Returns False when none was free (LastError tells why). Port 0 lets the
      system pick one. }
    function Start(APort: word; ATries: integer = 1): boolean;
    procedure Stop;
    function Active: boolean;
    { Accepts, reads, dispatches and writes. Returns True when anything
      happened. Calls nested inside a handler (the IDE processes messages
      while it compiles) return at once. }
    function Poll: boolean;
    { Answers a request. AHeaders: extra "Name: value" lines. Frees nothing:
      the request object is owned by its connection. Ignored when the
      connection is gone or the request was answered already. }
    procedure Respond(ARequest: TAIHttpRequest; AStatus: integer;
      const AContentType, ABody: string; AHeaders: TStrings = nil);
    function ConnectionCount: integer;
    property Port: word read FPort;
    property LastError: string read FLastError;
    property OnRequest: TAIHttpRequestEvent read FOnRequest write FOnRequest;
  end;

function AIHttpStatusText(AStatus: integer): string;

implementation

const
  INVALID_SOCK = TSocket(-1);
  {$ifdef MSWINDOWS}
  // Windows SO_REUSEADDR lets a second process steal the port; this is the
  // opposite: the port stays ours alone.
  SO_EXCLUSIVEADDRUSE_OPT = longint(not longint(SO_REUSEADDR));
  {$endif}
  {$ifdef LINUX}
  // Writing to a socket closed by the peer must not raise SIGPIPE, which
  // would end the IDE.
  SEND_FLAGS = $4000; // MSG_NOSIGNAL
  {$else}
  SEND_FLAGS = 0;
  {$endif}
  {$ifdef DARWIN}
  SO_NOSIGPIPE_OPT = $1022;
  {$endif}

function AIHttpStatusText(AStatus: integer): string;
begin
  case AStatus of
    200: Result := 'OK';
    202: Result := 'Accepted';
    204: Result := 'No Content';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    411: Result := 'Length Required';
    413: Result := 'Payload Too Large';
    415: Result := 'Unsupported Media Type';
    431: Result := 'Request Header Fields Too Large';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    503: Result := 'Service Unavailable';
    else
      Result := 'Status';
  end;
end;

function WouldBlock: boolean;
var
  e: integer;
begin
  e := SocketError;
  Result := (e = EsockEWOULDBLOCK) {$ifdef UNIX} or (e = ESysEAGAIN){$endif}
    or (e = EsockEINTR);
end;

procedure SetNonBlocking(ASocket: TSocket);
{$ifdef MSWINDOWS}
var
  arg: u_long;
begin
  arg := 1;
  ioctlsocket(ASocket, longint(FIONBIO), arg);
end;
{$else}
var
  flags: longint;
begin
  flags := FpFcntl(ASocket, F_GETFL, 0);
  FpFcntl(ASocket, F_SETFL, flags or O_NONBLOCK);
end;
{$endif}

procedure PrepareSocket(ASocket: TSocket);
{$ifdef DARWIN}
var
  one: longint;
{$endif}
begin
  SetNonBlocking(ASocket);
  {$ifdef DARWIN}
  one := 1;
  fpsetsockopt(ASocket, SOL_SOCKET, SO_NOSIGPIPE_OPT, @one, SizeOf(one));
  {$endif}
end;

procedure CloseSock(ASocket: TSocket);
begin
  if ASocket = INVALID_SOCK then
    Exit;
  fpshutdown(ASocket, 2);
  CloseSocket(ASocket);
end;

{ TAIHttpRequest }

constructor TAIHttpRequest.Create;
begin
  FHeaders := TStringList.Create;
  FHeaders.NameValueSeparator := ':';
  FHeaders.CaseSensitive := False;
end;

destructor TAIHttpRequest.Destroy;
begin
  FHeaders.Free;
  inherited Destroy;
end;

function TAIHttpRequest.Header(const AName: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FHeaders.Count - 1 do
    if SameText(FHeaders.Names[i], AName) then
      Exit(Trim(FHeaders.ValueFromIndex[i]));
end;

function TAIHttpRequest.Connected: boolean;
begin
  Result := Assigned(FConnection);
end;

{ TAIHttpConnection }

constructor TAIHttpConnection.Create(AServer: TAIHttpServer; ASocket: TSocket);
begin
  FServer := AServer;
  FSocket := ASocket;
  FState := hcsReading;
  FKeepAlive := True;
  FLastActivity := GetTickCount64;
end;

destructor TAIHttpConnection.Destroy;
begin
  // A request still waiting for its answer must see that the client left;
  // Respond frees it then.
  if Assigned(FRequest) then
    FRequest.FConnection := nil;
  CloseSock(FSocket);
  inherited Destroy;
end;

// Returns False when the peer closed the connection or it failed.
function TAIHttpConnection.ReadAvailable: boolean;
var
  buf: array[0..16383] of char;
  n: integer;
begin
  Result := True;
  repeat
    n := fprecv(FSocket, @buf[0], SizeOf(buf), 0);
    if n > 0 then
    begin
      SetLength(FIn, Length(FIn) + n);
      Move(buf[0], FIn[Length(FIn) - n + 1], n);
      FLastActivity := GetTickCount64;
    end
    else if n = 0 then
      Exit(False)
    else if WouldBlock then
      Exit(True)
    else
      Exit(False);
  until n < SizeOf(buf);
end;

function TAIHttpConnection.WritePending: boolean;
var
  n: integer;
begin
  Result := True;
  while FOut <> '' do
  begin
    n := fpsend(FSocket, @FOut[1], Length(FOut), SEND_FLAGS);
    if n > 0 then
    begin
      Delete(FOut, 1, n);
      FLastActivity := GetTickCount64;
    end
    else if (n < 0) and WouldBlock then
      Exit(True)
    else
      Exit(False);
  end;
end;

procedure TAIHttpConnection.SendError(AStatus: integer; const AMessage: string);
var
  req: TAIHttpRequest;
begin
  FKeepAlive := False;
  req := TAIHttpRequest.Create;
  req.FConnection := Self;
  FRequest := req;
  FState := hcsWaiting;
  FServer.Respond(req, AStatus, 'text/plain; charset=utf-8', AMessage);
end;

function DecodeChunked(const AData: string; out ABody: string;
  out AConsumed: integer): boolean;
var
  p, eol, size: integer;
  sizeText: string;
begin
  Result := False;
  ABody := '';
  AConsumed := 0;
  p := 1;
  while True do
  begin
    eol := Pos(#13#10, Copy(AData, p, 20));
    if eol = 0 then
      Exit;
    sizeText := Copy(AData, p, eol - 1);
    if Pos(';', sizeText) > 0 then
      sizeText := Copy(sizeText, 1, Pos(';', sizeText) - 1);
    size := StrToIntDef('$' + Trim(sizeText), -1);
    if size < 0 then
      raise EConvertError.Create('bad chunk size');
    p := p + eol + 1;
    if size = 0 then
    begin
      // trailers end with an empty line; the search starts at the CRLF of
      // the "0" line, so the match ends at (p - 2) + (eol - 1) + 3
      eol := Pos(#13#10#13#10, Copy(AData, p - 2, MaxInt));
      if eol = 0 then
        Exit;
      AConsumed := p + eol;
      Exit(True);
    end;
    if Length(AData) < p + size + 1 then
      Exit;
    ABody := ABody + Copy(AData, p, size);
    if Length(ABody) > AI_HTTP_MAX_BODY then
      raise EConvertError.Create('body too large');
    p := p + size + 2;
  end;
end;

// True when a complete request was taken from FIn into FRequest.
function TAIHttpConnection.TryParseRequest: boolean;
var
  headerEnd, i, len, consumed, sp1, sp2: integer;
  head, line, version, connHeader, body: string;
  lines: TStringList;
  req: TAIHttpRequest;
begin
  Result := False;
  // tolerate empty lines between pipelined requests
  while Copy(FIn, 1, 2) = #13#10 do
    Delete(FIn, 1, 2);
  headerEnd := Pos(#13#10#13#10, FIn);
  if headerEnd = 0 then
  begin
    if Length(FIn) > AI_HTTP_MAX_HEADER then
      SendError(431, 'Header too large');
    Exit;
  end;
  head := Copy(FIn, 1, headerEnd - 1);
  lines := TStringList.Create;
  req := TAIHttpRequest.Create;
  try
    lines.Text := head;
    if lines.Count = 0 then
    begin
      SendError(400, 'Bad request');
      Exit;
    end;
    line := lines[0];
    sp1 := Pos(' ', line);
    sp2 := Pos(' ', Copy(line, sp1 + 1, MaxInt)) + sp1;
    if (sp1 = 0) or (sp2 = sp1) then
    begin
      SendError(400, 'Bad request line');
      Exit;
    end;
    req.Method := UpperCase(Copy(line, 1, sp1 - 1));
    req.Path := Copy(line, sp1 + 1, sp2 - sp1 - 1);
    version := Copy(line, sp2 + 1, MaxInt);
    for i := 1 to lines.Count - 1 do
      if Pos(':', lines[i]) > 1 then
        req.Headers.Add(Trim(Copy(lines[i], 1, Pos(':', lines[i]) - 1)) + ':' +
          Trim(Copy(lines[i], Pos(':', lines[i]) + 1, MaxInt)));

    if SameText(req.Header('Transfer-Encoding'), 'chunked') then
    begin
      try
        if not DecodeChunked(Copy(FIn, headerEnd + 4, MaxInt), body, consumed) then
          Exit; // wait for more data
      except
        SendError(400, 'Bad chunked body');
        Exit;
      end;
      req.Body := body;
      Delete(FIn, 1, headerEnd + 3 + consumed);
    end
    else
    begin
      len := StrToIntDef(req.Header('Content-Length'), 0);
      if len < 0 then
      begin
        SendError(400, 'Bad Content-Length');
        Exit;
      end;
      if len > AI_HTTP_MAX_BODY then
      begin
        SendError(413, 'Body too large');
        Exit;
      end;
      if Length(FIn) < headerEnd + 3 + len then
        Exit; // wait for more data
      req.Body := Copy(FIn, headerEnd + 4, len);
      Delete(FIn, 1, headerEnd + 3 + len);
    end;

    connHeader := LowerCase(req.Header('Connection'));
    if SameText(version, 'HTTP/1.0') then
      FKeepAlive := Pos('keep-alive', connHeader) > 0
    else
      FKeepAlive := Pos('close', connHeader) = 0;
    req.FConnection := Self;
    FRequest := req;
    req := nil;
    Result := True;
  finally
    req.Free;
    lines.Free;
  end;
end;

{ TAIHttpServer }

constructor TAIHttpServer.Create;
begin
  FListener := INVALID_SOCK;
  FConnections := TList.Create;
end;

destructor TAIHttpServer.Destroy;
begin
  Stop;
  FConnections.Free;
  inherited Destroy;
end;

function TAIHttpServer.Start(APort: word; ATries: integer): boolean;
var
  sock: TSocket;
  addr: TInetSockAddr;
  opt: longint;
  len: TSockLen;
  i: integer;
  tryPort: integer;
begin
  Stop;
  Result := False;
  FLastError := '';
  if ATries < 1 then
    ATries := 1;
  for i := 0 to ATries - 1 do
  begin
    tryPort := APort + i;
    if (APort <> 0) and (tryPort > 65535) then
      Break;
    sock := fpsocket(AF_INET, SOCK_STREAM, 0);
    if sock = INVALID_SOCK then
    begin
      FLastError := 'socket() failed: ' + IntToStr(SocketError);
      Exit;
    end;
    opt := 1;
    {$ifdef MSWINDOWS}
    fpsetsockopt(sock, SOL_SOCKET, SO_EXCLUSIVEADDRUSE_OPT, @opt, SizeOf(opt));
    {$else}
    fpsetsockopt(sock, SOL_SOCKET, SO_REUSEADDR, @opt, SizeOf(opt));
    {$endif}
    FillChar(addr, SizeOf(addr), 0);
    addr.sin_family := AF_INET;
    addr.sin_port := htons(word(tryPort));
    addr.sin_addr := StrToNetAddr('127.0.0.1');
    if (fpbind(sock, @addr, SizeOf(addr)) = 0) and (fplisten(sock, 16) = 0) then
    begin
      PrepareSocket(sock);
      FListener := sock;
      len := SizeOf(addr);
      if fpgetsockname(sock, @addr, @len) = 0 then
        FPort := ntohs(addr.sin_port)
      else
        FPort := tryPort;
      Exit(True);
    end;
    FLastError := Format('port %d: bind failed (%d)', [tryPort, SocketError]);
    CloseSocket(sock);
    if APort = 0 then
      Break;
  end;
  if ATries > 1 then
    FLastError := Format('ports %d-%d are in use', [APort, APort + ATries - 1]);
end;

procedure TAIHttpServer.Stop;
var
  i: integer;
begin
  for i := FConnections.Count - 1 downto 0 do
    TAIHttpConnection(FConnections[i]).Free;
  FConnections.Clear;
  if FListener <> INVALID_SOCK then
  begin
    CloseSocket(FListener);
    FListener := INVALID_SOCK;
  end;
  FPort := 0;
end;

function TAIHttpServer.Active: boolean;
begin
  Result := FListener <> INVALID_SOCK;
end;

function TAIHttpServer.ConnectionCount: integer;
begin
  Result := FConnections.Count;
end;

procedure TAIHttpServer.AcceptConnections;
var
  sock: TSocket;
  addr: TInetSockAddr;
  len: TSockLen;
begin
  repeat
    len := SizeOf(addr);
    sock := fpaccept(FListener, @addr, @len);
    if sock = INVALID_SOCK then
      Exit;
    // Unix does not pass non-blocking mode on to accepted sockets.
    PrepareSocket(sock);
    FConnections.Add(TAIHttpConnection.Create(Self, sock));
  until False;
end;

procedure TAIHttpServer.CloseConnection(AConn: TAIHttpConnection);
begin
  FConnections.Remove(AConn);
  AConn.Free;
end;

procedure TAIHttpServer.DispatchRequest(AConn: TAIHttpConnection);
var
  req: TAIHttpRequest;
begin
  AConn.FState := hcsWaiting;
  req := AConn.FRequest;
  try
    if Assigned(FOnRequest) then
      FOnRequest(Self, req)
    else
      Respond(req, 404, 'text/plain', 'Not found');
  except
    on E: Exception do
      Respond(req, 500, 'text/plain; charset=utf-8', E.Message);
  end;
end;

function TAIHttpServer.Poll: boolean;
var
  i: integer;
  conn: TAIHttpConnection;
  before: integer;
  alive: boolean;
begin
  Result := False;
  if not Active or FPolling then
    Exit;
  FPolling := True;
  try
    before := FConnections.Count;
    AcceptConnections;
    Result := FConnections.Count <> before;
    i := 0;
    while i < FConnections.Count do
    begin
      conn := TAIHttpConnection(FConnections[i]);
      alive := True;
      if conn.FState <> hcsClosing then
      begin
        before := Length(conn.FIn);
        alive := conn.ReadAvailable;
        if Length(conn.FIn) <> before then
          Result := True;
      end;
      if alive and (conn.FState = hcsReading) and conn.TryParseRequest then
      begin
        Result := True;
        DispatchRequest(conn);
        // the handler may have stopped the server
        if not Active then
          Exit;
      end;
      if alive and (conn.FOut <> '') then
      begin
        Result := True;
        alive := conn.WritePending;
      end;
      if alive and (conn.FState = hcsClosing) and (conn.FOut = '') then
        alive := False;
      if alive and (conn.FState = hcsReading) and (conn.FIn = '') and
        (GetTickCount64 - conn.FLastActivity > AI_HTTP_IDLE_TIMEOUT_MS) then
        alive := False;
      if alive then
        Inc(i)
      else
      begin
        // a waiting request may still be answered; Respond sees the gap
        CloseConnection(conn);
        Result := True;
      end;
    end;
  finally
    FPolling := False;
  end;
end;

procedure TAIHttpServer.Respond(ARequest: TAIHttpRequest; AStatus: integer;
  const AContentType, ABody: string; AHeaders: TStrings);
var
  conn: TAIHttpConnection;
  s: string;
  i: integer;
begin
  if not Assigned(ARequest) or ARequest.Answered then
    Exit;
  ARequest.FAnswered := True;
  conn := ARequest.FConnection;
  if not Assigned(conn) then
  begin
    // the client left while the request waited
    ARequest.Free;
    Exit;
  end;
  s := Format('HTTP/1.1 %d %s'#13#10, [AStatus, AIHttpStatusText(AStatus)]);
  if AContentType <> '' then
    s := s + 'Content-Type: ' + AContentType + #13#10;
  s := s + 'Content-Length: ' + IntToStr(Length(ABody)) + #13#10;
  s := s + 'Cache-Control: no-store'#13#10;
  if Assigned(AHeaders) then
    for i := 0 to AHeaders.Count - 1 do
      s := s + AHeaders[i] + #13#10;
  if conn.FKeepAlive then
    s := s + 'Connection: keep-alive'#13#10
  else
    s := s + 'Connection: close'#13#10;
  s := s + #13#10 + ABody;
  conn.FOut := conn.FOut + s;
  conn.FRequest := nil;
  ARequest.Free;
  if conn.FKeepAlive then
    conn.FState := hcsReading
  else
    conn.FState := hcsClosing;
  // Answered from outside Poll (a permission card): send right away.
  if not FPolling then
    conn.WritePending;
end;

end.

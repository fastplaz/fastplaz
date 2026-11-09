{

Simple Redis Client Library
* for set and get data only.
- non-thread
- master-slave support

by: Luri Darmawan ( http://www.fastplaz.com )
requirement:
  - none (fpSend/fpRecv from the Sockets unit)
  - synapse (optional, define SYNAPSE)

USAGE:
[x] set value
Redis := TRedisLib.Create;
Redis['thekey'] := 'your string';

[x] get value
variable := Redis['thekey'];

[x] raw command
Redis.SendCommand(['HSET', 'user:1', 'name', 'Luri']);  // RESP array, any value is safe
Redis.SendString('PING');                               // inline command

[x] server address
Redis.ServerAddress := '127.0.0.1';   // IPv4 literal
Redis.ServerAddress := '::1';         // IPv6 literal, '[::1]' is accepted too
Redis.ServerAddress := 'redis.local'; // host name, IPv4 is tried first, then IPv6

USE MASTER SLAVE
Redis := TRedisLib.Create;
Redis.WriteToMaster := true;
Redis.MasterServerAddress := 'your.ip.address';
Redis.MasterPort: = '6378';

**********************************************************************}
unit redis_lib;

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

{$ifndef synapse}
  {$define fpcsocket}
{$endif}

uses
  {$ifdef synapse}
  blcksock,
  {$endif}
  {$ifdef fpcsocket}
  // WinSock2 / BaseUnix before Sockets so the Sockets names win on a clash
  {$ifdef windows}
  WinSock2,
  {$endif}
  {$ifdef unix}
  BaseUnix, UnixType, netdb,
  {$endif}
  Sockets, resolve,
  {$endif}
  Classes, SysUtils;

const
  REDIS_DEFAULT_SERVER = '127.0.0.1';
  REDIS_DEFAULT_PORT = 6379;
  REDIS_DEFAULT_TIMEOUT = 2500;
  REDIS_RECV_CHUNK_SIZE = 8192;
  __REDIS_RESPONSE_SINGLE_CHAR = '+'; // "+OK"
  __REDIS_RESPONSE_ERROR_CHAR = '-'; // error
  __REDIS_RESPONSE_INT_CHAR = ':'; //  ":100\r\n"
  __REDIS_RESPONSE_BULK_CHAR = '$'; // "$7\r\nexample\r\n"
  __REDIS_RESPONSE_MULTI_BULK_CHAR = '*'; // multiple bulk value

  {$ifdef fpcsocket}
  {$if defined(linux) or defined(android) or defined(freebsd) or defined(openbsd) or defined(netbsd)}
  REDIS_SEND_FLAGS = MSG_NOSIGNAL; // do not raise SIGPIPE when the server went away
  {$else}
  REDIS_SEND_FLAGS = 0;
  {$endif}
  {$endif}

type

  TRedisResponseType = (rrtStatus, rrtError,
    rrtNumeric, rrtBulk,
    rrtMultiBulk, rrtUnknown,
    rrtNull);

  {$ifdef fpcsocket}
  // one connect candidate: an IPv4 or IPv6 socket address plus its text form
  TRedisSockAddr = record
    Family: longint;
    Len: TSockLen;
    Text: string;
    case integer of
      0: (v4: TInetSockAddr);
      1: (v6: TInetSockAddr6);
  end;
  TRedisSockAddrList = array of TRedisSockAddr;
  {$endif}

  { TRedisLib }

  TRedisLib = class(TObject)
  private
    FServerAddress: string;
    FisAutoConnect: boolean;
    FLastError: integer;
    FLastErrorMessage: string;
    FLastMessage: string;
    FPassword: string;
    FPort: string;
    FPortInt: integer;
    FisConnected: boolean;
    FResponType: TRedisResponseType;
    FTimeOut: integer;
    FWriteToMaster: boolean;
    FRedisMaster: TRedisLib;
    FResolvedAddress: string;
    // raw bytes from the server, never passed through a code page conversion
    FRecvBuffer: RawByteString;

    {$ifdef synapse}
    sock: TTCPBlockSocket;
    {$endif}

    {$ifdef fpcsocket}
    FSocket: longint;
    {$endif}

    function Get(Key: string): string;
    function GetMasterPort: string;
    function GetMasterServerAddress: string;
    function GetMasterTimeOut: integer;
    function GetResponseType(Response: string): TRedisResponseType;
    procedure SetMasterPort(AValue: string);
    procedure SetMasterServerAddress(AValue: string);
    procedure SetMasterTimeOut(AValue: integer);
    procedure SetServerAddress(AValue: string);
    procedure SetPort(AValue: string);
    procedure SetTimeOut(AValue: integer);
    procedure SetValue(Key: string; AValue: string);
    procedure SetWriteToMaster(AValue: boolean);

    {$ifdef fpcsocket}
    function ResolveName4(const AHost: string; out AIp: in_addr): boolean;
    function ResolveName6(const AHost: string; out AIp: in6_addr): boolean;
    function ResolveAddresses(out AList: TRedisSockAddrList): boolean;
    function ConnectWithTimeout(const AAddr: TRedisSockAddr): boolean;
    procedure ApplySocketTimeout;
    {$endif}
    procedure SetSocketError(const AMessage: string);
    function SocketSend(const AData: RawByteString): boolean;
    function SocketRecv(out AData: RawByteString): boolean;
    function ReadLine(out ALine: RawByteString): boolean;
    function ReadBytes(ACount: integer; out AData: RawByteString): boolean;
    function ReadReply(out AReply: RawByteString): boolean;
    function Execute(const ARequest: RawByteString): string;
  public
    constructor Create(const ConfigName: string = 'default');
    destructor Destroy; override;

    property isConnected: boolean read FisConnected;
    property isAutoConnect: boolean read FisAutoConnect write FisAutoConnect;
    // IPv4 or IPv6 literal ('127.0.0.1', '::1', '[::1]') or a host name
    property ServerAddress: string read FServerAddress write SetServerAddress;
    // IP address the current/last connection was made to, '' when none
    property ResolvedAddress: string read FResolvedAddress;
    property Port: string read FPort write SetPort;
    property TimeOut: integer read FTimeOut write SetTimeOut;
    property LastError: integer read FLastError;
    property LastErrorMessage: string read FLastErrorMessage;
    property LastMessage: string read FLastMessage;
    property Values[Key: string]: string read Get write SetValue; default;
    property ResponType: TRedisResponseType read FResponType;
    property Password: string read FPassword write FPassword;

    // master
    property WriteToMaster: boolean read FWriteToMaster write SetWriteToMaster;
    property MasterServerAddress: string read GetMasterServerAddress write SetMasterServerAddress;
    property MasterPort: string read GetMasterPort write SetMasterPort;
    property MasterTimeOut: integer read GetMasterTimeOut write SetMasterTimeOut;


    function Connect: boolean;
    procedure Disconnect;
    // inline command, e.g. 'PING' or 'GET thekey'
    function SendString(Strings: string): string;
    // RESP array command, safe for values with spaces, quotes or newlines
    function SendCommand(const Args: array of string): string;
    // payload of a bulk reply ("$7"#13#10"example" -> "example")
    function BulkPayload(const AReply: string): string;

    function Auth(PasswordKey: string = ''): boolean;
    function Ping: boolean;
    procedure FlushAll;
    procedure FlushDB;
    function Info: string;
    // never sent a valid command ("SYNCH"); SYNC itself would turn the
    // connection into a replication stream, so this is now a no-op
    procedure Synch; deprecated 'SYNCH is not a Redis command, the call does nothing';

  end;

implementation

// Append bytes without the "+" operator: when the operands carry different
// code pages (e.g. a CP_UTF8 literal and a CP_ACP buffer) FPC would convert
// the data, which changes the bytes and makes the RESP "$<len>" header wrong.
procedure AppendBytes(var ABuffer: RawByteString; const AData: RawByteString);
var
  oldLen, dataLen: SizeInt;
begin
  dataLen := Length(AData);
  if dataLen = 0 then
    Exit;
  oldLen := Length(ABuffer);
  SetLength(ABuffer, oldLen + dataLen);
  if oldLen = 0 then
    SetCodePage(ABuffer, CP_NONE, False);
  Move(AData[1], ABuffer[oldLen + 1], dataLen);
end;

// position of the first CRLF at or after AFrom, 0 when there is none
function FindCRLF(const ABuffer: RawByteString; AFrom: SizeInt): SizeInt;
var
  len, i: SizeInt;
begin
  Result := 0;
  len := Length(ABuffer);
  if AFrom < 1 then
    AFrom := 1;
  i := AFrom;
  while i < len do
  begin
    if (ABuffer[i] = #13) and (ABuffer[i + 1] = #10) then
      Exit(i);
    Inc(i);
  end;
end;

{ TRedisLib }

procedure TRedisLib.SetServerAddress(AValue: string);
begin
  AValue := Trim(AValue);
  if AValue = '' then
    AValue := REDIS_DEFAULT_SERVER;
  if FServerAddress = AValue then
    Exit;
  FServerAddress := AValue;
end;

procedure TRedisLib.SetPort(AValue: string);
begin
  if FPort = AValue then
    Exit;
  FPortInt := StrToIntDef(AValue, 0);
  if (FPortInt < 1) or (FPortInt > 65535) then
  begin
    FPort := IntToStr(REDIS_DEFAULT_PORT);
    FPortInt := REDIS_DEFAULT_PORT;
  end
  else
    FPort := AValue;
end;

procedure TRedisLib.SetTimeOut(AValue: integer);
begin
  if FTimeOut = AValue then
    Exit;
  FTimeOut := AValue;
  {$ifdef fpcsocket}
  if FisConnected then
    ApplySocketTimeout;
  {$endif}
end;

function TRedisLib.Get(Key: string): string;
begin
  Result := '';
  if Key.IsEmpty then
    Exit;
  SendCommand(['GET', Key]);
  if FResponType = rrtBulk then
  begin
    FLastMessage := BulkPayload(FLastMessage);
    Result := FLastMessage;
  end;
end;

function TRedisLib.GetMasterPort: string;
begin
  Result := '';
  if Assigned(FRedisMaster) then
    Result := FRedisMaster.Port;
end;

function TRedisLib.GetMasterServerAddress: string;
begin
  Result := '';
  if Assigned(FRedisMaster) then
    Result := FRedisMaster.ServerAddress;
end;

function TRedisLib.GetMasterTimeOut: integer;
begin
  Result := 0;
  if Assigned(FRedisMaster) then
    Result := FRedisMaster.TimeOut;
end;

function TRedisLib.GetResponseType(Response: string): TRedisResponseType;
begin
  if Response = '' then
  begin
    Result := rrtUnknown;
    Exit;
  end;
  case Response[1] of
    __REDIS_RESPONSE_SINGLE_CHAR: Result := rrtStatus;
    __REDIS_RESPONSE_ERROR_CHAR: Result := rrtError;
    __REDIS_RESPONSE_BULK_CHAR: Result := rrtBulk;
    __REDIS_RESPONSE_MULTI_BULK_CHAR: Result := rrtMultiBulk;
    __REDIS_RESPONSE_INT_CHAR: Result := rrtNumeric;
    else
      Result := rrtUnknown;
  end;
  // "$-1" / "*-1": null bulk / null array
  if (Result in [rrtBulk, rrtMultiBulk]) and (Copy(Response, 2, 2) = '-1') then
    Result := rrtNull;
end;

procedure TRedisLib.SetMasterPort(AValue: string);
begin
  if Assigned(FRedisMaster) then
    FRedisMaster.Port := AValue;
end;

procedure TRedisLib.SetMasterServerAddress(AValue: string);
begin
  if Assigned(FRedisMaster) then
    FRedisMaster.ServerAddress := AValue;
end;

procedure TRedisLib.SetMasterTimeOut(AValue: integer);
begin
  if Assigned(FRedisMaster) then
    FRedisMaster.TimeOut := AValue;
end;

procedure TRedisLib.SetValue(Key: string; AValue: string);
begin
  if Key.IsEmpty then
    Exit;
  if FWriteToMaster and Assigned(FRedisMaster) then
    FRedisMaster.SendCommand(['SET', Key, AValue])
  else
    SendCommand(['SET', Key, AValue]);
end;

procedure TRedisLib.SetWriteToMaster(AValue: boolean);
begin
  if FWriteToMaster = AValue then
    Exit;
  FWriteToMaster := AValue;
  if FWriteToMaster then
  begin
    if not Assigned(FRedisMaster) then
      FRedisMaster := TRedisLib.Create();
  end
  else
    FreeAndNil(FRedisMaster);
end;

constructor TRedisLib.Create(const ConfigName: string);
begin
  FisConnected := False;
  FisAutoConnect := True;
  FPort := IntToStr(REDIS_DEFAULT_PORT);
  FPortInt := REDIS_DEFAULT_PORT;
  FTimeOut := REDIS_DEFAULT_TIMEOUT;
  FServerAddress := REDIS_DEFAULT_SERVER;
  FRecvBuffer := '';
  FRedisMaster := nil;
  {$ifdef synapse}
  sock := TTCPBlockSocket.Create;
  {$endif}
  {$ifdef fpcsocket}
  FSocket := -1;
  {$endif}
end;


destructor TRedisLib.Destroy;
begin
  Disconnect;
  {$ifdef synapse}
  FreeAndNil(sock);
  {$endif}
  FreeAndNil(FRedisMaster);
  inherited Destroy;
end;

{$ifdef fpcsocket}

{$ifdef windows}
// FPC 3.2 ships no IPv6 capable resolver for Windows, use getaddrinfo from ws2_32
// (the Sockets unit has already called WSAStartup)
{$push}{$packrecords C}
type
  PRedisAddrInfo = ^TRedisAddrInfo;
  TRedisAddrInfo = record
    ai_flags: longint;
    ai_family: longint;
    ai_socktype: longint;
    ai_protocol: longint;
    ai_addrlen: SizeUInt;
    ai_canonname: PAnsiChar;
    ai_addr: psockaddr;
    ai_next: PRedisAddrInfo;
  end;
{$pop}

function getaddrinfo(nodename, servname: PAnsiChar; hints: PRedisAddrInfo;
  out res: PRedisAddrInfo): longint; stdcall; external 'ws2_32.dll' name 'getaddrinfo';
procedure freeaddrinfo(ai: PRedisAddrInfo); stdcall; external 'ws2_32.dll' name 'freeaddrinfo';
{$endif}

// RFC 5952 text form: lower case, no leading zeros, longest zero run as "::"
// (NetAddrToStr6 from the Sockets unit gives "::0001" / "2001:0DB8::0001")
function In6AddrToText(const AIp: in6_addr): string;
var
  groups: array[0..7] of word;
  i, runStart, runLen, bestStart, bestLen: integer;
begin
  for i := 0 to 7 do
    groups[i] := (word(AIp.u6_addr8[i * 2]) shl 8) or AIp.u6_addr8[i * 2 + 1];

  // longest run of zero groups, at least two groups long
  bestStart := -1;
  bestLen := 1;
  i := 0;
  while i < 8 do
  begin
    if groups[i] = 0 then
    begin
      runStart := i;
      runLen := 0;
      while (i < 8) and (groups[i] = 0) do
      begin
        Inc(runLen);
        Inc(i);
      end;
      if runLen > bestLen then
      begin
        bestStart := runStart;
        bestLen := runLen;
      end;
    end
    else
      Inc(i);
  end;

  Result := '';
  i := 0;
  while i < 8 do
  begin
    if i = bestStart then
    begin
      Result := Result + '::';
      Inc(i, bestLen);
      Continue;
    end;
    if (Result <> '') and (Result[Length(Result)] <> ':') then
      Result := Result + ':';
    Result := Result + LowerCase(IntToHex(groups[i], 1));
    Inc(i);
  end;
end;

// "[::1]:6379" / "127.0.0.1:6379"
function EndpointText(const AAddr: TRedisSockAddr; const APort: string): string;
begin
  if AAddr.Family = AF_INET6 then
    Result := '[' + AAddr.Text + ']:' + APort
  else
    Result := AAddr.Text + ':' + APort;
end;

function TRedisLib.ResolveName4(const AHost: string; out AIp: in_addr): boolean;
var
  resolver: THostResolver;
begin
  AIp.s_addr := 0;
  resolver := THostResolver.Create(nil);
  try
    if resolver.NameLookup(AHost) then
      AIp := StrToNetAddr(resolver.AddressAsString);
  finally
    resolver.Free;
  end;
  Result := AIp.s_addr <> 0;
end;

function TRedisLib.ResolveName6(const AHost: string; out AIp: in6_addr): boolean;
{$ifdef windows}
var
  hints: TRedisAddrInfo;
  results, p: PRedisAddrInfo;
{$endif}
{$ifdef unix}
var
  entry: THostEntry6;
{$endif}
begin
  Result := False;
  FillChar(AIp, SizeOf(AIp), 0);
  {$ifdef windows}
  FillChar(hints, SizeOf(hints), 0);
  hints.ai_family := AF_INET6;
  hints.ai_socktype := SOCK_STREAM;
  results := nil;
  if getaddrinfo(PAnsiChar(AHost), nil, @hints, results) <> 0 then
    Exit;
  try
    p := results;
    while (p <> nil) and not Result do
    begin
      if (p^.ai_family = AF_INET6) and (p^.ai_addr <> nil) and
        (p^.ai_addrlen >= SizeOf(TInetSockAddr6)) then
      begin
        AIp := psockaddr_in6(p^.ai_addr)^.sin6_addr;
        Result := True;
      end;
      p := p^.ai_next;
    end;
  finally
    if results <> nil then
      freeaddrinfo(results);
  end;
  {$endif}
  {$ifdef unix}
  // AAAA lookup through the FPC resolver (no libc needed)
  if ResolveHostByName6(AHost, entry) then
  begin
    AIp := entry.Addr;
    Result := True;
  end;
  {$endif}
end;

// candidates in the order they are tried: an IP literal gives exactly one,
// a host name gives its IPv4 address first and its IPv6 address second
function TRedisLib.ResolveAddresses(out AList: TRedisSockAddrList): boolean;
var
  host: string;
  ip4: in_addr;
  ip6: in6_addr;
  n: integer;

  procedure Add4(const AIp: in_addr);
  begin
    SetLength(AList, n + 1);
    FillChar(AList[n].v6, SizeOf(AList[n].v6), 0);
    AList[n].Family := AF_INET;
    AList[n].Len := SizeOf(TInetSockAddr);
    AList[n].Text := NetAddrToStr(AIp);
    AList[n].v4.sin_family := AF_INET;
    AList[n].v4.sin_port := htons(FPortInt);
    AList[n].v4.sin_addr := AIp;
    Inc(n);
  end;

  procedure Add6(const AIp: in6_addr);
  begin
    SetLength(AList, n + 1);
    FillChar(AList[n].v6, SizeOf(AList[n].v6), 0);
    AList[n].Family := AF_INET6;
    AList[n].Len := SizeOf(TInetSockAddr6);
    AList[n].Text := In6AddrToText(AIp);
    {$if defined(freebsd) or defined(darwin) or defined(haiku)}
    AList[n].v6.sin6_len := SizeOf(TInetSockAddr6);
    {$endif}
    AList[n].v6.sin6_family := AF_INET6;
    AList[n].v6.sin6_port := htons(FPortInt);
    AList[n].v6.sin6_addr := AIp;
    Inc(n);
  end;

begin
  AList := nil;
  n := 0;
  host := FServerAddress;
  // "[::1]" as used in URLs
  if (Length(host) > 2) and (host[1] = '[') and (host[Length(host)] = ']') then
    host := Copy(host, 2, Length(host) - 2);

  ip4 := StrToNetAddr(host);
  if ip4.s_addr <> 0 then
    Add4(ip4)
  else if (Pos(':', host) > 0) and TryStrToHostAddr6(host, ip6) then
    Add6(ip6)
  else
  begin
    if ResolveName4(host, ip4) then
      Add4(ip4);
    if ResolveName6(host, ip6) then
      Add6(ip6);
  end;

  Result := n > 0;
  if not Result then
  begin
    FLastError := 1;
    FLastErrorMessage := 'cannot resolve host "' + FServerAddress + '"';
  end;
end;

// connect in non blocking mode and wait at most FTimeOut ms for the result,
// a plain blocking connect can hang for a minute on a host that never answers
function TRedisLib.ConnectWithTimeout(const AAddr: TRedisSockAddr): boolean;
var
  rc, soError: longint;
  optLen: TSockLen;
  tv: TTimeVal;
  writeSet, errorSet: TFDSet;
  endpoint: string;
  {$ifdef windows}
  nonBlocking: u_long;
  {$else}
  flags: longint;
  {$endif}
begin
  Result := False;
  endpoint := EndpointText(AAddr, FPort);
  if FTimeOut <= 0 then
  begin
    Result := fpConnect(FSocket, @AAddr.v4, AAddr.Len) = 0;
    if not Result then
      SetSocketError('cannot connect to ' + endpoint);
    Exit;
  end;

  {$ifdef windows}
  nonBlocking := 1;
  if WinSock2.ioctlsocket(FSocket, longint(FIONBIO), nonBlocking) <> 0 then
  {$else}
  flags := FpFcntl(FSocket, F_GETFL);
  if (flags = -1) or (FpFcntl(FSocket, F_SETFL, flags or O_NONBLOCK) = -1) then
  {$endif}
  begin
    SetSocketError('cannot set the socket to non blocking mode');
    Exit;
  end;

  if fpConnect(FSocket, @AAddr.v4, AAddr.Len) <> 0 then
  begin
    rc := SocketError;
    {$ifdef windows}
    if rc <> WSAEWOULDBLOCK then
    {$else}
    if rc <> ESysEINPROGRESS then
    {$endif}
    begin
      FLastError := rc;
      FLastErrorMessage := 'cannot connect to ' + endpoint + ' (error ' + IntToStr(rc) + ')';
      Exit;
    end;

    tv.tv_sec := FTimeOut div 1000;
    tv.tv_usec := (FTimeOut mod 1000) * 1000;
    {$ifdef windows}
    WinSock2.FD_ZERO(writeSet);
    WinSock2.FD_SET(FSocket, writeSet);
    WinSock2.FD_ZERO(errorSet);
    WinSock2.FD_SET(FSocket, errorSet);
    rc := WinSock2.select(0, nil, @writeSet, @errorSet, @tv);
    {$else}
    fpFD_ZERO(writeSet);
    fpFD_SET(FSocket, writeSet);
    fpFD_ZERO(errorSet);
    fpFD_SET(FSocket, errorSet);
    rc := fpSelect(FSocket + 1, nil, @writeSet, @errorSet, @tv);
    {$endif}
    if rc = 0 then
    begin
      {$ifdef windows}
      FLastError := WSAETIMEDOUT;
      {$else}
      FLastError := ESysETIMEDOUT;
      {$endif}
      FLastErrorMessage := 'connection to ' + endpoint + ' timed out after ' +
        IntToStr(FTimeOut) + ' ms';
      Exit;
    end;
    if rc < 0 then
    begin
      SetSocketError('select failed while connecting to ' + endpoint);
      Exit;
    end;

    // the socket is writable or in error, SO_ERROR holds the outcome
    soError := 0;
    optLen := SizeOf(soError);
    if fpgetsockopt(FSocket, SOL_SOCKET, SO_ERROR, @soError, @optLen) <> 0 then
    begin
      SetSocketError('cannot read the connect result for ' + endpoint);
      Exit;
    end;
    if soError <> 0 then
    begin
      FLastError := soError;
      FLastErrorMessage := 'cannot connect to ' + endpoint + ' (error ' + IntToStr(soError) + ')';
      Exit;
    end;
  end;

  // back to blocking mode, SO_RCVTIMEO/SO_SNDTIMEO handle the timeouts from here
  {$ifdef windows}
  nonBlocking := 0;
  if WinSock2.ioctlsocket(FSocket, longint(FIONBIO), nonBlocking) <> 0 then
  {$else}
  if FpFcntl(FSocket, F_SETFL, flags) = -1 then
  {$endif}
  begin
    SetSocketError('cannot set the socket back to blocking mode');
    Exit;
  end;
  Result := True;
end;

procedure TRedisLib.ApplySocketTimeout;
var
  {$ifdef windows}
  tv: longword; // milliseconds
  {$else}
  tv: TTimeVal;
  {$endif}
  {$ifdef darwin}
  one: longint;
  {$endif}
begin
  if FSocket = -1 then
    Exit;
  {$ifdef darwin}
  one := 1;
  fpsetsockopt(FSocket, SOL_SOCKET, SO_NOSIGPIPE, @one, SizeOf(one));
  {$endif}
  if FTimeOut <= 0 then
    Exit;
  {$ifdef windows}
  tv := FTimeOut;
  {$else}
  tv.tv_sec := FTimeOut div 1000;
  tv.tv_usec := (FTimeOut mod 1000) * 1000;
  {$endif}
  fpsetsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @tv, SizeOf(tv));
  fpsetsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @tv, SizeOf(tv));
end;
{$endif}

procedure TRedisLib.SetSocketError(const AMessage: string);
begin
  {$ifdef synapse}
  FLastError := sock.LastError;
  if sock.LastErrorDesc <> '' then
  begin
    FLastErrorMessage := AMessage + ': ' + sock.LastErrorDesc;
    if FLastError = 0 then
      FLastError := 1;
    Exit;
  end;
  {$endif}
  {$ifdef fpcsocket}
  FLastError := SocketError;
  {$endif}
  if FLastError = 0 then
    FLastError := 1;
  FLastErrorMessage := AMessage + ' (error ' + IntToStr(FLastError) + ')';
end;

function TRedisLib.Connect: boolean;
{$ifdef fpcsocket}
var
  addrs: TRedisSockAddrList;
  i: integer;
{$endif}
begin
  Result := FisConnected;
  FLastMessage := '';
  FLastError := 0;
  FLastErrorMessage := '';
  if FisConnected then
    exit;
  FRecvBuffer := '';
  FResolvedAddress := '';
  try
    {$ifdef synapse}
    sock.SocksTimeout := FTimeOut;
    sock.Connect(FServerAddress, FPort);
    if sock.LastError = 0 then
    begin
      FisConnected := True;
      Result := True;
    end
    else
      SetSocketError('cannot connect to ' + FServerAddress + ':' + FPort);
    {$endif}

    {$ifdef fpcsocket}
    if not ResolveAddresses(addrs) then
      Exit;
    // try every candidate, the error of the last failed one is kept
    for i := 0 to High(addrs) do
    begin
      FSocket := fpSocket(addrs[i].Family, SOCK_STREAM, 0);
      if FSocket = -1 then
      begin
        SetSocketError('cannot create a socket for ' + EndpointText(addrs[i], FPort));
        Continue;
      end;
      if ConnectWithTimeout(addrs[i]) then
      begin
        FResolvedAddress := addrs[i].Text;
        Break;
      end;
      CloseSocket(FSocket);
      FSocket := -1;
    end;
    if FSocket = -1 then
      Exit;
    FLastError := 0;
    FLastErrorMessage := '';
    ApplySocketTimeout;
    FisConnected := True;
    Result := True;
    {$endif}
  except
    on e: Exception do
    begin
      FLastError := 1;
      FLastErrorMessage := e.Message;
      Disconnect;
    end;
  end;
end;

procedure TRedisLib.Disconnect;
begin
  {$ifdef synapse}
  if FisConnected then
    sock.CloseSocket;
  {$endif}
  {$ifdef fpcsocket}
  if FSocket <> -1 then
  begin
    CloseSocket(FSocket);
    FSocket := -1;
  end;
  {$endif}
  FisConnected := False;
  FRecvBuffer := '';
end;

function TRedisLib.SocketSend(const AData: RawByteString): boolean;
{$ifdef fpcsocket}
var
  sent, total: integer;
  n: PtrInt;
{$endif}
begin
  Result := False;
  if AData = '' then
    Exit(True);

  {$ifdef synapse}
  sock.SendString(AData);
  if sock.LastError <> 0 then
  begin
    SetSocketError('send failed');
    Disconnect;
    Exit;
  end;
  Result := True;
  {$endif}

  {$ifdef fpcsocket}
  total := Length(AData);
  sent := 0;
  while sent < total do
  begin
    n := fpSend(FSocket, @AData[sent + 1], total - sent, REDIS_SEND_FLAGS);
    if n <= 0 then
    begin
      SetSocketError('send failed');
      Disconnect;
      Exit;
    end;
    Inc(sent, n);
  end;
  Result := True;
  {$endif}
end;

function TRedisLib.SocketRecv(out AData: RawByteString): boolean;
var
  n: PtrInt;
begin
  Result := False;
  AData := '';
  SetLength(AData, REDIS_RECV_CHUNK_SIZE);
  SetCodePage(AData, CP_NONE, False);

  {$ifdef synapse}
  // whatever is waiting on the socket, or an error after FTimeOut
  AData := sock.RecvPacket(FTimeOut);
  if sock.LastError <> 0 then
  begin
    SetSocketError('receive failed');
    Disconnect;
    AData := '';
    Exit;
  end;
  n := Length(AData);
  if n > 0 then
    SetCodePage(AData, CP_NONE, False);
  Result := n > 0;
  {$endif}

  {$ifdef fpcsocket}
  n := fpRecv(FSocket, @AData[1], Length(AData), 0);
  if n <= 0 then
  begin
    if n = 0 then
    begin
      FLastError := 1;
      FLastErrorMessage := 'connection closed by server';
    end
    else
      SetSocketError('receive failed or timed out');
    Disconnect;
    AData := '';
    Exit;
  end;
  SetLength(AData, n);
  Result := True;
  {$endif}
end;

// read one CRLF terminated line from the receive buffer, without the CRLF
function TRedisLib.ReadLine(out ALine: RawByteString): boolean;
var
  p, scanFrom: SizeInt;
  chunk: RawByteString;
begin
  Result := False;
  ALine := '';
  scanFrom := 1;
  repeat
    p := FindCRLF(FRecvBuffer, scanFrom);
    if p > 0 then
    begin
      ALine := Copy(FRecvBuffer, 1, p - 1);
      Delete(FRecvBuffer, 1, p + 1);
      Exit(True);
    end;
    // the CR may already be the last byte of the buffer, rescan from there
    scanFrom := Length(FRecvBuffer);
    if not SocketRecv(chunk) then
      Exit;
    AppendBytes(FRecvBuffer, chunk);
  until False;
end;

function TRedisLib.ReadBytes(ACount: integer; out AData: RawByteString): boolean;
var
  chunk: RawByteString;
begin
  Result := False;
  AData := '';
  while Length(FRecvBuffer) < ACount do
  begin
    if not SocketRecv(chunk) then
      Exit;
    AppendBytes(FRecvBuffer, chunk);
  end;
  AData := Copy(FRecvBuffer, 1, ACount);
  Delete(FRecvBuffer, 1, ACount);
  Result := True;
end;

// read one complete RESP reply; bulk and multi bulk payloads are appended
// to the header line separated by CRLF, e.g. "$7"#13#10"example"
function TRedisLib.ReadReply(out AReply: RawByteString): boolean;
var
  item: RawByteString;
  len, i: integer;
begin
  Result := ReadLine(AReply);
  if (not Result) or (AReply = '') then
    Exit;
  case AReply[1] of
    __REDIS_RESPONSE_BULK_CHAR:
    begin
      len := StrToIntDef(Copy(AReply, 2, Length(AReply) - 1), -1);
      if len >= 0 then
      begin
        Result := ReadBytes(len + 2, item); // payload + CRLF
        if Result then
        begin
          SetLength(item, len);
          AppendBytes(AReply, #13#10);
          AppendBytes(AReply, item);
        end;
      end;
    end;
    __REDIS_RESPONSE_MULTI_BULK_CHAR:
    begin
      len := StrToIntDef(Copy(AReply, 2, Length(AReply) - 1), -1);
      for i := 1 to len do
      begin
        Result := ReadReply(item);
        if not Result then
          Exit;
        AppendBytes(AReply, #13#10);
        AppendBytes(AReply, item);
      end;
    end;
  end;
end;

function TRedisLib.BulkPayload(const AReply: string): string;
var
  p: integer;
begin
  Result := '';
  if (AReply = '') or (AReply[1] <> __REDIS_RESPONSE_BULK_CHAR) then
    Exit;
  p := Pos(#13#10, AReply);
  if p = 0 then
    Exit;
  Result := Copy(AReply, p + 2, Length(AReply) - p - 1);
end;

function TRedisLib.Execute(const ARequest: RawByteString): string;
var
  reply: RawByteString;
begin
  Result := '';
  FLastError := 0;
  FLastErrorMessage := '';
  FResponType := rrtError;
  FLastMessage := '';
  if not FisConnected then
  begin
    if not FisAutoConnect then
    begin
      FLastError := 1;
      FLastErrorMessage := 'not connected';
      Exit;
    end;
    if not Connect then
      Exit;
  end;
  try
    if not SocketSend(ARequest) then
      Exit;
    if not ReadReply(reply) then
      Exit;
    // hand the bytes over unchanged, tagged with the default code page so the
    // caller can compare them with ordinary string literals
    SetCodePage(reply, DefaultSystemCodePage, False);
    FLastMessage := reply;
    FResponType := GetResponseType(FLastMessage);
    if FResponType = rrtError then
      FLastErrorMessage := Copy(FLastMessage, 2, Length(FLastMessage) - 1);
  except
    on e: Exception do
    begin
      FLastError := 1;
      FLastErrorMessage := e.Message;
      FLastMessage := '';
      Disconnect;
    end;
  end;
  Result := FLastMessage;
end;

function TRedisLib.Ping: boolean;
begin
  Result := SendString('PING') = '+PONG';
end;

procedure TRedisLib.FlushAll;
begin
  SendString('FLUSHALL');
end;

procedure TRedisLib.FlushDB;
begin
  SendString('FLUSHDB');
end;

function TRedisLib.Info: string;
begin
  Result := SendString('INFO');
  if FResponType = rrtBulk then
    Result := BulkPayload(Result);
end;

procedure TRedisLib.Synch;
begin
  FLastError := 1;
  FLastErrorMessage := 'Synch is deprecated: SYNCH is not a Redis command, nothing was sent';
  FLastMessage := '';
  FResponType := rrtError;
end;

function TRedisLib.SendString(Strings: string): string;
var
  request: RawByteString;
begin
  request := '';
  AppendBytes(request, Strings);
  AppendBytes(request, #13#10);
  Result := Execute(request);
end;

function TRedisLib.SendCommand(const Args: array of string): string;
var
  i: integer;
  request: RawByteString;
begin
  Result := '';
  if Length(Args) = 0 then
    Exit;
  request := '';
  AppendBytes(request, __REDIS_RESPONSE_MULTI_BULK_CHAR + IntToStr(Length(Args)) + #13#10);
  for i := 0 to High(Args) do
  begin
    // Length() is in bytes, the argument is copied as is whatever its code page
    AppendBytes(request, __REDIS_RESPONSE_BULK_CHAR + IntToStr(Length(Args[i])) + #13#10);
    AppendBytes(request, Args[i]);
    AppendBytes(request, #13#10);
  end;
  Result := Execute(request);
end;

function TRedisLib.Auth(PasswordKey: string): boolean;
begin
  if PasswordKey <> '' then
    FPassword := PasswordKey;
  Result := SendCommand(['AUTH', FPassword]) = '+OK';
end;


end.

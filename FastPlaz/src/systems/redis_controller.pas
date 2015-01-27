{

Simple Redis Client
* for set and get data only.
- non-thread
- master-slave support

by: Luri Darmawan ( http://www.fastplaz.com )
requirement:
  - synapse

USAGE:
[x] set value
Redis := TRedisConstroller.Create;
Redis['thekey'] := 'your string';

[x] get value
variable := Redis['thekey'];

USE MASTER SLAVE
Redis := TRedisConstroller.Create;
Redis.WriteToMaster := true;
Redis.MasterServerAddress := 'your.ip.address';
Redis.MasterPort: = '6378';

**********************************************************************}
unit redis_controller;

{$mode objfpc}{$H+}
{$include ../../define.inc}

interface

{$ifndef synapse}
  {$define fpcsocket}
{$endif}

uses
  Sockets,
  {$ifdef fpcsocket}
  {$endif}
  {$ifdef synapse}
  blcksock,
  {$endif}
  {$ifndef win32} cthreads, {$endif}
  Classes, SysUtils;

const
  REDIS_DEFAULT_SERVER = '127.0.0.1';
  REDIS_DEFAULT_PORT = 6379;
  REDIS_DEFAULT_TIMEOUT = 2500;
  __REDIS_RESPONSE_SINGLE_CHAR = '+'; // "+OK"
  __REDIS_RESPONSE_ERROR_CHAR = '-'; // error
  __REDIS_RESPONSE_INT_CHAR = ':'; //  ":100\r\n"
  __REDIS_RESPONSE_BULK_CHAR = '$'; // "$7\r\nexample\r\n"
  __REDIS_RESPONSE_MULTI_BULK_CHAR = '*'; // multiple bulk value

  _REDIS_SERVERADDRESS = 'redis/%s/server';
  _REDIS_PORT = 'redis/%s/port';
  _REDIS_TIMEOUT = 'redis/%s/timeout';

type

  TRedisResponseType = (rrtStatus, rrtError,
    rrtNumeric, rrtBulk,
    rrtMultiBulk, rrtUnknown,
    rrtNull);

  { TRedisConstroller }

  TRedisConstroller = class(TObject)
  private
    FServerAddress: string;
    FisAutoConnect: boolean;
    FLastError: integer;
    FLastMessage: string;
    FPassword: string;
    FPort: string;
    FPortInt: integer;
    FisConnected: boolean;
    FResponType: TRedisResponseType;
    FTimeOut: integer;
    FWriteToMaster: boolean;

    {$ifdef synapse}
    sock: TTCPBlockSocket;
    {$endif}

    {$ifdef fpcsocket}
    sock: TInetSockAddr;
    sockInt: longint;
    SocketIn, SocketOut: Text;
    {$endif}
    IpAddress: in_addr;

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
    procedure SetValue(Key: string; AValue: string);
    procedure SetWriteToMaster(AValue: boolean);
  public
    constructor Create(const ConfigName: string = 'default');
    destructor Destroy; override;

    property isConnected: boolean read FisConnected;
    property isAutoConnect: boolean read FisAutoConnect write FisAutoConnect;
    property ServerAddress: string read FServerAddress write SetServerAddress;
    property Port: string read FPort write SetPort;
    property TimeOut: integer read FTimeOut write FTimeOut;
    property LastError: integer read FLastError;
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
    function SendString(Strings: string): string;

    function Auth(PasswordKey: string = ''): boolean;
    function Ping: boolean;
    procedure FlushAll;
    procedure FlushDB;
    function Info: string;
    procedure Synch;

  end;

implementation

{ TRedisConstroller }
var
  RedisMaster: TRedisConstroller;

procedure TRedisConstroller.SetServerAddress(AValue: string);
begin
  if FServerAddress = AValue then
    Exit;
  FServerAddress := AValue;
  IpAddress := StrToNetAddr(AValue);
  if IpAddress.s_addr = 0 then
  begin
    FServerAddress := REDIS_DEFAULT_SERVER;
    IpAddress := StrToNetAddr(FServerAddress);
  end;
end;

procedure TRedisConstroller.SetPort(AValue: string);
begin
  if FPort = AValue then
    Exit;
  try
    FPortInt := StrToInt(AValue);
    FPort := AValue;
  except
    FPort := IntToStr(REDIS_DEFAULT_PORT);
    FPortInt := REDIS_DEFAULT_PORT;
  end;
end;

function TRedisConstroller.Get(Key: string): string;
var
  i: integer;
begin
  SendString('GET ' + Key);
  i := Pos(#13#10, FLastMessage);
  FLastMessage := Copy(FLastMessage, i + 2, Length(FLastMessage) - i - 1);
  Result := FLastMessage;
end;

function TRedisConstroller.GetMasterPort: string;
begin
  if FWriteToMaster then
    Result := RedisMaster.Port;
end;

function TRedisConstroller.GetMasterServerAddress: string;
begin
  if FWriteToMaster then
    Result := RedisMaster.ServerAddress;
end;

function TRedisConstroller.GetMasterTimeOut: integer;
begin
  if FWriteToMaster then
    Result := RedisMaster.TimeOut;
end;

function TRedisConstroller.GetResponseType(Response: string): TRedisResponseType;
var
  c: char;
begin
  if Response = '' then
  begin
    Result := rrtUnknown;
    Exit;
  end;
  c := copy(Response, 1, 1)[1];
  case c of
    __REDIS_RESPONSE_SINGLE_CHAR: Result := rrtStatus;
    __REDIS_RESPONSE_ERROR_CHAR: Result := rrtError;
    __REDIS_RESPONSE_BULK_CHAR: Result := rrtBulk;
    __REDIS_RESPONSE_MULTI_BULK_CHAR: Result := rrtMultiBulk;
    __REDIS_RESPONSE_INT_CHAR: Result := rrtNumeric;
    else
      Result := rrtUnknown;
  end;
end;

procedure TRedisConstroller.SetMasterPort(AValue: string);
begin
  if FWriteToMaster then
    RedisMaster.Port := AValue;
end;

procedure TRedisConstroller.SetMasterServerAddress(AValue: string);
begin
  if FWriteToMaster then
    RedisMaster.ServerAddress := AValue;
end;

procedure TRedisConstroller.SetMasterTimeOut(AValue: integer);
begin
  if FWriteToMaster then
    RedisMaster.TimeOut := AValue;
end;

procedure TRedisConstroller.SetValue(Key: string; AValue: string);
begin
  if FWriteToMaster then
  begin
    RedisMaster.SendString('SET ' + Key + ' "' + AValue + '"');
  end
  else
    SendString('SET ' + Key + ' "' + AValue + '"');
end;

procedure TRedisConstroller.SetWriteToMaster(AValue: boolean);
begin
  if FWriteToMaster = AValue then
    Exit;
  FWriteToMaster := AValue;
  if FWriteToMaster then
  begin
    RedisMaster := TRedisConstroller.Create();
  end;
end;

constructor TRedisConstroller.Create(const ConfigName: string);
begin
  FisConnected := False;
  FisAutoConnect := True;
  FPort := IntToStr(REDIS_DEFAULT_PORT);
  FPortInt := REDIS_DEFAULT_PORT;
  FTimeOut := REDIS_DEFAULT_TIMEOUT;
  ServerAddress := REDIS_DEFAULT_SERVER;
  {$ifdef synapse}
  sock := TTCPBlockSocket.Create;
  {$endif}

  {$ifdef FASTPLAZ}
  {
  ServerAddress := string(Config.GetValue(format(_REDIS_SERVERADDRESS, [ConfigName]), '127.0.0.2'));
  FPort := string(Config.GetValue(format(_REDIS_PORT, [ConfigName]), '6379'));
  FPortInt := StrToInt(FPort);
  FTimeOut := (Config.GetValue(format(_REDIS_TIMEOUT, [ConfigName]), REDIS_DEFAULT_TIMEOUT));
  }
  {$endif}
end;


destructor TRedisConstroller.Destroy;
begin
  if FisConnected then
  begin
    {$ifdef synapse}
    sock.CloseSocket;
    {$endif}
    {$ifdef fpcsocket}
    Close(SocketOut);
    {$endif}
  end;
  {$ifdef synapse}
  FreeAndNil(sock);
  {$endif}
  if FWriteToMaster then
  begin
    FreeAndNil(RedisMaster);
  end;
  inherited Destroy;
end;

function TRedisConstroller.Connect: boolean;
begin
  Result := FisConnected;
  FLastMessage := '';
  FLastError := 1;
  if FisConnected then
    exit;
  try
    {$ifdef synapse}
    sock.ConnectionTimeout := FTimeOut;
    sock.Connect(FServerAddress, FPort);
    FLastError := sock.LastError;
    if sock.LastError = 0 then
    begin
      FisConnected := True;
      Result := True;
    end;
    {$endif}
    {$ifdef fpcsocket}
    sockInt := fpSocket(AF_INET, SOCK_STREAM, 0);
    if sockInt = -1 then
      Exit;
    sock.sin_family := AF_INET;
    sock.sin_port := htons(FPortInt);
    sock.sin_addr.s_addr := IpAddress.s_addr;
    if Sockets.Connect(sockInt, sock, SocketIn, SocketOut) then
    begin
      FisConnected := True;
      Result := True;
      FLastError := 0;
    end;
    {$endif}
  except
    on e: Exception do
      FLastMessage := e.Message;
  end;
end;

function TRedisConstroller.Ping: boolean;
var
  s: string;
begin
  Result := False;
  s := SendString('PING');
  if s = '+PONG' then
    Result := True;
end;

procedure TRedisConstroller.FlushAll;
begin
  SendString('FLUSHALL');
end;

procedure TRedisConstroller.FlushDB;
begin
  SendString('FLUSHDB');
end;

function TRedisConstroller.Info: string;
begin
  Result := SendString('INFO');
end;

procedure TRedisConstroller.Synch;
begin
  SendString('SYNCH');
end;

function TRedisConstroller.SendString(Strings: string): string;
var
  s: string;
begin
  FLastError := 0;
  FResponType := rrtError;
  FLastMessage := '';
  if not FisConnected then
  begin
    if not FisAutoConnect then
      Exit;
    if not Connect then
      Exit;
  end;
  try

    {$ifdef synapse}
    sock.SendString(Strings + #13#10);
    FLastError := sock.LastError;
    if FLastError <> 0 then
      Exit;
    FLastMessage := trim(sock.RecvPacket(FTimeOut));
    FLastError := sock.LastError;
    {$endif}

    {$ifdef fpcsocket}
    Reset(SocketIn);
    ReWrite(SocketOut);
    WriteLn(SocketOut, Strings);
    Flush(SocketOut);
    ReadLn(SocketIn, FLastMessage);
    if copy(FLastMessage, 1, 1)[1] = '$' then
    begin
      s := Copy(FLastMessage, 2, 5);
      if StrToInt(s) <> -1 then
      begin
        ReadLn(SocketIn, s);
        FLastMessage := FLastMessage + #13#10 + s;
      end;
    end;
    FLastError := 0;
    {$endif}

    if FLastError = 0 then
    begin
      FResponType := GetResponseType(FLastMessage);
    end;
  except
    on e: Exception do
      FLastMessage := e.Message;
  end;
  Result := FLastMessage;
end;

function TRedisConstroller.Auth(PasswordKey: string): boolean;
begin
  Result := False;
  if PasswordKey <> '' then
    FPassword := PasswordKey;

  if SendString('AUTH ' + FPassword) = '+OK' then
    Result := True;
end;


end.

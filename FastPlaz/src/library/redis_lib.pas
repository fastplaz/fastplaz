{

Simple Redis Client Library
* for set and get data only.
- non-thread
- master-slave support

by: Luri Darmawan ( http://www.fastplaz.com )
requirement:
  - synapse (if any)

USAGE:
[x] set value
Redis := TRedisLib.Create;
Redis['thekey'] := 'your string';

[x] get value
variable := Redis['thekey'];

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
  Sockets,
  {$ifdef fpcsocket}
  {$endif}
  {$ifdef synapse}
  blcksock,
  {$endif}
  {$IFNDEF Windows} cthreads, {$ENDIF}
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

type

  TRedisResponseType = (rrtStatus, rrtError,
    rrtNumeric, rrtBulk,
    rrtMultiBulk, rrtUnknown,
    rrtNull);

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
    function SendString(Strings: string): string;

    function Auth(PasswordKey: string = ''): boolean;
    function Ping: boolean;
    procedure FlushAll;
    procedure FlushDB;
    function Info: string;
    procedure Synch;

  end;

implementation

{ TRedisLib }
var
  RedisMaster: TRedisLib;

procedure TRedisLib.SetServerAddress(AValue: string);
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

procedure TRedisLib.SetPort(AValue: string);
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

function TRedisLib.Get(Key: string): string;
var
  i: integer;
begin
  if Key.IsEmpty then
    Exit;
  SendString('GET ' + Key);
  i := Pos(#13#10, FLastMessage);
  FLastMessage := Copy(FLastMessage, i + 2, Length(FLastMessage) - i - 1);
  Result := FLastMessage;
end;

function TRedisLib.GetMasterPort: string;
begin
  if FWriteToMaster then
    Result := RedisMaster.Port;
end;

function TRedisLib.GetMasterServerAddress: string;
begin
  if FWriteToMaster then
    Result := RedisMaster.ServerAddress;
end;

function TRedisLib.GetMasterTimeOut: integer;
begin
  if FWriteToMaster then
    Result := RedisMaster.TimeOut;
end;

function TRedisLib.GetResponseType(Response: string): TRedisResponseType;
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

procedure TRedisLib.SetMasterPort(AValue: string);
begin
  if FWriteToMaster then
    RedisMaster.Port := AValue;
end;

procedure TRedisLib.SetMasterServerAddress(AValue: string);
begin
  if FWriteToMaster then
    RedisMaster.ServerAddress := AValue;
end;

procedure TRedisLib.SetMasterTimeOut(AValue: integer);
begin
  if FWriteToMaster then
    RedisMaster.TimeOut := AValue;
end;

procedure TRedisLib.SetValue(Key: string; AValue: string);
begin
  if Key.IsEmpty then
    Exit;
  FLastMessage := '+OK';
  if FWriteToMaster then
  begin
    RedisMaster.SendString('SET ' + Key + ' ''' + AValue + '''');
  end
  else
    SendString('SET ' + Key + ' ''' + AValue + '''');
end;

procedure TRedisLib.SetWriteToMaster(AValue: boolean);
begin
  if FWriteToMaster = AValue then
    Exit;
  FWriteToMaster := AValue;
  if FWriteToMaster then
  begin
    RedisMaster := TRedisLib.Create();
  end;
end;

constructor TRedisLib.Create(const ConfigName: string);
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
end;


destructor TRedisLib.Destroy;
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

function TRedisLib.Connect: boolean;
begin
  Result := FisConnected;
  FLastMessage := '';
  FLastError := 1;
  if FisConnected then
    exit;
  try
    {$ifdef synapse}
    sock.SocksTimeout := FTimeOut;
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

function TRedisLib.Ping: boolean;
var
  s: string;
begin
  Result := False;
  s := SendString('PING');
  if s = '+PONG' then
    Result := True;
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
end;

procedure TRedisLib.Synch;
begin
  SendString('SYNCH');
end;

function TRedisLib.SendString(Strings: string): string;
var
  s: string;
begin
  FLastError := 0;
  FLastErrorMessage := '';
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
    end else
    begin
      FLastErrorMessage := FLastMessage;
      FLastMessage := '';
    end;
  except
    on e: Exception do
    begin
      FLastErrorMessage := e.Message;
    end;
  end;
  Result := FLastMessage;
end;

function TRedisLib.Auth(PasswordKey: string): boolean;
begin
  Result := False;
  if PasswordKey <> '' then
    FPassword := PasswordKey;

  if SendString('AUTH ' + FPassword) = '+OK' then
    Result := True;
end;


end.


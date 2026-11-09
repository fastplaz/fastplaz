unit test_redis_lib;

{
  Tests for redis_lib (TRedisLib).

  TRedisLibOfflineTest needs no server.
  TRedisLibServerTest talks to a real Redis server and is ignored (not failed)
  when no server is reachable. The server defaults to 127.0.0.1:6379 and can
  be changed with the environment variables
    FASTPLAZ_TEST_REDIS_HOST, FASTPLAZ_TEST_REDIS_PORT, FASTPLAZ_TEST_REDIS_PASSWORD
  Every key written by the tests starts with "fastplaz:test:" and is deleted
  again in TearDown; the database is never flushed.
}

{$mode objfpc}{$H+}

interface

uses
  redis_lib, Classes, SysUtils, DateUtils, fpcunit, testregistry;

const
  REDIS_TEST_ENV_HOST = 'FASTPLAZ_TEST_REDIS_HOST';
  REDIS_TEST_ENV_PORT = 'FASTPLAZ_TEST_REDIS_PORT';
  REDIS_TEST_ENV_PASSWORD = 'FASTPLAZ_TEST_REDIS_PASSWORD';
  REDIS_TEST_KEY_PREFIX = 'fastplaz:test:';

type

  { TRedisLibOfflineTest }

  TRedisLibOfflineTest = class(TTestCase)
  published
    procedure TestDefaults;
    procedure TestInvalidPortFallsBack;
    procedure TestEmptyServerAddressFallsBack;
    procedure TestBulkPayload;
    procedure TestMasterIsOptional;
    procedure TestConnectRefused;
    procedure TestConnectTimeout;
    procedure TestIPv6LiteralForms;
    procedure TestIPv6Unreachable;
    procedure TestSendWithoutAutoConnect;
    procedure TestUnresolvableHost;
    procedure TestSynchSendsNothing;
  end;

  { TRedisLibServerTest }

  TRedisLibServerTest = class(TTestCase)
  private
    FRedis: TRedisLib;
    FKeys: TStringList;
    function Key(const AName: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPing;
    procedure TestSetGet;
    procedure TestSpecialCharacters;
    procedure TestCodePageTaggedValue;
    procedure TestEmptyValue;
    procedure TestMissingKeyIsNull;
    procedure TestLargeValue;
    procedure TestInfo;
    procedure TestNumericReply;
    procedure TestMultiBulkReply;
    procedure TestHashCommand;
    procedure TestErrorReply;
    procedure TestInlineCommand;
    procedure TestReadTimeoutAndReconnect;
    procedure TestDisconnect;
    procedure TestConnectByHostName;
    procedure TestConnectIPv6Loopback;
    procedure TestWriteToMaster;
  end;

function RedisTestHost: string;
function RedisTestPort: string;
function RedisTestPassword: string;
function NewRedisTestClient: TRedisLib;

implementation

var
  ServerChecked: boolean = False;
  ServerAvailable: boolean = False;
  ServerMessage: string = '';

function RedisTestHost: string;
begin
  Result := Trim(GetEnvironmentVariable(REDIS_TEST_ENV_HOST));
  if Result = '' then
    Result := REDIS_DEFAULT_SERVER;
end;

function RedisTestPort: string;
begin
  Result := Trim(GetEnvironmentVariable(REDIS_TEST_ENV_PORT));
  if Result = '' then
    Result := IntToStr(REDIS_DEFAULT_PORT);
end;

function RedisTestPassword: string;
begin
  Result := GetEnvironmentVariable(REDIS_TEST_ENV_PASSWORD);
end;

// a connected (and authenticated) client, or nil with the reason in ServerMessage
function NewRedisTestClient: TRedisLib;
begin
  Result := TRedisLib.Create;
  Result.ServerAddress := RedisTestHost;
  Result.Port := RedisTestPort;
  if not Result.Connect then
  begin
    ServerMessage := Format('no redis server at %s:%s (%s)',
      [RedisTestHost, RedisTestPort, Result.LastErrorMessage]);
    FreeAndNil(Result);
    Exit;
  end;
  if RedisTestPassword <> '' then
    if not Result.Auth(RedisTestPassword) then
    begin
      ServerMessage := Format('redis AUTH failed at %s:%s (%s)',
        [RedisTestHost, RedisTestPort, Result.LastErrorMessage]);
      FreeAndNil(Result);
      Exit;
    end;
  if not Result.Ping then
  begin
    ServerMessage := Format('redis at %s:%s did not answer PING (%s)',
      [RedisTestHost, RedisTestPort, Result.LastErrorMessage]);
    FreeAndNil(Result);
  end;
end;

{ TRedisLibOfflineTest }

procedure TRedisLibOfflineTest.TestDefaults;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    AssertEquals(REDIS_DEFAULT_SERVER, R.ServerAddress);
    AssertEquals(IntToStr(REDIS_DEFAULT_PORT), R.Port);
    AssertEquals(REDIS_DEFAULT_TIMEOUT, R.TimeOut);
    AssertTrue('auto connect is on by default', R.isAutoConnect);
    AssertFalse('not connected after Create', R.isConnected);
    AssertFalse('WriteToMaster is off by default', R.WriteToMaster);
    AssertEquals('no resolved address before Connect', '', R.ResolvedAddress);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestInvalidPortFallsBack;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    R.Port := '6380';
    AssertEquals('6380', R.Port);
    R.Port := 'abc';
    AssertEquals('non numeric port', IntToStr(REDIS_DEFAULT_PORT), R.Port);
    R.Port := '70000';
    AssertEquals('port out of range', IntToStr(REDIS_DEFAULT_PORT), R.Port);
    R.Port := '0';
    AssertEquals('port zero', IntToStr(REDIS_DEFAULT_PORT), R.Port);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestEmptyServerAddressFallsBack;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    R.ServerAddress := 'redis.example.test';
    AssertEquals('host names are kept', 'redis.example.test', R.ServerAddress);
    R.ServerAddress := '   ';
    AssertEquals(REDIS_DEFAULT_SERVER, R.ServerAddress);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestBulkPayload;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    AssertEquals('example', R.BulkPayload('$7'#13#10'example'));
    AssertEquals('payload with CRLF', 'a'#13#10'b', R.BulkPayload('$4'#13#10'a'#13#10'b'));
    AssertEquals('empty bulk', '', R.BulkPayload('$0'#13#10));
    AssertEquals('null bulk', '', R.BulkPayload('$-1'));
    AssertEquals('status is not bulk', '', R.BulkPayload('+OK'));
    AssertEquals('empty reply', '', R.BulkPayload(''));
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestMasterIsOptional;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    AssertEquals('no master address without WriteToMaster', '', R.MasterServerAddress);
    AssertEquals('no master port without WriteToMaster', '', R.MasterPort);
    R.MasterServerAddress := '10.0.0.1';
    AssertEquals('master setter ignored without WriteToMaster', '', R.MasterServerAddress);

    R.WriteToMaster := True;
    R.MasterServerAddress := '10.0.0.1';
    R.MasterPort := '6380';
    R.MasterTimeOut := 500;
    AssertEquals('10.0.0.1', R.MasterServerAddress);
    AssertEquals('6380', R.MasterPort);
    AssertEquals(500, R.MasterTimeOut);

    R.WriteToMaster := False;
    AssertEquals('master released', '', R.MasterServerAddress);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestConnectRefused;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    R.ServerAddress := '127.0.0.1';
    R.Port := '1'; // tcpmux, nothing listens there
    R.TimeOut := 1000;
    AssertFalse('connect must fail', R.Connect);
    AssertFalse('not connected', R.isConnected);
    AssertTrue('LastError is set', R.LastError <> 0);
    AssertTrue('LastErrorMessage names the server: ' + R.LastErrorMessage,
      Pos('127.0.0.1:1', R.LastErrorMessage) > 0);

    AssertEquals('auto connect fails again, no reply', '', R.SendString('PING'));
    AssertTrue('LastError set by SendString', R.LastError <> 0);
    AssertTrue(R.ResponType = rrtError);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestConnectTimeout;
var
  R: TRedisLib;
  t0: TDateTime;
  elapsed: int64;
begin
  R := TRedisLib.Create;
  try
    // 192.0.2.0/24 (TEST-NET-1, RFC 5737) is never routed, the SYN gets no
    // answer; a blocking connect would hang for the OS default (20 s and more)
    R.ServerAddress := '192.0.2.1';
    R.TimeOut := 1000;
    t0 := Now;
    AssertFalse('connect must fail', R.Connect);
    elapsed := MilliSecondsBetween(Now, t0);
    AssertFalse(R.isConnected);
    AssertTrue('LastError is set', R.LastError <> 0);
    AssertTrue('message: ' + R.LastErrorMessage, R.LastErrorMessage <> '');
    AssertTrue(Format('gave up within 4 s, took %d ms', [elapsed]), elapsed < 4000);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestIPv6LiteralForms;
var
  R: TRedisLib;
begin
  // nothing listens on port 1; the point is that every form is parsed as the
  // IPv6 loopback address and reported as "[::1]:1"
  R := TRedisLib.Create;
  try
    R.Port := '1';
    // Windows reports a refused loopback connect only after its SYN retries,
    // so each attempt runs into this timeout; keep it short
    R.TimeOut := 500;
    R.ServerAddress := '::1';
    AssertFalse('::1 must not connect on port 1', R.Connect);
    AssertTrue('::1 -> ' + R.LastErrorMessage, Pos('[::1]:1', R.LastErrorMessage) > 0);

    R.ServerAddress := '[::1]';
    AssertEquals('bracket form is kept as given', '[::1]', R.ServerAddress);
    AssertFalse(R.Connect);
    AssertTrue('[::1] -> ' + R.LastErrorMessage, Pos('[::1]:1', R.LastErrorMessage) > 0);

    R.ServerAddress := '0:0:0:0:0:0:0:1';
    AssertFalse(R.Connect);
    AssertTrue('long form -> ' + R.LastErrorMessage, Pos('[::1]:1', R.LastErrorMessage) > 0);

    R.ServerAddress := '2001:DB8:0:0:0:0:2:1';
    AssertFalse(R.Connect);
    AssertTrue('RFC 5952 text -> ' + R.LastErrorMessage, Pos('[2001:db8::2:1]:1', R.LastErrorMessage) > 0);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestIPv6Unreachable;
var
  R: TRedisLib;
  t0: TDateTime;
  elapsed: int64;
begin
  R := TRedisLib.Create;
  try
    // 2001:db8::/32 is the documentation prefix (RFC 3849), never routed
    R.ServerAddress := '2001:db8::1';
    R.TimeOut := 1000;
    t0 := Now;
    AssertFalse('connect must fail', R.Connect);
    elapsed := MilliSecondsBetween(Now, t0);
    AssertTrue('LastError is set', R.LastError <> 0);
    AssertTrue('message: ' + R.LastErrorMessage, Pos('[2001:db8::1]', R.LastErrorMessage) > 0);
    AssertTrue(Format('gave up within 4 s, took %d ms', [elapsed]), elapsed < 4000);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestSendWithoutAutoConnect;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    R.isAutoConnect := False;
    AssertEquals('', R.SendString('PING'));
    AssertFalse(R.isConnected);
    AssertTrue(R.LastError <> 0);
    AssertEquals('not connected', R.LastErrorMessage);
    AssertFalse('Ping is false when not connected', R.Ping);
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestUnresolvableHost;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    // ".invalid" is reserved by RFC 2606 and never resolves
    R.ServerAddress := 'no-such-host.invalid';
    R.TimeOut := 1000;
    AssertFalse('connect must fail', R.Connect);
    AssertTrue('LastError is set', R.LastError <> 0);
    AssertTrue('message: ' + R.LastErrorMessage, R.LastErrorMessage <> '');
  finally
    R.Free;
  end;
end;

procedure TRedisLibOfflineTest.TestSynchSendsNothing;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    R.ServerAddress := '127.0.0.1';
    R.Port := '1';
    {$push}{$warn 5043 off} // deprecated symbol, that is the point of the test
    R.Synch;
    {$pop}
    AssertFalse('no connection was opened', R.isConnected);
    AssertTrue(R.LastError <> 0);
    AssertTrue(R.ResponType = rrtError);
    AssertTrue('message mentions SYNCH: ' + R.LastErrorMessage, Pos('SYNCH', R.LastErrorMessage) > 0);
  finally
    R.Free;
  end;
end;

{ TRedisLibServerTest }

function TRedisLibServerTest.Key(const AName: string): string;
begin
  Result := REDIS_TEST_KEY_PREFIX + AName;
  if FKeys.IndexOf(Result) = -1 then
    FKeys.Add(Result);
end;

procedure TRedisLibServerTest.SetUp;
begin
  FKeys := TStringList.Create;
  if not ServerChecked then
  begin
    ServerChecked := True;
    FRedis := NewRedisTestClient;
    ServerAvailable := Assigned(FRedis);
    if not ServerAvailable then
      WriteLn('  ', ServerMessage, ' - ', ClassName, ' ignored');
  end
  else if ServerAvailable then
    FRedis := NewRedisTestClient;

  if not Assigned(FRedis) then
  begin
    // TearDown does not run when SetUp raises
    FreeAndNil(FKeys);
    Ignore(ServerMessage);
  end;
end;

procedure TRedisLibServerTest.TearDown;
var
  i: integer;
  args: array of string;
begin
  if Assigned(FRedis) and (FKeys.Count > 0) then
  begin
    SetLength(args, FKeys.Count + 1);
    args[0] := 'DEL';
    for i := 0 to FKeys.Count - 1 do
      args[i + 1] := FKeys[i];
    FRedis.TimeOut := REDIS_DEFAULT_TIMEOUT;
    FRedis.SendCommand(args);
  end;
  FreeAndNil(FRedis);
  FreeAndNil(FKeys);
end;

procedure TRedisLibServerTest.TestPing;
begin
  AssertTrue(FRedis.isConnected);
  AssertTrue('ResolvedAddress is set after Connect', FRedis.ResolvedAddress <> '');
  AssertTrue(FRedis.Ping);
  AssertEquals('+PONG', FRedis.LastMessage);
  AssertTrue(FRedis.ResponType = rrtStatus);
  AssertEquals(0, FRedis.LastError);
end;

procedure TRedisLibServerTest.TestSetGet;
begin
  FRedis[Key('plain')] := 'hello';
  AssertTrue('SET answers with a status', FRedis.ResponType = rrtStatus);
  AssertEquals('+OK', FRedis.LastMessage);

  AssertEquals('hello', FRedis[Key('plain')]);
  AssertTrue('GET answers with a bulk', FRedis.ResponType = rrtBulk);
  AssertEquals('LastMessage holds the payload', 'hello', FRedis.LastMessage);

  FRedis[Key('plain')] := 'changed';
  AssertEquals('changed', FRedis[Key('plain')]);

  AssertEquals('empty key is ignored', '', FRedis['']);
end;

procedure TRedisLibServerTest.TestSpecialCharacters;
var
  s: string;
begin
  // single quotes broke the old inline "SET key 'value'" command
  s := 'it''s a "quoted" value, with spaces' + #13#10 + 'and a new line, '
    + 'tab' + #9 + 'and bytes: h' + #$C3#$A9 + 'llo ' + #$E6#$97#$A5#$E6#$9C#$AC;
  FRedis[Key('special')] := s;
  AssertTrue(FRedis.ResponType = rrtStatus);
  AssertEquals(s, FRedis[Key('special')]);

  // a key with a space is fine with the RESP array protocol
  FRedis[Key('with space')] := 'x';
  AssertEquals('x', FRedis[Key('with space')]);
end;

procedure TRedisLibServerTest.TestCodePageTaggedValue;
var
  s, g: string;
begin
  // Lazarus and BOM sources tag their literals CP_UTF8; when the default
  // code page differs FPC converts on "+", which once broke the "$<len>" header
  s := 'caf' + #$C3#$A9 + ' ' + #$E6#$97#$A5#$E6#$9C#$AC#$E8#$AA#$9E;
  SetCodePage(RawByteString(s), CP_UTF8, False);
  FRedis[Key('cp')] := s;
  AssertTrue('SET with CP_UTF8 value: ' + FRedis.LastErrorMessage, FRedis.ResponType = rrtStatus);

  g := FRedis[Key('cp')];
  AssertEquals('byte length', Length(s), Length(g));
  AssertTrue('bytes unchanged', CompareByte(s[1], g[1], Length(s)) = 0);
  AssertEquals('result carries the default code page', DefaultSystemCodePage, StringCodePage(g));
end;

procedure TRedisLibServerTest.TestEmptyValue;
begin
  FRedis[Key('empty')] := '';
  AssertTrue(FRedis.ResponType = rrtStatus);
  AssertEquals('', FRedis[Key('empty')]);
  AssertTrue('empty string is a bulk, not null', FRedis.ResponType = rrtBulk);
end;

procedure TRedisLibServerTest.TestMissingKeyIsNull;
begin
  FRedis.SendCommand(['DEL', Key('missing')]);
  AssertEquals('', FRedis[Key('missing')]);
  AssertTrue('null bulk', FRedis.ResponType = rrtNull);
  AssertEquals('$-1', FRedis.LastMessage);
  AssertEquals(0, FRedis.LastError);
end;

procedure TRedisLibServerTest.TestLargeValue;
var
  big, got: string;
  i: integer;
begin
  // many receive chunks for one bulk reply
  SetLength(big, 300000);
  for i := 1 to Length(big) do
    big[i] := Chr(Ord('a') + (i mod 26));
  FRedis[Key('big')] := big;
  AssertTrue(FRedis.ResponType = rrtStatus);
  got := FRedis[Key('big')];
  AssertEquals('length', Length(big), Length(got));
  AssertTrue('content', got = big);
end;

procedure TRedisLibServerTest.TestInfo;
var
  s: string;
begin
  s := FRedis.Info;
  AssertTrue('bulk reply', FRedis.ResponType = rrtBulk);
  AssertTrue('header stripped', Pos('$', s) <> 1);
  AssertTrue('contains redis_version', Pos('redis_version:', s) > 0);
  AssertTrue('multi line', Pos(#10, s) > 0);
end;

procedure TRedisLibServerTest.TestNumericReply;
begin
  FRedis.SendCommand(['DEL', Key('counter')]);
  AssertEquals(':1', FRedis.SendCommand(['INCR', Key('counter')]));
  AssertTrue(FRedis.ResponType = rrtNumeric);
  AssertEquals(':2', FRedis.SendCommand(['INCR', Key('counter')]));
  AssertEquals(':5', FRedis.SendCommand(['INCRBY', Key('counter'), '3']));
end;

procedure TRedisLibServerTest.TestMultiBulkReply;
var
  s: string;
begin
  FRedis.SendCommand(['DEL', Key('list')]);
  AssertEquals(':3', FRedis.SendCommand(['RPUSH', Key('list'), 'one', 'two', 'three']));

  s := FRedis.SendCommand(['LRANGE', Key('list'), '0', '-1']);
  AssertTrue(FRedis.ResponType = rrtMultiBulk);
  AssertEquals('*3'#13#10'$3'#13#10'one'#13#10'$3'#13#10'two'#13#10'$5'#13#10'three', s);

  s := FRedis.SendCommand(['LRANGE', Key('list'), '5', '9']);
  AssertTrue('empty array', FRedis.ResponType = rrtMultiBulk);
  AssertEquals('*0', s);

  // ping with argument keeps the connection in sync after a multi bulk
  AssertEquals('$4'#13#10'pong', FRedis.SendCommand(['PING', 'pong']));
end;

procedure TRedisLibServerTest.TestHashCommand;
var
  s: string;
begin
  FRedis.SendCommand(['DEL', Key('hash')]);
  FRedis.SendCommand(['HSET', Key('hash'), 'name', 'Luri Darmawan', 'site', 'fastplaz.com']);
  AssertTrue(FRedis.ResponType = rrtNumeric);
  AssertEquals('Luri Darmawan', FRedis.BulkPayload(FRedis.SendCommand(['HGET', Key('hash'), 'name'])));
  s := FRedis.SendCommand(['HGETALL', Key('hash')]);
  AssertTrue(FRedis.ResponType = rrtMultiBulk);
  AssertTrue(Pos('fastplaz.com', s) > 0);
end;

procedure TRedisLibServerTest.TestErrorReply;
var
  s: string;
begin
  s := FRedis.SendString('NOSUCHCOMMAND x y');
  AssertTrue('error reply', FRedis.ResponType = rrtError);
  AssertEquals('transport error is 0 for a server error reply', 0, FRedis.LastError);
  AssertEquals('raw reply starts with "-"', '-', Copy(s, 1, 1));
  AssertEquals('error text without "-"', 'ERR', Copy(FRedis.LastErrorMessage, 1, 3));
  AssertTrue('still connected', FRedis.isConnected);

  // wrong type
  FRedis[Key('string')] := 'text';
  FRedis.SendCommand(['LPUSH', Key('string'), 'x']);
  AssertTrue(FRedis.ResponType = rrtError);
  AssertEquals('WRONGTYPE', Copy(FRedis.LastErrorMessage, 1, 9));
end;

procedure TRedisLibServerTest.TestInlineCommand;
begin
  FRedis[Key('inline')] := 'hello';
  AssertEquals('$5'#13#10'hello', FRedis.SendString('GET ' + Key('inline')));
  AssertEquals('+PONG', FRedis.SendString('PING'));
  AssertEquals(':1', FRedis.SendString('EXISTS ' + Key('inline')));
end;

procedure TRedisLibServerTest.TestReadTimeoutAndReconnect;
var
  t0: TDateTime;
  elapsed: int64;
begin
  // BLPOP on an empty list blocks server side, the client must give up
  FRedis.SendCommand(['DEL', Key('nolist')]);
  FRedis.TimeOut := 1000;
  t0 := Now;
  AssertEquals('', FRedis.SendCommand(['BLPOP', Key('nolist'), '0']));
  elapsed := MilliSecondsBetween(Now, t0);
  AssertTrue('LastError set', FRedis.LastError <> 0);
  AssertTrue('message: ' + FRedis.LastErrorMessage, FRedis.LastErrorMessage <> '');
  AssertFalse('disconnected after timeout', FRedis.isConnected);
  AssertTrue(Format('gave up within 3 s, took %d ms', [elapsed]), elapsed < 3000);
  AssertTrue(Format('waited at least 500 ms, took %d ms', [elapsed]), elapsed >= 500);

  FRedis.TimeOut := REDIS_DEFAULT_TIMEOUT;
  AssertTrue('auto reconnect: ' + FRedis.LastErrorMessage, FRedis.Ping);
  AssertTrue(FRedis.isConnected);
end;

procedure TRedisLibServerTest.TestDisconnect;
begin
  FRedis.Disconnect;
  AssertFalse(FRedis.isConnected);
  FRedis.Disconnect; // twice is harmless
  AssertTrue('reconnects on demand', FRedis.Ping);
  AssertTrue(FRedis.isConnected);
end;

procedure TRedisLibServerTest.TestConnectByHostName;
var
  R: TRedisLib;
begin
  R := TRedisLib.Create;
  try
    if RedisTestHost = '127.0.0.1' then
      R.ServerAddress := 'localhost'
    else
      R.ServerAddress := RedisTestHost;
    R.Port := RedisTestPort;
    AssertTrue('connect: ' + R.LastErrorMessage, R.Connect);
    if RedisTestPassword <> '' then
      AssertTrue(R.Auth(RedisTestPassword));
    AssertTrue(R.Ping);
  finally
    R.Free;
  end;
end;

procedure TRedisLibServerTest.TestConnectIPv6Loopback;
var
  R: TRedisLib;
begin
  if RedisTestHost <> '127.0.0.1' then
    Ignore('only run against the default local server');
  R := TRedisLib.Create;
  try
    R.ServerAddress := '::1';
    R.Port := RedisTestPort;
    if not R.Connect then
      // default redis.conf binds "127.0.0.1 -::1", but a server may be IPv4 only
      Ignore('redis does not listen on [::1]:' + RedisTestPort + ' (' + R.LastErrorMessage + ')');
    AssertEquals('::1', R.ResolvedAddress);
    if RedisTestPassword <> '' then
      AssertTrue(R.Auth(RedisTestPassword));
    AssertTrue(R.Ping);
    R[Key('ipv6')] := 'over ipv6';
    AssertEquals('over ipv6', R[Key('ipv6')]);
    R.SendCommand(['DEL', Key('ipv6')]);
  finally
    R.Free;
  end;
end;

procedure TRedisLibServerTest.TestWriteToMaster;
var
  R: TRedisLib;
begin
  // master and replica are the same server here, the point is the routing
  R := TRedisLib.Create;
  try
    R.ServerAddress := RedisTestHost;
    R.Port := RedisTestPort;
    R.WriteToMaster := True;
    R.MasterServerAddress := RedisTestHost;
    R.MasterPort := RedisTestPort;
    if RedisTestPassword <> '' then
    begin
      AssertTrue(R.Auth(RedisTestPassword));
      // the master connection authenticates on its own
      R.SendCommand(['PING']); // make sure the replica side is connected
    end;
    R[Key('master')] := 'via master';
    AssertEquals('via master', R[Key('master')]);
  finally
    R.Free;
  end;
end;

initialization
  RegisterTest(TRedisLibOfflineTest);
  RegisterTest(TRedisLibServerTest);

end.

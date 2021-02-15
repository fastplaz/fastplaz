{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit whois_integration;

{
  // USAGE:

  Whois := TWhoisIntegration.Create;
  if Whois.Find( 'yourdomain.com') then
  begin
    Result := 'Domain: ' + UpperCase( domainName)
      + '\nRegistrar: ' + Whois.Registrar
      + '\nStatus: ' + Whois.Status
      + '\nUpdated Date: ' + Whois.UpdatedDate
      + '\nCreation Date: ' + Whois.CreationDate
      + '\nExpiration Date: ' + Whois.ExpiredDate
      + '\nName Server: ' + Whois.NameServer;
  end;
  Whois.Free;

}
{$mode objfpc}{$H+}

interface

uses
  {$IFNDEF Windows}
  cthreads,
  {$ENDIF}
  logutil_lib,
  Sockets, RegExpr, fpjson, common,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

const
  WHOIS_DEFAULT_SERVER = '';
  WHOIS_DEFAULT_PORT = 43;
  WHOIS_DEFAULT_TIMEOUT = 2500;

  _WHOIS_REGEX_REGISTRAR = 'Registrar: ([a-zA-Z0-9\ ,.\-]+)';
  _WHOIS_REGEX_REGISTRANT_NAME = 'Registrant Name:([a-zA-Z0-9\ ,.\-]+)';
  _WHOIS_REGEX_REGISTRANT_EMAIL = 'Registrant Email:([a-zA-Z0-9\ ,.\-@]+)';
  _WHOIS_REGEX_REGISTRAR_ORGANISATION = 'Registrar Organization:([a-zA-Z0-9\ ,.\-]+)';
  _WHOIS_REGEX_NAMESERVER = 'Name Server:([a-zA-Z0-9.\ \-]+)';
  _WHOIS_REGEX_STATUS = 'Status: ([a-zA-Z0-9]+)';
  _WHOIS_REGEX_STATUS_DOTID = 'Status:([a-zA-Z0-9]+)';
  _WHOIS_REGEX_UPDATEDDATE = 'Updated Date:([a-zA-Z0-9\ \-:,.]+)';
  _WHOIS_REGEX_UPDATEDDATE_DOTID = 'Last Updated On:([a-zA-Z0-9\ \-:,.]+)';
  _WHOIS_REGEX_CREATEDDATE = 'Creation Date:([a-zA-Z0-9\ \-,.]+)';
  _WHOIS_REGEX_CREATEDDATE_DOTID = 'Created On:([a-zA-Z0-9\ \-:,.]+)';
  _WHOIS_REGEX_EXPIREDDATE = 'Expiration Date:([a-zA-Z0-9\ \-:,.]+)';
  _WHOIS_REGEX_EXPIREDDATE2 = 'Expiry Date:([a-zA-Z0-9\ \-:,.]+)';
  _WHOIS_REGEX_NOTFOUND = 'No match for';

  _WHOIS_SERVER_LIST_FILENAME = 'files/whois-server-list.json';

type

  { TWhoisIntegration }

  TWhoisIntegration = class(TInterfacedObject)
  private
    regex: TRegExpr;
    FData: TStringList;
    FisConnected: boolean;
    FLastError: integer;
    FLastMessage: string;
    FPort: string;
    FPortInt: integer;
    FServer: string;

    sock: TInetSockAddr;
    sockInt: longint;
    SocketIn, SocketOut: Text;
    IpAddress: in_addr;

    ServerList: TJSONData;

    function getCreateDate: string;
    function getExpiredDate: string;
    function getNameServer: string;
    function getRegistrant: string;
    function getRegistrar: string;
    function getStatus: string;
    function getUpdatedDate: string;
    procedure setPort(AValue: string);
    procedure setServerAddress(AValue: string);

    function Connect: boolean;
    function sendQuery(DomainName: string): boolean;
    function isDomainID(DomainName: string): boolean;
    function getExtension(DomainName: string): string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;
    function Find(DomainName: string): boolean;
    function LoadServerList: boolean;

    property Server: string read FServer write setServerAddress;
    property Port: string read FPort write setPort;
    property Data: TStringList read FData write FData;

    property Registrar: string read getRegistrar;
    property Registrant: string read getRegistrant;
    property NameServer: string read getNameServer;
    property Status: string read getStatus;
    property CreationDate: string read getCreateDate;
    property ExpiredDate: string read getExpiredDate;
    property UpdatedDate: string read getUpdatedDate;

    property isConnected: boolean read FisConnected;
    property LastError: integer read FLastError;
    property LastMessage: string read FLastMessage;
  end;

implementation

{ TWhoisIntegration }

procedure TWhoisIntegration.setPort(AValue: string);
begin
  if FPort = AValue then
    Exit;
  try
    FPortInt := StrToInt(AValue);
    FPort := AValue;
  except
    FPort := IntToStr(WHOIS_DEFAULT_PORT);
    FPortInt := WHOIS_DEFAULT_PORT;
  end;
end;

function TWhoisIntegration.getCreateDate: string;
begin
  Result := '-';
  regex.Expression := _WHOIS_REGEX_CREATEDDATE;
  if regex.Exec(FData.Text) then
    Result := regex.Match[1]
  else
  begin
    regex.Expression := _WHOIS_REGEX_CREATEDDATE_DOTID;
    if regex.Exec(FData.Text) then
      Result := regex.Match[1];
  end;
end;

function TWhoisIntegration.getExpiredDate: string;
begin
  Result := '-';
  regex.Expression := _WHOIS_REGEX_EXPIREDDATE;
  if regex.Exec(FData.Text) then
    Result := regex.Match[1]
  else
  begin
    regex.Expression := _WHOIS_REGEX_EXPIREDDATE2;
    if regex.Exec(FData.Text) then
      Result := regex.Match[1];
  end;
end;

function TWhoisIntegration.getNameServer: string;
begin
  Result := '-';
  regex.Expression := _WHOIS_REGEX_NAMESERVER;
  if regex.Exec(FData.Text) then
  begin
    Result := regex.Match[1];
    while regex.ExecNext do
    begin
      Result := Result + ' ' + regex.Match[1];
    end;
  end;
  Result := Result.Replace('Name Server: ', '');
end;

function TWhoisIntegration.getRegistrant: string;
begin
  Result := '-';
  regex.Expression := _WHOIS_REGEX_REGISTRANT_NAME;
  if regex.Exec(FData.Text) then
  begin
    Result := regex.Match[1];
    regex.Expression := _WHOIS_REGEX_REGISTRANT_EMAIL;
    if regex.Exec(FData.Text) then
      Result := Result + ' (' + regex.Match[1] + ')';
  end;
end;

function TWhoisIntegration.getRegistrar: string;
begin
  Result := '-';
  regex.Expression := _WHOIS_REGEX_REGISTRAR;
  if regex.Exec(FData.Text) then
    Result := regex.Match[1]
  else
  begin
    regex.Expression := _WHOIS_REGEX_REGISTRAR_ORGANISATION;
    if regex.Exec(FData.Text) then
      Result := regex.Match[1];
  end;
end;

function TWhoisIntegration.getStatus: string;
var
  i: integer;
begin
  Result := '-';
  regex.Expression := _WHOIS_REGEX_STATUS;
  if regex.Exec(FData.Text) then
    Result := regex.Match[1]
  else
  begin
    regex.Expression := _WHOIS_REGEX_STATUS_DOTID;
    if regex.Exec(FData.Text) then
      Result := regex.Match[1];
      while regex.ExecNext do
      begin
        Result := Result + ', ' + regex.Match[1];
      end;
  end;
end;

function TWhoisIntegration.getUpdatedDate: string;
begin
  Result := '-';
  regex.Expression := _WHOIS_REGEX_UPDATEDDATE;
  if regex.Exec(FData.Text) then
    Result := regex.Match[1]
  else
  begin
    regex.Expression := _WHOIS_REGEX_UPDATEDDATE_DOTID;
    if regex.Exec(FData.Text) then
      Result := regex.Match[1];
  end;
end;

procedure TWhoisIntegration.setServerAddress(AValue: string);
begin
  if FServer = AValue then
    Exit;
  FServer := AValue;
  FServer := GetHostNameIP(FServer);
  IpAddress := StrToNetAddr(FServer);
  if IpAddress.s_addr = 0 then
  begin
    FServer := WHOIS_DEFAULT_SERVER;
    IpAddress := StrToNetAddr(FServer);
  end;
end;

function TWhoisIntegration.Connect: boolean;
begin
  Result := FisConnected;
  FLastMessage := '';
  FLastError := 1;
  if FisConnected then
    exit;

  try
    IpAddress := StrToNetAddr(FServer);
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
  except
    on E: Exception do
    begin
      FLastMessage := E.Message;
    end;
  end;

end;

function TWhoisIntegration.sendQuery(DomainName: string): boolean;
var
  s: string;
begin
  Result := False;
  FLastError := 0;
  FLastMessage := 'try..';
  if not FisConnected then
  begin
    if not Connect then
      Exit;
  end;

  try
    Data.Clear;
    Reset(SocketIn);
    ReWrite(SocketOut);
    WriteLn(SocketOut, DomainName + #13);
    Flush(SocketOut);
    //ReadLn(SocketIn, FLastMessage);
    //ReadLn(SocketIn, s);
    //FLastMessage := 'x:' + FLastMessage + '====/====' + s;
    while not EOF(SocketIn) do
    begin
      ReadLn(SocketIn, s);
      Data.Add(s);
    end;
    FLastMessage := 'Ok';
    Result := True;
  except
    on E: Exception do
    begin
      FLastMessage := E.Message;
    end;
  end;

end;

function TWhoisIntegration.isDomainID(DomainName: string): boolean;
var
  i: integer;
begin
  Result := False;
  i := Length(DomainName) - Pos('.id', DomainName);
  if i = 2 then
    Result := True;
end;

function TWhoisIntegration.getExtension(DomainName: string): string;
var
  lst: TStrings;
begin
  lst := Explode(DomainName, '.');
  Result := lst.ValueFromIndex[lst.Count - 1];
  lst.Free;
  //Result := Copy(DomainName, pos('.', DomainName) + 1);
end;

constructor TWhoisIntegration.Create;
begin
  FServer := WHOIS_DEFAULT_SERVER;
  FPort := IntToStr(WHOIS_DEFAULT_PORT);
  FPortInt := WHOIS_DEFAULT_PORT;
  FLastError := 0;
  FLastMessage := '';
  FisConnected := False;

  regex := TRegExpr.Create;
  FData := TStringList.Create;
  FData.Delimiter := ':';
end;

destructor TWhoisIntegration.Destroy;
begin
  FData.Free;
  regex.Free;

  if Assigned(ServerList) then
    ServerList.Free;
end;

function TWhoisIntegration.Find(DomainName: string): boolean;
var
  ext, notfound: string;
begin
  Result := False;
  ext := getExtension(DomainName);
  try
    Server := ServerList.GetPath(ext + '[0]').AsString;
    notfound := ServerList.GetPath(ext + '[1]').AsString;
  except
    Server := WHOIS_DEFAULT_SERVER;
  end;
  if sendQuery(DomainName) then
  begin
    regex.Expression := _WHOIS_REGEX_NOTFOUND;
    if not regex.Exec(FData.Text) then
      Result := True;
  end;
end;

function TWhoisIntegration.LoadServerList: boolean;
var
  lst: TStringList;
begin
  Result := False;
  if not FileExists(_WHOIS_SERVER_LIST_FILENAME) then
    Exit;
  lst := TStringList.Create;
  lst.LoadFromFile(_WHOIS_SERVER_LIST_FILENAME);
  try
    ServerList := GetJSON(lst.Text);
    Result := True;
  except
    on E: Exception do
    begin
    end;
  end;
end;

end.


{
TODO: switch server whois
https://github.com/regru/php-whois/blob/master/src/Phois/Whois/whois.servers.json
}

unit session_controller;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_app, md5, IniFiles, fpjson, DateUtils, sqldb, HTTPDefs, Variants,
  Classes, SysUtils;

const
  MaxIniCreate = 5;
  _SESSION_SESSION = 'session';
  _SESSION_DATA = 'data';
  _SESSION_COOKIE_KEY = '_';
  _SESSION_COOKIE_ACTIVE_DAY = 3;

  _SESSION_ACTIVE = 'active';         // Start time of session
  _SESSION_KEYSTART = 'start';         // Start time of session
  _SESSION_KEYLAST = 'last';          // Last seen time of session
  _SESSION_IPADDRESS = 'ipaddr';
  _SESSION_USER_AGENT = 'useragent';
  _SESSION_FLASHMESSAGE = 'flash';
  _SESSION_TIMEOUT_DEFAULT = 3 * 1440; // in minutes (default: 3 days)
  _SESSION_STORAGE_FILE = 1;
  _SESSION_STORAGE_DATABASE = 2;
  // session files live in "<TEMP>/sessions" (TEMP may point to a custom
  // directory); this is the fallback when TEMP is not set or not writable
  _SESSION_DIR_DEFAULT = 'ztemp' + PathDelim + 'sessions';
  _SESSION_DIR_SUBDIR = 'sessions';
  TDateTimeEpsilon = 2.2204460493e-16;

  // every value is bound as a parameter; nothing from the request is ever
  // concatenated into the SQL text
  _SESSION_SQL_FIELDS = '(sessid, ipaddr, lastused, uid, remember, vars)';
  _SESSION_SQL_VALUES = '(:sessid, :ipaddr, :lastused, :uid, :remember, :vars)';
  _SESSION_SQL_UPDATE =
    'REPLACE INTO session_info ' + _SESSION_SQL_FIELDS + ' VALUES ' + _SESSION_SQL_VALUES;
  _SESSION_SQL_UPDATE_SQLITE =
    'INSERT OR REPLACE INTO session_info ' + _SESSION_SQL_FIELDS + ' VALUES ' + _SESSION_SQL_VALUES;
  _SESSION_SQL_UPDATE_PG =
    'INSERT INTO session_info ' + _SESSION_SQL_FIELDS + ' VALUES ' + _SESSION_SQL_VALUES +
    ' ON CONFLICT (sessid) DO UPDATE SET' +
    ' ipaddr = EXCLUDED.ipaddr,' +
    ' lastused = EXCLUDED.lastused,' +
    ' uid = EXCLUDED.uid,' +
    ' remember = EXCLUDED.remember,' +
    ' vars = EXCLUDED.vars';
  _SESSION_SQL_DELETE = 'DELETE FROM session_info WHERE sessid = :sessid';

type

  { TSessionController }

  TSessionController = class(TObject)
  private
    FCookiePath: string;
    FExpired: boolean;
    FForceUniqueID: string;
    FIniFile: TMemInifile;
    FIsNewSession: boolean;
    FLastAccess: TDateTime;
    FSessionTimeout: integer;
    FSessionStarted, FSessionTerminated: boolean;
    FSessionPrefix, FSessionSuffix, FSessionExtension, FHttpCookie,
    FCookieID, FSessionID: string;
    FSessionDir: string;
    FSessionVars: TStringList;
    FSessionTable: TObject; // TSessionModel, created on demand
    FToken: string;
    function GenerateSessionID: string;
    function SessionFileName: string;
    function CreateIniFile(const FileName: string): TMemIniFile;
    procedure DeleteIniFile;
    function PrepareSessionDir(const ADir: string): boolean;
    function DefaultSessionDir: string;
    function GetInterval: double;
    function GetIsExpired: boolean;
    function GetTimeOut: integer;
    function GetValue(variable: string): string;
    procedure SetSessionDir(AValue: string);
    procedure SetTimeOut(AValue: integer);
    procedure SetValue(variable: string; AValue: string);
    procedure UpdateIniFile;

    function UpdateDatabase: boolean;
    function DeleteDatabaseRow(const ASessionID: string): boolean;
    procedure LoadFromDatabase;

    function StartSessionWithFile: boolean;
    function StartSessionWithDatabase: boolean;
  public
    Storage: integer;
    constructor Create();
    destructor Destroy; override;
    property Values[variable: string]: string read GetValue write SetValue; default;
    property HttpCookie: string read FHttpCookie;
    property CookieID: string read FCookieID;
    property CookiePath: string read FCookiePath write FCookiePath;
    property Token: string read FToken;
    property SessionID: string read FSessionID write FSessionID;
    property SessionDir: string read FSessionDir write SetSessionDir;
    // in minutes; 0 (or less) never expires
    property TimeOut: integer read GetTimeOut write SetTimeOut;
    property LastAccess: TDateTime read FLastAccess;

    // seconds since the previous request of this session
    property Interval: double read GetInterval;
    // True when the session that came in with this request had timed out
    // (its data was discarded); stays True for the rest of the request
    property IsExpired: boolean read GetIsExpired;
    property IsStarted: boolean read FSessionStarted;
    property IsTerminated: boolean read FSessionTerminated;
    property IsNewSession: boolean read FIsNewSession;

    property SessionPrefix: string read FSessionPrefix write FSessionPrefix;
    property SessionSuffix: string read FSessionSuffix write FSessionSuffix;
    property ForceUniqueID: string read FForceUniqueID write FForceUniqueID;

    function StartSession: boolean;
    function RestartSession: boolean;
    // issue a fresh cookie/session id and move the data over (call after login)
    function RegenerateID: boolean;
    procedure SendCookie;
    procedure Clear;
    procedure DeleteKey(const Key: string);
    procedure EndSession(const Force: boolean = True);
    procedure Terminate;
    procedure ForceUpdate;

    function ReadDateTime(const variable: string): TDateTime;
    function ReadInteger(const variable: string): integer;

    function _DateTimeDiff(const ANow, AThen: TDateTime): TDateTime;
    function GetData(): string;
  end;

// session values are stored one per line (ini file / "vars" column), so line
// breaks and the escape character itself are encoded
function EncodeSessionValue(const AValue: string): string;
function DecodeSessionValue(const AValue: string): string;

implementation

uses logutil_lib, common, session_model;

function EncodeSessionValue(const AValue: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(AValue) do
    case AValue[i] of
      '\': Result := Result + '\\';
      #13: Result := Result + '\r';
      #10: Result := Result + '\n';
      else
        Result := Result + AValue[i];
    end;
end;

function DecodeSessionValue(const AValue: string): string;
var
  i: integer;
begin
  Result := '';
  i := 1;
  while i <= Length(AValue) do
  begin
    if (AValue[i] = '\') and (i < Length(AValue)) then
    begin
      Inc(i);
      case AValue[i] of
        'r': Result := Result + #13;
        'n': Result := Result + #10;
        '\': Result := Result + '\';
        else
          Result := Result + '\' + AValue[i];
      end;
    end
    else
      Result := Result + AValue[i];
    Inc(i);
  end;
end;

{ TSessionController }

function TSessionController.GenerateSessionID: string;
begin
  if FForceUniqueID.IsEmpty then
    Result := FCookieID
  else
    Result := FForceUniqueID;
  Result := Result + '-' + Application.EnvironmentVariable['HTTP_USER_AGENT']
    + '-' + GetUserIpAddress;
  Result := MD5Print(MD5String(Result));
  if not FSessionPrefix.IsEmpty then
    Result := FSessionPrefix + '-' + Result;
  if not FSessionSuffix.IsEmpty then
    Result := Result + '-' + FSessionSuffix;
end;

function TSessionController.SessionFileName: string;
begin
  Result := FSessionDir + FSessionID + FSessionExtension;
end;

function TSessionController.CreateIniFile(const FileName: string): TMemIniFile;
var
  Count: integer;
begin
  Count := 0;
  Result := nil;
  repeat
    Inc(Count);
    try
      Result := TMemIniFile.Create(FileName, False);
    except
      On E: EFCreateError do
      begin
        if Count > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: EFOpenError do
      begin
        if Count > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: Exception do
        raise;
    end;
  until (Result <> nil);
end;

procedure TSessionController.DeleteIniFile;
begin
  try
    DeleteFile(SessionFileName);
  except
  end;
end;

function TSessionController.PrepareSessionDir(const ADir: string): boolean;
begin
  Result := False;
  try
    if not DirectoryExists(ADir) then
      ForceDirectories(ADir);
    Result := DirectoryIsWritable(ADir);
  except
  end;
end;

// "%TEMP%/sessions", or ztemp/sessions when TEMP is not set
function TSessionController.DefaultSessionDir: string;
begin
  Result := Application.EnvironmentVariable['TEMP'];
  if Result = '' then
    Result := _SESSION_DIR_DEFAULT
  else
    Result := IncludeTrailingPathDelimiter(Result) + _SESSION_DIR_SUBDIR;
  Result := IncludeTrailingPathDelimiter(Result);
end;

function TSessionController.GetInterval: double;
begin
  Result := (((Now - FLastAccess) + TDateTimeEpsilon) * SecsPerDay);
  if Result < 0 then
    Result := 0;
end;

function TSessionController.GetIsExpired: boolean;
begin
  Result := FExpired;
  if Result or (FSessionTimeout <= 0) or (not FSessionStarted) then
    Exit;
  // TimeOut is in minutes, Interval in seconds
  if Interval > (FSessionTimeout * 60.0) then
  begin
    FExpired := True;
    Clear; // resets FLastAccess, so this runs once per request
    Result := True;
  end;
end;

function TSessionController.GetTimeOut: integer;
begin
  Result := FSessionTimeout;
end;

function TSessionController.GetValue(variable: string): string;
begin
  Result := '';
  if (not FSessionStarted) or (FSessionTerminated) then
    Exit;

  if Storage = _SESSION_STORAGE_FILE then
  begin
    if (FIniFile = nil) then
      Exit;
    Result := FIniFile.ReadString(_SESSION_DATA, variable, '');
  end;

  if Storage = _SESSION_STORAGE_DATABASE then
    Result := FSessionVars.Values[variable];

  Result := DecodeSessionValue(Result);
end;

procedure TSessionController.SetSessionDir(AValue: string);
begin
  if AValue = '' then
    Exit;
  AValue := IncludeTrailingPathDelimiter(AValue);
  if FSessionDir = AValue then
    Exit;
  FSessionDir := AValue;
end;

procedure TSessionController.SetTimeOut(AValue: integer);
begin
  FSessionTimeout := AValue;
end;

procedure TSessionController.SetValue(variable: string; AValue: string);
begin
  if (not FSessionStarted) or (FSessionTerminated) then
    Exit;
  AValue := EncodeSessionValue(AValue);
  if Storage = _SESSION_STORAGE_FILE then
  begin
    if (FIniFile = nil) then
      Exit;
    try
      FIniFile.WriteString(_SESSION_DATA, variable, AValue);
      UpdateIniFile;
    except
      on E: Exception do
        LogUtil.Add('session write failed: ' + E.Message, 'sessions', True);
    end;
  end;

  if Storage = _SESSION_STORAGE_DATABASE then
  begin
    FSessionVars.Values[variable] := AValue;
    UpdateDatabase;
  end;
end;

procedure TSessionController.UpdateIniFile;
var
  Count: integer;
  OK: boolean;
begin
  if FIniFile = nil then
    Exit;
  Count := 0;
  OK := False;
  repeat
    Inc(Count);
    try
      FIniFile.UpdateFile;
      OK := True;
    except
      On E: EFCreateError do
      begin
        if Count > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: EFOpenError do
      begin
        if Count > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: Exception do
        raise;
    end;
  until OK;
end;

function TSessionController.UpdateDatabase: boolean;
var
  q: TSQLQuery;
  sql, connectorType: string;
begin
  Result := False;
  if FSessionTable = nil then
    Exit;
  connectorType := LowerCase(TSessionModel(FSessionTable).Connector.ConnectorType);
  if connectorType.StartsWith('postgre') then
    sql := _SESSION_SQL_UPDATE_PG
  else if Pos('sqlite', connectorType) > 0 then
    sql := _SESSION_SQL_UPDATE_SQLITE
  else
    sql := _SESSION_SQL_UPDATE;

  q := TSQLQuery.Create(nil);
  try
    try
      q.DataBase := TSessionModel(FSessionTable).Connector;
      q.SQL.Text := sql;
      q.Params.ParamByName('sessid').AsString := FSessionID;
      q.Params.ParamByName('ipaddr').AsString := GetUserIpAddress;
      q.Params.ParamByName('lastused').AsLargeInt := DateTimeToUnix(Now, False);
      q.Params.ParamByName('uid').AsInteger := s2i(FSessionVars.Values[SESSION_FIELD_UID]);
      q.Params.ParamByName('remember').AsInteger := 0;
      q.Params.ParamByName('vars').AsString := FSessionVars.Text;
      q.ExecSQL;
      TSQLConnection(q.DataBase).Transaction.Commit;
      Result := True;
    except
      on E: Exception do
        LogUtil.Add('session db write failed: ' + E.Message, 'sessions', True);
    end;
  finally
    q.Free;
  end;
end;

function TSessionController.DeleteDatabaseRow(const ASessionID: string): boolean;
var
  q: TSQLQuery;
begin
  Result := False;
  if FSessionTable = nil then
    Exit;
  q := TSQLQuery.Create(nil);
  try
    try
      q.DataBase := TSessionModel(FSessionTable).Connector;
      q.SQL.Text := _SESSION_SQL_DELETE;
      q.Params.ParamByName('sessid').AsString := ASessionID;
      q.ExecSQL;
      TSQLConnection(q.DataBase).Transaction.Commit;
      Result := True;
    except
      on E: Exception do
        LogUtil.Add('session db delete failed: ' + E.Message, 'sessions', True);
    end;
  finally
    q.Free;
  end;
end;

procedure TSessionController.LoadFromDatabase;
var
  lastUsed: int64;
begin
  FSessionVars.Text := VarToStr(TSessionModel(FSessionTable).Value[SESSION_FIELD_VARS]);
  lastUsed := StrToInt64Def(VarToStr(TSessionModel(FSessionTable).Value[SESSION_FIELD_LASTUSED]), 0);
  if lastUsed > 0 then
    FLastAccess := UnixToDateTime(lastUsed, False)
  else
    FLastAccess := Now;
end;

function TSessionController._DateTimeDiff(const ANow, AThen: TDateTime): TDateTime;
begin
  Result := ANow - AThen;
  if (ANow > 0) and (AThen < 0) then
    Result := Result - 0.5
  else if (ANow < -1.0) and (AThen > -1.0) then
    Result := Result + 0.5;
end;

function TSessionController.GetData(): string;
var
  lst: TStringList;
begin
  Result := '';
  if Storage = _SESSION_STORAGE_FILE then
  begin
    if FIniFile = nil then
      Exit;
    lst := TStringList.Create;
    try
      FIniFile.GetStrings(lst);
      Result := lst.Text;
    finally
      lst.Free;
    end;
  end;

  if Storage = _SESSION_STORAGE_DATABASE then
    Result := FSessionVars.Text;
end;

constructor TSessionController.Create();
var
  lstr: TStrings;
begin
  inherited Create();
  FSessionVars := TStringList.Create;
  FSessionTable := nil;
  FLastAccess := 0;
  FIsNewSession := False;
  FExpired := False;
  FHttpCookie := Application.EnvironmentVariable['HTTP_COOKIE'];
  FHttpCookie := FHttpCookie.Replace(' ', '');
  lstr := Explode(FHttpCookie, ';');
  FCookieID := lstr.Values[_SESSION_COOKIE_KEY];
  if FCookieID.IsEmpty then
  begin
    FCookieID := RandomString(41, 'fastplaz');
    FIsNewSession := True;
  end;
  FToken := lstr.Values['token'];
  FreeAndNil(lstr);
  FCookiePath := '';
  FSessionPrefix := '';
  FSessionSuffix := '';
  FForceUniqueID := '';
  FSessionID := GenerateSessionID();
  FSessionDir := DefaultSessionDir;
  FSessionExtension := '.ses';
  FSessionStarted := False;
  FSessionTerminated := False;
  Storage := _SESSION_STORAGE_FILE;
  FSessionTimeout := _SESSION_TIMEOUT_DEFAULT;
end;

destructor TSessionController.Destroy;
begin
  FreeAndNil(FIniFile);
  FreeAndNil(FSessionTable);
  FreeAndNil(FSessionVars);
  inherited Destroy;
end;

function TSessionController.StartSession: boolean;
begin
  Result := True;
  if FSessionStarted then
    Exit;
  FExpired := False;
  FSessionTerminated := False;
  FSessionID := GenerateSessionID();

  Result := False;
  if Storage = _SESSION_STORAGE_FILE then
    Result := StartSessionWithFile;

  if Storage = _SESSION_STORAGE_DATABASE then
    Result := StartSessionWithDatabase;
end;

function TSessionController.RestartSession: boolean;
begin
  Clear;
  ForceUpdate;
  FreeAndNil(FIniFile);
  FSessionStarted := False;
  Result := StartSession;
end;

function TSessionController.RegenerateID: boolean;
var
  oldID, oldFile: string;
  lst: TStringList;
  newIni: TMemIniFile;
begin
  Result := False;
  if (not FSessionStarted) or FSessionTerminated then
    Exit;
  oldID := FSessionID;
  oldFile := SessionFileName;
  FCookieID := RandomString(41, 'fastplaz');
  FSessionID := GenerateSessionID();

  if Storage = _SESSION_STORAGE_FILE then
  begin
    if FIniFile = nil then
      Exit;
    lst := TStringList.Create;
    try
      FIniFile.GetStrings(lst);
      newIni := CreateIniFile(SessionFileName);
      newIni.SetStrings(lst);
    finally
      lst.Free;
    end;
    FreeAndNil(FIniFile);
    FIniFile := newIni;
    try
      UpdateIniFile;
    except
      on E: Exception do
      begin
        LogUtil.Add('session regenerate failed: ' + E.Message, 'sessions', True);
        Exit;
      end;
    end;
    DeleteFile(oldFile);
  end;

  if Storage = _SESSION_STORAGE_DATABASE then
  begin
    if not UpdateDatabase then
      Exit;
    DeleteDatabaseRow(oldID);
  end;

  FIsNewSession := True;
  SendCookie;
  Result := True;
end;

procedure TSessionController.SendCookie;
var
  cookie: TCookie;
begin
  try
    if Application.Response = nil then
      Exit;
    cookie := Application.Response.Cookies.Add;
    cookie.Name := _SESSION_COOKIE_KEY;
    cookie.Value := FCookieID;
    cookie.Path := '/';
    if not FCookiePath.IsEmpty then
      cookie.Path := FCookiePath;
    cookie.HttpOnly := True;
    cookie.SameSite := ssLax;
    cookie.Secure := (ServerVariable('HTTPS') = 'on');
  except
    on E: Exception do
      LogUtil.Add('session cookie: ' + E.Message, 'sessions', True);
  end;
end;

function TSessionController.StartSessionWithFile: boolean;
begin
  Result := False;

  if FSessionDir = '' then
    FSessionDir := DefaultSessionDir;
  FSessionDir := IncludeTrailingPathDelimiter(FSessionDir);
  if not PrepareSessionDir(FSessionDir) then
  begin
    LogUtil.Add('session directory is not writable: ' + FSessionDir, 'sessions', True);
    FSessionDir := IncludeTrailingPathDelimiter(_SESSION_DIR_DEFAULT);
    if not PrepareSessionDir(FSessionDir) then
    begin
      LogUtil.Add('session directory is not writable: ' + FSessionDir, 'sessions', True);
      Exit;
    end;
  end;

  //-- create session file
  FreeAndNil(FIniFile);
  FIniFile := CreateIniFile(SessionFileName);
  if FIniFile = nil then
    Exit;

  // init session file
  if not FIniFile.ReadBool(_SESSION_SESSION, _SESSION_ACTIVE, False) then
  begin
    FIniFile.WriteBool(_SESSION_SESSION, _SESSION_ACTIVE, True);
    FIniFile.WriteString(_SESSION_SESSION, _SESSION_IPADDRESS, GetUserIpAddress);
    FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYSTART, Now);
    FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYLAST, Now);
  end;

  FLastAccess := FIniFile.ReadDateTime(_SESSION_SESSION, _SESSION_KEYLAST, Now);
  FSessionStarted := True;

  // an expired session is wiped (GetIsExpired -> Clear) and continues empty
  if not GetIsExpired then
  begin
    FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYLAST, Now);
    try
      UpdateIniFile;
    except
      on E: Exception do
      begin
        LogUtil.Add('session write failed: ' + E.Message, 'sessions', True);
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TSessionController.StartSessionWithDatabase: boolean;
begin
  Result := False;
  FreeAndNil(FSessionTable);
  try
    FSessionTable := TSessionModel.Create();
    if TSessionModel(FSessionTable).Find(FSessionID) then
      LoadFromDatabase
    else
    begin
      FSessionVars.Clear;
      FLastAccess := Now;
    end;
  except
    on E: Exception do
    begin
      LogUtil.Add('session db: ' + E.Message, 'sessions', True);
      Exit;
    end;
  end;
  FSessionStarted := True;
  if not GetIsExpired then // GetIsExpired -> Clear already writes the row
    Result := UpdateDatabase
  else
    Result := True;
end;

procedure TSessionController.Clear;
begin
  try
    if Storage = _SESSION_STORAGE_FILE then
    begin
      if FIniFile = nil then
        Exit;
      FIniFile.EraseSection(_SESSION_DATA);
      FIniFile.WriteBool(_SESSION_SESSION, _SESSION_ACTIVE, True);
      FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYSTART, Now);
      FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYLAST, Now);
      UpdateIniFile;
    end;

    if Storage = _SESSION_STORAGE_DATABASE then
    begin
      FSessionVars.Clear;
      UpdateDatabase;
    end;
    FLastAccess := Now;
  except
    on E: Exception do
      LogUtil.Add('session clear failed: ' + E.Message, 'sessions', True);
  end;
end;

procedure TSessionController.DeleteKey(const Key: string);
var
  i: integer;
begin
  if (not FSessionStarted) or FSessionTerminated then
    Exit;
  if Storage = _SESSION_STORAGE_FILE then
  begin
    if FIniFile = nil then
      Exit;
    FIniFile.DeleteKey(_SESSION_DATA, Key);
    try
      UpdateIniFile;
    except
      on E: Exception do
        LogUtil.Add('session write failed: ' + E.Message, 'sessions', True);
    end;
  end;

  if Storage = _SESSION_STORAGE_DATABASE then
  begin
    i := FSessionVars.IndexOfName(Key);
    if i <> -1 then
      FSessionVars.Delete(i);
    UpdateDatabase;
  end;
end;

procedure TSessionController.EndSession(const Force: boolean);
begin
  try
    if Storage = _SESSION_STORAGE_FILE then
    begin
      if FIniFile = nil then
        Exit;
      FIniFile.WriteBool(_SESSION_SESSION, _SESSION_ACTIVE, False);
      FIniFile.EraseSection(_SESSION_DATA);
      if Force then
      begin
        FreeAndNil(FIniFile);
        DeleteIniFile;
        FSessionTerminated := True;
        // a later StartSession (the next request of a daemon, or a login
        // after a forced logout) starts afresh instead of staying terminated
        FSessionStarted := False;
      end
      else
        UpdateIniFile;
    end;

    if Storage = _SESSION_STORAGE_DATABASE then
    begin
      if Force then
      begin
        FSessionVars.Clear;
        DeleteDatabaseRow(FSessionID);
        FSessionTerminated := True;
        FSessionStarted := False;
      end
      else
        Clear;
    end;
  except
    on E: Exception do
      LogUtil.Add('session end failed: ' + E.Message, 'sessions', True);
  end;
end;

procedure TSessionController.Terminate;
begin
  EndSession;
end;

procedure TSessionController.ForceUpdate;
begin
  if (not FSessionStarted) or FSessionTerminated then
    Exit;
  if Storage = _SESSION_STORAGE_FILE then
    UpdateIniFile;
  if Storage = _SESSION_STORAGE_DATABASE then
    UpdateDatabase;
end;

function TSessionController.ReadDateTime(const variable: string): TDateTime;
begin
  Result := StrToDateTimeDef(GetValue(variable), 0);
end;

function TSessionController.ReadInteger(const variable: string): integer;
begin
  Result := s2i(GetValue(variable));
end;

end.

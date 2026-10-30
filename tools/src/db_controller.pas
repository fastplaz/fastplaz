unit db_controller;

{$mode objfpc}{$H+}

interface

uses
  Dialogs,
  de_common,
  // Database
  sqldb, sqldblib, mysql50conn, mysql51conn, mysql55conn,
  mysql56conn, mysql57conn, mysql80conn,
  sqlite3conn, pqconnection,
  LCLProc, Classes, SysUtils;

type

//supported drvier:
//  mysql5.7,

  { TQueryThread }

  TQueryThread = class(TThread)
  private
    FCallBack: TOnSQLCallback;
    FStatusCode: integer;
    FStatusText: string;
    FQuery: TSQLQuery;
    procedure synchronizeStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
    property Query: TSQLQuery read FQuery write FQuery;
    property CallBack: TOnSQLCallback read FCallBack write FCallBack;
  end;

  { TDatabaseController }

  TDatabaseController = class
  private
    FQueryThread: TQueryThread;
    FLibraryPath: string;
    FMessage: string;
    FOnAfterConnect: TNotifyEvent;
    FOnAfterDisconnect: TNotifyEvent;
    FOutput: string;
    FProcessing: boolean;
    function getConnected: boolean;
    function getSQL: TStringList;
    procedure setSQL(AValue: TStringList);
    procedure ShowStatus;
    procedure SetLibraryPath(AValue: string);
  protected
  public
    Transaction: TSQLTransaction;
    SQLConnector: TSQLConnector;
    LibraryLoader: TSQLDBLibraryLoader;
    Query: TSQLQuery;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    procedure Init;
    //procedure Execute; override;

    function Open: boolean;
    function OpenThread(ASQLQuery: TSQLQuery; ASQL: string = '';
      ACallBack: TOnSQLCallback = nil): boolean;
    procedure Close(ForceClose: boolean = False);

    function LoadLibrary(ALibraryFileName: string): boolean;
    procedure UnloadLibrary;
  published
    property Connected: boolean read getConnected;
    property SQL: TStringList read getSQL write setSQL;
    property Output: string read FOutput;
    property LibraryPath: string read FLibraryPath write SetLibraryPath;
    property Message: string read FMessage;
    property Processing: boolean read FProcessing;

    // callback
    property OnAfterConnect: TNotifyEvent read FOnAfterConnect write FOnAfterConnect;
    property OnAfterDisconnect: TNotifyEvent
      read FOnAfterDisconnect write FOnAfterDisconnect;
  end;

var
  DatabaseController: TDatabaseController;

function PrepareDatabaseController: boolean;

// FPC sqldb: TMySQLxxConnection.DoInternalConnect initialises the client
// library but does not release it when connecting fails afterwards
// (version mismatch, wrong password, ...). The library stays loaded and
// loading another library fails with "MySQL interface already initialized".
function MySQLLibraryRefCount(const AConnectorType: string): integer;
procedure MySQLLibraryRelease(const AConnectorType: string; ACount: integer);
// Open connection, release the leaked library reference if it fails
procedure OpenConnection(AConnector: TSQLConnector);
// Windows does not search the library folder for its dependencies
// (libmysql.dll needs libssl/libcrypto from mysql\bin), add the library
// folder and its sibling "bin" folder to the PATH of this process
procedure AddLibrarySearchPath(const ALibraryFileName: string);

implementation

uses
  {$ifdef windows}Windows,{$endif}
  mysql50dyn, mysql51dyn, mysql55dyn, mysql56dyn, mysql57dyn, mysql80dyn;

procedure AddLibrarySearchPath(const ALibraryFileName: string);
{$ifdef windows}
var
  i: integer;
  path, dir: string;
  dirs: array[0..1] of string;
begin
  dir := ExtractFileDir(ExpandFileName(ALibraryFileName));
  dirs[0] := ExpandFileName(dir + '\..\bin');
  dirs[1] := dir;
  path := SysUtils.GetEnvironmentVariable('PATH');
  for i := Low(dirs) to High(dirs) do
  begin
    if not DirectoryExists(dirs[i]) then
      Continue;
    if Pos(';' + LowerCase(dirs[i]) + ';', ';' + LowerCase(path) + ';') > 0 then
      Continue;
    path := dirs[i] + ';' + path;
  end;
  SetEnvironmentVariable('PATH', PChar(path));
end;
{$else}
begin
end;
{$endif}

function MySQLLibraryRefCount(const AConnectorType: string): integer;
begin
  // InitialiseMysql without library name only increases the reference count
  // when the library is already loaded, DoReleaseMysql returns the count
  Result := 0;
  case AConnectorType of
    'MySQL 5.0':
      if mysql50dyn.MysqlLibraryHandle <> NilHandle then
      begin
        mysql50dyn.InitialiseMysql;
        Result := mysql50dyn.DoReleaseMysql;
      end;
    'MySQL 5.1':
      if mysql51dyn.MysqlLibraryHandle <> NilHandle then
      begin
        mysql51dyn.InitialiseMysql;
        Result := mysql51dyn.DoReleaseMysql;
      end;
    'MySQL 5.5':
      if mysql55dyn.MysqlLibraryHandle <> NilHandle then
      begin
        mysql55dyn.InitialiseMysql;
        Result := mysql55dyn.DoReleaseMysql;
      end;
    'MySQL 5.6':
      if mysql56dyn.MysqlLibraryHandle <> NilHandle then
      begin
        mysql56dyn.InitialiseMysql;
        Result := mysql56dyn.DoReleaseMysql;
      end;
    'MySQL 5.7':
      if mysql57dyn.MysqlLibraryHandle <> NilHandle then
      begin
        mysql57dyn.InitialiseMysql;
        Result := mysql57dyn.DoReleaseMysql;
      end;
    'MySQL 8.0':
      if mysql80dyn.MysqlLibraryHandle <> NilHandle then
      begin
        mysql80dyn.InitialiseMysql;
        Result := mysql80dyn.DoReleaseMysql;
      end;
  end;
end;

procedure MySQLLibraryRelease(const AConnectorType: string; ACount: integer);
var
  i: integer;
begin
  for i := 1 to ACount do
    case AConnectorType of
      'MySQL 5.0': mysql50dyn.ReleaseMySQL;
      'MySQL 5.1': mysql51dyn.ReleaseMySQL;
      'MySQL 5.5': mysql55dyn.ReleaseMySQL;
      'MySQL 5.6': mysql56dyn.ReleaseMySQL;
      'MySQL 5.7': mysql57dyn.ReleaseMySQL;
      'MySQL 8.0': mysql80dyn.ReleaseMySQL;
    end;
end;

procedure OpenConnection(AConnector: TSQLConnector);
var
  refCount: integer;
begin
  refCount := MySQLLibraryRefCount(AConnector.ConnectorType);
  try
    AConnector.Open;
  except
    MySQLLibraryRelease(AConnector.ConnectorType,
      MySQLLibraryRefCount(AConnector.ConnectorType) - refCount);
    raise;
  end;
end;

function PrepareDatabaseController: boolean;
begin
  Result := False;

  if not Assigned(DatabaseController) then
  begin
    DatabaseController := TDatabaseController.Create(True);
    //DatabaseController.FreeOnTerminate := False;
  end;

  Result := True;
end;

{ TQueryThread }

procedure TQueryThread.synchronizeStatus;
begin
  if (FCallBack <> nil) then
  begin
    FCallBack(FStatusCode, FStatusText, FQuery);
  end;
end;

procedure TQueryThread.Execute;
begin
  FStatusCode := -1;
  FStatusText := '';
  if FQuery <> nil then
  begin
    try
      if preg_match(QUERY_SELECT_REGEX, FQuery.SQL.Text.Trim) then
      begin
        FQuery.Prepare;
        FQuery.Open
      end
      else
      begin
        FQuery.ExecSQL;
      end;
      FStatusText := 'OK';
      FStatusCode := 0;
    except
      on E: Exception do
      begin
        FStatusText := E.Message;
      end;
    end;
  end;

  Synchronize(@synchronizeStatus);
end;

constructor TQueryThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FCallBack := nil;
  FQuery := nil;
end;

{ TDatabaseController }

procedure TDatabaseController.ShowStatus;
begin
  {
  if OnUpdate <> nil then
  begin
    OnUpdate(nil);
    FOutput := '';
  end;
  }
end;

function TDatabaseController.getSQL: TStringList;
begin
  Result := Query.SQL;
end;

function TDatabaseController.getConnected: boolean;
begin
  Result := False;
  try
    if Assigned(SQLConnector) then
    begin
      Result := SQLConnector.Connected;
    end;
  except
    on E: Exception do
    begin
      FMessage := E.Message;
    end;
  end;
end;

procedure TDatabaseController.setSQL(AValue: TStringList);
begin
  Query.SQL.Text := AValue.Text;
end;

procedure TDatabaseController.SetLibraryPath(AValue: string);
begin
  if FLibraryPath = AValue then
    Exit;
  FLibraryPath := AValue;
end;

constructor TDatabaseController.Create(CreateSuspended: boolean);
begin
  //inherited Create(CreateSuspended);

  FProcessing := False;
  Transaction := TSQLTransaction.Create(nil);
  SQLConnector := TSQLConnector.Create(nil);
  SQLConnector.Transaction := Transaction;
  Query := TSQLQuery.Create(nil);
  Query.DataBase := SQLConnector;
end;

destructor TDatabaseController.Destroy;
begin
  if SQLConnector.Connected then
    SQLConnector.Close(True);
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
  Query.Free;
  SQLConnector.Free;
  Transaction.Free;

  inherited Destroy;
end;

{
procedure TDatabaseController.Execute;
begin
  //inherited Execute;


  //-- end
  //if OnSuccess <> nil then
  //  OnSuccess(nil);
end;
}

procedure TDatabaseController.Init;
begin
  //  DB_LibLoader
end;

function TDatabaseController.Open: boolean;
begin
  Result := False;
  FMessage := '';
  if Query.Active then
    Query.Close;
  try
    if preg_match(QUERY_SELECT_REGEX, Query.SQL.Text.Trim) then
      Query.Open
    else
      Query.ExecSQL;
    Result := True;
  except
    on E: Exception do
    begin
      FMessage := E.Message;
    end;
  end;
end;

function TDatabaseController.OpenThread(ASQLQuery: TSQLQuery; ASQL: string;
  ACallBack: TOnSQLCallback): boolean;
begin
  Result := False;

  if ASQLQuery = nil then
    Exit;
  if not ASQL.IsEmpty then
    ASQLQuery.SQL.Text := ASQL;

  ASQLQuery.DataBase := DatabaseController.SQLConnector;
  FQueryThread := TQueryThread.Create(True);
  FQueryThread.CallBack := ACallBack;
  FQueryThread.Query := ASQLQuery;
  FQueryThread.Start;
  Result := True;
end;

procedure TDatabaseController.Close(ForceClose: boolean);
begin
  SQLConnector.Close(ForceClose);
end;

function TDatabaseController.LoadLibrary(ALibraryFileName: string): boolean;
begin
  Result := False;
  if (ALibraryFileName.IsEmpty) or
    (not FileExists(ALibraryFileName)) then
    Exit;

  if not Assigned(LibraryLoader) then
    LibraryLoader := TSQLDBLibraryLoader.Create(nil);

  try
    // release previous library first, so another library can be loaded
    UnloadLibrary;
    LibraryLoader.ConnectionType := SQLConnector.ConnectorType;
    LibraryLoader.LibraryName := ALibraryFileName;
    AddLibrarySearchPath(ALibraryFileName);
    // LoadLibrary only once, setting Enabled := True also loads the library
    LibraryLoader.LoadLibrary;
    Result := True;
  except
    on E: Exception do
    begin
      FMessage := E.Message;
    end;
  end;
end;

procedure TDatabaseController.UnloadLibrary;
begin
  if Assigned(LibraryLoader) and LibraryLoader.Enabled then
    LibraryLoader.UnloadLibrary;
end;

finalization
  if Assigned(DatabaseController) then
    DatabaseController.Free;

end.

unit database_lib;

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  {$ifdef GREYHOUND}
  ghSQL, ghSQLdbLib,
  {$endif}
  fastplaz_app, fphttp, db, fpjson, jsonparser, fgl,
  sqldb, sqldblib, mysql50conn, mysql51conn, mysql55conn, mysql56conn, mysql57conn, mysql80conn,
  sqlite3conn, pqconnection, IBConnection, TypInfo,
  variants, Classes, SysUtils;

type

  generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
  TFieldValueMap = specialize TStringHashMap<variant>;

  { TFPSQLConnector }

  TFPSQLConnector = class(TSQLConnector)
  private
    FSkipLibraryVersionCheck: boolean;
    procedure ApplySkipLibraryVersionCheck;
    procedure SetSkipLibraryVersionCheck(AValue: boolean);
  protected
    procedure CreateProxy; override;
  public
    property Proxy;
    // MySQL only: skip the client library version check. The proxy connection
    // is created on Open, so the flag is stored here and applied at that time.
    property SkipLibraryVersionCheck: boolean read FSkipLibraryVersionCheck write SetSkipLibraryVersionCheck;
  end;

  { TSimpleModel }

  TSimpleModel = class
  private
    FDebug: boolean;
    FFieldPrefix: string;
    FFieldQuote: string;
    FRecordCountFromArray: Integer;
    AMessage: string;
    FConnector : TSQLConnector;
    FFieldList : TStrings;
    FTableName : string;
    FAliasName : string;
    FGroupField : string;
    FSelectField : string;
    FJoinList : TStrings;

    FScriptFieldNames,
    FScriptWhere,
    FScriptLimit,
    FScriptOrderBy : string;
    FUseSoftDelete: boolean;
    FSoftDeleteField: string;

    primaryKeyValue : string;
    FGenFields : TStringList;
    function getAliasName: string;
    function GetEOF: boolean;
    function GetLastInsertID: LongInt;
    function GetRecordCount: Longint;
    function getSQL: TStringlist;
    function GetTablePrefix: string;
    procedure setAliasName(AValue: string);
    procedure _queryPrepare;
    function  _queryOpen(const AUniDirectional: boolean = false):boolean;
    procedure DoAfterOpen(DataSet: TDataSet);
    procedure DoBeforeOpen(DataSet: TDataSet);

    function getFieldList_mysql: TStrings;
    function getFieldList_sqlite: TStrings;
    function getFieldList_postgresql: TStrings;

    function GetFieldList: TStrings;
    function GetFieldValue( FieldName: String): Variant;

    procedure GenerateScript;
  public
    FieldValueMap : TFieldValueMap;
    primaryKey : string;
    Data : TSQLQuery;
    StartTime, StopTime, ElapsedTime : Cardinal;
    constructor Create( const DefaultTableName:string=''; const pPrimaryKey:string='');
    destructor Destroy; override;
    procedure SetFieldValue( FieldName: String; AValue: Variant);
    property TableName : string Read FTableName write FTableName;
    // TableName/AliasName as identifiers for the active connector:
    // `db`.`groups` on MySQL and SQLite, "db"."groups" on PostgreSQL. A
    // name that already carries a quote, a space or a parenthesis is used
    // as given.
    function QuoteIdentifier(const AName: string): string;
    function QuotedTableName: string;
    function QuotedAliasName: string;
    property TablePrefix : string read GetTablePrefix;
    property FieldPrefix: string read FFieldPrefix write FFieldPrefix;
    property FieldQuote: string read FFieldQuote write FFieldQuote;
    property AliasName : string read getAliasName write setAliasName;
    Property Value[ FieldName: String] : Variant Read GetFieldValue Write SetFieldValue; default;
    Property FieldLists: TStrings Read GetFieldList;
    property RecordCount: Longint read GetRecordCount;
    property RecordCountFromArray: Integer read FRecordCountFromArray;
    property EOF: boolean read GetEOF;
    property LastInsertID: LongInt read GetLastInsertID;
    property Message: string read AMessage;
    // With UseSoftDelete, Delete marks rows through SoftDeleteField
    // (default is_deleted) and Find/All/GetAll skip marked rows.
    property UseSoftDelete: boolean read FUseSoftDelete write FUseSoftDelete;
    property SoftDeleteField: string read FSoftDeleteField write FSoftDeleteField;

    function ParamByName(Const AParamName : String) : TParam;
    property SQL : TStringlist read getSQL;
    // SQL string literal for the active connector: single quotes doubled,
    // backslashes doubled as well when the connector is MySQL
    function QuoteValue(const AValue: string): string;
    function Exec( ASQL:String): boolean;
    // Runs a script of several ';'-separated statements on the write
    // connection in one transaction; see SplitSQLStatements.
    function ExecScript( const ASQL: string): boolean;

    function All(const AUniDirectional: boolean = false):boolean;
    function GetAll( Limit: Integer = 0; Offset: Integer = 0; const AUniDirectional: boolean = false):boolean;
    function AsJsonArray(NoFieldName: boolean = False): TJSONArray;
    procedure FieldToJSON(const A: TJSONArray; const FieldName: string; const DefaultJSON: string = '{}');
    //function Get( where, order):boolean;

    function Find( const KeyIndex:integer):boolean;
    function Find( const KeyIndex:String):boolean;
    function Find( const Where:array of string; const Order:string = ''; const Limit:integer = 0; const CustomField:string=''; const Offset: integer = 0):boolean;
    function FindFirst( const Where:array of string; const Order:string = ''; const CustomField:string=''):boolean;

    procedure AddJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string; const AJoinWhere: string = '');
    procedure AddLeftJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string; const AJoinWhere: string = '');
    procedure AddInnerJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string; const AJoinWhere: string = '');
    procedure AddCustomJoin( const JointType:string; const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string; const AJoinWhere: string = '');

    procedure GroupBy( const GroupField:string);
    function RecordsTotalFiltered( const ASQL:string = ''; ForceClose: boolean = True):integer;


    procedure Clear;
    procedure New;
    procedure Close;
    function  Save( Where:string='';AutoCommit:boolean=True):boolean;
    function  Delete( Where:string='';AutoCommit:boolean=True):boolean;
    function  Delete( ID: LongInt): boolean;
    function  SoftDelete( Where:string='';AutoCommit:boolean=True):boolean;
    function  SoftDelete( ID: LongInt): boolean;
    function  Update( AutoCommit:boolean=True):boolean;

    procedure First;
    procedure Prior;
    procedure Next;
    procedure Last;
    procedure StartTransaction;
    procedure ReStartTransaction;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;


    // SQL Builder
    function Select( FieldNames:string): TSimpleModel;
    function Where( Conditions:string): TSimpleModel;
    function Where( ConditionFormat:string; ConditionArgs: array of const): TSimpleModel;
    function OrWhere( Conditions:string): TSimpleModel;
    function OrderBy( FieldNames:string): TSimpleModel;
    function Limit( LimitNumber:integer; Offset:integer=0): TSimpleModel;
    function Open( AUniDirectional:Boolean = False): Boolean;
  published
    property Debug: boolean read FDebug write FDebug;
    property Connector: TSQLConnector read FConnector;
  end;

function DataBaseInit( const RedirecURL:string = ''; const DBConfigName: string = ''):boolean;

// Connection character set of database/<ConnectionName>/charset.
// Not set (or ""): the default of the driver, "utf8mb4" for MySQL 5.5 and
// later, "utf8" for MySQL 4.x - 5.1 (utf8mb4 needs MySQL server 5.5.3),
// '' (the library default) for the other drivers.
// "none": '' as well, the charset of the server/library is kept.
function DataBaseCharSet( const ConnectionName: string; const ConnectorType: string): string;

// Standalone mode, called by fastplaz_handler around every request.
// Finished: rolls back what the request left uncommitted (as the end of a
// CGI process does) and closes the connections, so a dropped connection
// ("server has gone away") or an open transaction never reaches the next
// request. With DataBasePersistent the connections stay open; Started
// then closes them when they were idle for DataBaseIdleTimeout seconds or
// longer. A closed connection is opened again by DataBaseInit or by the
// next query. InitializeFastPlaz sets both from systems/server/db_persistent
// and systems/server/db_idle_timeout.
procedure DataBaseRequestStarted;
procedure DataBaseRequestFinished;

function  QueryOpenToJson( SQL: string; var ResultJSON: TJSONObject; const aParams : array of string; SQLCount: string = ''; Where: string = ''; Order: string =''; Limit: integer=0; Offset: integer=0; Echo: integer = 0; sParams: string =''; NoFieldName : boolean = True): boolean;
function  QueryOpenToJson( SQL: string; var ResultJSON: TJSONObject; NoFieldName : boolean = True): boolean;
function  QueryOpenToJson( SQL: string; var ResultArray: TJSONArray; NoFieldName : boolean = True): boolean;
function  QueryExecToJson( SQL: string; var ResultJSON: TJSONObject): boolean;
function  QueryExec( SQL: string):boolean;

// Splits a SQL script into statements on ';'. Quoted text ('..', "..",
// `..`, $tag$..$tag$) is kept as is, -- and /* */ comments are dropped
// (MySQL /*! ... */ hints are kept), and the ';' inside the BEGIN..END
// body of a CREATE TRIGGER does not end the statement. Client commands
// such as DELIMITER are not supported.
function  SplitSQLStatements( const ASQL: string): TStringArray;
// Runs every statement of ASQL with ExecuteDirect (SQLite and MySQL accept
// only one statement per call) and commits once. On the first failure the
// transaction is rolled back, AErrorMessage is set and Result is False.
function  ExecSQLScript( AConnector: TSQLConnector; const ASQL: string; out AErrorMessage: string): boolean;
// ExecSQLScript on the configured write connection
function  QueryExecScript( SQL: string): boolean;

function  DataToJSON( Data : TSQLQuery; var ResultJSON: TJSONArray; NoFieldName : boolean = True):boolean;

var
  QueryExecRowAffected: Integer;
  DataBasePersistent: boolean = False;
  DataBaseIdleTimeout: integer = 300;

implementation

uses common, config_lib, logutil_lib, dateutils;

var
  DB_Connector : TSQLConnector;
  DB_Transaction : TSQLTransaction;
  DB_LibLoader : TSQLDBLibraryLoader;
  DB_LastRequest : TDateTime = 0;

  DB_Connector_Write : TSQLConnector;
  DB_Transaction_Write : TSQLTransaction;
  DB_LibLoader_Write : TSQLDBLibraryLoader;

{ TFPSQLConnector }

procedure TFPSQLConnector.ApplySkipLibraryVersionCheck;
begin
  // SkipLibraryVersionCheck is a published property of every TMySQLxxConnection,
  // so RTTI covers all MySQL versions without naming each class.
  if Assigned(Proxy) and IsPublishedProp(Proxy, 'SkipLibraryVersionCheck') then
    SetOrdProp(Proxy, 'SkipLibraryVersionCheck', Ord(FSkipLibraryVersionCheck));
end;

procedure TFPSQLConnector.SetSkipLibraryVersionCheck(AValue: boolean);
begin
  FSkipLibraryVersionCheck := AValue;
  ApplySkipLibraryVersionCheck;
end;

procedure TFPSQLConnector.CreateProxy;
begin
  inherited CreateProxy;
  ApplySkipLibraryVersionCheck;
end;

function DataBaseCharSet(const ConnectionName: string;
  const ConnectorType: string): string;
var
  driver: string;
begin
  Result := Trim(string( Config.GetValue( UnicodeString( format( _DATABASE_CHARSET, [ConnectionName])), UnicodeString(''))));
  if SameText(Result, 'none') then
    Exit('');
  if Result <> '' then
    Exit;

  driver := LowerCase(Trim(ConnectorType));
  if Pos('mysql', driver) <> 1 then
    Exit('');  // PostgreSQL, SQLite, ...: the library default
  if (Pos('mysql 4.', driver) = 1) or (driver = 'mysql 5.0') or (driver = 'mysql 5.1') then
    Result := 'utf8'
  else
    Result := 'utf8mb4';
end;

function DataBaseInit(const RedirecURL: string; const DBConfigName: string
  ): boolean;
var
  s, workDir : string;
begin
  Result := False;
  AppData.useDatabase := True;

  // already connected: keep the active configuration untouched (in
  // standalone mode the flags are reset for every request)
  if DB_Connector.Connected then
  begin
    AppData.databaseActive := True;
    Result := True;
    Exit;
  end;

  // multidb - prepare
  AppData.databaseRead := string( Config.GetValue( _DATABASE_OPTIONS_READ, 'default'));
  AppData.databaseWrite := string( Config.GetValue( UnicodeString( _DATABASE_OPTIONS_WRITE), UnicodeString( AppData.databaseRead)));
  if not DBConfigName.IsEmpty then
  begin
    AppData.databaseRead := DBConfigName;
    AppData.databaseWrite := DBConfigName;
  end;
  // table prefix and version check belong to the selected configuration
  AppData.tablePrefix := string( Config.GetValue( UnicodeString( format( _DATABASE_TABLE_PREFIX, [AppData.databaseRead])), UnicodeString('')));
  AppData.databaseVersionCheck := Config.GetValue( UnicodeString( format( _DATABASE_VERSION_CHECK, [AppData.databaseRead])), True);

  // client library (optional): an empty value uses the system library
  s := string( Config.GetValue( UnicodeString( format( _DATABASE_LIBRARY, [AppData.databaseRead])), UnicodeString('')));
  // loaded once per process; a reconnect does not load it again
  if (s <> '') and (not DB_LibLoader.Enabled) then
  begin
    workDir := GetCurrentDir;
    // resolve the library path (relative to the current directory or absolute)
    if not SetCurrentDir( ExtractFilePath( s)) then
    begin
      DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ AppData.databaseRead, s]));
    end;
    s := GetCurrentDir + DirectorySeparator + ExtractFileName( s);
    try
      DB_LibLoader.ConnectionType:= string( Config.GetValue( UnicodeString( format( _DATABASE_DRIVER, [AppData.databaseRead])), UnicodeString('')));
      DB_LibLoader.LibraryName:= s;
      DB_LibLoader.Enabled:= True;
      DB_LibLoader.LoadLibrary;
    except
      on E: Exception do begin
        LogUtil.Add( 'Database Init: Load Library, ' + E.Message);
        if RedirecURL = '' then
        begin
          //TODO: check if database library cannot loaded
          DisplayError( 'Database Init: Load Library, ' + E.Message);
        end
        else
          Redirect( RedirecURL);
      end;
    end;
    // back to the working directory (the application root; with --root it
    // is not the directory of the executable)
    SetCurrentDir(workDir);
  end;

  if not DB_Connector.Connected then
  begin
    DB_Connector.HostName:= string( Config.GetValue( UnicodeString( format( _DATABASE_HOSTNAME, [AppData.databaseRead])), UnicodeString('localhost')));
    DB_Connector.ConnectorType := string( Config.GetValue( UnicodeString( format( _DATABASE_DRIVER, [AppData.databaseRead])), UnicodeString('')));
    DB_Connector.UserName:= string( Config.GetValue( UnicodeString( format( _DATABASE_USERNAME, [AppData.databaseRead])), UnicodeString('')));
    DB_Connector.Password:= string( Config.GetValue( UnicodeString( format( _DATABASE_PASSWORD, [AppData.databaseRead])), UnicodeString('')));
    DB_Connector.DatabaseName:= string( Config.GetValue( UnicodeString( format( _DATABASE_DATABASENAME, [AppData.databaseRead])), UnicodeString('test')));
    if Config.GetValue( UnicodeString( format( _DATABASE_PORT, [AppData.databaseRead])), UnicodeString('')) <> '' then
      DB_Connector.Params.Values['port'] := string( Config.GetValue( UnicodeString( format( _DATABASE_PORT, [AppData.databaseRead])), UnicodeString('')));
    //tabletype := Config.GetValue( _DATABASE_TABLETYPE, '');
    // default utf8mb4 on MySQL: no "SET CHARACTER SET" needed in the app
    DB_Connector.CharSet := DataBaseCharSet(AppData.databaseRead, DB_Connector.ConnectorType);

    // Skip Library Version Check from MySQL.
    // The proxy connection does not exist before Open, so the flag is kept on
    // the connector and applied when the proxy is created.
    if DB_Connector is TFPSQLConnector then
      TFPSQLConnector(DB_Connector).SkipLibraryVersionCheck := not AppData.databaseVersionCheck;

    try
      DB_Connector.Open;
      AppData.databaseActive := True;
      Result := True;
    except
      on E: Exception do
      begin
        if RedirecURL = '' then
        begin
          DisplayError( 'Database Error: '+ E.Message)
        end
        else
          Redirect( RedirecURL);
      end;
    end;
  end;//-- if not DB_Connector.Connected then

end;

procedure EndConnection(AConnector: TSQLConnector; AClose: boolean);
begin
  if (not Assigned(AConnector)) or (not AConnector.Connected) then
    Exit;
  try
    if Assigned(AConnector.Transaction) and AConnector.Transaction.Active then
      AConnector.Transaction.Rollback;
  except
    on E: Exception do
      LogUtil.Add('end of request, rollback: ' + E.Message, 'database');
  end;
  if AClose then
  try
    AConnector.Close;
  except
    on E: Exception do
      LogUtil.Add('end of request, close: ' + E.Message, 'database');
  end;
end;

procedure DataBaseRequestStarted;
begin
  if not DataBasePersistent then
    Exit;
  if (DataBaseIdleTimeout > 0) and (DB_LastRequest > 0) and
    (MilliSecondsBetween(Now, DB_LastRequest) >= Int64(DataBaseIdleTimeout) * 1000) then
  begin
    EndConnection(DB_Connector, True);
    EndConnection(DB_Connector_Write, True);
  end;
end;

procedure DataBaseRequestFinished;
begin
  EndConnection(DB_Connector, not DataBasePersistent);
  EndConnection(DB_Connector_Write, not DataBasePersistent);
  DB_LastRequest := Now;
end;

function QueryOpenToJson(SQL: string; var ResultJSON: TJSONObject;
  const aParams: array of string; SQLCount: string; Where: string;
  Order: string; Limit: integer; Offset: integer; Echo: integer;
  sParams: string; NoFieldName: boolean): boolean;
var
  q: TSQLQuery;
  Data: TJSONArray;
  i, iTotal, iFiltered: integer;
begin

  Result := False;
  q := TSQLQuery.Create(nil);
  q.UniDirectional := True;
  q.DataBase := DB_Connector;


  try
    q.SQL.Text := SQLCount;
    q.Prepare;
    q.Open;
    iTotal := q.Fields[0].AsInteger;
    q.Close;

    q.SQL.Text := SQLCount;
    if Where <> '' then
    begin
      q.SQL.Text := q.SQL.Text + Where;

      for i := low(aParams) to high(aParams) do
      begin
        q.ParamByName(aParams[i]).AsString := '%' + sParams + '%';
      end;
    end;
    q.Prepare;
    q.Open;
    iFiltered := q.Fields[0].AsInteger;
    q.Close;

    q.SQL.Text := SQL;
    if Where <> '' then
      q.SQL.Text := q.SQL.Text + Where;
    if Order <> '' then
      q.SQL.Text := q.SQL.Text + ' ORDER BY ' + Order;
    if Limit > 0 then
      q.SQL.Text := q.SQL.Text + ' LIMIT ' + IntToStr(Limit);
    if Offset > 0 then
      q.SQL.Text := q.SQL.Text + ' OFFSET ' + IntToStr(Offset);
    q.Prepare;

    if Where <> '' then
      for i := low(aParams) to high(aParams) do
      begin
        q.ParamByName(aParams[i]).AsString := '%' + sParams + '%';
      end;
    q.Open;

    Data := TJSONArray.Create();
    DataToJSON(q, Data, NoFieldName);
    ResultJSON.Add('draw', Echo);
    ResultJSON.Add('recordsTotal', iTotal);
    ResultJSON.Add('recordsFiltered', iFiltered);
    ResultJSON.Add('data', Data);
    Result := True;
  except
    on E: Exception do
    begin
      ResultJSON.Add('msg', E.Message);
    end;
  end;

  {$ifdef debug}
  ResultJSON.Add('sql', SQL);
  {$endif}
  FreeAndNil(q);
end;

function QueryOpenToJson(SQL: string; var ResultJSON: TJSONObject;
  NoFieldName: boolean): boolean;
var
  q : TSQLQuery;
  data : TJSONArray;
begin
  Result := False;
  q := TSQLQuery.Create(nil);
  q.UniDirectional:=True;
  q.DataBase := DB_Connector;
  q.SQL.Text:= SQL;

  try
    q.Open;
    data := TJSONArray.Create();
    DataToJSON( q, data, NoFieldName);
    //ResultJSON.Add( 'count', q.RowsAffected);
    ResultJSON.Add( 'count', data.Count);
    ResultJSON.Add( 'data', data);
    Result := True;
  except
    on E: Exception do begin
      ResultJSON.Add( 'msg', E.Message);
    end;
  end;

  {$ifdef debug}
  ResultJSON.Add( 'sql', SQL);
  {$endif}
  FreeAndNil( q);
end;

function QueryOpenToJson(SQL: string; var ResultArray: TJSONArray;
  NoFieldName: boolean): boolean;
var
  q : TSQLQuery;
begin
  Result := False;
  q := TSQLQuery.Create(nil);
  q.UniDirectional:=True;
  q.DataBase := DB_Connector;
  q.SQL.Text:= SQL;

  try
    q.Open;
    DataToJSON( q, ResultArray, NoFieldName);
    Result := True;
  except
    on E: Exception do begin
      DisplayError(e.Message);
    end;
  end;

  {$ifdef debug}
  {$endif}
  FreeAndNil( q);
end;



function DatabaseSecond_Prepare( const ConnectionName:string = 'default'; Mode:string = ''):TSQLConnector;
var
  s, workDir : string;
begin
  Result := nil; //DB_Connector_Write
  if ((Assigned( DB_Connector_Write)) and (Mode = 'write')) then
  begin
    Result := DB_Connector_Write;
    Exit;
  end;

  // client library (optional): resolved like DataBaseInit does, so a relative
  // as well as an absolute path is accepted
  s := string( Config.GetValue( UnicodeString( format( _DATABASE_LIBRARY, [ConnectionName])), UnicodeString('')));
  if (s <> '') and not (Assigned(DB_LibLoader_Write) and DB_LibLoader_Write.Enabled) then
  begin
    workDir := GetCurrentDir;
    if not SetCurrentDir( ExtractFilePath( s)) then
    begin
      DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ ConnectionName, s]));
    end;
    s := GetCurrentDir + DirectorySeparator + ExtractFileName( s);
    if not FileExists( s) then
    begin
      SetCurrentDir(workDir);
      DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ ConnectionName, s]));
    end;

    if not Assigned( DB_LibLoader_Write) then DB_LibLoader_Write := TSQLDBLibraryLoader.Create( nil);
    try
      DB_LibLoader_Write.ConnectionType:= string( Config.GetValue( UnicodeString( format( _DATABASE_DRIVER, [ConnectionName])), UnicodeString('')));
      DB_LibLoader_Write.LibraryName:= s;
      DB_LibLoader_Write.Enabled:= True;
      DB_LibLoader_Write.LoadLibrary;
    except
      on E: Exception do begin
          DisplayError( 'Database Init ('+ConnectionName+'): Load Library, '+E.Message)
      end;
    end;
    SetCurrentDir(workDir);
  end;

  if not Assigned( DB_Connector_Write) then begin
    DB_Transaction_Write := TSQLTransaction.Create( nil);
    DB_Connector_Write := TFPSQLConnector.Create( nil);
    DB_Connector_Write.Transaction := DB_Transaction_Write;
  end;
  DB_Connector_Write.HostName:= string( Config.GetValue( UnicodeString( format( _DATABASE_HOSTNAME, [ConnectionName])), UnicodeString('localhost')));
  DB_Connector_Write.ConnectorType := string( Config.GetValue( UnicodeString( format( _DATABASE_DRIVER, [ConnectionName])), UnicodeString('')));
  DB_Connector_Write.UserName:= string( Config.GetValue( UnicodeString( format( _DATABASE_USERNAME, [ConnectionName])), UnicodeString('')));
  DB_Connector_Write.Password:= string( Config.GetValue( UnicodeString( format( _DATABASE_PASSWORD, [ConnectionName])), UnicodeString('')));
  DB_Connector_Write.DatabaseName:= string( Config.GetValue( UnicodeString( format( _DATABASE_DATABASENAME, [ConnectionName])), UnicodeString('test')));
  DB_Connector_Write.CharSet := DataBaseCharSet(ConnectionName, DB_Connector_Write.ConnectorType);
  if DB_Connector_Write is TFPSQLConnector then
    TFPSQLConnector(DB_Connector_Write).SkipLibraryVersionCheck :=
      not Config.GetValue( UnicodeString( format( _DATABASE_VERSION_CHECK, [ConnectionName])), True);

  if Config.GetValue( UnicodeString( format( _DATABASE_PORT, [ConnectionName])), UnicodeString('')) <> '' then
    DB_Connector_Write.Params.Values['port'] := string( Config.GetValue( UTF8Decode(format( _DATABASE_PORT, [ConnectionName])), UnicodeString('')));
  //tabletype := Config.GetValue( _DATABASE_TABLETYPE, '');

  //log database

  try
    DB_Connector_Write.Open;
  except
    on E: Exception do
    begin
      DisplayError( 'Database ("'+ConnectionName+'") Error : '+ E.Message)
    end;
  end;

  Result := DB_Connector_Write;
end;

function QueryExecToJson(SQL: string; var ResultJSON: TJSONObject): boolean;
var
  q : TSQLQuery;
begin
  Result:=false;
  QueryExecRowAffected := -1;
  q := TSQLQuery.Create(nil);
  q.UniDirectional:=True;
  if AppData.databaseRead = AppData.databaseWrite then
    q.DataBase := DB_Connector
  else
  begin
    q.DataBase := DatabaseSecond_Prepare( AppData.databaseWrite, 'write');
    if q.DataBase = nil then
    begin
      DisplayError( format( _ERR_DATABASE_CANNOT_CONNECT, [ AppData.databaseWrite]));
    end;
  end;
  try
    q.SQL.Text:= SQL;
    q.ExecSQL;
    ResultJSON.Add( 'count', q.RowsAffected);
    QueryExecRowAffected := q.RowsAffected;
    TSQLConnector( q.DataBase).Transaction.Commit;
    //DB_Connector.Transaction.Commit;
    Result:=True;
  except
    on E: Exception do begin
      ResultJSON.Add( 'msg', E.Message);
    end;
  end;
  {$ifdef debug}
  ResultJSON.Add( 'sql', SQL);
  {$endif}
  FreeAndNil(q);
end;

function QueryExec(SQL: string): boolean;
var
  q : TSQLQuery;
begin
  Result := false;
  QueryExecRowAffected := -1;
  q := TSQLQuery.Create(nil);
  q.UniDirectional:=True;
  if AppData.databaseRead = AppData.databaseWrite then
    q.DataBase := DB_Connector
  else
  begin
    q.DataBase := DatabaseSecond_Prepare( AppData.databaseWrite, 'write');
    if q.DataBase = nil then
    begin
      DisplayError( format( _ERR_DATABASE_CANNOT_CONNECT, [ AppData.databaseWrite]));
    end;
  end;
  try
    q.SQL.Text:= SQL;
    q.ExecSQL;
    QueryExecRowAffected := q.RowsAffected;
    TSQLConnector( q.DataBase).Transaction.Commit;
    //DB_Connector.Transaction.Commit;
    Result:=True;
  except
    on E: Exception do begin
      if AppData.debug then
        LogUtil.Add( E.Message, 'DB');
    end;
  end;
  {$ifdef debug}
  {$endif}
  FreeAndNil(q);
end;

function IsSQLIdentChar(const C: char): boolean;
begin
  Result := C in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;

// ABare is the statement so far with quoted text and comments blanked out.
// True while a CREATE statement (trigger, procedure) is inside BEGIN..END,
// so a ';' there belongs to the body. CASE..END is counted as well, and
// MySQL "END IF/LOOP/WHILE/REPEAT" closes a block that was never opened.
function IsInsideSQLBlock(const ABare: string): boolean;
var
  i, n, start, depth: integer;
  w, prev: string;
  first: boolean;
begin
  Result := False;
  depth := 0;
  first := True;
  prev := '';
  i := 1;
  n := Length(ABare);
  while i <= n do
  begin
    if not IsSQLIdentChar(ABare[i]) then
    begin
      Inc(i);
      Continue;
    end;
    start := i;
    while (i <= n) and IsSQLIdentChar(ABare[i]) do
      Inc(i);
    w := UpperCase(Copy(ABare, start, i - start));
    if first then
    begin
      if w <> 'CREATE' then
        Exit;
      first := False;
    end;
    if w = 'BEGIN' then
      Inc(depth)
    else if w = 'CASE' then
    begin
      if prev <> 'END' then
        Inc(depth);
    end
    else if w = 'END' then
      Dec(depth)
    else if (prev = 'END') and ((w = 'IF') or (w = 'LOOP') or (w = 'WHILE')
      or (w = 'REPEAT')) then
      Inc(depth);
    prev := w;
  end;
  Result := depth > 0;
end;

function SplitSQLStatements(const ASQL: string): TStringArray;
var
  i, j, k, n, count: integer;
  c: char;
  cur, bare, tag: string;

  procedure AddStatement;
  var
    s: string;
  begin
    s := Trim(cur);
    cur := '';
    bare := '';
    if s = '' then
      Exit;
    if count = Length(Result) then
      SetLength(Result, count * 2 + 4);
    Result[count] := s;
    Inc(count);
  end;

begin
  Result := nil;
  count := 0;
  cur := '';
  bare := '';
  n := Length(ASQL);
  i := 1;
  while i <= n do
  begin
    c := ASQL[i];

    // '..', "..", `..`: a doubled quote stays inside the literal
    if c in ['''', '"', '`'] then
    begin
      j := i + 1;
      while j <= n do
      begin
        if ASQL[j] = c then
        begin
          if (j < n) and (ASQL[j + 1] = c) then
            Inc(j, 2)
          else
            Break;
        end
        else
          Inc(j);
      end;
      if j > n then
        j := n;
      cur := cur + Copy(ASQL, i, j - i + 1);
      bare := bare + ' ';
      i := j + 1;
      Continue;
    end;

    // -- line comment
    if (c = '-') and (i < n) and (ASQL[i + 1] = '-') then
    begin
      while (i <= n) and not (ASQL[i] in [#10, #13]) do
        Inc(i);
      cur := cur + ' ';
      bare := bare + ' ';
      Continue;
    end;

    // /* block comment */, MySQL /*! hint */ is kept
    if (c = '/') and (i < n) and (ASQL[i + 1] = '*') then
    begin
      j := Pos('*/', ASQL, i + 2);
      if j = 0 then
        j := n
      else
        j := j + 1;
      if (i + 2 <= n) and (ASQL[i + 2] = '!') then
        cur := cur + Copy(ASQL, i, j - i + 1)
      else
        cur := cur + ' ';
      bare := bare + ' ';
      i := j + 1;
      Continue;
    end;

    // PostgreSQL $$..$$ / $tag$..$tag$ body
    if (c = '$') and not ((i > 1) and IsSQLIdentChar(ASQL[i - 1])) then
    begin
      j := i + 1;
      while (j <= n) and IsSQLIdentChar(ASQL[j]) do
        Inc(j);
      if (j <= n) and (ASQL[j] = '$')
        and not ((j > i + 1) and (ASQL[i + 1] in ['0'..'9'])) then
      begin
        tag := Copy(ASQL, i, j - i + 1);
        k := Pos(tag, ASQL, j + 1);
        if k = 0 then
          k := n
        else
          k := k + Length(tag) - 1;
        cur := cur + Copy(ASQL, i, k - i + 1);
        bare := bare + ' ';
        i := k + 1;
        Continue;
      end;
    end;

    if (c = ';') and not IsInsideSQLBlock(bare) then
    begin
      AddStatement;
      Inc(i);
      Continue;
    end;

    cur := cur + c;
    bare := bare + c;
    Inc(i);
  end;
  AddStatement;
  SetLength(Result, count);
end;

function ExecSQLScript(AConnector: TSQLConnector; const ASQL: string;
  out AErrorMessage: string): boolean;
var
  statements: TStringArray;
  i: integer;
begin
  Result := False;
  AErrorMessage := '';
  if AConnector = nil then
  begin
    AErrorMessage := 'no database connection';
    Exit;
  end;
  statements := SplitSQLStatements(ASQL);
  i := 0;
  try
    while i <= High(statements) do
    begin
      AConnector.ExecuteDirect(statements[i]);
      Inc(i);
    end;
    if Assigned(AConnector.Transaction) and AConnector.Transaction.Active then
      AConnector.Transaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      AErrorMessage := Format('statement %d: %s', [i + 1, E.Message]);
      try
        if Assigned(AConnector.Transaction) and AConnector.Transaction.Active then
          AConnector.Transaction.Rollback;
      except
      end;
    end;
  end;
end;

// the connector QueryExec and Save use for writes, nil when the second
// connection cannot be opened
function WriteConnector(ADefault: TSQLConnector): TSQLConnector;
begin
  if AppData.databaseRead = AppData.databaseWrite then
    Result := ADefault
  else
  begin
    Result := DatabaseSecond_Prepare(AppData.databaseWrite, 'write');
    if Result = nil then
      DisplayError(format(_ERR_DATABASE_CANNOT_CONNECT, [AppData.databaseWrite]));
  end;
end;

function QueryExecScript(SQL: string): boolean;
var
  msg: string;
begin
  Result := ExecSQLScript(WriteConnector(DB_Connector), SQL, msg);
  if (not Result) and AppData.debug then
    LogUtil.Add(msg, 'DB');
end;

function DataToJSON(Data: TSQLQuery; var ResultJSON: TJSONArray;
  NoFieldName: boolean): boolean;
var
  item : TJSONObject;
  item_array : TJSONArray;
  field_name : string;
  i,j:integer;
begin
  Result:=False;
  i:=1;
  item := nil;
  item_array := nil;
  try
    Data.First;
    while not Data.EOF do
    begin
      // create only the container that is added to the result,
      // the other one would never be freed
      if NoFieldName then
        item_array := TJSONArray.Create
      else
        item := TJSONObject.Create();
      for j:=0 to Data.FieldCount-1 do
      begin
        field_name:= Data.FieldDefs.Items[j].Name;
        if NoFieldName then
        begin
          if Data.FieldDefs.Items[j].DataType = ftDateTime then
          begin
            if (Data.FieldByName(field_name).AsDateTime) = 0 then
              item_array.add( '')
            else
              item_array.add( FormatDateTime('YYYY/MM/DD HH:nn:ss', Data.FieldByName(field_name).AsDateTime));
          end
          else if Data.FieldDefs.Items[j].DataType = ftAutoInc then
          begin
            item_array.add( Data.FieldByName(field_name).AsInteger);
          end
          else if Data.FieldDefs.Items[j].DataType = ftInteger then
          begin
            item_array.add( Data.FieldByName(field_name).AsInteger);
          end
          else if Data.FieldDefs.Items[j].DataType = ftSmallint then
          begin
            item_array.add( Data.FieldByName(field_name).AsInteger);
          end
          else if Data.FieldDefs.Items[j].DataType = ftLargeint then
          begin
            item_array.add( Data.FieldByName(field_name).AsLargeInt);
          end
          else if Data.FieldDefs.Items[j].DataType = ftFloat then
          begin
            item_array.add( Data.FieldByName(field_name).AsFloat);
          end
          else if Data.FieldDefs.Items[j].DataType = ftUnknown then
          begin
            try
              item_array.add( Data.FieldByName(field_name).AsString);
            except
              item_array.add( '');
            end;
          end
          else
            item_array.add( Data.FieldByName(field_name).AsString);
        end
        else
        begin // with fieldname

          if Data.FieldDefs.Items[j].DataType = ftDateTime then
          begin
            if (Data.FieldByName(field_name).AsDateTime) = 0 then
              item.add( field_name, '')
            else
              item.add( field_name, FormatDateTime('YYYY/MM/DD HH:nn:ss', Data.FieldByName(field_name).AsDateTime));
          end
          else if Data.FieldDefs.Items[j].DataType = ftAutoInc then
          begin
            item.Add(field_name, Data.FieldByName(field_name).AsInteger);
          end
          else if Data.FieldDefs.Items[j].DataType = ftInteger then
          begin
            item.Add(field_name, Data.FieldByName(field_name).AsInteger);
          end
          else if Data.FieldDefs.Items[j].DataType = ftSmallint then
          begin
            item.Add(field_name, Data.FieldByName(field_name).AsInteger);
          end
          else if Data.FieldDefs.Items[j].DataType = ftLargeint then
          begin
            item.Add(field_name, Data.FieldByName(field_name).AsLargeInt);
          end
          else if Data.FieldDefs.Items[j].DataType = ftFloat then
          begin
            item.Add(field_name, Data.FieldByName(field_name).AsFloat);
          end
          else if Data.FieldDefs.Items[j].DataType = ftUnknown then
          begin
            try
              item.Add(field_name, Data.FieldByName(field_name).AsString);
            except
              item.Add(field_name, '');
            end;
          end
          else
            item.Add(field_name, Data.FieldByName(field_name).AsString);

        end;//-- if nofieldname
      end;
      if NoFieldName then
        ResultJSON.Add( item_array)
      else
        ResultJSON.Add( item);
      i:=i+1;
      Data.Next;
    end;
    Data.First;
    Result:=True;
  except
    on E: Exception do begin
      die( E.Message);
    end;
  end;
end;


{ TSimpleModel }

procedure TSimpleModel._queryPrepare;
begin
  FRecordCountFromArray := 0;
  FieldLists;
  if Data.Active then Data.Close;
end;

function TSimpleModel.GetRecordCount: Longint;
begin
  Result := -1;
  if FRecordCountFromArray > 0 then
  begin
    Result := FRecordCountFromArray;
    Exit;
  end;
  if not Data.Active then Exit;
  Result := Data.RecordCount;
end;

function TSimpleModel.getSQL: TStringlist;
begin
  Result := Data.SQL;
end;

function TSimpleModel.GetTablePrefix: string;
begin
  Result := AppData.tablePrefix;
end;

function TSimpleModel.QuoteIdentifier(const AName: string): string;
var
  q: string;
  parts: TStringArray;
  i: integer;
begin
  Result := AName;
  if AName = '' then
    Exit;
  q := FFieldQuote;
  if (q = '`') and Assigned(FConnector)
    and (Pos('postgre', LowerCase(FConnector.ConnectorType)) > 0) then
    q := '"';
  if (q = '') or (Pos(q, AName) > 0) or (Pos(' ', AName) > 0) or (Pos('(', AName) > 0) then
    Exit;
  parts := AName.Split(['.']);
  Result := '';
  for i := 0 to Length(parts) - 1 do
  begin
    if i > 0 then
      Result := Result + '.';
    Result := Result + q + parts[i] + q;
  end;
end;

function TSimpleModel.QuotedTableName: string;
begin
  Result := QuoteIdentifier(FTableName);
end;

function TSimpleModel.QuotedAliasName: string;
begin
  Result := QuoteIdentifier(FAliasName);
end;

procedure TSimpleModel.setAliasName(AValue: string);
begin
  FAliasName := AValue;
end;

function TSimpleModel.getAliasName: string;
begin
  Result := FAliasName;
end;

function TSimpleModel.GetEOF: boolean;
begin
  Result := Data.EOF;
end;

function TSimpleModel.GetLastInsertID: LongInt;
var
  s : string;
begin
  Result := 0;
  try
    if Data.Active then Data.Close;
    s := LowerCase( TSQLConnector( Data.DataBase).ConnectorType);
    if Pos('postgre', s) > 0 then
      Data.SQL.Text := 'SELECT lastval() AS lastid'
    else if Pos('sqlite', s) > 0 then
      Data.SQL.Text := 'SELECT last_insert_rowid() AS lastid'
    else
      // no FROM clause: with a table name it returns one row per table row
      Data.SQL.Text := 'SELECT LAST_INSERT_ID() AS lastid';
    Data.Open;
    if Data.RecordCount > 0 then
      Result := Data.FieldValues['lastid'];
  except
  end;
end;

function TSimpleModel._queryOpen(const AUniDirectional: boolean): boolean;
var
  s : string;
begin
  Result := False;
  FRecordCountFromArray := 0;
  try
    if Data.Active then Data.Close;
    Data.UniDirectional := AUniDirectional;
    Data.Open;
    if not AUniDirectional then
    begin
      Last;
      First;
    end;
    if Data.RecordCount > 0 then
      Result := True;
  except
    on E: Exception do
    begin
      if ((AppData.debug) and (AppData.debugLevel <= 1)) then
      begin
        LogUtil.add( E.Message, 'DB');
        LogUtil.add( Data.SQL.Text, 'DB');
        s := #10'<pre>'#10+Data.SQL.Text+#10'</pre>'#10;
      end;
      die( E.Message + s);
      DisplayError( E.Message + s);
    end;
  end;
end;

procedure TSimpleModel.DoAfterOpen(DataSet: TDataSet);
var
  s : string;
begin
  StopTime:= _GetTickCount;
  ElapsedTime:= StopTime - StartTime;
  if not AppData.debug then Exit;
  s := StringReplace( 'sql||'+i2s(ElapsedTime)+'||'+Data.SQL.Text, #13#10, #9#9#9, [rfReplaceAll]);
  _DebugInfo.Add( s);
end;

procedure TSimpleModel.DoBeforeOpen(DataSet: TDataSet);
begin
  StartTime:= _GetTickCount;
end;

function TSimpleModel.getFieldList_mysql: TStrings;
begin
  Data.SQL.Text:= 'SHOW COLUMNS FROM ' + QuotedTableName;
  Data.Open;
  FSelectField := '';
  while not Data.Eof do begin
    FFieldList.Add( FTableName + '.' +Data.FieldByName('Field').AsString);
    FSelectField := FSelectField + ',' + FTableName + '.' + Data.FieldByName('Field').AsString;
    Data.Next;
  end;
  Data.Close;
  FSelectField := Copy( FSelectField, 2, Length(FSelectField)-1);
  Result := FFieldList;
end;

function TSimpleModel.getFieldList_sqlite: TStrings;
begin
  Data.SQL.Text:= 'PRAGMA table_info('+QuotedTableName+');';
  Data.Open;
  FSelectField := '';
  while not Data.Eof do begin
    FFieldList.Add( FTableName + '.' +Data.FieldByName('name').AsString);
    FSelectField := FSelectField + ',' + FTableName + '.' + Data.FieldByName('name').AsString;
    Data.Next;
  end;
  Data.Close;
  FSelectField := Copy( FSelectField, 2, Length(FSelectField)-1);
  Result := FFieldList;
end;

function TSimpleModel.getFieldList_postgresql: TStrings;
begin
  Data.SQL.Text:= 'SELECT column_name as name FROM information_schema.columns WHERE table_name = ''' + FTableName + '''';
  Data.Open;
  FSelectField := '';
  while not Data.Eof do begin
    FFieldList.Add( FTableName + '.' +Data.FieldByName('name').AsString);
    FSelectField := FSelectField + ',' + FTableName + '.' + Data.FieldByName('name').AsString;
    Data.Next;
  end;
  Data.Close;
  FSelectField := Copy( FSelectField, 2, Length(FSelectField)-1);
  Result := FFieldList;
end;

function TSimpleModel.GetFieldList: TStrings;
var
  s : string;
begin
  If Assigned(FFieldList) then begin
    Result:=FFieldList;
  end else begin
    Result := Nil;
    FFieldList:=TStringList.Create;
    if Data.Active then Data.Close;

    //TODO: create auto query, depend on databasetype
    s := lowercase(DB_Connector.ConnectorType);
    if (Pos('mysql', s) > 0) then Result := getFieldList_mysql;
    if (Pos('sqlite3', s) > 0) then Result := getFieldList_sqlite;
    if (Pos('postgre', s) > 0) then Result := getFieldList_postgresql;
    // unknown driver or no column found: select everything instead of
    // generating "SELECT  FROM table"
    if FSelectField = '' then
      FSelectField := FTableName + '.*';
    Result := FFieldList;
  end;
end;

function TSimpleModel.GetFieldValue(FieldName: String): Variant;
begin
  if not Data.Active then Exit;
  try
    if Data.FieldByName( FFieldPrefix + FieldName).AsVariant = null then
      Result := ''
    else
      Result := Data.FieldByName( FFieldPrefix + FieldName).AsVariant;
  except
    on E: Exception do begin
      die( 'getFieldValue ['+FieldName+']: ' + e.Message);
    end;
  end;
end;

procedure TSimpleModel.SetFieldValue(FieldName: String; AValue: Variant);
var
  i : integer;
begin
  if not Assigned( FGenFields) then
  begin
    FGenFields := TStringList.Create;
    FieldValueMap := TFieldValueMap.Create;
  end;
  // assigning the same field twice must not produce a duplicate column in Save
  i := FGenFields.IndexOf( FieldName);
  if i = -1 then
    FGenFields.Add( FieldName)
  else
    FieldName := FGenFields[i];
  FieldValueMap[FieldName] := AValue;
end;

constructor TSimpleModel.Create(const DefaultTableName: string; const pPrimaryKey: string);
var
  i : integer;
  s : string;
begin
  FDebug := False;
  FScriptFieldNames := '';
  FScriptWhere := '';
  FScriptLimit := '';
  FScriptOrderBy := '';
  FFieldPrefix := '';
  FFieldQuote := '`';
  primaryKey := pPrimaryKey;
  primaryKeyValue := '';
  AMessage := '';
  FRecordCountFromArray := 0;
  FConnector := DB_Connector;
  if DefaultTableName = '' then
  begin
    for i:=2 to length( ToString) do
    begin
      if (ToString[i] in ['A'..'Z']) then
      begin
        if FTableName <> '' then
          FTableName := FTableName + '_' + LowerCase( ToString[i])
        else
          FTableName := FTableName + LowerCase( ToString[i])
      end
      else
        FTableName := FTableName + ToString[i];
    end;
    FTableName:= copy( FTableName, 1, Length(FTableName)-6) + 's';
  end else
    FTableName:= DefaultTableName;
  FTableName:= AppData.tablePrefix+FTableName;
  FAliasName := FTableName.Replace('.', '_');

  // primary key name
  if pPrimaryKey = '' then
  begin
    s := copy( ToString, 2, Length(ToString)-6);
    primaryKey := '';
    for i:=1 to length( s) do
    begin
      if (s[i] in ['A'..'Z']) then
        primaryKey := primaryKey + s[i];
    end;
    primaryKey := primaryKey.ToLower + 'id';
  end;

  FJoinList := TStringList.Create;
  {$ifdef zeos}
  still not supported
  {$else}
  FGenFields := nil;
  FieldValueMap := nil;
  FUseSoftDelete := False;
  FSoftDeleteField := 'is_deleted';
  Data := TSQLQuery.Create(nil);
  Data.UniDirectional:=True;
  Data.DataBase := DB_Connector;
  Data.AfterOpen:= @DoAfterOpen;
  Data.BeforeOpen:= @DoBeforeOpen;
  {$endif}
end;

destructor TSimpleModel.Destroy;
begin
  if Data.Active then Data.Close;
  FreeAndNil( FJoinList);
  if Assigned( FFieldList) then FreeAndNil( FFieldList);
  if Assigned( FGenFields) then FreeAndNil( FGenFields);
  if Assigned( FieldValueMap) then FreeAndNil( FieldValueMap);
  FreeAndNil( Data);
end;

function TSimpleModel.ParamByName(const AParamName: String): TParam;
begin
  Result := Data.ParamByName( AParamName);
end;

function TSimpleModel.Exec(ASQL: String): boolean;
begin
  Result := QueryExec( ASQL);
end;

function TSimpleModel.ExecScript(const ASQL: string): boolean;
var
  msg: string;
begin
  Result := ExecSQLScript(WriteConnector(FConnector), ASQL, msg);
  AMessage := msg;
end;

function TSimpleModel.All(const AUniDirectional: boolean): boolean;
begin
  result := GetAll(0,0, AUniDirectional);
end;

{
same with:
  Object.Find([]);
}
function TSimpleModel.GetAll(Limit: Integer; Offset: Integer;
  const AUniDirectional: boolean): boolean;
begin
  _queryPrepare;
  Data.SQL.Text := 'SELECT ' + FSelectField.Replace(FTableName + '.', QuotedTableName + '.')
    + ' FROM ' + QuotedTableName;
  if FUseSoftDelete and (Trim(FSoftDeleteField) <> '') then
    Data.SQL.Add('WHERE ' + QuotedTableName + '.' + FSoftDeleteField + '=0');
  if Limit > 0 then
    Data.SQL.Add('LIMIT ' + i2s(Limit));
  if Offset > 0 then
    Data.SQL.Add('OFFSET ' + i2s(Offset));
  _queryOpen(AUniDirectional);
  Result := true;
end;

function TSimpleModel.AsJsonArray(NoFieldName: boolean): TJSONArray;
begin
  Result := TJSONArray.Create;
  FRecordCountFromArray := 0;
  if not Data.Active then Exit;
  DataToJSON(Data, Result, NoFieldName);
  FRecordCountFromArray := Result.Count;
end;

// USAGE:
//   FieldToJSON(toolsAsArray, 'parameters', '{}'); // atau '[]' jika kamu mau default array
procedure TSimpleModel.FieldToJSON(const A: TJSONArray;
  const FieldName: string; const DefaultJSON: string);
var
  i: Integer;
  Obj: TJSONObject;
  _Data: TJSONData;
  AsStr: String;
  Parsed: TJSONData;
begin
  if (A = nil) or (FieldName = '') then Exit;

  for i := 0 to A.Count - 1 do
  begin
    if not (A.Items[i] is TJSONObject) then
      Continue;

    Obj := TJSONObject(A.Items[i]);

    // cari field; kalau tidak ada, tambahkan default
    _Data := Obj.Find(FieldName);
    if _Data = nil then
    begin
      Parsed := GetJSON(DefaultJSON, False);
      Obj.Add(FieldName, Parsed);
      Continue;
    end;

    // kalau data sudah berupa JSON object/array/primitive non-string,
    // biarkan saja (hindari double-parse). Umumnya dari DB masih string.
    if _Data.JSONType <> jtString then
      Continue;

    // ambil string lalu hapus field lama, ganti dengan hasil parse
    AsStr := _Data.AsString;
    Obj.Delete(FieldName); // Delete juga akan membebaskan Data lama

    Parsed := ParseJSONOrDefault(AsStr, DefaultJSON);
    Obj.Add(FieldName, Parsed);
  end;
end;

// Die does not stop the request, so both overloads return False when the
// model has no primary key instead of running "WHERE =<value>".
function TSimpleModel.Find(const KeyIndex: integer): boolean;
var
  s : string;
begin
  Result := False;
  if primaryKey = '' then
  begin
    Die( 'primaryKey not defined');
    Exit;
  end;
  s := primaryKey + '=' + i2s( KeyIndex);
  result := Find( [s], '');
end;

// The key is quoted with QuoteValue: it used to be concatenated as is, so a
// key such as "O'Brien" broke the query and "1' OR '1'='1" matched every row.
function TSimpleModel.Find(const KeyIndex: String): boolean;
var
  s : string;
begin
  Result := False;
  if primaryKey = '' then
  begin
    Die( 'primaryKey not defined');
    Exit;
  end;
  s := primaryKey + '=' + QuoteValue(KeyIndex);
  result := Find( [s], '');
end;

function TSimpleModel.Find(const Where: array of string; const Order: string;
  const Limit: integer; const CustomField: string; const Offset: integer
  ): boolean;
var
  i,j : integer;
  _selectField, _joinSQL,
  sWhere : string;
  _joinfield,
  _join : TStrings;
begin
  Result := false;
  if not DB_Connector.Connected then begin
    DisplayError(_ERR_DATABASE_NOT_INITIALIZED);
    Exit;
  end;

  primaryKeyValue := '';
  sWhere := '';
  if high(Where)>=0 then
  begin
    for i:=low(Where) to high(Where) do
    begin
      if sWhere = '' then
        sWhere := Where[i]
      else begin
        if trim( Where[i]) <> '' then
          sWhere := sWhere + ' AND ' + Where[i];
      end;

    end;
    sWhere := sWhere.Replace(' AND OR ', ' OR ');
  end;
  // soft deleted rows are not part of the result
  if FUseSoftDelete and (Trim(FSoftDeleteField) <> '') then
  begin
    if sWhere = '' then
      sWhere := QuotedAliasName + '.' + FSoftDeleteField + '=0'
    else
      sWhere := '(' + sWhere + ') AND ' + QuotedAliasName + '.' + FSoftDeleteField + '=0';
  end;
  if CustomField = '' then begin
    _queryPrepare;
    // the FROM clause aliases the table, so the generated field list uses
    // the (quoted) alias: "groups" is a reserved word on MySQL 8
    _selectField := FSelectField.Replace( FTableName + '.', QuotedAliasName + '.');
  end else begin
    _selectField:= CustomField;
  end;
  _joinSQL := ' ';
  // prepare join
  if FJoinList.Count > 0 then begin
    for i:=0 to FJoinList.Count-1 do begin
      _join := Explode( FJoinList[i], '|');
      _joinSQL := _joinSQL + #13#10 + _join[0] + ' ' + _join[1]
        + ' ON ' + _join[1] + '.' + _join[2] + '=' + _join[3]
        ;
      if _join.count > 5 then
        if _join[5] <> '' then
          _joinSQL += ' AND ' + _join[5];
      _selectField := _selectField + #13;
      if _join.count > 4 then begin  // if view joined field
        _joinfield := Explode( _join[4], ',');
        for j:=0 to _joinfield.Count-1 do begin
          _selectField := _selectField + ' ,' + _join[1] + '.' + _joinfield[j];
        end;
        FreeAndNil( _joinfield);
      end;
      FreeAndNil( _join);
    end;
  end;
  // prepare join - end
  Data.SQL.Text := 'SELECT ' + _selectField + #13#10'FROM ' + QuotedTableName + ' ' + QuotedAliasName + _joinSQL;

  if sWhere <> '' then
    Data.SQL.Add( 'WHERE ' + sWhere);
  if FGroupField <> '' then
    Data.SQL.Add( 'GROUP BY ' + FGroupField);
  if Order <> '' then
    Data.SQL.Add( 'ORDER BY ' + Order);
  if Limit > 0 then begin
    Data.SQL.Add( 'LIMIT ' + IntToStr( Limit));
  end;
  if Offset > 0 then
    Data.SQL.Add( 'OFFSET ' + Offset.ToString);
  if Data.Active then
    Data.Close;
  Data.UniDirectional:=False;
  if Debug then LogUtil.Add(Data.SQL.Text, 'DB-' + FTableName);
  Result := _queryOpen;
  Data.Last;
  Data.First;
  if (Data.RecordCount = 1) and (primaryKey <> '') then
  begin
    primaryKeyValue := '';
    try
      primaryKeyValue := Data.FieldByName( primaryKey).AsString;
    except
    end;
  end;
end;

function TSimpleModel.FindFirst(const Where: array of string;
  const Order: string; const CustomField: string): boolean;
begin
  Result := Find( Where, Order, 1, CustomField);
end;

procedure TSimpleModel.AddJoin(const JoinTable: string;
  const JoinField: string; const RefField: string;
  const FieldNameList: array of string; const AJoinWhere: string);
begin
  AddCustomJoin( '', JoinTable, JoinField, RefField, FieldNameList, AJoinWhere);
end;

procedure TSimpleModel.AddLeftJoin(const JoinTable: string;
  const JoinField: string; const RefField: string;
  const FieldNameList: array of string; const AJoinWhere: string);
begin
  AddCustomJoin( 'LEFT', JoinTable, JoinField, RefField, FieldNameList, AJoinWhere);
end;

procedure TSimpleModel.AddInnerJoin(const JoinTable: string;
  const JoinField: string; const RefField: string;
  const FieldNameList: array of string; const AJoinWhere: string);
begin
  AddCustomJoin( 'INNER', JoinTable, JoinField, RefField, FieldNameList, AJoinWhere);
end;

procedure TSimpleModel.AddCustomJoin(const JointType: string;
  const JoinTable: string; const JoinField: string; const RefField: string;
  const FieldNameList: array of string; const AJoinWhere: string);
var
  i : integer;
  s : string;
begin
  s:='';
  for i:=low(FieldNameList) to high(FieldNameList) do begin
    if s = '' then
      s := FieldNameList[i]
    else
      s := s + ','+FieldNameList[i];
  end;
  FJoinList.Add( JointType + ' JOIN|'+AppData.tablePrefix+JoinTable+'|'+JoinField+'|'+AppData.tablePrefix+RefField+'|'+s+'|'+AJoinWhere);
end;

procedure TSimpleModel.GroupBy(const GroupField: string);
begin
  FGroupField:= GroupField;
end;

function TSimpleModel.RecordsTotalFiltered(const ASQL: string;
  ForceClose: boolean): integer;
begin
  Result := 0;
  if Data.Active then
  begin
    if not ForceClose then
    begin
      Result := Data.RecordCount;
      Exit;
    end;
    Data.Close;
  end;
  if ASQL = '' then
    Exit;
  Data.SQL.Text := ASQL;
  Data.Open;
  if RecordCount > 0 then
  begin
    Result := Data.Fields.Fields[0].AsInteger;
  end;
end;

procedure TSimpleModel.Clear;
begin
  New;
end;

procedure TSimpleModel.New;
begin
  if Assigned( FGenFields) then FGenFields.Clear;
  if Assigned( FieldValueMap) then FieldValueMap.Clear;
  Data.SQL.Clear;
  primaryKeyValue:='';
  Close;
end;

procedure TSimpleModel.Close;
begin
  if Data.Active then Data.Close;
  if Assigned( FJoinList) then FJoinList.Clear;
  FGroupField := '';
end;

function TSimpleModel.Save(Where: string; AutoCommit: boolean): boolean;
var
  sSQL : TStringList;
  i : integer;
  s : string;
begin
  Result := false;
  AMessage := '';

  if not DB_Connector.Connected then begin
    DisplayError(_ERR_DATABASE_NOT_INITIALIZED);
    Exit;
  end;

  // nothing was assigned through Value[] / SetFieldValue
  if (not Assigned( FGenFields)) or (FGenFields.Count = 0) then
  begin
    AMessage := 'no field value to save';
    Exit;
  end;

  if AppData.databaseRead = AppData.databaseWrite then
  begin
    if ((Data.Active) and (primaryKeyValue='')) then Exit;
    if Data.Active then Data.Close;
    //Data.DataBase := DB_Connector;
  end
  else
  begin
    if Data.Active then Data.Close;
    Data.DataBase := DatabaseSecond_Prepare( AppData.databaseWrite, 'write');
    if Data.DataBase = nil then
    begin
      DisplayError( format( _ERR_DATABASE_CANNOT_CONNECT, [ AppData.databaseWrite]));
      Data.DataBase := DB_Connector;
      Exit;
    end;
  end;

  sSQL := TStringList.Create;
  if Where <> '' then
  begin
    sSQL.Add( 'UPDATE ' + QuotedTableName + ' SET ');
    for i:=0 to FGenFields.Count-1 do
    begin
      s := FFieldQuote + FGenFields[i] + FFieldQuote +'=:'+FGenFields[i];
      if i <> FGenFields.Count-1 then s:= s + ',' ;
      sSQL.Add( s);
    end;
    sSQL.Add( 'WHERE ' + Where);
    primaryKeyValue:='';
  end
  else
  begin //-- new data
    sSQL.Add( 'INSERT INTO '+QuotedTableName+' (');
    s := '';
    for i:= 0 to FGenFields.Count-1 do
    begin
      s := s + FFieldQuote + FGenFields[i] + FFieldQuote + ',';
    end;
    s := s.Substring(0, s.Length-1);
    sSQL.Add( s);
    sSQL.Add( ') VALUES (');
    s := Implode( FGenFields, ',', ':');
    //s := Implode( FGenValues, ',', '''', '''');
    sSQL.Add(s);
    sSQL.Add( ')');
  end;

  try
    Data.SQL.Text:= sSQL.Text;
    for i:=0 to FGenFields.Count-1 do
    begin
      s := FGenFields[i];
      Data.Params.ParamByName( s).Value := FieldValueMap[s];
    end;
    Data.ExecSQL;
    // commit on the connection that ran the statement (read/write split)
    if AutoCommit then TSQLConnector( Data.DataBase).Transaction.Commit;
    Result := True;
  except
    on E: Exception do begin
      // a failed statement is reported through the result and Message
      // (it used to end the request with DisplayError)
      AMessage := E.Message;
      if AppData.debug then begin
        LogUtil.add( E.Message);
        LogUtil.add( Data.SQL.Text);
      end;
      if AutoCommit then
        try
          TSQLConnector( Data.DataBase).Transaction.RollbackRetaining;
        except
        end;
    end;
  end;
  FreeAndNil(sSQL);
  Data.DataBase := DB_Connector;
end;

function TSimpleModel.Delete(Where: string; AutoCommit: boolean): boolean;
var
  s : string;
begin
  if FUseSoftDelete then
  begin
    Result := SoftDelete(Where, AutoCommit);
    Exit;
  end;
  Result := false;
  AMessage := '';
  if ((Where='') and (Data.Active)) then
  begin
    if RecordCount <> 1 then exit;
  end;
  // no WHERE and no record found before: never run "DELETE ... WHERE id="
  if (Where = '') and ((primaryKey = '') or (primaryKeyValue = '')) then
    Exit;
  if Data.Active then Data.Close;

  if not DB_Connector.Connected then begin
    DisplayError(_ERR_DATABASE_NOT_INITIALIZED);
    Exit;
  end;

  if AppData.databaseRead = AppData.databaseWrite then
  begin
    //Data.DataBase := DB_Connector;
  end
  else
  begin
    Data.DataBase := DatabaseSecond_Prepare( AppData.databaseWrite, 'write');
    if Data.DataBase = nil then
    begin
      DisplayError( format( _ERR_DATABASE_CANNOT_CONNECT, [ AppData.databaseWrite]));
      Data.DataBase := DB_Connector;
      Exit;
    end;
  end;

  s := 'DELETE FROM ' + QuotedTableName + ' WHERE ';
  if Where = '' then
  begin
    s:= s + primaryKey + '=' + QuotedStr( primaryKeyValue);
  end
  else
  begin
    s := s + Where;
  end;
  try
    Data.SQL.Text:= s;
    Data.ExecSQL;
    // commit on the connection that ran the statement (read/write split)
    if AutoCommit then TSQLConnector( Data.DataBase).Transaction.Commit;
    Result := True;
  except
    on E: Exception do begin
      AMessage := E.Message;
      if AppData.debug then begin
        LogUtil.add( E.Message);
        LogUtil.add( Data.SQL.Text);
      end;
      if AutoCommit then
        try
          TSQLConnector( Data.DataBase).Transaction.RollbackRetaining;
        except
        end;
    end;
  end;

  Data.DataBase := DB_Connector;
end;

function TSimpleModel.Delete(ID: LongInt): boolean;
begin
  if FUseSoftDelete then
  begin
    Result := SoftDelete(ID);
    Exit;
  end;
  Result := False;
  if primaryKey = '' then
    Exit;
  Result := Delete( primaryKey+'='+i2s(ID));
end;

// SQL string literal for AValue; MySQL also treats backslash as an escape.
function TSimpleModel.QuoteValue(const AValue: string): string;
var
  s: string;
begin
  s := AValue;
  if Assigned(Connector) and (Pos('mysql', LowerCase(Connector.ConnectorType)) > 0) then
    s := StringReplace(s, '\', '\\', [rfReplaceAll]);
  Result := QuotedStr(s);
end;

// Marks rows as deleted instead of removing them. This used to be a stub
// that returned False, so Delete with UseSoftDelete did nothing.
function TSimpleModel.SoftDelete(Where: string; AutoCommit: boolean): boolean;
var
  s: string;
begin
  Result := False;
  if Trim(FSoftDeleteField) = '' then
    Exit;
  if ((Where='') and (Data.Active)) then
  begin
    if RecordCount <> 1 then exit;
  end;
  if (Where = '') and ((primaryKey = '') or (primaryKeyValue = '')) then
    Exit;
  if Data.Active then Data.Close;
  if not DB_Connector.Connected then begin
    DisplayError(_ERR_DATABASE_NOT_INITIALIZED);
    Exit;
  end;
  if AppData.databaseRead <> AppData.databaseWrite then
  begin
    Data.DataBase := DatabaseSecond_Prepare( AppData.databaseWrite, 'write');
    if Data.DataBase = nil then
    begin
      DisplayError( format( _ERR_DATABASE_CANNOT_CONNECT, [ AppData.databaseWrite]));
      Data.DataBase := DB_Connector;
      Exit;
    end;
  end;
  s := 'UPDATE ' + QuotedTableName + ' SET ' + FFieldQuote + FSoftDeleteField + FFieldQuote + '=1 WHERE ';
  if Where = '' then
    s := s + primaryKey + '=' + QuotedStr( primaryKeyValue)
  else
    s := s + Where;
  try
    Data.SQL.Text := s;
    Data.ExecSQL;
    if AutoCommit then TSQLConnector( Data.DataBase).Transaction.Commit;
    Result := True;
  except
    on E: Exception do begin
      AMessage := E.Message;
      if AppData.debug then begin
        LogUtil.add( E.Message);
        LogUtil.add( Data.SQL.Text);
      end;
    end;
  end;
  Data.DataBase := DB_Connector;
end;

function TSimpleModel.SoftDelete(ID: LongInt): boolean;
begin
  Result := False;
  if primaryKey = '' then
    Exit;
  Result := SoftDelete( primaryKey+'='+i2s(ID));
end;

function TSimpleModel.Update(AutoCommit: boolean): boolean;
begin
  Result := False;
  // no record was found before: an empty value would give "WHERE id="
  if primaryKey.IsEmpty or primaryKeyValue.IsEmpty then Exit;
  Result :=  Save(primaryKey+'='+QuotedStr( primaryKeyValue), AutoCommit);
end;

procedure TSimpleModel.First;
begin
  Data.First;
end;

procedure TSimpleModel.Prior;
begin
  Data.Prior;
end;

procedure TSimpleModel.Next;
begin
  Data.Next;
end;

procedure TSimpleModel.Last;
begin
  Data.Last;
end;

procedure TSimpleModel.StartTransaction;
begin
  DB_Transaction.StartTransaction;
end;

procedure TSimpleModel.ReStartTransaction;
begin
  if DB_Transaction.Active then
    DB_Transaction.Active:= false;
  DB_Transaction.StartTransaction;
end;

procedure TSimpleModel.Commit;
begin
  DB_Transaction.Commit;
end;

procedure TSimpleModel.CommitRetaining;
begin
  DB_Transaction.CommitRetaining;
end;

procedure TSimpleModel.Rollback;
begin
  DB_Transaction.Rollback;
end;

procedure TSimpleModel.RollbackRetaining;
begin
  DB_Transaction.RollbackRetaining;
end;

procedure TSimpleModel.GenerateScript;
begin
  Clear;
  // Where/OrderBy/Limit without a Select used to produce "SELECT  FROM"
  if Trim(FScriptFieldNames) = '' then
    FScriptFieldNames := '*';
  Data.SQL.Text := 'SELECT ' + FScriptFieldNames;
  Data.SQL.Add( 'FROM ' + QuotedTableName);
  if FScriptWhere <> '' then
    Data.SQL.Add( 'WHERE ' + FScriptWhere);
  if FScriptOrderBy <> '' then
    Data.SQL.Add( 'ORDER BY ' + FScriptOrderBy);
  if FScriptLimit <> '' then
    Data.SQL.Add( FScriptLimit);
end;

function TSimpleModel.Select(FieldNames: string): TSimpleModel;
begin
  FScriptFieldNames := FieldNames;
  FScriptWhere := '';
  FScriptLimit := '';
  FScriptOrderBy := '';
  GenerateScript;
  Result := Self;
end;

function TSimpleModel.Where(Conditions: string): TSimpleModel;
begin
  if FScriptWhere = '' then
    FScriptWhere := Conditions
  else
    FScriptWhere := FScriptWhere + ' AND ' + Conditions;
  GenerateScript;
  Result := Self;
end;

function TSimpleModel.Where(ConditionFormat: string;
  ConditionArgs: array of const): TSimpleModel;
begin
  if FScriptWhere = '' then
    FScriptWhere := Format( ConditionFormat, ConditionArgs)
  else
    FScriptWhere := FScriptWhere + ' AND ' + Format( ConditionFormat, ConditionArgs);
  GenerateScript;
  Result := Self;
end;

function TSimpleModel.OrWhere(Conditions: string): TSimpleModel;
begin
  FScriptWhere := FScriptWhere + ' OR ' + Conditions;
  GenerateScript;
  Result := Self;
end;

function TSimpleModel.OrderBy(FieldNames: string): TSimpleModel;
begin
  FScriptOrderBy := FieldNames;
  GenerateScript;
  Result := Self;
end;

function TSimpleModel.Limit(LimitNumber: integer; Offset: integer
  ): TSimpleModel;
begin
  if LimitNumber > 0 then
    FScriptLimit := 'LIMIT ' + i2s(LimitNumber);
  if Offset > 0 then
    FScriptLimit := FScriptLimit + ' OFFSET ' + i2s(Offset);
  GenerateScript;
  Result := Self;
end;

function TSimpleModel.Open(AUniDirectional: Boolean): Boolean;
begin
  Result := False;
  if Data.SQL.Text = '' then
    Exit;
  Result := _queryOpen( AUniDirectional);
  if not Result then
    Exit;
  try
    Data.Last;
    Data.First;
  except
    on E:Exception do
    begin
      AMessage := E.Message;
    end;
  end;
  if (Data.RecordCount = 1) and (primaryKey <> '') then
  begin
    try
      primaryKeyValue := Data.FieldByName( primaryKey).AsString;
    except
    end;
  end;
end;

initialization
  DB_LibLoader := TSQLDBLibraryLoader.Create( nil);
  DB_Transaction := TSQLTransaction.Create( nil);
  DB_Connector := TFPSQLConnector.Create( nil);
  DB_Connector.Transaction := DB_Transaction;

  DB_Connector_Write := nil;
  DB_Transaction_Write := nil;
  DB_LibLoader_Write := nil;


finalization
  FreeAndNil( DB_Connector);
  FreeAndNil( DB_Transaction);
  FreeAndNil( DB_LibLoader);

  if Assigned( DB_Connector_Write) then FreeAndNil( DB_Connector_Write);
  if Assigned( DB_Transaction_Write) then FreeAndNil( DB_Transaction_Write);
  if Assigned( DB_LibLoader_Write) then FreeAndNil( DB_LibLoader_Write);

end.
















unit database_lib;

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  {$ifdef GREYHOUND}
  ghSQL, ghSQLdbLib,
  {$endif}
  fpcgi, fphttp, db, fpjson, jsonparser, fgl,
  sqldb, sqldblib, mysql50conn, mysql51conn, mysql55conn, mysql56conn, mysql57conn, mysql80conn,
  sqlite3conn, pqconnection, IBConnection, TypInfo,
  variants, Classes, SysUtils;

type

  generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
  TFieldValueMap = specialize TStringHashMap<variant>;

  TFPSQLConnector = class(TSQLConnector)
  public
    property Proxy;
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
    property UseSoftDelete: boolean read FUseSoftDelete write FUseSoftDelete;

    function ParamByName(Const AParamName : String) : TParam;
    property SQL : TStringlist read getSQL;
    function Exec( ASQL:String): boolean;

    function All(const AUniDirectional: boolean = false):boolean;
    function GetAll( Limit: Integer = 0; Offset: Integer = 0; const AUniDirectional: boolean = false):boolean;
    function AsJsonArray(NoFieldName: boolean = False): TJSONArray;
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
  end;

function DataBaseInit( const RedirecURL:string = ''; const DBConfigName: string = ''):boolean;

function  QueryOpenToJson( SQL: string; var ResultJSON: TJSONObject; const aParams : array of string; SQLCount: string = ''; Where: string = ''; Order: string =''; Limit: integer=0; Offset: integer=0; Echo: integer = 0; sParams: string =''; NoFieldName : boolean = True): boolean;
function  QueryOpenToJson( SQL: string; var ResultJSON: TJSONObject; NoFieldName : boolean = True): boolean;
function  QueryOpenToJson( SQL: string; var ResultArray: TJSONArray; NoFieldName : boolean = True): boolean;
function  QueryExecToJson( SQL: string; var ResultJSON: TJSONObject): boolean;
function  QueryExec( SQL: string):boolean;
function  DataToJSON( Data : TSQLQuery; var ResultJSON: TJSONArray; NoFieldName : boolean = True):boolean;

var
  QueryExecRowAffected: Integer;

implementation

uses common, config_lib, fastplaz_handler, logutil_lib;

var
  DB_Connector : TSQLConnector;
  DB_Transaction : TSQLTransaction;
  DB_LibLoader : TSQLDBLibraryLoader;

  DB_Connector_Write : TSQLConnector;
  DB_Transaction_Write : TSQLTransaction;
  DB_LibLoader_Write : TSQLDBLibraryLoader;

function DataBaseInit(const RedirecURL: string; const DBConfigName: string
  ): boolean;
var
  s : string;
begin
  Result := False;
  AppData.useDatabase := True;

  if DB_Connector.Connected then
  begin
    Result := True;
  end;

  // multidb - prepare
  AppData.databaseRead := string( Config.GetValue( _DATABASE_OPTIONS_READ, 'default'));
  AppData.databaseWrite := string( Config.GetValue( _DATABASE_OPTIONS_WRITE, UnicodeString( AppData.databaseRead)));
  AppData.tablePrefix := string( Config.GetValue( UnicodeString( format( _DATABASE_TABLE_PREFIX, [AppData.databaseRead])), ''));
  AppData.databaseVersionCheck := Config.GetValue( UnicodeString( format( _DATABASE_VERSION_CHECK, [AppData.databaseRead])), True);

  if not DBConfigName.IsEmpty then
  begin
    AppData.databaseRead := DBConfigName;
    AppData.databaseWrite := DBConfigName;
  end;

  // currentdirectory mesti dipindah
  //s := GetCurrentDir + DirectorySeparator + string( Config.GetValue( format( _DATABASE_LIBRARY, [AppData.databaseRead]), ''));
  s := string( Config.GetValue( UnicodeString( format( _DATABASE_LIBRARY, [AppData.databaseRead])), ''));

  if not SetCurrentDir( ExtractFilePath( s)) then
  begin
    DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ AppData.databaseRead, s]));
  end;
  s := GetCurrentDir + DirectorySeparator + ExtractFileName( string( Config.GetValue( UnicodeString( format( _DATABASE_LIBRARY, [AppData.databaseRead])), '')));
  if not FileExists( s) then
  begin
    //ulil
    //SetCurrentDir(ExtractFilePath(Application.ExeName));
    //DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ AppData.databaseRead, s]));
  end;

  if Config.GetValue( UnicodeString( format( _DATABASE_LIBRARY, [AppData.databaseRead])), '') <> '' then begin
    try
      DB_LibLoader.ConnectionType:= string( Config.GetValue( UnicodeString( format( _DATABASE_DRIVER, [AppData.databaseRead])), ''));
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
  end;
  // back to app directory
  SetCurrentDir(ExtractFilePath(Application.ExeName));

  if not DB_Connector.Connected then
  begin
    DB_Connector.HostName:= string( Config.GetValue( UnicodeString( format( _DATABASE_HOSTNAME, [AppData.databaseRead])), 'localhost'));
    DB_Connector.ConnectorType := string( Config.GetValue( UnicodeString( format( _DATABASE_DRIVER, [AppData.databaseRead])), ''));
    DB_Connector.UserName:= string( Config.GetValue( UnicodeString( format( _DATABASE_USERNAME, [AppData.databaseRead])), ''));
    DB_Connector.Password:= string( Config.GetValue( UnicodeString( format( _DATABASE_PASSWORD, [AppData.databaseRead])), ''));
    DB_Connector.DatabaseName:= string( Config.GetValue( UnicodeString( format( _DATABASE_DATABASENAME, [AppData.databaseRead])), 'test'));
    if Config.GetValue( UnicodeString( format( _DATABASE_PORT, [AppData.databaseRead])), '') <> '' then
      DB_Connector.Params.Values['port'] := string( Config.GetValue( UnicodeString( format( _DATABASE_PORT, [AppData.databaseRead])), ''));
    //tabletype := Config.GetValue( _DATABASE_TABLETYPE, '');
    s := string( Config.GetValue( UnicodeString( format( _DATABASE_CHARSET, [AppData.databaseRead])), ''));
    if s <> '' then
      DB_Connector.CharSet := s;

    // Skip Library Version Check from MySQL
    if not AppData.databaseVersionCheck then
    begin
      if TFPSQLConnector(DB_Connector).Proxy is TConnectionName then
      begin
        if DB_Connector.ConnectorType.ToLower.StartsWith('mysql') then
        begin
          TConnectionName(TFPSQLConnector(DB_Connector).Proxy).SkipLibraryVersionCheck:= True;
        end;
      end;
    end;

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
    if Offset >= 0 then
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
  s : string;
begin
  Result := nil; //DB_Connector_Write
  if ((Assigned( DB_Connector_Write)) and (Mode = 'write')) then
  begin
    Result := DB_Connector_Write;
    Exit;
  end;

  s := GetCurrentDir + DirectorySeparator + string( Config.GetValue( UnicodeString( format( _DATABASE_LIBRARY, [ConnectionName])), ''));
  if not SetCurrentDir( ExtractFilePath( s)) then
  begin
    DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ ConnectionName, s]));
  end;
  s := GetCurrentDir + DirectorySeparator + ExtractFileName( string( Config.GetValue( UnicodeString( format( _DATABASE_LIBRARY, [ ConnectionName])), '')));
  if not FileExists( s) then
  begin
    SetCurrentDir(ExtractFilePath(Application.ExeName));
    DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ ConnectionName, s]));
  end;

  if Config.GetValue( UnicodeString( format( _DATABASE_LIBRARY, [ ConnectionName])), '') <> '' then
  begin
    if not Assigned( DB_LibLoader_Write) then DB_LibLoader_Write := TSQLDBLibraryLoader.Create( nil);
    try
      DB_LibLoader_Write.ConnectionType:= string( Config.GetValue( UnicodeString( format( _DATABASE_DRIVER, [ConnectionName])), ''));
      DB_LibLoader_Write.LibraryName:= s;
      DB_LibLoader_Write.Enabled:= True;
      DB_LibLoader_Write.LoadLibrary;
    except
      on E: Exception do begin
          DisplayError( 'Database Init ('+ConnectionName+'): Load Library, '+E.Message)
      end;
    end;
  end;
  // back to app directory
  SetCurrentDir(ExtractFilePath(Application.ExeName));

  if not Assigned( DB_Connector_Write) then begin
    DB_Transaction_Write := TSQLTransaction.Create( nil);
    DB_Connector_Write := TSQLConnector.Create( nil);
    DB_Connector_Write.Transaction := DB_Transaction_Write;
  end;
  DB_Connector_Write.HostName:= string( Config.GetValue( UnicodeString( format( _DATABASE_HOSTNAME, [ConnectionName])), 'localhost'));
  DB_Connector_Write.ConnectorType := string( Config.GetValue( UnicodeString( format( _DATABASE_DRIVER, [ConnectionName])), ''));
  DB_Connector_Write.UserName:= string( Config.GetValue( UnicodeString( format( _DATABASE_USERNAME, [ConnectionName])), ''));
  DB_Connector_Write.Password:= string( Config.GetValue( UnicodeString( format( _DATABASE_PASSWORD, [ConnectionName])), ''));
  DB_Connector_Write.DatabaseName:= string( Config.GetValue( UnicodeString( format( _DATABASE_DATABASENAME, [ConnectionName])), 'test'));
  s := string( Config.GetValue( UnicodeString( format( _DATABASE_CHARSET, [ConnectionName])), ''));
  if s <> '' then
    DB_Connector.CharSet := s;

  if Config.GetValue( UnicodeString( format( _DATABASE_PORT, [ConnectionName])), '') <> '' then
    DB_Connector_Write.Params.Values['port'] := string( Config.GetValue( UTF8Decode(format( _DATABASE_PORT, [ConnectionName])), ''));
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

function DataToJSON(Data: TSQLQuery; var ResultJSON: TJSONArray;
  NoFieldName: boolean): boolean;
var
  item : TJSONObject;
  item_array : TJSONArray;
  field_name, fieldType : string;
  i,j:integer;
begin
  Result:=False;
  i:=1;
  try
    Data.First;
    while not Data.EOF do
    begin
      item := TJSONObject.Create();
      item_array := TJSONArray.Create;
      for j:=0 to Data.FieldCount-1 do
      begin
        field_name:= Data.FieldDefs.Items[j].Name;
        fieldType := GetEnumName(TypeInfo(TFieldType), Ord(Data.FieldDefs.Items[j].DataType));
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
begin
  Result := 0;
  try
    if Data.Active then Data.Close;
    Data.SQL.Text := 'SELECT LAST_INSERT_ID() as lastid FROM ' + TableName;
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
    if not AUniDirectional then
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
  Data.SQL.Text:= 'SHOW COLUMNS FROM ' + FTableName;
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
  Data.SQL.Text:= 'PRAGMA table_info('+FTableName+');';
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
      die( 'getFieldValue: ' + e.Message);
    end;
  end;
end;

procedure TSimpleModel.SetFieldValue(FieldName: String; AValue: Variant);
begin
  if not Assigned( FGenFields) then
  begin
    FGenFields := TStringList.Create;
    FGenFields := TStringList.Create;
    FieldValueMap := TFieldValueMap.Create;
  end;
  FieldValueMap[FieldName] := AValue;
  FGenFields.Add( FieldName);
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
  Data.SQL.Text := 'SELECT ' + FSelectField + ' FROM ' + FTableName;
  if Limit > 0 then
    Data.SQL.Add('LIMIT ' + i2s(Limit));
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

function TSimpleModel.Find(const KeyIndex: integer): boolean;
var
  s : string;
begin
  if primaryKey = '' then
  begin
    Die( 'primayKey not defined');
  end;
  s := primaryKey + '=' + i2s( KeyIndex);
  result := Find( [s], '');
end;

function TSimpleModel.Find(const KeyIndex: String): boolean;
var
  s : string;
begin
  if primaryKey = '' then
  begin
    Die( 'primayKey not defined');
  end;
  s := primaryKey + '=''' + KeyIndex + '''';
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
  if not DB_Connector.Connected then begin
    DisplayError(_ERR_DATABASE_NOT_INITIALIZED);
    Exit;
  end;

  primaryKeyValue := '';
  Result := false;
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
  if CustomField = '' then begin
    _queryPrepare;
    _selectField := FSelectField;
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
  Data.SQL.Text := 'SELECT ' + _selectField + #13#10'FROM ' + FTableName + ' ' + FAliasName + _joinSQL;

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
end;

function TSimpleModel.Save(Where: string; AutoCommit: boolean): boolean;
var
  sSQL : TStringList;
  i : integer;
  s : string;
begin
  Result := false;

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
    end;
  end;

  if not DB_Connector.Connected then begin
    DisplayError(_ERR_DATABASE_NOT_INITIALIZED);
    Exit;
  end;

  sSQL := TStringList.Create;
  if Where <> '' then
  begin
    sSQL.Add( 'UPDATE ' + TableName + ' SET ');
    for i:=0 to FGenFields.Count-1 do
    begin
      s := FFieldQuote + FGenFields[i] + FFieldQuote +'=:'+FGenFields[i];
      if i <> FGenFields.Count-1 then s:= s + ',' ;
      sSQL.Add( s);
    end;

    if Where <> '' then
    begin
      sSQL.Add( 'WHERE ' + Where);
      primaryKeyValue:='';
    end
    else
      sSQL.Add( 'WHERE ' + primaryKey + '=''' + primaryKeyValue + '''');

  end
  else
  begin //-- new data
    sSQL.Add( 'INSERT INTO '+TableName+' (');
    s := Implode( FGenFields, ',');
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
    for i:=0 to FieldValueMap.Count-1 do
    begin
      s := FieldValueMap.Keys[i];
      Data.Params.ParamByName( FGenFields[i]).Value := FieldValueMap[s];
    end;
    Data.ExecSQL;
    if AutoCommit then Commit;
    Result := True;
  except
    on E: Exception do begin
      if AppData.debug then begin
        LogUtil.add( E.Message);
        LogUtil.add( Data.SQL.Text);
      end;
      DisplayError( e.Message);
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
  if ((Where='') and (Data.Active)) then
  begin
    if RecordCount <> 1 then exit;
  end;
  if Data.Active then Data.Close;

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
    end;
  end;

  if not DB_Connector.Connected then begin
    DisplayError(_ERR_DATABASE_NOT_INITIALIZED);
    Exit;
  end;

  s := 'DELETE FROM ' + TableName + ' WHERE ';
  if Where = '' then
  begin
    s:= s + primaryKey + '=' + primaryKeyValue;
  end
  else
  begin
    s := s + Where;
  end;
  try
    Data.SQL.Text:= s;
    Data.ExecSQL;
    if AutoCommit then Commit;
    Result := True;
  except
    on E: Exception do begin
      if AppData.debug then begin
        LogUtil.add( E.Message);
        LogUtil.add( Data.SQL.Text);
      end;
      DisplayError( 'DB:' + e.Message);
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

function TSimpleModel.SoftDelete(Where: string; AutoCommit: boolean): boolean;
begin
  Result := False;
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
  if primaryKey.IsEmpty then Exit;
  Result :=  Save(primaryKey+'='+primaryKeyValue, AutoCommit);
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
  Data.SQL.Text := 'SELECT ' + FScriptFieldNames;
  Data.SQL.Add( 'FROM ' + FTableName);
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
  Data.UniDirectional := AUniDirectional;
  Result := _queryOpen;
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
  DB_Connector := TSQLConnector.Create( nil);
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
















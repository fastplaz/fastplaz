unit database_lib;

{$mode objfpc}{$H+}

interface

uses
  fpcgi, fphttp, db, fpjson, jsonparser, fgl,
  sqldb, sqldblib, mysql50conn, mysql51conn, mysql55conn, {mysql56conn,}
  sqlite3conn, pqconnection,
  variants, Classes, SysUtils;

type

  generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
  TFieldValueMap = specialize TStringHashMap<variant>;

  { TSimpleModel }

  TSimpleModel = class
  private
    FConnector : TSQLConnector;
    FFieldList : TStrings;
    FTableName : string;
    FGroupField : string;
    FSelectField : string;
    FJoinList : TStrings;

    primaryKeyValue : string;
    FGenFields : TStringList;
    function GetEOF: boolean;
    function GetRecordCount: Longint;
    procedure _queryPrepare;
    function  _queryOpen:boolean;
    procedure DoAfterOpen(DataSet: TDataSet);
    procedure DoBeforeOpen(DataSet: TDataSet);

    function getFieldList_mysql: TStrings;
    function getFieldList_sqlite: TStrings;
    function getFieldList_postgresql: TStrings;

    function GetFieldList: TStrings;
    function GetFieldValue( FieldName: String): Variant;
    procedure SetFieldValue( FieldName: String; AValue: Variant);
  public
    FieldValueMap : TFieldValueMap;
    primaryKey : string;
    Data : TSQLQuery;
    StartTime, StopTime, ElapsedTime : Cardinal;
    constructor Create( const DefaultTableName:string=''; const pPrimaryKey:string='');
    destructor Destroy; override;
    property TableName : string Read FTableName write FTableName;
    Property Value[ FieldName: String] : Variant Read GetFieldValue Write SetFieldValue; default;
    Property FieldLists: TStrings Read GetFieldList;
    property RecordCount: Longint read GetRecordCount;
    property EOF: boolean read GetEOF;

    function ParamByName(Const AParamName : String) : TParam;

    function All:boolean;
    function GetAll:boolean;
    //function Get( where, order):boolean;

    function Find( const KeyIndex:integer):boolean;
    function Find( const KeyIndex:String):boolean;
    function Find( const Where:array of string; const Order:string = ''; const Limit:integer = 0; const CustomField:string=''):boolean;
    function FindFirst( const Where:array of string; const Order:string = ''; const CustomField:string=''):boolean;

    procedure AddJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string);
    procedure AddLeftJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string);
    procedure AddInnerJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string);
    procedure AddCustomJoin( const JointType:string; const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string);

    procedure GroupBy( const GroupField:string);

    procedure Clear;
    procedure New;
    function  Save( Where:string='';AutoCommit:boolean=True):boolean;
    function  Delete( Where:string='';AutoCommit:boolean=True):boolean;

    procedure Next;
    procedure StartTransaction;
    procedure ReStartTransaction;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;

  end;

procedure DataBaseInit( const RedirecURL:string = '');
function  QueryOpenToJson( SQL: string; var ResultJSON: TJSONObject): boolean;
function  QueryExecToJson( SQL: string; var ResultJSON: TJSONObject): boolean;
function  DataToJSON( Data : TSQLQuery; var ResultJSON: TJSONArray):boolean;

implementation

uses common, config_lib, fastplaz_handler, logutil_lib;

var
  DB_Connector : TSQLConnector;
  DB_Transaction : TSQLTransaction;
  DB_LibLoader : TSQLDBLibraryLoader;

  DB_Connector_Write : TSQLConnector;
  DB_Transaction_Write : TSQLTransaction;
  DB_LibLoader_Write : TSQLDBLibraryLoader;

procedure DataBaseInit(const RedirecURL: string);
var
  s : string;
begin
  AppData.useDatabase := True;

  // multidb - prepare
  AppData.databaseRead := Config.GetValue( _DATABASE_OPTIONS_READ, 'default');
  AppData.databaseWrite := Config.GetValue( _DATABASE_OPTIONS_WRITE, AppData.databaseRead);
  AppData.tablePrefix := Config.GetValue( format( _DATABASE_TABLE_PREFIX, [AppData.databaseRead]), '');

  // currentdirectory mesti dipindah
  s := GetCurrentDir + DirectorySeparator + string( Config.GetValue( format( _DATABASE_LIBRARY, [AppData.databaseRead]), ''));

  if not SetCurrentDir( ExtractFilePath( s)) then
  begin
    DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ AppData.databaseRead, s]));
  end;
  s := GetCurrentDir + DirectorySeparator + ExtractFileName( string( Config.GetValue( format( _DATABASE_LIBRARY, [AppData.databaseRead]), '')));
  if not FileExists( s) then
  begin
    SetCurrentDir(ExtractFilePath(Application.ExeName));
    DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ AppData.databaseRead, s]));
  end;

  if Config.GetValue( format( _DATABASE_LIBRARY, [AppData.databaseRead]), '') <> '' then begin
    try
      DB_LibLoader.ConnectionType:= string( Config.GetValue( format( _DATABASE_DRIVER, [AppData.databaseRead]), ''));
      DB_LibLoader.LibraryName:= s;
      DB_LibLoader.Enabled:= True;
      DB_LibLoader.LoadLibrary;
    except
      on E: Exception do begin
        if RedirecURL = '' then
          DisplayError( 'Database Init: Load Library, '+E.Message)
        else
          Redirect( RedirecURL);
      end;
    end;
  end;
  // back to app directory
  SetCurrentDir(ExtractFilePath(Application.ExeName));

  DB_Connector.HostName:= string( Config.GetValue( format( _DATABASE_HOSTNAME, [AppData.databaseRead]), 'localhost'));
  DB_Connector.ConnectorType := string( Config.GetValue( format( _DATABASE_DRIVER, [AppData.databaseRead]), ''));
  DB_Connector.UserName:= string( Config.GetValue( format( _DATABASE_USERNAME, [AppData.databaseRead]), ''));
  DB_Connector.Password:= string( Config.GetValue( format( _DATABASE_PASSWORD, [AppData.databaseRead]), ''));
  DB_Connector.DatabaseName:= string( Config.GetValue( format( _DATABASE_DATABASENAME, [AppData.databaseRead]), 'test'));

  if Config.GetValue( format( _DATABASE_PORT, [AppData.databaseRead]), '') <> '' then
    DB_Connector.Params.Values['port'] := string( Config.GetValue( format( _DATABASE_PORT, [AppData.databaseRead]), ''));
  //tabletype := Config.GetValue( _DATABASE_TABLETYPE, '');

  //log database

  try
    DB_Connector.Open;
    AppData.databaseActive := True;
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
end;

function QueryOpenToJson(SQL: string; var ResultJSON: TJSONObject): boolean;
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
    DataToJSON( q, data);
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

  s := GetCurrentDir + DirectorySeparator + string( Config.GetValue( format( _DATABASE_LIBRARY, [ConnectionName]), ''));
  if not SetCurrentDir( ExtractFilePath( s)) then
  begin
    DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ ConnectionName, s]));
  end;
  s := GetCurrentDir + DirectorySeparator + ExtractFileName( string( Config.GetValue( format( _DATABASE_LIBRARY, [ ConnectionName]), '')));
  if not FileExists( s) then
  begin
    SetCurrentDir(ExtractFilePath(Application.ExeName));
    DisplayError( Format(_ERR_DATABASE_LIBRARY_NOT_EXIST, [ ConnectionName, s]));
  end;

  if Config.GetValue( format( _DATABASE_LIBRARY, [ ConnectionName]), '') <> '' then
  begin
    if not Assigned( DB_LibLoader_Write) then DB_LibLoader_Write := TSQLDBLibraryLoader.Create( nil);
    try
      DB_LibLoader_Write.ConnectionType:= string( Config.GetValue( format( _DATABASE_DRIVER, [ConnectionName]), ''));
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
  DB_Connector_Write.HostName:= string( Config.GetValue( format( _DATABASE_HOSTNAME, [ConnectionName]), 'localhost'));
  DB_Connector_Write.ConnectorType := string( Config.GetValue( format( _DATABASE_DRIVER, [ConnectionName]), ''));
  DB_Connector_Write.UserName:= string( Config.GetValue( format( _DATABASE_USERNAME, [ConnectionName]), ''));
  DB_Connector_Write.Password:= string( Config.GetValue( format( _DATABASE_PASSWORD, [ConnectionName]), ''));
  DB_Connector_Write.DatabaseName:= string( Config.GetValue( format( _DATABASE_DATABASENAME, [ConnectionName]), 'test'));

  if Config.GetValue( format( _DATABASE_PORT, [ConnectionName]), '') <> '' then
    DB_Connector_Write.Params.Values['port'] := string( Config.GetValue( format( _DATABASE_PORT, [ConnectionName]), ''));
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

function DataToJSON(Data: TSQLQuery; var ResultJSON: TJSONArray): boolean;
var
  item : TJSONObject;
  field_name : string;
  i,j:integer;
begin
  Result:=False;
  i:=1;
  try
    while not Data.EOF do
    begin
      item := TJSONObject.Create();
      for j:=0 to Data.FieldCount-1 do
      begin
        field_name:= Data.FieldDefs.Items[j].Name;
        item.Add(field_name, Data.FieldByName(field_name).AsString);
      end;
      ResultJSON.Add( item);
      i:=i+1;
      Data.Next;
    end;
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
  FieldLists;
  if Data.Active then Data.Close;
end;

function TSimpleModel.GetRecordCount: Longint;
begin
  Result := -1;
  if not Data.Active then Exit;
  Result := Data.RecordCount;
end;

function TSimpleModel.GetEOF: boolean;
begin
  Result := Data.EOF;
end;

function TSimpleModel._queryOpen: boolean;
begin
  Result := False;
  try
    if Data.Active then Data.Close;
    Data.Open;
    if Data.RecordCount > 0 then
      Result := True;
  except
    on E: Exception do begin
      if AppData.debug then begin
        LogUtil.add( E.Message);
        LogUtil.add( Data.SQL.Text);
      end;
      DisplayError( E.Message);
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
  Result := Data.FieldByName( FieldName).AsVariant;
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
begin
  primaryKey := pPrimaryKey;
  primaryKeyValue := '';
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


  FJoinList := TStringList.Create;
  {$ifdef zeos}
  still not supported
  {$else}
  FGenFields := nil;
  FieldValueMap := nil;
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

function TSimpleModel.All: boolean;
begin
  result := GetAll();
end;

{
same with:
  Object.Find([]);
}
function TSimpleModel.GetAll: boolean;
begin
  _queryPrepare;
  Data.SQL.Text := 'SELECT ' + FSelectField + ' FROM ' + FTableName;
  _queryOpen;
  Result := true;
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
  const Limit: integer; const CustomField: string): boolean;
var
  i,j : integer;
  _selectField, _joinSQL,
  sWhere : string;
  _joinfield,
  _join : TStrings;
begin
  Clear;
  primaryKeyValue := '';
  Result := false;
  sWhere := '';
  if high(Where)>=0 then
    for i:=low(Where) to high(Where) do
    begin
      if sWhere = '' then
        sWhere := Where[i]
      else begin
        if trim( Where[i]) <> '' then
          sWhere := sWhere + ' AND ' + Where[i];
      end;

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
      _joinSQL:= _joinSQL + #13#10 + _join[0] + ' ' + _join[1]
        + ' ON ' + _join[1] + '.' + _join[2] + '=' + _join[3]
        ;
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
  Data.SQL.Text := 'SELECT ' + _selectField + #13#10'FROM ' + FTableName + ' ' + FTableName + _joinSQL;

  if sWhere <> '' then
    Data.SQL.Add( 'WHERE ' + sWhere);
  if FGroupField <> '' then
    Data.SQL.Add( 'GROUP BY ' + FGroupField);
  if Order <> '' then
    Data.SQL.Add( 'ORDER BY ' + Order);
  if Limit > 0 then begin
    Data.SQL.Add( 'LIMIT ' + IntToStr( Limit));
  end;
  Data.UniDirectional:=False;
  Result := _queryOpen;
  Data.Last;
  Data.First;
  if (Data.RecordCount = 1) and (primaryKey <> '') then
  begin
    primaryKeyValue := Data.FieldByName( primaryKey).AsString;
  end;
end;

function TSimpleModel.FindFirst(const Where: array of string;
  const Order: string; const CustomField: string): boolean;
begin
  Result := Find( Where, Order, 1, CustomField);
end;

procedure TSimpleModel.AddJoin(const JoinTable: string;
  const JoinField: string; const RefField: string;
  const FieldNameList: array of string);
begin
  AddCustomJoin( 'LEFT', JoinTable, JoinField, RefField, FieldNameList);
end;

procedure TSimpleModel.AddLeftJoin(const JoinTable: string;
  const JoinField: string; const RefField: string;
  const FieldNameList: array of string);
begin
  AddCustomJoin( 'LEFT', JoinTable, JoinField, RefField, FieldNameList);
end;

procedure TSimpleModel.AddInnerJoin(const JoinTable: string;
  const JoinField: string; const RefField: string;
  const FieldNameList: array of string);
begin
  AddCustomJoin( 'INNER', JoinTable, JoinField, RefField, FieldNameList);
end;

procedure TSimpleModel.AddCustomJoin(const JointType: string;
  const JoinTable: string; const JoinField: string; const RefField: string;
  const FieldNameList: array of string);
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
  FJoinList.Add( JointType + ' JOIN|'+AppData.tablePrefix+JoinTable+'|'+JoinField+'|'+AppData.tablePrefix+RefField+'|'+s);
end;

procedure TSimpleModel.GroupBy(const GroupField: string);
begin
  FGroupField:= GroupField;
end;

procedure TSimpleModel.Clear;
begin
  New;
end;

procedure TSimpleModel.New;
begin
  if Assigned( FGenFields) then FGenFields.Clear;
  if Assigned( FieldValueMap) then FieldValueMap.Clear;
  primaryKeyValue:='';
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

  sSQL := TStringList.Create;
  if Where <> '' then
  begin
    sSQL.Add( 'UPDATE ' + TableName + ' SET ');
    for i:=0 to FGenFields.Count-1 do
    begin
      s := FGenFields[i]+'=:'+FGenFields[i];
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

procedure TSimpleModel.Next;
begin
  Data.Next;
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

  if not Assigned( DB_Connector_Write) then FreeAndNil( DB_Connector_Write);
  if not Assigned( DB_Transaction_Write) then FreeAndNil( DB_Transaction_Write);
  if not Assigned( DB_LibLoader_Write) then FreeAndNil( DB_LibLoader_Write);

end.
















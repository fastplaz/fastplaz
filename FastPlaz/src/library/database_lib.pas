unit database_lib;
{

public function:
- FieldLists
- GetAll



}
{$mode objfpc}{$H+}

interface

uses
  fpcgi, fphttp, db,
  sqldb, sqldblib, mysql50conn, mysql51conn, mysql55conn, {mysql56conn,}
  sqlite3conn, pqconnection,
  Classes, SysUtils;

type

  { TSimpleModel }

  TSimpleModel = class
  private
    FConnector : TSQLConnector;
    FFieldList : TStrings;
    FTableName : string;
    FGroupField : string;
    FSelectField : string;
    FJoinList : TStrings;
    function GetRecordCount: Longint;
    procedure _queryPrepare;
    function  _queryOpen:boolean;
    procedure DoAfterOpen(DataSet: TDataSet);
    procedure DoBeforeOpen(DataSet: TDataSet);

    function GetFieldList: TStrings;
    function GetFieldValue( FieldName: String): TField;
    procedure SetFieldValue( FieldName: String; AValue: TField);
  public
    Data : TSQLQuery;
    StartTime, StopTime, ElapsedTime : Cardinal;
    constructor Create( const DefaultTableName:string='');
    destructor Destroy; override;
    property TableName : string Read FTableName write FTableName;
    Property Value[ FieldName: String] : TField Read GetFieldValue Write SetFieldValue; default;
    Property FieldLists: TStrings Read GetFieldList;
    property RecordCount: Longint read GetRecordCount;

    function GetAll:boolean;
    //function Get( where, order):boolean;

    function Find( const Where:array of string; const Order:string = ''; const Limit:integer = 0; const CustomField:string=''):boolean;
    function FindFirst( const Where:array of string; const Order:string = ''; const CustomField:string=''):boolean;

    procedure AddJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string);
    procedure AddLeftJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string);
    procedure AddInnerJoin( const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string);
    procedure AddCustomJoin( const JointType:string; const JoinTable:string; const JoinField:string; const RefField: string; const FieldNameList:array of string);

    procedure GroupBy( const GroupField:string);

    procedure StartTransaction;
    procedure ReStartTransaction;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;

  end;

procedure DataBaseInit( const RedirecURL:string = '');

implementation

uses common, custom_handler, logutil_lib;

var
  _DB : TSQLConnector;
  __Transaction : TSQLTransaction;
  ___DBLibLoader : TSQLDBLibraryLoader;

procedure DataBaseInit(const RedirecURL: string);
begin
  if Config.GetValue( _DATABASE_LIBRARY, '') <> '' then begin
    ___DBLibLoader.ConnectionType:= Config.GetValue( _DATABASE_DRIVER, '');
    ___DBLibLoader.LibraryName:= Config.GetValue( _DATABASE_LIBRARY, '');;
    ___DBLibLoader.Enabled:= True;
    try
      ___DBLibLoader.LoadLibrary;
    except
      on E: Exception do begin
        if RedirecURL = '' then
          Die( E.Message)
        else
          _Redirect( RedirecURL);
      end;
    end;
  end;

  _DB.HostName:= Config.GetValue( _DATABASE_HOSTNAME, 'localhost');
  _DB.ConnectorType := Config.GetValue( _DATABASE_DRIVER, '');
  _DB.UserName:= Config.GetValue( _DATABASE_USERNAME, 'root');
  _DB.Password:= Config.GetValue( _DATABASE_PASSWORD, 'root');
  _DB.DatabaseName:= Config.GetValue( _DATABASE_DATABASENAME, 'test');
  //tabletype := Config.GetValue( _DATABASE_TABLETYPE, '');

  //log database

  try
    _DB.Open;
  except
    on E: Exception do begin
      if RedirecURL = '' then
        Die( E.Message)
      else
        _Redirect( RedirecURL);
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
  Result := 0;
  if not Data.Active then Exit;
  Result := Data.RecordCount;
end;

function TSimpleModel._queryOpen: boolean;
begin
  try
    Data.Open;
    Result := True;
  except
    on E: Exception do begin
      if AppData.debug then begin
        LogUtil.add( E.Message);
        LogUtil.add( Data.SQL.Text);
      end;
      Die( E.Message);
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

function TSimpleModel.GetFieldList: TStrings;
begin
  If Assigned(FFieldList) then begin

  end else begin
    FFieldList:=TStringList.Create;

    if Data.Active then Data.Close;
    Data.SQL.Text:= 'SHOW COLUMNS FROM ' + FTableName;
    Data.Open;
    FSelectField := '';
    while not Data.Eof do begin
      FFieldList.Add( FTableName + '.' +Data.FieldByName('Field').AsString);
      FSelectField := FSelectField + ',' + FTableName + '.' + Data.FieldByName('Field').AsString;
      Data.Next;
    end;
    FSelectField := Copy( FSelectField, 2, Length(FSelectField)-1);
  end;
  Result:=FFieldList;

end;

function TSimpleModel.GetFieldValue(FieldName: String): TField;
begin
  if not Data.Active then Exit;
  Result := Data.FieldByName( FieldName);
end;

procedure TSimpleModel.SetFieldValue( FieldName: String; AValue: TField);
begin

end;

constructor TSimpleModel.Create(const DefaultTableName: string);
var
  i : integer;
begin
  FConnector := _DB;
  if DefaultTableName = '' then begin
    for i:=2 to length( ToString)-1 do begin
      if (ToString[i] in ['A'..'Z']) then begin
        if FTableName <> '' then
          FTableName := FTableName + '_' + LowerCase( ToString[i])
        else
          FTableName := FTableName + LowerCase( ToString[i])
      end
      else
        FTableName := FTableName + ToString[i];
    end;
  end else
    FTableName:= DefaultTableName;
  FJoinList := TStringList.Create;
  {$ifdef zeos}
  sssss
  {$else}
  Data := TSQLQuery.Create(nil);
  Data.DataBase := _DB;
  Data.AfterOpen:= @DoAfterOpen;
  Data.BeforeOpen:= @DoBeforeOpen;
  {$endif}
end;

destructor TSimpleModel.Destroy;
begin
  if Data.Active then Data.Close;
  FreeAndNil( FJoinList);
  if Assigned( FFieldList) then
    FreeAndNil( FFieldList);
  FreeAndNil( Data);
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

function TSimpleModel.Find(const Where: array of string; const Order: string;
  const Limit: integer; const CustomField: string): boolean;
var
  i,j : integer;
  _selectField, _joinSQL,
  sWhere : string;
  _joinfield,
  _join : TStrings;
begin
  Result := false;
  sWhere := '';
  if high(Where)>=0 then
    for i:=low(Where) to high(Where) do begin
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
  _queryOpen;
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
  FJoinList.Add( JointType + ' JOIN|'+JoinTable+'|'+JoinField+'|'+RefField+'|'+s);
end;

procedure TSimpleModel.GroupBy(const GroupField: string);
begin
  FGroupField:= GroupField;
end;

procedure TSimpleModel.StartTransaction;
begin
  __Transaction.StartTransaction;
end;

procedure TSimpleModel.ReStartTransaction;
begin
  if __Transaction.Active then
    __Transaction.Active:= false;
  __Transaction.StartTransaction;
end;

procedure TSimpleModel.Commit;
begin
  __Transaction.Commit;
end;

procedure TSimpleModel.CommitRetaining;
begin
  __Transaction.CommitRetaining;
end;

procedure TSimpleModel.Rollback;
begin
  __Transaction.Rollback;
end;

procedure TSimpleModel.RollbackRetaining;
begin
  __Transaction.RollbackRetaining;
end;

initialization
  ___DBLibLoader := TSQLDBLibraryLoader.Create( nil);
  __Transaction := TSQLTransaction.Create( nil);
  _DB := TSQLConnector.Create( nil);
  _DB.Transaction := __Transaction;


finalization
  FreeAndNil( _DB);
  FreeAndNil( __Transaction);
  FreeAndNil( ___DBLibLoader);

end.


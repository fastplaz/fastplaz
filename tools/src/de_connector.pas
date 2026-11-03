unit de_connector;

{$mode objfpc}{$H+}

interface

{$include de.inc}

uses
  thread_custom, http_lib,
  {$ifdef FDE_DESKTOP}
  // dock desktop
  AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg, AnchorDockPanel,
  {$endif}

  // FCL, RTL
  fpjson, Classes, SysUtils, Math, StrUtils, Graphics,
  // LCL
  LCLProc, LCLType, LclIntf, Clipbrd, Forms, Controls, StdCtrls, Dialogs, ComCtrls,
  ActnList, XMLPropStorage, ExtCtrls, Buttons, Menus, IniPropStorage, SQLDB,
  // LazUtils
  //LazUTF8Classes, LazFileUtils, LazFileCache, LazLoggerBase,
  // Codetools
  //CodeCache, CodeToolManager, BasicCodeTools, FileProcs,
  // IDEIntf
  LazIDEIntf, PackageIntf, ProjectIntf, PackageDependencyIntf,
  IDEWindowIntf, IDEMsgIntf, IDEImagesIntf,
  IDEExternToolIntf,
  // Database Explorer
  DB, db_controller, model_lib, tools_lib,
  de_connection_editor, de_common, config_lib;

type

  { TIDEFDEWindow }

  TIDEFDEWindow = class(TForm)
    actConnect: TAction;
    actDisconnect: TAction;
    actAddConnection: TAction;
    actEditConnection: TAction;
    actDeleteConnection: TAction;
    actCopyFieldNames: TAction;
    actCreateORMModel: TAction;
    actTest: TAction;
    actTableOpen: TAction;
    actOnOffConnection: TAction;
    ActionListMain: TActionList;
    barBottom: TStatusBar;
    barHeader: TToolBar;
    DBImages: TImageList;
    pnlLoading: TPanel;
    PropStorage: TIniPropStorage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    popupMenuMain: TPopupMenu;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    tmrVersionCheck: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    tvConnectionList: TTreeView;
    procedure actAddConnectionExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actCreateORMModelExecute(Sender: TObject);
    procedure actDeleteConnectionExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actEditConnectionExecute(Sender: TObject);
    procedure actCopyFieldNamesExecute(Sender: TObject);
    procedure actOnOffConnectionExecute(Sender: TObject);
    procedure actTableOpenExecute(Sender: TObject);
    procedure actTestExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure FormUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure popupMenuMainPopup(Sender: TObject);
    procedure tmrVersionCheckTimer(Sender: TObject);
    procedure tvConnectionListClick(Sender: TObject);
    procedure tvConnectionListDblClick(Sender: TObject);
    procedure tvConnectionListDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure tvConnectionListDragOver(Sender, Source: TObject;
      X, Y: integer; State: TDragState; var Accept: boolean);
    procedure tvConnectionListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    // callback
    procedure onConnectorAfterConnect(Sender: TObject);
    procedure onConnectorAfterDisconnect(Sender: TObject);
    procedure onConnectorLog(Sender: TSQLConnection; EventType: TDBEventType;
      const Msg: string);
    procedure onQueryAfterOpen(DataSet: TDataSet);
  private
    tableList: TStrings;
    databaseType: string;
    lastActiveNode: TTreeNode;
    databaseAsData: TJSONData;
    // filled by worker thread, read by callback in main thread
    FConnectError: string;
    FViewDatabaseName: string;
    FUpdateFile: string;
    FUpdateMessage: string;
    function isMainThread: boolean;

    procedure showTableProc(Sender: TObject);
    procedure showTableCallbackProc(Sender: TObject);
    procedure showViewProc(Sender: TObject);
    procedure showViewCallbackProc(Sender: TObject);

    // (re)loads databaseAsData from the configuration
    function loadDatabaseConfig: boolean;
    procedure saveDatabaseConfig;
    function getConfigValue(AIndex: integer; AFieldName: string): string;
    procedure disableControl;
    procedure enableControl;
    procedure disconnectDatabase(Sender: TObject);
    procedure disconnectDatabaseCallbackProc(Sender: TObject);

    procedure saveFormState;
    procedure restoreFormState;
    // runs in a worker thread, result in FUpdateMessage
    procedure versionCheckProc(Sender: TObject);
    procedure versionCheckCallbackProc(Sender: TObject);
  public
    ConfigurationPath, ConfigurationFileName: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure log(AMessage: string; ACategory: string = '';
      UrgencyLevel: TMessageLineUrgency = mluNone);
    procedure BrowserOpen;
    procedure LoadConfiguration;

    procedure PrepareConnection(AIndex: integer);
    procedure BuildTreeView;
    function GetFieldNames(ATableName: string): string;
  end;

var
  IDEFDEWindow: TIDEFDEWindow;

procedure CreateIDEConnectorWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);

implementation

uses de_dbbrowser, fastplaz_tools_register;

procedure CreateIDEConnectorWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if CompareText(aFormName, FDE_WINDOW_NAME) <> 0 then
    exit;
  IDEWindowCreators.CreateForm(IDEFDEWindow, TIDEFDEWindow, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm := IDEFDEWindow;
end;

{$R *.lfm}


constructor TIDEFDEWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Name <> FDE_WINDOW_NAME then
    RaiseGDBException('');

  // Intialization

  tableList := TStringList.Create;
  Caption := FDE_WINDOW_NAME;
  lastActiveNode := nil;
  actOnOffConnection.ImageIndex := ICO_CONNECTION_OFF;
  LoadConfiguration;
  BuildTreeView;
  barHeader.Hide;
  pnlLoading.Hide;
  pnlLoading.Left := (Width-pnlLoading.Width) div 2;
  actTest.Visible := False;
  tmrVersionCheck.Enabled := True;
end;

destructor TIDEFDEWindow.Destroy;
begin
  if Assigned(Config) then
  begin
    Config.SetString('cache/checkout', FormatDateTime('yyyy-mm-dd HH:nn:ss', Now));
    Config.Flush;
  end;

  tableList.Free;
  FreeAndNil(databaseAsData);
  if Assigned(IDEFDEBrowserWindow) then
    FreeAndNil(IDEFDEBrowserWindow);
  FreeAndNil(Config);
  if IDEFDEWindow = Self then
    IDEFDEWindow := nil;
  inherited Destroy;
end;

function TIDEFDEWindow.isMainThread: boolean;
begin
  Result := GetCurrentThreadId = MainThreadID;
end;

procedure TIDEFDEWindow.log(AMessage: string; ACategory: string;
  UrgencyLevel: TMessageLineUrgency);
begin
  // IDE messages window and LCL controls are not thread safe
  if not isMainThread then
    Exit;
  {$ifdef FDE_DESKTOP}
  barBottom.SimpleText := ACategory + ': ' + AMessage;
  {$else}
  if ACategory.IsEmpty then
    IDEMessagesWindow.AddCustomMessage(UrgencyLevel, AMessage, '', 0, 0, 'FastPlaz')
  else
    IDEMessagesWindow.AddCustomMessage(UrgencyLevel, ACategory + ': ' +
    AMessage, '', 0, 0, 'FastPlaz');
  {$endif}

  if Assigned(IDEFDEBrowserWindow) then
  begin
    IDEFDEBrowserWindow.Log(AMessage, ACategory, UrgencyLevel);
  end;
end;

procedure TIDEFDEWindow.BrowserOpen;
begin
  if Assigned(IDEFDEBrowserWindow) then
  begin
    //
  end
  else
  begin
    IDEFDEBrowserWindow := TIDEFDEBrowserWindow.Create(nil);
  end;
  IDEFDEBrowserWindow.Enabled := True;

  {$ifdef FDE_DESKTOP}
  DockMaster.ShowControl(FDE_BROWSER_WINDOW_NAME, True);
  {$else}
  IDEWindowCreators.ShowForm(FDE_BROWSER_WINDOW_NAME, True);
  {$endif}

  {
  if not Assigned(IDEFDEBrowserWindow) then
    IDEFDEBrowserWindow := TIDEFDEBrowserWindow.Create(nil);
  IDEFDEBrowserWindow.Query.DataBase := DBConnector;
  IDEFDEBrowserWindow.Show;
  }
end;

procedure TIDEFDEWindow.LoadConfiguration;
begin
  ConfigurationPath := GetUserDir + FDE_CONFIG_PATH;
  ConfigurationFileName := ConfigurationPath + DirectorySeparator + FDE_CONFIG_FILE_NAME;
  if not DirectoryExists(ConfigurationPath) then
    CreateDir(ConfigurationPath);

  PropStorage.IniFileName := ConfigurationPath + DirectorySeparator + 'property';
  restoreFormState;

  Config := TMyConfig.Create(Self);
  Config.Filename := ConfigurationFileName;
  Config.Formatted := True;

  // initial config
  if not FileExists(ConfigurationFileName) then
  begin
    databaseAsData := GetJSON('[]');
    TJSONArray(databaseAsData).Add(
      TJSONObject.Create(['name', 'Test (localhost)', 'type', 'mysql',
      'driver', 'MySQL 8.0', 'hostname', 'localhost', 'port', '',
      'username', 'root', 'password', '', 'database_name', 'test',
      'charset', '', 'prefix', '', 'library', ''])
      );
    saveDatabaseConfig;
    FreeAndNil(databaseAsData);
  end;

  Config.SetString('cache/checkin', FormatDateTime('yyyy-mm-dd HH:nn:ss', Now));
  Config.Flush;

  log('Load configuration done.', 'FDE', mluNote);
end;

procedure TIDEFDEWindow.PrepareConnection(AIndex: integer);
var
  libraryFileName: string;
begin
  if DatabaseController.SQLConnector.Connected then
    DatabaseController.SQLConnector.Close(True);
  with DatabaseController.SQLConnector do
  begin
    ConnectorType := getConfigValue(AIndex, 'driver');
    HostName := getConfigValue(AIndex, 'hostname');
    UserName := getConfigValue(AIndex, 'username');
    Password := getConfigValue(AIndex, 'password');
    DatabaseName := getConfigValue(AIndex, 'database_name');
    // an empty value removes the parameter
    Params.Values['Port'] := Trim(getConfigValue(AIndex, 'port'));

    //Custom Library
    libraryFileName := getConfigValue(AIndex, 'library');
    if libraryFileName.IsEmpty then
      DatabaseController.UnloadLibrary
    else
    if not DatabaseController.LoadLibrary(libraryFileName) then
      log(DatabaseController.Message, 'FDE', mluError);

    AfterConnect := @onConnectorAfterConnect;
    AfterDisconnect := @onConnectorAfterDisconnect;
    LogEvents := [detExecute, detActualSQL];
    OnLog := @onConnectorLog;
    DatabaseController.Query.AfterOpen := @onQueryAfterOpen;
  end;
end;

procedure TIDEFDEWindow.BuildTreeView;
var
  i: integer;
  dbName, dbType: string;
  mainNode, dbNode: TTreeNode;
begin
  tvConnectionList.Items.Clear;
  lastActiveNode := nil;
  if not loadDatabaseConfig then
    Exit;

  mainNode := tvConnectionList.Items.Add(nil, 'Database');
  mainNode.ImageIndex := 0;
  mainNode.SelectedIndex := 0;
  for i := 0 to databaseAsData.Count - 1 do
  begin
    dbName := getConfigValue(i, 'name');
    dbType := getConfigValue(i, 'type');
    dbNode := tvConnectionList.Items.AddChild(mainNode, dbName);

    case dbType of
      'mysql': dbNode.ImageIndex := ICO_MYSQL;
      'postgres': dbNode.ImageIndex := ICO_POSTGRES;
      'oracle': dbNode.ImageIndex := ICO_ORACLE;
      'odbc': dbNode.ImageIndex := ICO_ODBC;
      'interbase': dbNode.ImageIndex := ICO_INTERBASE;
      'mssql': dbNode.ImageIndex := ICO_MSSQL;
      'sqlite': dbNode.ImageIndex := ICO_SQLITE;
      'firebird': dbNode.ImageIndex := ICO_FIREBIRD;
    end;
    dbNode.SelectedIndex := dbNode.ImageIndex;

  end;
  mainNode.Expand(False);
end;

function TIDEFDEWindow.loadDatabaseConfig: boolean;
var
  s: string;
begin
  Result := False;
  FreeAndNil(databaseAsData);
  s := Config.GetString('databases');
  if s.IsEmpty then
    Exit;
  try
    databaseAsData := GetJSON(s);
  except
    on E: Exception do
    begin
      log('Invalid connection list in ' + ConfigurationFileName + ': ' +
        E.Message, 'FDE', mluError);
      Exit;
    end;
  end;
  Result := databaseAsData is TJSONArray;
  if not Result then
    FreeAndNil(databaseAsData);
end;

procedure TIDEFDEWindow.saveDatabaseConfig;
begin
  Config.SetDataValue('databases', databaseAsData);
  Config.Flush;
end;

// running in worker thread: no log, no LCL access
procedure TIDEFDEWindow.showTableProc(Sender: TObject);
begin
  FConnectError := '';
  try
    OpenConnection(DatabaseController.SQLConnector);
    if not DatabaseController.SQLConnector.Connected then
    begin
      FConnectError := 'Connection failed';
      Exit;
    end;
    DatabaseController.SQLConnector.GetTableNames(tableList, False);
  except
    on E: Exception do
      FConnectError := E.Message;
  end;
end;

procedure TIDEFDEWindow.showTableCallbackProc(Sender: TObject);
var
  i: integer;
  tableNode: TTreeNode;
begin
  enableControl;
  if not FConnectError.IsEmpty then
  begin
    log(FConnectError, 'FDE', mluError);
    Exit;
  end;
  if not DatabaseController.Connected then
    Exit;

  // AfterConnect is skipped while connecting from worker thread
  onConnectorAfterConnect(nil);

  tableNode := tvConnectionList.Items.AddChild(tvConnectionList.Selected, 'Tables');
  tableNode.ImageIndex := ICO_TABLE;
  tableNode.SelectedIndex := ICO_TABLE;

  for i := 0 to tableList.Count - 1 do
  begin
    tvConnectionList.Items.AddChild(tableNode, tableList[i]);
  end;
  tvConnectionList.Selected.Expand(True);

  FViewDatabaseName := getConfigValue(tvConnectionList.Selected.Index, 'database_name');
  disableControl;
  Call(@showViewProc, @showViewCallbackProc);
end;

// running in worker thread: no log, no LCL access
procedure TIDEFDEWindow.showViewProc(Sender: TObject);
var
  s: string;
begin
  FConnectError := '';
  s := Format(SQL_SHOW_VIEWS_MYSQL, [FViewDatabaseName]);
  if databaseType = 'postgres' then
    s := SQL_SHOW_VIEWS_POSTGRES;

  if not DatabaseController.Connected then
    Exit;

  try
    DatabaseController.SQL.Text := s;
    DatabaseController.Query.Prepare;
    DatabaseController.Query.Open;
  except
    on E: Exception do
      FConnectError := E.Message;
  end;
end;

procedure TIDEFDEWindow.showViewCallbackProc(Sender: TObject);
var
  s, fieldName: string;
  viewNode: TTreeNode;
begin
  log(tvConnectionList.Selected.Text + ' connected', 'FDE');
  enableControl;
  viewNode := tvConnectionList.Items.AddChild(tvConnectionList.Selected, 'Views');
  viewNode.ImageIndex := ICO_VIEW;
  viewNode.SelectedIndex := ICO_VIEW;

  actOnOffConnection.ImageIndex := ICO_CONNECTION_ON;
  if not FConnectError.IsEmpty then
  begin
    log(FConnectError, 'FDE', mluError);
    Exit;
  end;
  if (not DatabaseController.Query.Active) or
    (DatabaseController.Query.RecordCount = 0) then
    Exit;

  fieldName := DatabaseController.Query.Fields[0].FieldName;
  repeat
    s := DatabaseController.Query.FieldByName(fieldName).AsString;
    tvConnectionList.Items.AddChild(viewNode, s);

    DatabaseController.Query.Next;
  until DatabaseController.Query.EOF;

  tvConnectionList.Selected.Expand(True);
end;

function TIDEFDEWindow.GetFieldNames(ATableName: string): string;
var
  fieldList: TStrings;
begin
  Result := '';
  fieldList := TStringList.Create;
  try
    DatabaseController.SQLConnector.GetFieldNames(ATableName, fieldList);
    Result := fieldList.Text.Trim.Replace(#13, '').Replace(#10, ', ', [rfReplaceAll]);
  finally
    fieldList.Free;
  end;
end;


procedure TIDEFDEWindow.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  //if Shift=[] then ;
  //if (Key=VK_ESCAPE) then
  //  ModalResult:=mrCancel;
end;

procedure TIDEFDEWindow.FormResize(Sender: TObject);
begin
  pnlLoading.Left := (Width-pnlLoading.Width) div 2;
end;

procedure TIDEFDEWindow.FormShow(Sender: TObject);
begin
  restoreFormState;
end;

procedure TIDEFDEWindow.FormStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
end;

procedure TIDEFDEWindow.FormUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
end;

procedure TIDEFDEWindow.popupMenuMainPopup(Sender: TObject);
begin
  actConnect.Enabled := False;
  actDisconnect.Enabled := False;
  actCopyFieldNames.Enabled := False;
  actEditConnection.Enabled := False;
  actDeleteConnection.Enabled := False;
  actCreateORMModel.Enabled := False;
  actTableOpen.Enabled := False;

  if tvConnectionList.Selected = nil then
    Exit;

  if tvConnectionList.Selected.Level = 1 then
  begin
    actConnect.Enabled := True;
    actDisconnect.Enabled := True;
    actEditConnection.Enabled := True;
    actDeleteConnection.Enabled := True;
  end;

  if tvConnectionList.Selected.Level = 3 then
  begin
    actCopyFieldNames.Enabled := True;
    actTableOpen.Enabled := True;
    {$ifndef FDE_DESKTOP}
    actCreateORMModel.Enabled := True;
    {$endif}
  end;

end;

procedure TIDEFDEWindow.tmrVersionCheckTimer(Sender: TObject);
begin
  tmrVersionCheck.Enabled := False;
  FUpdateFile := PackageDirectory(FASTPLAZ_TOOLS_PACKAGE);
  if FUpdateFile = '' then
    Exit;
  FUpdateFile := FUpdateFile + '..' + DirectorySeparator + LASTUPDATE_FILENAME;
  if not FileExists(FUpdateFile) then
    Exit;
  FUpdateMessage := '';
  // the request must not block the IDE
  Call(@versionCheckProc, @versionCheckCallbackProc);
end;

procedure TIDEFDEWindow.tvConnectionListClick(Sender: TObject);
begin

end;

procedure TIDEFDEWindow.tvConnectionListDblClick(Sender: TObject);
begin
  if tvConnectionList.Selected = nil then
    Exit;

  if tvConnectionList.Selected.Level = 0 then
  begin
    // close connection;
    Exit;
  end;

  // Open connection
  if tvConnectionList.Selected.Level = 1 then
  begin
    databaseType := getConfigValue(tvConnectionList.Selected.Index, 'type');
    if lastActiveNode <> nil then
    begin
      lastActiveNode.DeleteChildren;
    end;
    if Assigned(IDEFDEBrowserWindow) then
    begin
      IDEFDEBrowserWindow.DataSource.DataSet := nil;
      IDEFDEBrowserWindow.Disconnect;
    end;

    log('Connecting to ' + tvConnectionList.Selected.Text, 'FDE', mluProgress);
    PrepareDatabaseController;
    PrepareConnection(tvConnectionList.Selected.Index);

    disableControl;
    lastActiveNode := tvConnectionList.Selected;

    // Asycn Call
    //Application.QueueAsyncCall(@showTableAsync, PtrInt(messageData));
    Call(@showTableProc, @showTableCallbackProc);

    {
    ShowTables;
    ShowViews(getConfigValue(tvConnectionList.Selected.Index, 'database_name'));
    }
  end;

  // View table, view, function/procedure
  if tvConnectionList.Selected.Level = 3 then
  begin
    BrowserOpen;
    IDEFDEBrowserWindow.DatabaseType := databaseType;
    IDEFDEBrowserWindow.PreviewTable(tvConnectionList.Selected.Text);
    //IDEFDEBrowserWindow.pgMain.ActivePage := IDEFDEBrowserWindow.tabEditor;
  end;

end;

procedure TIDEFDEWindow.tvConnectionListDragDrop(Sender, Source: TObject;
  X, Y: integer);
begin

end;

procedure TIDEFDEWindow.tvConnectionListDragOver(Sender, Source: TObject;
  X, Y: integer; State: TDragState; var Accept: boolean);
begin

end;

procedure TIDEFDEWindow.tvConnectionListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
  begin

  end;
end;

procedure TIDEFDEWindow.onConnectorAfterConnect(Sender: TObject);
begin
  if not isMainThread then
    Exit;
  log('Database connected', 'FDE');
  if Assigned(IDEFDEBrowserWindow) then
  begin
    IDEFDEBrowserWindow.pgMain.Enabled := True;
    IDEFDEBrowserWindow.pgMain.ActivePage := IDEFDEBrowserWindow.tabEditor;
    IDEFDEBrowserWindow.tabStructure.TabVisible := True;
    IDEFDEBrowserWindow.EnableControl;
  end;
end;

procedure TIDEFDEWindow.onConnectorAfterDisconnect(Sender: TObject);
begin
  if not isMainThread then
    Exit;
  log('Database disconnected', 'FDE');
  if Assigned(IDEFDEBrowserWindow) then
  begin
    IDEFDEBrowserWindow.editor.Clear;
    IDEFDEBrowserWindow.pgMain.Enabled := False;
    IDEFDEBrowserWindow.tabStructure.TabVisible := False;
    IDEFDEBrowserWindow.DisableControl;
    IDEFDEBrowserWindow.btnFileName.Caption := '';
  end;
end;

procedure TIDEFDEWindow.onConnectorLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: string);
begin
  if EventType = detActualSQL then
    log(Msg.Replace(#13, ' ').Replace(#10, ' '), 'SQL', mluNote);
end;

procedure TIDEFDEWindow.onQueryAfterOpen(DataSet: TDataSet);
var
  s: string;
begin
  if not isMainThread then
    Exit;
  if Assigned(IDEFDEBrowserWindow) then
  begin
    s := IntToStr(DatabaseController.Query.RecordCount);
    IDEFDEBrowserWindow.barBottom.Panels[1].Text := 'Record count: ' + s;
  end;
end;

function TIDEFDEWindow.getConfigValue(AIndex: integer; AFieldName: string): string;
var
  item: TJSONData;
begin
  Result := '';
  if not Assigned(databaseAsData) then
    if not loadDatabaseConfig then
      Exit;
  if (AIndex < 0) or (AIndex >= databaseAsData.Count) then
    Exit;
  item := databaseAsData.Items[AIndex].FindPath(AFieldName);
  if (item <> nil) and not (item.JSONType in [jtNull, jtArray, jtObject]) then
    Result := item.AsString;
end;

procedure TIDEFDEWindow.disableControl;
begin
  pnlLoading.Show;
  tvConnectionList.BackgroundColor := $f0f0f0;
  tvConnectionList.Enabled := False;
  tvConnectionList.Cursor := crHourGlass;
end;

procedure TIDEFDEWindow.enableControl;
begin
  pnlLoading.Hide;
  tvConnectionList.BackgroundColor := clWhite;
  tvConnectionList.Enabled := True;
  tvConnectionList.Cursor := crDefault;
end;

procedure TIDEFDEWindow.disconnectDatabase(Sender: TObject);
begin
  if not Assigned(DatabaseController) then
    Exit;
  if DatabaseController.SQLConnector.Connected then
    DatabaseController.SQLConnector.Close(True);
end;

procedure TIDEFDEWindow.disconnectDatabaseCallbackProc(Sender: TObject);
begin
  enableControl;
  Log('Database Disconnected', 'FDE', mluDebug);
end;

procedure TIDEFDEWindow.saveFormState;
begin
  Exit;
  PropStorage.WriteInteger('left', Parent.Left);
  PropStorage.WriteInteger('top', Parent.Top);
end;

procedure TIDEFDEWindow.restoreFormState;
var
  i: integer;
begin
  Exit;
  i := PropStorage.ReadInteger('left', 0);
  if i > 0 then
    Left := i;
  i := PropStorage.ReadInteger('top', 0);
  if i > 0 then
    Top := i;
end;

// running in worker thread: no log, no LCL access
procedure TIDEFDEWindow.versionCheckProc(Sender: TObject);
var
  http: THTTPLib;
  response: IHTTPResponse;
  lst: TStringList;
begin
  FUpdateMessage := '';
  http := THTTPLib.Create();
  lst := TStringList.Create;
  try
    try
      http.URL := LASTUPDATE_URL;
      response := http.Get();
      if response.ResultCode <> 200 then
        Exit;
      lst.LoadFromFile(FUpdateFile);
      if Trim(lst.Text) <> Trim(response.ResultText) then
        FUpdateMessage := RS_THERE_ARE_DEVELOPMENT_UPDATE + Trim(response.ResultText);
    except
      on E: Exception do
        FUpdateMessage := '';
    end;
  finally
    lst.Free;
    http.Free;
  end;
end;

procedure TIDEFDEWindow.versionCheckCallbackProc(Sender: TObject);
begin
  if not FUpdateMessage.IsEmpty then
    log(FUpdateMessage, 'FastPlaz', mluImportant);
end;


procedure TIDEFDEWindow.FormCreate(Sender: TObject);
begin
  Caption := rsDBExplorer;
end;

procedure TIDEFDEWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  disconnectDatabase( nil);
  saveFormState;
end;

procedure TIDEFDEWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure TIDEFDEWindow.actConnectExecute(Sender: TObject);
begin
  tvConnectionListDblClick(Sender);
end;

procedure TIDEFDEWindow.actCreateORMModelExecute(Sender: TObject);
begin
  if (tvConnectionList.Selected = nil) or (tvConnectionList.Selected.Level <> 3) then
    Exit;
  ModelNameFromTable(tvConnectionList.Selected.Text, ModelName, CustomTableName);

  {$ifndef FDE_DESKTOP}
  GenerateFromFDE := True;
  try
    LazarusIDE.DoNewEditorFile(TFileDescModel.Create,
      LowerCase(SanitizeIdentifier(ModelName)) + '_model.pas', '',
      [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  finally
    GenerateFromFDE := False;
  end;
  {$endif}
end;

procedure TIDEFDEWindow.actDisconnectExecute(Sender: TObject);
begin
  if lastActiveNode = nil then
    Exit;
  if lastActiveNode <> nil then
    lastActiveNode.DeleteChildren;

  actOnOffConnection.ImageIndex := ICO_CONNECTION_OFF;
  if Assigned(IDEFDEBrowserWindow) then
  begin
    IDEFDEBrowserWindow.Disconnect;
    // browser is useless without connection, close (hide) it as well
    IDEFDEBrowserWindow.Close;
  end;

  disableControl;
  Call( @disconnectDatabase, @disconnectDatabaseCallbackProc);
end;

procedure TIDEFDEWindow.actAddConnectionExecute(Sender: TObject);
var
  dataType: string;
begin
  fDatabaseConnectionEditor := TfDatabaseConnectionEditor.Create(Application);
  try
    if fDatabaseConnectionEditor.ShowModal = mrOk then
    begin
      if not loadDatabaseConfig then
        databaseAsData := TJSONArray.Create;

      dataType := fDatabaseConnectionEditor.DatabaseType;

      TJSONArray(databaseAsData).Add(
        TJSONObject.Create(['name', fDatabaseConnectionEditor.edtName.Text,
        'type', dataType, 'driver', fDatabaseConnectionEditor.cbbDriver.Text,
        'hostname', fDatabaseConnectionEditor.edtHostname.Text,
        'port', fDatabaseConnectionEditor.edtPort.Text, 'username',
        fDatabaseConnectionEditor.edtUsername.Text, 'password',
        fDatabaseConnectionEditor.edtPassword.Text, 'database_name',
        fDatabaseConnectionEditor.edtDatabaseName.Text, 'charset',
        '', 'prefix', fDatabaseConnectionEditor.edtTablePrefix.Text,
        'library', fDatabaseConnectionEditor.edtLibrary.Text])
        );
      saveDatabaseConfig;
      BuildTreeView;

      log('New connection added');
    end;
  finally
    FreeAndNil(fDatabaseConnectionEditor);
  end;
end;

procedure TIDEFDEWindow.actDeleteConnectionExecute(Sender: TObject);
var
  i: integer;
  dbName: string;
begin
  if (tvConnectionList.Selected = nil) or (tvConnectionList.Selected.Level <> 1) then
    Exit;
  i := tvConnectionList.Selected.Index;
  dbName := getConfigValue(i, 'name');

  // Yes (the default) cancels, see rsAskCancelOperation
  if MessageDlg(rsConfirmation, Format(rsAskCancelOperation, [dbName]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    Exit;

  if not loadDatabaseConfig then
    Exit;
  if i >= databaseAsData.Count then
    Exit;
  if tvConnectionList.Selected = lastActiveNode then
    actDisconnect.Execute;

  TJSONArray(databaseAsData).Delete(i);
  saveDatabaseConfig;
  BuildTreeView;

  log('Delete "' + dbName + '" connection');
end;

procedure TIDEFDEWindow.actEditConnectionExecute(Sender: TObject);
var
  i: integer;
  dbType: string;
  item: TJSONObject;
begin
  if (tvConnectionList.Selected = nil) or (tvConnectionList.Selected.Level <> 1) then
    Exit;
  if not loadDatabaseConfig then
    Exit;
  i := tvConnectionList.Selected.Index;
  if (i >= databaseAsData.Count) or not (databaseAsData.Items[i] is TJSONObject) then
    Exit;
  item := TJSONObject(databaseAsData.Items[i]);

  fDatabaseConnectionEditor := TfDatabaseConnectionEditor.Create(Application);
  try
    fDatabaseConnectionEditor.edtName.Text := getConfigValue(i, 'name');
    fDatabaseConnectionEditor.cbbDriver.Text := getConfigValue(i, 'driver').Trim;

    fDatabaseConnectionEditor.edtHostname.Text := getConfigValue(i, 'hostname');
    fDatabaseConnectionEditor.edtUsername.Text := getConfigValue(i, 'username');
    fDatabaseConnectionEditor.edtPassword.Text := getConfigValue(i, 'password');
    fDatabaseConnectionEditor.edtPort.Text := getConfigValue(i, 'port');
    fDatabaseConnectionEditor.edtDatabaseName.Text := getConfigValue(i, 'database_name');
    fDatabaseConnectionEditor.edtTablePrefix.Text := getConfigValue(i, 'prefix');
    fDatabaseConnectionEditor.edtLibrary.Text := getConfigValue(i, 'library');

    if fDatabaseConnectionEditor.ShowModal = mrOk then
    begin
      dbType := fDatabaseConnectionEditor.DatabaseType;

      with fDatabaseConnectionEditor do
      begin
        // Strings replaces the value or adds the missing key
        item.Strings['name'] := edtName.Text;
        item.Strings['type'] := dbType;
        item.Strings['driver'] := cbbDriver.Text;
        item.Strings['hostname'] := edtHostname.Text;
        item.Strings['port'] := edtPort.Text;
        item.Strings['username'] := edtUsername.Text;
        item.Strings['password'] := edtPassword.Text;
        item.Strings['database_name'] := edtDatabaseName.Text;
        item.Strings['charset'] := '';
        item.Strings['prefix'] := edtTablePrefix.Text;
        item.Strings['library'] := edtLibrary.Text;
      end;

      saveDatabaseConfig;
      BuildTreeView;
      log('Connection "' + item.Strings['name'] + '" updated');
    end;
  finally
    FreeAndNil(fDatabaseConnectionEditor);
  end;
end;

procedure TIDEFDEWindow.actCopyFieldNamesExecute(Sender: TObject);
var
  s: string;
begin
  if (tvConnectionList.Selected = nil) or (tvConnectionList.Selected.Level <> 3) then
    exit;

  s := GetFieldNames(tvConnectionList.Selected.Text);
  Clipboard.AsText := s;
  {$ifndef FDE_DESKTOP}
  InserTextToEditor(s);
  {$endif}
end;

procedure TIDEFDEWindow.actOnOffConnectionExecute(Sender: TObject);
begin
  if not Assigned(DatabaseController) then
    Exit;
  if DatabaseController.Connected then
    actDisconnect.Execute;
end;

procedure TIDEFDEWindow.actTableOpenExecute(Sender: TObject);
begin
  if (tvConnectionList.Selected <> nil) and (tvConnectionList.Selected.Level = 3) then
    tvConnectionListDblClick(Sender);
end;

procedure TIDEFDEWindow.actTestExecute(Sender: TObject);
begin
  // hidden action, kept for the designer
end;


end.

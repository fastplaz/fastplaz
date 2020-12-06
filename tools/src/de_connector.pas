unit de_connector;

{$mode objfpc}{$H+}

interface

{$include de.inc}

uses
  thread_custom,
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
  LazUTF8Classes, LazFileUtils, LazFileCache, LazLoggerBase,
  // Codetools
  //CodeCache, CodeToolManager, BasicCodeTools, FileProcs,
  // IDEIntf
  LazIDEIntf, PackageIntf, ProjectIntf, PackageDependencyIntf,
  IDEWindowIntf, IDEMsgIntf, IDEImagesIntf,
  IDEExternToolIntf,
  // Database Explorer
  DB, db_controller, model_lib,
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
    messageData: TMessageData;
    FThread: TCustomThread;
    databaseType: string;
    lastActiveNode: TTreeNode;
    databaseAsData: TJSONData;
    procedure ShowTables;
    procedure ShowViews(ADatabaseName: string);

    procedure showTableAsync(Data: PtrInt);
    procedure showTableProc(Sender: TObject);
    procedure showTableCallbackProc(Sender: TObject);
    procedure showViewProc(Sender: TObject);
    procedure showViewCallbackProc(Sender: TObject);

    function getConfigValue(AIndex: integer; AFieldName: string): string;
    procedure callbackTest(const AStatus: integer; const AMessage: string;
      var AQuery: TSQLQuery);
    procedure OnExecuteProc(Sender: TObject);
    procedure OnSuccessProc(Sender: TObject);
    procedure disableControl;
    procedure enableControl;
    procedure disconnectDatabase(Sender: TObject);
    procedure disconnectDatabaseCallbackProc(Sender: TObject);

    procedure saveFormState;
    procedure restoreFormState;
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

uses de_dbbrowser;

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
end;

destructor TIDEFDEWindow.Destroy;
begin
  Config['cache/checkout'] := FormatDateTime('yyyy-mm-dd HH:nn:ss', Now);
  Config.Flush;

  tableList.Free;
  if Assigned(IDEFDEBrowserWindow) then
    IDEFDEBrowserWindow.Free;
  Config.Free;
  inherited Destroy;
end;

procedure TIDEFDEWindow.log(AMessage: string; ACategory: string;
  UrgencyLevel: TMessageLineUrgency);
begin
  {$ifdef FDE_DESKTOP}
  barBottom.SimpleText := ACategory + ': ' + AMessage;
  {$else}
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
    IDEFDEBrowserWindow.Enabled := True;
  end
  else
  begin
    //IDEFDEBrowserWindow := TIDEFDEBrowserWindow.Create(nil);
  end;

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
var
  s: string;
begin
  ConfigurationPath := GetUserDir + FDE_CONFIG_PATH;
  ConfigurationFileName := ConfigurationPath + DirectorySeparator + FDE_CONFIG_FILE_NAME;
  if not DirectoryExistsUTF8(ConfigurationPath) then
    CreateDirUTF8(ConfigurationPath);

  PropStorage.IniFileName := ConfigurationPath + DirectorySeparator + 'property';
  restoreFormState;

  Config := TMyConfig.Create(Self);
  Config.Filename := ConfigurationFileName;
  Config.Formatted := True;

  // initial config
  if not FileExistsUTF8(ConfigurationFileName) then
  begin
    databaseAsData := GetJSON('[]');
    TJSONArray(databaseAsData).Add(
      TJSONObject.Create(['name', 'Test (localhost)', 'type', 'mysql',
      'driver', 'MySQL 5.7', 'hostname', 'localhost', 'port', '',
      'username', 'root', 'password', '', 'database_name', 'test',
      'charset', '', 'prefix', '', 'library', ''])
      );
    Config.SetDataValue('databases', databaseAsData);
    FreeAndNil(databaseAsData);
  end;

  Config['cache/checkin'] := FormatDateTime('yyyy-mm-dd HH:nn:ss', Now);
  Config.Flush;

  log('Load configuration done.', 'FDE');
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

    //Custom Library
    libraryFileName := getConfigValue(AIndex, 'library');
    if not libraryFileName.IsEmpty then
      DatabaseController.LoadLibrary(libraryFileName);

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
  s: string;
  dbName, dbType, dbDriver, dbHostname, dbPort, dbUsername, dbPassword,
  dbDatabaseName, dbChartset, dbPrefix, dbLibrary: string;
  mainNode, dbNode: TTreeNode;
begin
  tvConnectionList.Items.Clear;
  s := Config['databases'];
  if s.IsEmpty then
    Exit;
  ;

  databaseAsData := GetJSON(s);

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

  FreeAndNil(databaseAsData);
end;

procedure TIDEFDEWindow.ShowTables;
var
  i: integer;
  s, fieldTableName: string;
  tableNode, itemNode: TTreeNode;
begin
  try
    DatabaseController.SQLConnector.Open;
  except
    on E: Exception do
    begin
      log(E.Message, 'FDE', mluError);
      Exit;
    end;
  end;

  if not DatabaseController.SQLConnector.Connected then
  begin
    //TODO: display error
    Exit;
  end;

  tableNode := tvConnectionList.Items.AddChild(tvConnectionList.Selected, 'Tables');
  tableNode.ImageIndex := ICO_TABLE;
  tableNode.SelectedIndex := ICO_TABLE;

  DatabaseController.SQLConnector.GetTableNames(tableList, False);
  for i := 0 to tableList.Count - 1 do
  begin
    itemNode := tvConnectionList.Items.AddChild(tableNode, tableList[i]);
  end;
  tvConnectionList.Selected.Expand(True);
end;

procedure TIDEFDEWindow.ShowViews(ADatabaseName: string);
var
  i: integer;
  s, fieldName: string;
  tableNode, itemNode: TTreeNode;
begin
  try
    DatabaseController.SQLConnector.Open;
  except
    on E: Exception do
    begin
      log(E.Message, 'FDE', mluError);
      Exit;
    end;
  end;

  if not DatabaseController.SQLConnector.Connected then
  begin
    //TODO: display error
    Exit;
  end;

  tableNode := tvConnectionList.Items.AddChild(tvConnectionList.Selected, 'Views');
  tableNode.ImageIndex := ICO_VIEW;
  tableNode.SelectedIndex := ICO_VIEW;

  s := Format(SQL_SHOW_VIEWS_MYSQL, [ADatabaseName]);
  if databaseType = 'postgres' then
    s := SQL_SHOW_VIEWS_POSTGRES;
  DatabaseController.SQL.Text := s;
  DatabaseController.Query.Prepare;
  DatabaseController.Query.Open;
  if DatabaseController.Query.RecordCount = 0 then
    Exit;
  if DatabaseController.Query.Active then
  begin

    fieldName := DatabaseController.Query.Fields[0].FieldName;
    repeat
      s := DatabaseController.Query.FieldByName(fieldName).AsString;
      itemNode := tvConnectionList.Items.AddChild(tableNode, s);

      DatabaseController.Query.Next;
    until DatabaseController.Query.EOF;

    tvConnectionList.Selected.Expand(True);
  end;
end;

procedure TIDEFDEWindow.showTableAsync(Data: PtrInt);
begin
  ShowTables;
  ShowViews(getConfigValue(tvConnectionList.Selected.Index, 'database_name'));
  enableControl;
end;

procedure TIDEFDEWindow.showTableProc(Sender: TObject);
begin
  try
    DatabaseController.SQLConnector.Open;
    if not DatabaseController.SQLConnector.Connected then
      Exit;
    DatabaseController.SQLConnector.GetTableNames(tableList, False);
  except
    on E: Exception do
    begin
      log(E.Message, 'FDE', mluError);
    end;
  end;
end;

procedure TIDEFDEWindow.showTableCallbackProc(Sender: TObject);
var
  i: integer;
  tableNode, itemNode: TTreeNode;
begin
  enableControl;
  if not DatabaseController.Connected then
    Exit;
  tableNode := tvConnectionList.Items.AddChild(tvConnectionList.Selected, 'Tables');
  tableNode.ImageIndex := ICO_TABLE;
  tableNode.SelectedIndex := ICO_TABLE;

  for i := 0 to tableList.Count - 1 do
  begin
    itemNode := tvConnectionList.Items.AddChild(tableNode, tableList[i]);
  end;
  tvConnectionList.Selected.Expand(True);

  disableControl;
  Call(@showViewProc, @showViewCallbackProc);
end;

procedure TIDEFDEWindow.showViewProc(Sender: TObject);
var
  s, databaseName, fieldName: string;
  tableNode, itemNode: TTreeNode;
begin
  databaseName := getConfigValue(tvConnectionList.Selected.Index, 'database_name');

  s := Format(SQL_SHOW_VIEWS_MYSQL, [databaseName]);
  if databaseType = 'postgres' then
    s := SQL_SHOW_VIEWS_POSTGRES;

  if not DatabaseController.Connected then
    Exit;

  DatabaseController.SQL.Text := s;
  DatabaseController.Query.Prepare;
  DatabaseController.Query.Open;
end;

procedure TIDEFDEWindow.showViewCallbackProc(Sender: TObject);
var
  i: integer;
  s, fieldName: string;
  viewNode, itemNode: TTreeNode;
begin
  log(tvConnectionList.Selected.Text + ' connected', 'FDE');
  enableControl;
  viewNode := tvConnectionList.Items.AddChild(tvConnectionList.Selected, 'Views');
  viewNode.ImageIndex := ICO_VIEW;
  viewNode.SelectedIndex := ICO_VIEW;

  actOnOffConnection.ImageIndex := ICO_CONNECTION_ON;
  if DatabaseController.Query.RecordCount = 0 then
    Exit;

  fieldName := DatabaseController.Query.Fields[0].FieldName;
  repeat
    s := DatabaseController.Query.FieldByName(fieldName).AsString;
    itemNode := tvConnectionList.Items.AddChild(viewNode, s);

    DatabaseController.Query.Next;
  until DatabaseController.Query.EOF;

  tvConnectionList.Selected.Expand(True);
end;

function TIDEFDEWindow.GetFieldNames(ATableName: string): string;
var
  s: string;
  fieldList: TStrings;
begin
  Result := '';
  fieldList := TStringList.Create;
  DatabaseController.SQLConnector.GetFieldNames(ATableName, fieldList);
  Result := fieldList.Text.Trim.Replace(#10, ', ', [rfReplaceAll]);
  fieldList.Free;
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

procedure TIDEFDEWindow.tvConnectionListClick(Sender: TObject);
begin

end;

procedure TIDEFDEWindow.tvConnectionListDblClick(Sender: TObject);
var
  i: integer;
  s: string;
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
  if Assigned(IDEFDEBrowserWindow) then
  begin
    s := IntToStr(DatabaseController.Query.RecordCount);
    IDEFDEBrowserWindow.barBottom.Panels[1].Text := 'Record count: ' + s;
  end;
end;

function TIDEFDEWindow.getConfigValue(AIndex: integer; AFieldName: string): string;
var
  s: string;
begin
  try
    Result := '';
    if not Assigned(databaseAsData) then
    begin
      s := Config['databases'];
      if s.IsEmpty then
        Exit;
      databaseAsData := GetJSON(s);
    end;
    Result := databaseAsData.Items[AIndex].FindPath(AFieldName).AsString;
  except
    on E: Exception do
    begin
      ShowMessage(e.Message);
    end;
  end;
end;

// FOR TESTING ONLY
procedure TIDEFDEWindow.callbackTest(const AStatus: integer;
  const AMessage: string; var AQuery: TSQLQuery);
var
  prefix: string;
begin
  {
  prefix := '';
  if AStatus <> 0 then
    prefix := 'Error:';

  enableControl;
  log(prefix + AMessage, 'Callback: ' + AQuery.RecordCount.ToString);
  }
end;

procedure TIDEFDEWindow.OnExecuteProc(Sender: TObject);
begin
  TCustomThread(Sender).StatusText := 'welcome OnExecuteProc ...';
end;

procedure TIDEFDEWindow.OnSuccessProc(Sender: TObject);
begin
  Caption := TCustomThread(Sender).StatusText;
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


procedure TIDEFDEWindow.FormCreate(Sender: TObject);
begin
  Caption := rsDBExplorer;
end;

procedure TIDEFDEWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
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
  if tvConnectionList.Selected.Level <> 3 then
    Exit;
  CustomTableName := '';
  ModelName := tvConnectionList.Selected.Text;
  if ModelName[modelName.Length] <> 's' then
    CustomTableName := ModelName
  else
    ModelName := ModelName.Substring(0, ModelName.Length - 1);

  {$ifndef FDE_DESKTOP}
  GenerateFromFDE := True;
  LazarusIDE.DoNewEditorFile(TFileDescModel.Create,
    ModelName + '_model.pas', '',
    [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  //GenerateFromFDE := False;
  {$endif}
end;

procedure TIDEFDEWindow.actDisconnectExecute(Sender: TObject);
begin
  if lastActiveNode = nil then
    Exit;
  if lastActiveNode <> nil then
    lastActiveNode.DeleteChildren;

  Call( @disconnectDatabase, @disconnectDatabaseCallbackProc);

  actOnOffConnection.ImageIndex := ICO_CONNECTION_OFF;
  if Assigned(IDEFDEBrowserWindow) then
    IDEFDEBrowserWindow.Disconnect;
end;

procedure TIDEFDEWindow.actAddConnectionExecute(Sender: TObject);
var
  s, dataType: string;
begin
  fDatabaseConnectionEditor := TfDatabaseConnectionEditor.Create(Application);
  if fDatabaseConnectionEditor.ShowModal = mrOk then
  begin
    s := Config['databases'];
    databaseAsData := GetJSON(s);

    dataType := 'firebird';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex < 8 then
      dataType := 'mysql';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex = 8 then
      dataType := 'odbc';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex = 9 then
      dataType := 'oracle';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex = 10 then
      dataType := 'postgres';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex = 11 then
      dataType := 'sqlite';

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
    Config.SetDataValue('databases', databaseAsData);
    Config.Flush;
    FreeAndNil(databaseAsData);

    BuildTreeView;

    log('New connection added');
  end;
  fDatabaseConnectionEditor.Free;
end;

procedure TIDEFDEWindow.actDeleteConnectionExecute(Sender: TObject);
var
  i: integer;
  s, dbName: string;
begin
  if ((tvConnectionList.Selected.Level <> 1) or
    (tvConnectionList.Selected.Index = -1)) then
    Exit;

  if MessageDlg(rsConfirmation, rsAskCancelOperation, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    Exit;
  end;

  s := Config['databases'];
  if s.IsEmpty then
    Exit;
  databaseAsData := GetJSON(s);

  i := tvConnectionList.Selected.Index;
  dbName := databaseAsData.Items[i].FindPath('name').AsString;

  TJSONArray(databaseAsData).Delete(i);

  Config.SetDataValue('databases', databaseAsData);
  Config.Flush;
  FreeAndNil(databaseAsData);

  BuildTreeView;

  log('Delete "' + dbName + '" connection');
end;

procedure TIDEFDEWindow.actEditConnectionExecute(Sender: TObject);
var
  i: integer;
  s: string;
  dbName, dbType, dbDriver, dbHostname, dbPort, dbUsername, dbPassword,
  dbDatabaseName, dbChartset, dbPrefix, dbLibrary: string;
begin
  s := Config['databases'];
  if s.IsEmpty then
    Exit;
  databaseAsData := GetJSON(s);

  i := tvConnectionList.Selected.Index;
  dbName := databaseAsData.Items[i].FindPath('name').AsString;
  dbType := databaseAsData.Items[i].FindPath('type').AsString;
  dbDatabaseName := databaseAsData.Items[i].FindPath('database_name').AsString;

  fDatabaseConnectionEditor := TfDatabaseConnectionEditor.Create(Application);
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
    dbType := 'firebird';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex < 8 then
      dbType := 'mysql';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex = 8 then
      dbType := 'odbc';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex = 9 then
      dbType := 'oracle';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex = 10 then
      dbType := 'postgres';
    if fDatabaseConnectionEditor.cbbDriver.ItemIndex = 11 then
      dbType := 'sqlite';

    with fDatabaseConnectionEditor do
    begin
      databaseAsData.Items[i].FindPath('name').Value := edtName.Text;
      databaseAsData.Items[i].FindPath('type').Value := dbType;
      databaseAsData.Items[i].FindPath('driver').Value := cbbDriver.Text;
      databaseAsData.Items[i].FindPath('hostname').Value := edtHostname.Text;
      databaseAsData.Items[i].FindPath('port').Value := edtPort.Text;
      databaseAsData.Items[i].FindPath('username').Value := edtUsername.Text;
      databaseAsData.Items[i].FindPath('password').Value := edtPassword.Text;
      databaseAsData.Items[i].FindPath('database_name').Value := edtDatabaseName.Text;
      databaseAsData.Items[i].FindPath('charset').Value := '';
      databaseAsData.Items[i].FindPath('prefix').Value := '';
      databaseAsData.Items[i].FindPath('library').Value := edtLibrary.Text;
    end;

    Config.SetDataValue('databases', databaseAsData);
    Config.Flush;
    FreeAndNil(databaseAsData);

    BuildTreeView;
    log('OK');
  end;
  fDatabaseConnectionEditor.Free;

  FreeAndNil(databaseAsData);
end;

procedure TIDEFDEWindow.actCopyFieldNamesExecute(Sender: TObject);
var
  s: string;
begin
  if tvConnectionList.Selected.Level <> 3 then
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
  if tvConnectionList.Selected.Level = 3 then
    tvConnectionListDblClick(Sender);
end;

// FOR TESTING ONLY
procedure TIDEFDEWindow.actTestExecute(Sender: TObject);
begin
  //Call(@OnExecuteProc, @OnSuccessProc);
end;


end.

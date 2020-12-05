unit de_dbbrowser;
//TODO: auto save query on close

{$mode objfpc}{$H+}

interface

uses
  de_common, de_dbmemoedit, db_controller,thread_custom,
  config_lib,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, DBGrids,
  // INTF
  IDEExternToolIntf,
  {$ifndef FDE_DESKTOP}
  PackageIntf, IDEWindowIntf, LazIDEIntf,
  {$endif}
  // Editor
  SynEdit, SynHighlighterSQL, SynCompletion, SynEditKeyCmds,
  SynHighlighterMulti, Clipbrd, LCLProc, LCLType, DB, SQLDB, Grids, ActnList,
  StdCtrls, Menus, IniPropStorage, Types, SynEditTypes;

type

  { TIDEFDEBrowserWindow }

  TIDEFDEBrowserWindow = class(TForm)
    actExecQuery: TAction;
    actFieldCopy: TAction;
    actOpenFile: TAction;
    ActionList: TActionList;
    barBottom: TStatusBar;
    DataSource: TDataSource;
    editor: TSynEdit;
    Grid: TDBGrid;
    PropStorage: TIniPropStorage;
    lbLog: TListBox;
    lblStructureTableName: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    pgMain: TPageControl;
    popupEditor: TPopupMenu;
    popupGrid: TPopupMenu;
    Splitter1: TSplitter;
    StringGrid: TStringGrid;
    SynCompletion1: TSynCompletion;
    MultiSyn: TSynMultiSyn;
    SynSQL: TSynSQLSyn;
    tabEditor: TTabSheet;
    tabContent: TTabSheet;
    tabStructure: TTabSheet;
    TabSheet3: TTabSheet;
    barHeader: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    btnFileName: TToolButton;
    procedure actExecQueryExecute(Sender: TObject);
    procedure actFieldCopyExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure editorKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure editorKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure editorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure editorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure GridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure QueryBeforeOpen(DataSet: TDataSet);
    procedure tabContentContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: boolean);
    procedure tabContentEnter(Sender: TObject);
  private
    messageData: TMessageData;
    fEndOfLine: boolean;
    iLastPosX, iLastPosY: integer;
    pastSQL: string;
    procedure setupScheme(AControl: TSynEdit);
    procedure updateCursorPosition(AControl: TSynEdit; ACurrentPostX: integer);
    procedure getTableStructure(ATableName: string);

    // Tab Control
    function getTabControl(ATableName: string): TTabSheet;
    function generateTab(ATableName: string): TTabSheet;
    procedure destroyAllTab;
    procedure onQueryAsync(Data: PtrInt);
    procedure onQueryCallback(const AStatus: integer; const AMessage: string;
      var AQuery: TSQLQuery);

    procedure saveFormState;
    procedure restoreFormState;
  public
    DatabaseType: string;
    procedure DisableControl;
    procedure EnableControl;
    procedure PreviewTable(ATableName: string);
    procedure Disconnect;
    procedure Log(AMessage: string; AGroup: string = ''; UrgencyLevel: TMessageLineUrgency = mluNone);

  end;

var
  IDEFDEBrowserWindow: TIDEFDEBrowserWindow;

procedure CreateIDEBrowserWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);

implementation

procedure CreateIDEBrowserWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  {$ifndef FDE_DESKTOP}
  if CompareText(aFormName, FDE_BROWSER_WINDOW_NAME) <> 0 then
    exit;
  IDEWindowCreators.CreateForm(IDEFDEBrowserWindow, TIDEFDEBrowserWindow,
    DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm := IDEFDEBrowserWindow;
  {$endif}
end;


{$R *.lfm}

{ TIDEFDEBrowserWindow }

procedure TIDEFDEBrowserWindow.FormCreate(Sender: TObject);
begin
  if Name <> FDE_BROWSER_WINDOW_NAME then
    RaiseGDBException('');

  // form state
  PropStorage.IniFileName := GetUserDir + FDE_CONFIG_PATH + DirectorySeparator + 'property';

  //Caption := FDE_BROWSER_WINDOW_NAME;
  Caption := 'DB Browser';
  btnFileName.Caption := '';
  editor.Clear;
  SynSQL.Enabled := True;
  MultiSyn.Enabled := True;
  UseDockManager := True;
  fEndOfLine := False;
  iLastPosX := 0;
  iLastPosY := 0;

  lbLog.Clear;
  lbLog.Align := alClient;
  pgMain.Align := alClient;
  Grid.Align := alClient;
  StringGrid.Align := alClient;
  pgMain.ActivePage := tabEditor;
  tabContent.TabVisible := False;
  tabEditor.TabVisible := False;

  lbLog.Font.Name := 'Courier New';
  color := $3A4546;
  barHeader.Font.Color := $f0f0f0;
  setupScheme(editor);

  Enabled := True;
end;

procedure TIDEFDEBrowserWindow.actExecQueryExecute(Sender: TObject);
var
  s, sql: string;
  sqlEditor: TSynEdit;
  dq: TSQLQuery;
  tabSheet: TTabSheet;
begin
  //tabSheet := generateTab('contacts');
  barBottom.Panels[1].Text := '';

  // get active editor
  if not (pgMain.ActivePage.Components[TAB_EDITOR_INDEX] is TSynEdit) then
    Exit;

  sqlEditor := TSynEdit(pgMain.ActivePage.Components[TAB_EDITOR_INDEX]);
  dq := TSQLQuery(pgMain.ActivePage.Components[TAB_QUERY_INDEX]);
  if dq.Active then
    dq.Close;

  if sqlEditor.Text.IsEmpty then
    Exit;//TODO: warning

  sql := sqlEditor.Text;

  if sqlEditor.SelAvail then
  begin
    sql := sqlEditor.SelText;
  end
  else
  begin
    if pastSQL.IsEmpty then
      sql := sqlEditor.Text
    else
      sql := pastSQL;
    pastSQL := '';
  end;

  if preg_match(QUERY_DELETE_REGEX, sql.Trim) then
  begin
    if MessageDlg(rsConfirmation, rsAskCancelDangerOperation,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Exit;
    end;
  end;

  // thread mode
  //DatabaseController.OpenThread(dq, sql, @onQueryCallback);


  // direct mode
  dq.SQL.Text := sql;
  try
    if preg_match(QUERY_SELECT_REGEX, sql.Trim) then
      dq.Open
    else
      dq.ExecSQL;
  except
    on E: Exception do
    begin
      s := E.Message.Replace(#13, ' ').Replace(#10, ' ');
      Log(s, 'ERROR', mluError);
      barBottom.Panels[1].Text := s;
    end;
  end;


end;

procedure TIDEFDEBrowserWindow.actFieldCopyExecute(Sender: TObject);
var
  s: string;
begin
  if not (pgMain.ActivePage.Components[TAB_EDITOR_INDEX] is TSynEdit) then
    Exit;

  s := TDBGrid(pgMain.ActivePage.Components[TAB_GRID_INDEX]).SelectedField.AsString;
  Clipboard.AsText := s;
end;

procedure TIDEFDEBrowserWindow.actOpenFileExecute(Sender: TObject);
begin
  // get active editor
  if not (pgMain.ActivePage.Components[TAB_EDITOR_INDEX] is TSynEdit) then
    Exit;

  with TOpenDialog.Create(Application) do
  begin
    Title := 'Open SQL file';
    Filter := 'SQL files (*.sql)|*.sql';
    InitialDir := Config['cache/last_dir'];
    Options := [ofReadOnly, ofEnableSizing];
    if Execute then
    begin
      Config['cache/last_dir'] := ExtractFilePath(FileName);
      Config.Flush;
      TSynEdit(pgMain.ActivePage.Components[TAB_EDITOR_INDEX]).Lines.LoadFromFile(
        FileName);
      btnFileName.Caption := FileName;
      barBottom.Panels[1].Text := 'Open file: ' + FileName;
    end;
    Free;
  end;
end;

procedure TIDEFDEBrowserWindow.btnFileNameClick(Sender: TObject);
begin
end;

procedure TIDEFDEBrowserWindow.editorKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  {$IFDEF DARWIN}
  if TSynEdit(Sender).Focused then
  begin
    if (((ssShift in Shift)) and (ssMeta in Shift)) then
    begin
      case Key of
        VK_KEYUP:
        begin
          TSynEdit(Sender).CommandProcessor(TSynEditorCommand(ecEditorTop), ' ', nil);
          key := 0;
        end;
        VK_KEYDOWN:
        begin
          TSynEdit(Sender).CommandProcessor(TSynEditorCommand(ecEditorBottom), ' ', nil);
          key := 0;
        end;
        VK_KEYLEFT:
        begin
          TSynEdit(Sender).CommandProcessor(TSynEditorCommand(ecSelWordLeft), ' ', nil);
          Key := 0;
        end;
        VK_KEYRIGHT:
        begin
          TSynEdit(Sender).CommandProcessor(TSynEditorCommand(ecLineEnd), ' ', nil);
          key := 0;
        end;
      end;
    end;
    if (Shift = [ssMeta]) then
    begin
      case Key of
        VK_C:
        begin
          TSynEdit(Sender).CopyToClipboard;
          Key := 0;
        end;
        VK_X:
        begin
          TSynEdit(Sender).CutToClipboard;
          Key := 0;
        end;
        VK_V:
        begin
          TSynEdit(Sender).PasteFromClipboard;
          Key := 0;
        end;
        VK_R:
        begin
          actExecQuery.Execute;
          Key := 0;
        end;
        VK_A:
        begin
          TSynEdit(Sender).SelectAll;
          Key := 0;
        end;
      end;

    end;
  end;
  {$ENDIF DARWIN}
end;

procedure TIDEFDEBrowserWindow.editorKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if ((not (ssShift in Shift)) and (not (ssCtrl in Shift))) then
  begin
    case Key of
      VK_HOME:
      begin
        fEndOfLine := False;
        iLastPosX := 1;
      end;
      VK_END:
      begin // end
        fEndOfLine := True;
      end;
      VK_KEYUP:
      begin // up
        //UpdateCursorPosition(0);
      end;
      VK_KEYDOWN:
      begin // down
        //UpdateCursorPosition(0);
      end;
      VK_KEYLEFT:
      begin
        iLastPosX := TSynEdit(Sender).CaretX;
      end;
      VK_KEYRIGHT:
      begin
        iLastPosX := TSynEdit(Sender).CaretX;
      end;

    end;
  end;

end;

procedure TIDEFDEBrowserWindow.editorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
end;

procedure TIDEFDEBrowserWindow.editorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  fEndOfLine := False;
  if TSynEdit(Sender).SelAvail then
    Exit;
  updateCursorPosition(TSynEdit(Sender), TSynEdit(Sender).CaretX);
end;

procedure TIDEFDEBrowserWindow.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  saveFormState;
end;

procedure TIDEFDEBrowserWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
end;

procedure TIDEFDEBrowserWindow.FormDestroy(Sender: TObject);
begin
end;

procedure TIDEFDEBrowserWindow.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
end;

procedure TIDEFDEBrowserWindow.FormShow(Sender: TObject);
begin
  restoreFormState;
end;

procedure TIDEFDEBrowserWindow.GridDblClick(Sender: TObject);
begin
  if (Sender as TDBGrid).SelectedField = nil then
    Exit;
  if (Sender as TDBGrid).SelectedField.DataType = ftMemo then
  begin
    with TfMemoEdit.Create(IDEFDEBrowserWindow) do
    begin
      try
        Caption := (Sender as TDBGrid).SelectedField.FieldName;
        edt.Text := (Sender as TDBGrid).SelectedField.AsString;
        if ShowModal = mrOk then
        begin
          (Sender as TDBGrid).SelectedField.AsString := edt.Text;
        end;
      finally
        Free;
      end;
    end;
  end;
end;

// Draw memo text instead of (Memo), draw other text as usual.
procedure TIDEFDEBrowserWindow.GridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
// set DefaultDrawing:= true;
var
  OverrideDraw: boolean;
  //determine if we're going to override normal Lazarus draw routines
  OurDisplayString: string;
  CurrentField: TField;
  DataRow: integer;
begin
  OverrideDraw := False;

  // Make sure selected cells are highlighted
  if (gdSelected in State) then
  begin
    (Sender as TDBGrid).Canvas.Brush.Color := clHighlight;
  end
  else
  begin
    (Sender as TDBGrid).Canvas.Brush.Color := (Sender as TDBGrid).Color;
  end;

  // Draw background in any case - thanks to ludob on the forum:
  (Sender as TDBGrid).Canvas.FillRect(Rect);

  //Foreground
  try
    CurrentField := Column.Field;
    if CurrentField.DataType = ftMemo then
    begin
      OverrideDraw := True;
    end;
  except
    on E: Exception do
    begin
      // We might have an inactive datalink or whatever,
      // in that case, pass on our problems to the LCL
      OverrideDraw := False;
    end;
  end;

  //Exception: fixed header should always be drawn like normal:
  // this never gets picked up as OnDrawColumnCell apparently only deals with data cells!!!
  if (gdFixed in State) then
  begin
    OverrideDraw := False;
  end;

  if OverrideDraw = False then
  begin
    // Call normal procedure to handle drawing for us.
    (Sender as TDBGrid).DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end
  else
  begin
    // Get to work displaying our memo contents
    // Basically shamelessly ripped from
    // DefaultDrawColumnCell
    OurDisplayString := '';
    if CurrentField <> nil then
    begin
      //DO display memo ;) OurDisplayString is string to be displayed
      try
        OurDisplayString := CurrentField.AsString; //DisplayText will only show (Memo)
      except
        // Ignore errors; use empty string as specified above
      end;
    end;
    //Actual foreground drawing, taken from Grids.DrawCellText coding:
    (Sender as TDBGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, OurDisplayString);
  end;
end;

procedure TIDEFDEBrowserWindow.GridKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = 13 then
  begin
    if (Sender as TDBGrid).SelectedField.DataType = ftMemo then
    begin
      key := 0;
      GridDblClick(Sender);
    end;
  end;
end;

procedure TIDEFDEBrowserWindow.QueryBeforeOpen(DataSet: TDataSet);
begin

end;

procedure TIDEFDEBrowserWindow.tabContentContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: boolean);
begin

end;

procedure TIDEFDEBrowserWindow.tabContentEnter(Sender: TObject);
begin
end;

procedure TIDEFDEBrowserWindow.setupScheme(AControl: TSynEdit);
var
  schemeDir: string;
  {$ifndef FDE_DESKTOP}
  Pkg: TIDEPackage;
  {$endif}
begin
  AControl.Font.Name := 'Courier New';
  AControl.Font.Size := 12;

  AControl.Color := $222827;
  AControl.LineHighlightColor.Background := $3A4546;
  AControl.Gutter.Color := $3A4546;
  AControl.Font.Color := $f0f0f0;
  AControl.Gutter.Parts[1].MarkupInfo.Background := $3A4546;
  AControl.Gutter.Parts[1].MarkupInfo.Foreground := $f0f0f0;


  {$ifdef FDE_DESKTOP}
  schemeDir := '.';
  {$else}
  Pkg := PackageEditingInterface.FindPackageWithName('fastplaz_tools');
  if Pkg <> nil then
    schemeDir := Pkg.DirectoryExpanded;
  {$endif}

  //if FileExists(schemeDir) then
  SynSQL.LoadFromFile(schemeDir + DirectorySeparator + 'scheme/sql/black.scheme');
end;

procedure TIDEFDEBrowserWindow.updateCursorPosition(AControl: TSynEdit;
  ACurrentPostX: integer);
var
  iRow, lineLength, currentPostX: integer;
  s: string;
begin
  currentPostX := AControl.CaretX;
  iRow := AControl.CaretY - 1;
  s := AControl.Lines.Strings[iRow];
  lineLength := length(s) + 1;
  if fEndOfLine then
  begin
    AControl.CaretX := lineLength;
    exit;
  end;
  if currentPostX > lineLength then
    AControl.CaretX := lineLength;
end;

procedure TIDEFDEBrowserWindow.getTableStructure(ATableName: string);
var
  i: integer;
  s, sql: string;

  function getFieldValue(AFieldName: string): string;
  begin
    Result := '';
    try
      Result := DatabaseController.Query.FieldValues[AFieldName];
    except
    end;
  end;

begin
  sql := 'describe ' + ATableName;
  if DatabaseType = 'postgres' then
  begin
    sql := Format(SQL_STRUCTURE_TABLE_POSTGRES, [ATableName]);
  end;

  DatabaseController.SQL.Text := sql;
  if DatabaseController.Open then
  begin
    StringGrid.FixedCols := 0;
    StringGrid.ColCount := 6;
    StringGrid.RowCount := 1;
    StringGrid.Rows[0].CommaText := 'Field,Type,Null,Key,Default,Extra';
    i := 1;
    repeat
      s := getFieldValue('Field') + ',' + getFieldValue('Type') +
        ',' + getFieldValue('Null') + ',' + getFieldValue('Key') +
        ',' + getFieldValue('Default') + ',' + getFieldValue('Extra');

      StringGrid.RowCount := StringGrid.RowCount + 1;
      StringGrid.Rows[i].CommaText := s;

      i := i + 1;
      DatabaseController.Query.Next;
    until DatabaseController.Query.EOF;

  end;
  lblStructureTableName.Caption := 'Structure: ' + ATableName;

end;

function TIDEFDEBrowserWindow.getTabControl(ATableName: string): TTabSheet;
var
  i: integer;
  tabName: string;
begin
  Result := nil;
  tabName := TAB_PREFIX + ATableName;
  for i := 0 to pgMain.ControlCount - 1 do
  begin
    if pgMain.Controls[i].Name = tabName then
    begin
      Result := TTabSheet(pgMain.Controls[i]);
      Exit;
    end;
  end;
end;

function TIDEFDEBrowserWindow.generateTab(ATableName: string): TTabSheet;
var
  i: integer;
  s: string;
  tableTab: TTabSheet;
  tmpEditor: TSynEdit;
  dq: TSQLQuery;
  ds: TDataSource;

begin
  tableTab := getTabControl(ATableName);
  if tableTab = nil then
  begin

    // generate Tab
    tableTab := TTabSheet.Create(pgMain);
    tableTab.Parent := pgMain;
    tableTab.Name := TAB_PREFIX + ATableName;
    tableTab.Caption := ATableName;
    tableTab.Hint := ATableName;
    tableTab.PageIndex := 0;

    //#0
    with TSplitter.Create(tableTab) do
    begin
      Parent := tableTab;
      Align := alTop;
      TabOrder := 0;
      Color := $f0f0f0;
    end;

    //#1
    tmpEditor := TSynEdit.Create(tableTab);
    with tmpEditor do
    begin
      Parent := tableTab;
      Align := alTop;
      Highlighter := MultiSyn;
      PopupMenu := popupEditor;
      setupScheme(tmpEditor);
      TabOrder := 0;

      OnKeyDown := @editorKeyDown;
      OnKeyUp := @editorKeyUp;
      OnMouseDown := @editorMouseDown;
      OnMouseUp := @editorMouseUp;
    end;

    // create data grid
    //#2
    dq := TSQLQuery.Create(tableTab);
    dq.DataBase := DatabaseController.SQLConnector;
    //#3
    ds := TDataSource.Create(tableTab);
    ds.DataSet := dq;
    //#4
    with TDBGrid.Create(tableTab) do
    begin
      Parent := tableTab;
      Align := alClient;
      DataSource := ds;
      AutoEdit := False;
      BorderStyle := bsNone;
      PopupMenu := popupGrid;
      OnDblClick := @GridDblClick;
      OnDrawColumnCell := @GridDrawColumnCell;
      OnKeyDown := @GridKeyDown;
      Options := Options + [dgAutoSizeColumns, dgDblClickAutoSize, dgRowSelect] -
        [dgEditing];
    end;

  end;

  pgMain.ActivePage := tableTab;
  Result := tableTab;
end;

procedure TIDEFDEBrowserWindow.destroyAllTab;
var
  i: integer;
begin
  for i := (pgMain.ControlCount - 1) downto 0 do
  begin
    if pgMain.Controls[i].Hint = '' then
      Continue;
    pgMain.Controls[i].Free;
  end;
  StringGrid.Clear;
end;

procedure TIDEFDEBrowserWindow.onQueryAsync(Data: PtrInt);
begin

end;

procedure TIDEFDEBrowserWindow.onQueryCallback(const AStatus: integer;
  const AMessage: string; var AQuery: TSQLQuery);
begin
  exit; //ulil
  if AStatus <> 0 then
  begin
    Log(AMessage, 'Error', mluError);
    barBottom.Panels[1].Text := 'Error: ' + AMessage;
    ShowMessage('Error: ' + AMessage);
  end;

end;

procedure TIDEFDEBrowserWindow.saveFormState;
begin
  PropStorage.WriteInteger('left', Parent.Left);
  PropStorage.WriteInteger('top', Parent.Top);
  PropStorage.Save;
end;

procedure TIDEFDEBrowserWindow.restoreFormState;
var
  i: integer;
begin
  i := PropStorage.ReadInteger('left',0);
  if i > 0 then
    Left := i;
  i := PropStorage.ReadInteger('top',0);
  if i > 0 then
    Top := i;
end;

procedure TIDEFDEBrowserWindow.DisableControl;
begin
  barBottom.Panels[1].Text := 'Disconnected';
  Enabled := False;
end;

procedure TIDEFDEBrowserWindow.EnableControl;
begin
  barBottom.Panels[1].Text := '';
  Enabled := True;
end;

procedure TIDEFDEBrowserWindow.PreviewTable(ATableName: string);
var
  s: string;
  tabSheet: TTabSheet;
begin
  getTableStructure(ATableName);
  tabSheet := generateTab(ATableName);

  s := 'SELECT * '#10'FROM ' + ATableName + #10'LIMIT 100';

  if tabSheet.Components[TAB_EDITOR_INDEX] is TSynEdit then
  begin
    with (tabSheet.Components[TAB_EDITOR_INDEX] as TSynEdit) do
    begin
      if Modified then
      begin
        pastSQL := s;
        Lines.Add('');
        Lines.Add('### Table: ' + ATableName);
        Text := Text + s;
        CaretY := Lines.Count;
      end
      else
        Text := s;
    end;
  end;

  actExecQuery.Execute;
  //DataSource.DataSet := DatabaseController.Query;
end;

procedure TIDEFDEBrowserWindow.Disconnect;
begin
  destroyAllTab;

end;

procedure TIDEFDEBrowserWindow.Log(AMessage: string; AGroup: string;
  UrgencyLevel: TMessageLineUrgency);
var
  s: string;
begin
  s := AMessage.Replace(#13, ' ').Replace(#10, ' ');
  if not AGroup.IsEmpty then
    s := AGroup + ': ' + s;
  lbLog.AddItem(s, nil);
  if lbLog.Items.Count > 100 then
    lbLog.Items.Delete(0);
  lbLog.Selected[lbLog.Count - 1] := True;
  lbLog.ItemIndex := lbLog.Count - 1;
end;

end.

unit de_dbbrowser;
//TODO: auto save query on close

{$mode objfpc}{$H+}

interface

uses
  de_common, de_dbmemoedit, db_controller, tools_lib,
  config_lib,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, DBGrids,
  // INTF
  IDEExternToolIntf,
  {$ifndef FDE_DESKTOP}
  PackageIntf, IDEWindowIntf, LazIDEIntf, IDEImagesIntf,
  {$endif}
  // Editor
  SynEdit, SynHighlighterSQL, SynCompletion, SynEditKeyCmds,
  SynHighlighterMulti, Clipbrd, LCLProc, LCLType, DB, SQLDB, Grids, ActnList,
  StdCtrls, Menus, IniPropStorage, Types, SynEditTypes, Math;

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
    tbsLog: TTabSheet;
    barHeader: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
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
    fEndOfLine: boolean;
    iLastPosX, iLastPosY: integer;
    pastSQL: string;
    // editor, query and grid of the active table tab; False on other tabs
    function activeTabControls(out AEditor: TSynEdit; out AQuery: TSQLQuery;
      out AGrid: TDBGrid): boolean;
    function quoteIdentifier(const AName: string): string;
    procedure setupScheme(AControl: TSynEdit);
    procedure updateCursorPosition(AControl: TSynEdit; ACurrentPostX: integer);
    procedure getTableStructure(ATableName: string);
    procedure autoSizeColumns(AGrid: TDBGrid);

    // Tab Control
    function getTabControl(ATableName: string): TTabSheet;
    function generateTab(ATableName: string): TTabSheet;
    procedure destroyAllTab;

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
  {$ifndef FDE_DESKTOP}
  // icon only buttons, use the IDE icons
  barHeader.Images := IDEImages.Images_16;
  ActionList.Images := IDEImages.Images_16;
  actExecQuery.ImageIndex := IDEImages.LoadImage('menu_run');
  actOpenFile.ImageIndex := IDEImages.LoadImage('laz_open');
  {$endif}
  setupScheme(editor);

  Enabled := True;
end;

function TIDEFDEBrowserWindow.activeTabControls(out AEditor: TSynEdit;
  out AQuery: TSQLQuery; out AGrid: TDBGrid): boolean;
var
  page: TTabSheet;
begin
  Result := False;
  AEditor := nil;
  AQuery := nil;
  AGrid := nil;
  page := pgMain.ActivePage;
  if (page = nil) or (page.ComponentCount <= TAB_GRID_INDEX) then
    Exit;
  if not (page.Components[TAB_EDITOR_INDEX] is TSynEdit) then
    Exit;
  if not (page.Components[TAB_QUERY_INDEX] is TSQLQuery) then
    Exit;
  if not (page.Components[TAB_GRID_INDEX] is TDBGrid) then
    Exit;
  AEditor := TSynEdit(page.Components[TAB_EDITOR_INDEX]);
  AQuery := TSQLQuery(page.Components[TAB_QUERY_INDEX]);
  AGrid := TDBGrid(page.Components[TAB_GRID_INDEX]);
  Result := True;
end;

function TIDEFDEBrowserWindow.quoteIdentifier(const AName: string): string;
begin
  if DatabaseType = 'mysql' then
    Result := '`' + StringReplace(AName, '`', '``', [rfReplaceAll]) + '`'
  else
    Result := '"' + StringReplace(AName, '"', '""', [rfReplaceAll]) + '"';
end;

procedure TIDEFDEBrowserWindow.actExecQueryExecute(Sender: TObject);
var
  s, sql: string;
  sqlEditor: TSynEdit;
  dq: TSQLQuery;
  tabGrid: TDBGrid;
begin
  barBottom.Panels[1].Text := '';

  if not activeTabControls(sqlEditor, dq, tabGrid) then
    Exit;
  if dq.Active then
    dq.Close;

  if Trim(sqlEditor.Text) = '' then
  begin
    barBottom.Panels[1].Text := 'Nothing to run';
    Exit;
  end;

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
    // Yes (the default) cancels, see rsAskCancelDangerOperation
    if MessageDlg(rsConfirmation, rsAskCancelDangerOperation,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      Exit;
  end;

  dq.SQL.Text := sql;
  try
    if preg_match(QUERY_SELECT_REGEX, sql.Trim) then
    begin
      dq.Open;
      autoSizeColumns(tabGrid);
    end
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
  sqlEditor: TSynEdit;
  dq: TSQLQuery;
  tabGrid: TDBGrid;
begin
  if not activeTabControls(sqlEditor, dq, tabGrid) then
    Exit;
  if tabGrid.SelectedField = nil then
    Exit;
  Clipboard.AsText := tabGrid.SelectedField.AsString;
end;

procedure TIDEFDEBrowserWindow.actOpenFileExecute(Sender: TObject);
var
  sqlEditor: TSynEdit;
  dq: TSQLQuery;
  tabGrid: TDBGrid;
  dialog: TOpenDialog;
begin
  if not activeTabControls(sqlEditor, dq, tabGrid) then
    Exit;

  dialog := TOpenDialog.Create(Application);
  try
    dialog.Title := 'Open SQL file';
    dialog.Filter := 'SQL files (*.sql)|*.sql';
    dialog.InitialDir := Config.GetString('cache/last_dir');
    dialog.Options := [ofReadOnly, ofEnableSizing];
    if dialog.Execute then
    begin
      Config.SetString('cache/last_dir', ExtractFilePath(dialog.FileName));
      Config.Flush;
      try
        sqlEditor.Lines.LoadFromFile(dialog.FileName);
        btnFileName.Caption := dialog.FileName;
        barBottom.Panels[1].Text := 'Open file: ' + dialog.FileName;
      except
        on E: Exception do
        begin
          Log(E.Message, 'ERROR', mluError);
          barBottom.Panels[1].Text := E.Message;
        end;
      end;
    end;
  finally
    dialog.Free;
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
  // the IDE may destroy this window first (owned by LazarusIDE.OwningComponent),
  // do not leave a dangling global reference
  if IDEFDEBrowserWindow = Self then
    IDEFDEBrowserWindow := nil;
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
      except
      end;
      Free;
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
  if (Key = 13) and ((Sender as TDBGrid).SelectedField <> nil) then
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
  schemeDir := '.' + DirectorySeparator;
  {$else}
  schemeDir := '';
  Pkg := PackageEditingInterface.FindPackageWithName('fastplaz_tools');
  if Pkg <> nil then
    schemeDir := IncludeTrailingPathDelimiter(Pkg.DirectoryExpanded);
  {$endif}

  schemeDir := schemeDir + 'scheme' + DirectorySeparator + 'sql' +
    DirectorySeparator + 'black.scheme';
  if FileExists(schemeDir) then
  try
    SynSQL.LoadFromFile(schemeDir);
  except
    on E: Exception do
      Log('Colour scheme not loaded: ' + E.Message, 'ERROR', mluError);
  end;
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
const
  COLUMNS: array[0..5] of string = ('Field', 'Type', 'Null', 'Key', 'Default', 'Extra');
var
  i, row: integer;
  sql: string;
  field: TField;
begin
  sql := 'describe ' + quoteIdentifier(ATableName);
  if DatabaseType = 'postgres' then
    sql := Format(SQL_STRUCTURE_TABLE_POSTGRES,
      [StringReplace(ATableName, '''', '''''', [rfReplaceAll])]);

  // header only until the structure is known
  StringGrid.BeginUpdate;
  try
    StringGrid.Clear;
    StringGrid.FixedCols := 0;
    StringGrid.ColCount := Length(COLUMNS);
    StringGrid.RowCount := 1;
    for i := Low(COLUMNS) to High(COLUMNS) do
      StringGrid.Cells[i, 0] := COLUMNS[i];

    DatabaseController.SQL.Text := sql;
    if DatabaseController.Open then
    begin
      row := 1;
      while not DatabaseController.Query.EOF do
      begin
        StringGrid.RowCount := row + 1;
        for i := Low(COLUMNS) to High(COLUMNS) do
        begin
          field := DatabaseController.Query.FindField(COLUMNS[i]);
          if field <> nil then
            StringGrid.Cells[i, row] := field.AsString;
        end;
        Inc(row);
        DatabaseController.Query.Next;
      end;
    end
    else
      Log(DatabaseController.Message, 'ERROR', mluError);
  finally
    StringGrid.EndUpdate;
  end;
  lblStructureTableName.Caption := 'Structure: ' + ATableName;
end;

procedure TIDEFDEBrowserWindow.autoSizeColumns(AGrid: TDBGrid);
const
  MAX_ROWS = 100;
  MAX_COLUMN_WIDTH = 400;
  MAX_TEXT_LENGTH = 100;
  CELL_PADDING = 16;
var
  i, n: integer;
  s: string;
  widths: array of integer;
  dataSet: TDataSet;
  bookmark: TBookmark;
  field: TField;
begin
  if (AGrid = nil) or (AGrid.DataSource = nil) then
    Exit;
  dataSet := AGrid.DataSource.DataSet;
  if (dataSet = nil) or (not dataSet.Active) or (AGrid.Columns.Count = 0) then
    Exit;
  AGrid.HandleNeeded;

  // column title
  SetLength(widths, AGrid.Columns.Count);
  AGrid.Canvas.Font.Assign(AGrid.TitleFont);
  for i := 0 to AGrid.Columns.Count - 1 do
    widths[i] := AGrid.Canvas.TextWidth(AGrid.Columns[i].Title.Caption);

  // content, measure the loaded rows only
  AGrid.Canvas.Font.Assign(AGrid.Font);
  dataSet.DisableControls;
  bookmark := dataSet.GetBookmark;
  try
    dataSet.First;
    n := 0;
    while (not dataSet.EOF) and (n < MAX_ROWS) do
    begin
      for i := 0 to AGrid.Columns.Count - 1 do
      begin
        field := AGrid.Columns[i].Field;
        if field = nil then
          Continue;
        // memo is drawn as text, see GridDrawColumnCell
        if field.DataType = ftMemo then
          s := field.AsString
        else
          s := field.DisplayText;
        s := Copy(s, 1, MAX_TEXT_LENGTH);
        widths[i] := Max(widths[i], AGrid.Canvas.TextWidth(s));
      end;
      Inc(n);
      dataSet.Next;
    end;
    dataSet.GotoBookmark(bookmark);
  finally
    dataSet.FreeBookmark(bookmark);
    dataSet.EnableControls;
  end;

  AGrid.BeginUpdate;
  try
    for i := 0 to AGrid.Columns.Count - 1 do
      AGrid.Columns[i].Width := Min(widths[i] + CELL_PADDING, MAX_COLUMN_WIDTH);
  finally
    AGrid.EndUpdate;
  end;
end;

function TIDEFDEBrowserWindow.getTabControl(ATableName: string): TTabSheet;
var
  i: integer;
begin
  Result := nil;
  // table tabs carry the table name in Hint, the designer tabs have none
  for i := 0 to pgMain.PageCount - 1 do
  begin
    if (pgMain.Pages[i].Hint = ATableName) and (pgMain.Pages[i].Hint <> '') then
      Exit(pgMain.Pages[i]);
  end;
end;

function TIDEFDEBrowserWindow.generateTab(ATableName: string): TTabSheet;
var
  tableTab: TTabSheet;
  tmpEditor: TSynEdit;
  dq: TSQLQuery;
  ds: TDataSource;

begin
  tableTab := getTabControl(ATableName);
  if tableTab = nil then
  begin

    // generate Tab, no Name: a table name is not always a valid component name
    tableTab := TTabSheet.Create(pgMain);
    tableTab.Parent := pgMain;
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
      // column width is set by autoSizeColumns after the query is opened
      Options := Options + [dgDblClickAutoSize, dgRowSelect] -
        [dgEditing, dgAutoSizeColumns];
    end;

  end;

  pgMain.ActivePage := tableTab;
  Result := tableTab;
end;

procedure TIDEFDEBrowserWindow.destroyAllTab;
var
  i: integer;
begin
  for i := pgMain.PageCount - 1 downto 0 do
  begin
    if pgMain.Pages[i].Hint = '' then
      Continue;
    pgMain.Pages[i].Free;
  end;
  StringGrid.Clear;
  lblStructureTableName.Caption := '';
end;

procedure TIDEFDEBrowserWindow.saveFormState;
begin
  Exit;
  PropStorage.WriteInteger('left', Parent.Left);
  PropStorage.WriteInteger('top', Parent.Top);
end;

procedure TIDEFDEBrowserWindow.restoreFormState;
var
  i: integer;
begin
  Exit;
  i := PropStorage.ReadInteger('left',0);
  if i > 0 then
    Left := i;
  i := PropStorage.ReadInteger('top',0);
  if i > 0 then
    Top := i;
end;

procedure TIDEFDEBrowserWindow.DisableControl;
begin
  barHeader.Visible := False;
  barBottom.Panels[1].Text := 'Disconnected';
  Enabled := False;
end;

procedure TIDEFDEBrowserWindow.EnableControl;
begin
  barHeader.Visible := True;
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

  s := 'SELECT * '#10'FROM ' + quoteIdentifier(ATableName) + #10'LIMIT 100';

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
  pgMain.ActivePage := tbsLog;
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

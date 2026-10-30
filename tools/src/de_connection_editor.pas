unit de_connection_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, EditBtn, SQLDB, SQLDBLib;

type

  { TfDatabaseConnectionEditor }

  TfDatabaseConnectionEditor = class(TForm)
    cbbDriver: TComboBox;
    edtTablePrefix: TEdit;
    edtLibrary: TFileNameEdit;
    Label10: TLabel;
    lblDriver: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    tbl_Test: TButton;
    edtName: TEdit;
    edtHostname: TEdit;
    edtPort: TEdit;
    edtUsername: TEdit;
    edtPassword: TEdit;
    edtDatabaseName: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    btnCancel: TButton;
    btnOk: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure edtNameKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure btnCancelClick(Sender: TObject);
    procedure tbl_TestClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function DatabaseType: string;
  end;

var
  fDatabaseConnectionEditor: TfDatabaseConnectionEditor;

implementation

uses
  db_controller;

{$R *.lfm}

{ TfDatabaseConnectionEditor }

procedure TfDatabaseConnectionEditor.FormKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if key = 27 then
    Close;
end;

procedure TfDatabaseConnectionEditor.FormCreate(Sender: TObject);
begin
  edtName.Clear;
  edtHostname.Clear;
  edtUsername.Clear;
  edtPassword.Clear;
  edtDatabaseName.Clear;
  edtPort.Clear;
  edtLibrary.Clear;
  cbbDriver.Style:= csDropDownList;
  cbbDriver.ItemIndex := cbbDriver.Items.IndexOf('MySQL 8.0');
end;

procedure TfDatabaseConnectionEditor.edtNameKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then
  begin
    key := #0;
    SelectNext(activecontrol, True, True);
    exit;
  end;
end;

procedure TfDatabaseConnectionEditor.btnOkClick(Sender: TObject);
begin
  if edtName.Text = '' then Exit;
  if cbbDriver.Text = '' then Exit;
  if edtHostname.Text = '' then Exit;
  //if edtUsername.Text = '' then Exit;
  if edtDatabaseName.Text = '' then Exit;
  ModalResult := mrOK;
end;

function TfDatabaseConnectionEditor.DatabaseType: string;
begin
  // based on driver name, not on the item position in the list
  Result := 'mysql';
  case cbbDriver.Text of
    'Firebird': Result := 'firebird';
    'ODBC': Result := 'odbc';
    'Oracle': Result := 'oracle';
    'PostgreSQL': Result := 'postgres';
    'SQLite3': Result := 'sqlite';
  end;
end;

procedure TfDatabaseConnectionEditor.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfDatabaseConnectionEditor.tbl_TestClick(Sender: TObject);
var
  connector: TSQLConnector;
  transaction: TSQLTransaction;
  libraryLoader: TSQLDBLibraryLoader;
  libraryLoaded: boolean;
begin
  if cbbDriver.ItemIndex < 0 then
  begin
    MessageDlg('Select driver first', mtWarning, [mbOK], 0);
    cbbDriver.SetFocus;
    Exit;
  end;
  if (edtHostname.Text = '') or (edtDatabaseName.Text = '') then
  begin
    MessageDlg('Hostname and database name are required', mtWarning, [mbOK], 0);
    if edtDatabaseName.Text = '' then
      edtDatabaseName.SetFocus;
    if edtHostname.Text = '' then
      edtHostname.SetFocus;
    Exit;
  end;

  // separate connection, does not touch the active DB Explorer connection
  libraryLoader := nil;
  libraryLoaded := False;
  connector := TSQLConnector.Create(nil);
  transaction := TSQLTransaction.Create(nil);
  Screen.Cursor := crHourGlass;
  try
    try
      connector.ConnectorType := cbbDriver.Text;
      connector.Transaction := transaction;
      connector.HostName := edtHostname.Text;
      connector.UserName := edtUsername.Text;
      connector.Password := edtPassword.Text;
      connector.DatabaseName := edtDatabaseName.Text;
      if edtPort.Text <> '' then
        connector.Params.Values['Port'] := edtPort.Text;

      if edtLibrary.FileName <> '' then
      begin
        libraryLoader := TSQLDBLibraryLoader.Create(nil);
        libraryLoader.ConnectionType := connector.ConnectorType;
        libraryLoader.LibraryName := edtLibrary.FileName;
        AddLibrarySearchPath(edtLibrary.FileName);
        libraryLoader.LoadLibrary;
        libraryLoaded := True;
      end;

      OpenConnection(connector);
      connector.Close(True);
      Screen.Cursor := crDefault;
      MessageDlg('Connection success', mtInformation, [mbOK], 0);
    except
      on E: Exception do
      begin
        Screen.Cursor := crDefault;
        MessageDlg('Connection failed' + LineEnding + LineEnding + E.Message,
          mtError, [mbOK], 0);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    transaction.Free;
    connector.Free;
    if Assigned(libraryLoader) then
    begin
      // only unload what this test loaded, keep the active connection library
      if libraryLoaded then
      try
        libraryLoader.UnloadLibrary;
      except
      end;
      libraryLoader.Free;
    end;
  end;
end;


end.


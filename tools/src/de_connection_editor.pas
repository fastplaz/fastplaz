unit de_connection_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, EditBtn;

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
  end;

var
  fDatabaseConnectionEditor: TfDatabaseConnectionEditor;

implementation

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
  cbbDriver.ItemIndex := 7;
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

procedure TfDatabaseConnectionEditor.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfDatabaseConnectionEditor.tbl_TestClick(Sender: TObject);
begin
  {
  if cbbServerType.ItemIndex = 0 then begin
    alert( 'Select Server Type first');
  Exit;
  end;
  if (edtHostname.Text = '') or (edtDatabaseName.Text = '') then begin
    alert( 'Field is not complete', mtWarning);
    if edtHostname.Text = '' then edtHostname.SetFocus;
    if edtDatabaseName.Text = '' then edtDatabaseName.SetFocus;
    Exit;
  end;
  if dDatabase.TestConnection(
    cbbServerType.ItemIndex,
    edtHostname.Text,
    edtUsername.Text, edtPassword.Text, edtDatabaseName.Text,
    edtPort.Text
  ) then
    alert( 'Conection Success ....')
  else
    alert( 'Conection Failed ....');
  }
end;


end.


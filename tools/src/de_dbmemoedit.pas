unit de_dbmemoedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, IniPropStorage;

type

  { TfMemoEdit }

  TfMemoEdit = class(TForm)
    edt: TMemo;
    PropStorage: TIniPropStorage;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { private declarations }
    procedure saveFormState;
    procedure restoreFormState;
  public
    { public declarations }
  end;

var
  fMemoEdit: TfMemoEdit;

implementation

{$R *.lfm}

uses de_common;

{ TfMemoEdit }

procedure TfMemoEdit.FormCreate(Sender: TObject);
begin
  edt.Align := alClient;
  edt.Font.Name := 'Courier New';
  PropStorage.IniFileName := GetUserDir + FDE_CONFIG_PATH + DirectorySeparator + 'property';
end;

procedure TfMemoEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  saveFormState;
end;

procedure TfMemoEdit.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    ModalResult := mrCancel;
  end;
end;

procedure TfMemoEdit.FormShow(Sender: TObject);
begin
  restoreFormState;
end;

procedure TfMemoEdit.SpeedButton1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfMemoEdit.SpeedButton2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfMemoEdit.saveFormState;
begin
  PropStorage.WriteInteger('left', Left);
  PropStorage.WriteInteger('top', Top);
end;

procedure TfMemoEdit.restoreFormState;
var
  i: integer;
begin
  Exit;
  i := PropStorage.ReadInteger('left',0);
  if i <> 0 then
    Left := i;
  i := PropStorage.ReadInteger('top',0);
  if i <> 0 then
    Top := i;
end;

end.


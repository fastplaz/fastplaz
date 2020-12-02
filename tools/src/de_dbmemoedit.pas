unit de_dbmemoedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TfMemoEdit }

  TfMemoEdit = class(TForm)
    edt: TMemo;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fMemoEdit: TfMemoEdit;

implementation

{$R *.lfm}

{ TfMemoEdit }

procedure TfMemoEdit.FormCreate(Sender: TObject);
begin
  edt.Align := alClient;
  edt.Font.Name := 'Courier New';
end;

procedure TfMemoEdit.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    ModalResult := mrCancel;
  end;
end;

procedure TfMemoEdit.SpeedButton1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfMemoEdit.SpeedButton2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.


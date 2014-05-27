unit model_wzd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel;

type

  { TfModelWizard }

  TfModelWizard = class(TForm)
    edt_ModelName: TEdit;
    Label1: TLabel;
    mem: TMemo;
    pnl_Button: TButtonPanel;
    lbl_Title: TLabel;
    pnl_Top: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
  public
  end;

var
  fModelWizard: TfModelWizard;

implementation

{$R *.lfm}

{ TfModelWizard }

procedure TfModelWizard.FormCreate(Sender: TObject);
begin
  mem.Color:= Color;
end;

procedure TfModelWizard.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TfModelWizard.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult=mrOK then
  begin
    if edt_ModelName.Text = '' then
    begin
      edt_ModelName.Color:=clYellow;
      edt_ModelName.SetFocus;
      CanClose:=False;
    end;
  end;
end;

procedure TfModelWizard.OKButtonClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

end.


unit project_wzd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, EditBtn;

type

  { TfProjectWizard }

  TfProjectWizard = class(TForm)
    cbx_GenerateStructure: TCheckBox;
    edt_ProjectName: TEdit;
    edt_WebRootDir: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbl_Title: TLabel;
    mem: TMemo;
    pnl_Button: TButtonPanel;
    pnl_Top: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fProjectWizard: TfProjectWizard;

implementation

{$R *.lfm}

{ TfProjectWizard }

procedure TfProjectWizard.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfProjectWizard.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.


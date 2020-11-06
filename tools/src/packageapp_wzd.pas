unit packageapp_wzd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, EditBtn;

type

  { TfPackageWizard }

  TfPackageWizard = class(TForm)
    cbb_Type: TComboBox;
    edt_ProjectName: TEdit;
    edt_TargetDir: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbl_Title: TLabel;
    mem: TMemo;
    pnl_Button: TButtonPanel;
    pnl_Top: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public

  end;

var
  fPackageWizard: TfPackageWizard;

implementation

{$R *.lfm}

{ TfPackageWizard }

procedure TfPackageWizard.FormCreate(Sender: TObject);
begin
  edt_ProjectName.Text := 'fastplaz';
  edt_TargetDir.Text := IncludeTrailingPathDelimiter(GetUserDir) +
    'examples' + DirectorySeparator;
end;

procedure TfPackageWizard.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfPackageWizard.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.


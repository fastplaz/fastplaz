unit projectapi_wzd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, EditBtn;

type

  { TfProjectAPIWizard }

  TfProjectAPIWizard = class(TForm)
    cbx_GenerateStructure: TCheckBox;
    edt_ProjectName: TEdit;
    edt_WebRootDir: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbl_Title: TLabel;
    mem: TMemo;
    mem_Header: TMemo;
    pnl_Button: TButtonPanel;
    pnl_Top: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fProjectAPIWizard: TfProjectAPIWizard;

implementation

{$R *.lfm}

{ TfProjectAPIWizard }

procedure TfProjectAPIWizard.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfProjectAPIWizard.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfProjectAPIWizard.FormCreate(Sender: TObject);
begin
  edt_ProjectName.Text := 'fastplazapi';
end;

end.


unit packageapp_wzd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, EditBtn, tools_lib;

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
    rg_Mode: TRadioGroup;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    function RunMode: TAppRunMode;
  end;

var
  fPackageWizard: TfPackageWizard;

implementation

{$R *.lfm}

{ TfPackageWizard }

function TfPackageWizard.RunMode: TAppRunMode;
begin
  if rg_Mode.ItemIndex = 1 then
    Result := armCGI
  else
    Result := armHTTP;
end;

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


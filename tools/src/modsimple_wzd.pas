unit modsimple_wzd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel;

const
  c_modsimple_baseurl = 'http://yourdomain/';

type

  { TfModuleSimpleWizard }

  TfModuleSimpleWizard = class(TForm)
    edt_ModuleName: TEdit;
    edt_Permalink: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbl_URL: TLabel;
    mem: TMemo;
    pnl_Button: TButtonPanel;
    lbl_Title: TLabel;
    pnl_Top: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure edt_ModuleNameChange(Sender: TObject);
    procedure edt_PermalinkChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
  public
  end;

var
  fModuleSimpleWizard: TfModuleSimpleWizard;

implementation

{$R *.lfm}

{ TfModuleSimpleWizard }

procedure TfModuleSimpleWizard.FormCreate(Sender: TObject);
begin
  mem.Color := Color;
end;

procedure TfModuleSimpleWizard.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfModuleSimpleWizard.edt_ModuleNameChange(Sender: TObject);
begin
  edt_Permalink.Text := StringReplace(LowerCase(edt_ModuleName.Text),
    ' ', '_', [rfReplaceAll]);
  edt_PermalinkChange( Sender);
end;

procedure TfModuleSimpleWizard.edt_PermalinkChange(Sender: TObject);
begin
  if edt_Permalink.Text <> '' then
  begin
    lbl_URL.Caption := c_modsimple_baseurl + LowerCase(edt_Permalink.Text);
  end
  else
    lbl_URL.Caption := c_modsimple_baseurl + lowerCase(edt_Permalink.Text) + '/';
end;

procedure TfModuleSimpleWizard.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin
    if edt_ModuleName.Text = '' then
    begin
      edt_ModuleName.Color := clYellow;
      edt_ModuleName.SetFocus;
      CanClose := False;
    end;
  end;
end;

procedure TfModuleSimpleWizard.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.


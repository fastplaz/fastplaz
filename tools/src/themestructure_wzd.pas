unit themestructure_wzd;

{$mode objfpc}{$H+}

interface

uses
  LazIDEIntf,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ButtonPanel, EditBtn;

type

  { TfThemeStructure }

  TfThemeStructure = class(TForm)
    edt_ThemeName: TEdit;
    edt_TargetDir: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbl_Title: TLabel;
    mem: TMemo;
    pnl_Button: TButtonPanel;
    pnl_Top: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure edt_ThemeNameKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
  public
    function CreateTheme(const ThemeName, TargetDirectory: string): boolean;
  end;

var
  fThemeStructure: TfThemeStructure;

implementation

{$R *.lfm}

uses webstructure_lib, fastplaz_tools_register;

{ TfThemeStructure }

procedure TfThemeStructure.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

function TfThemeStructure.CreateTheme(
  const ThemeName, TargetDirectory: string): boolean;
begin
  Result := False;
  with TWebStructure.Create do
  begin
    Result := GenerateThemeStructure(ThemeName, TargetDirectory);
    Free;
  end;
  ShowMessage('Create Theme Done.');
  log('Theme structure: ' + TargetDirectory + DirectorySeparator + ThemeName);
  Result := True;
end;

procedure TfThemeStructure.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfThemeStructure.edt_ThemeNameKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if key = 32 then
    key := 0;
end;

procedure TfThemeStructure.FormCreate(Sender: TObject);
begin
  edt_ThemeName.Clear;
  edt_TargetDir.Directory := ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename);
end;


end.


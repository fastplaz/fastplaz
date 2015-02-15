unit webstructure_wzd;

{$mode objfpc}{$H+}

interface

uses
  LazIDEIntf,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ButtonPanel, EditBtn;

type

  { TfWebStructure }

  TfWebStructure = class(TForm)
    edt_TargetDir: TDirectoryEdit;
    Label1: TLabel;
    lbl_Title: TLabel;
    mem: TMemo;
    pnl_Button: TButtonPanel;
    pnl_Top: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
  public
    function CreateStructure(TargetDirectory: string): boolean;
  end;

var
  fWebStructure: TfWebStructure;

implementation

{$R *.lfm}

uses webstructure_lib, fastplaz_tools_register;

{ TfWebStructure }

procedure TfWebStructure.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfWebStructure.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfWebStructure.CreateStructure(TargetDirectory: string): boolean;
begin
  Result := False;
  if not DirectoryExistsUTF8(TargetDirectory) then
  begin
    //ShowMessage( 'Directory is not exists.');
    //Exit;
  end;

  with TWebStructure.Create do
  begin
    Result := GenerateStructure(TargetDirectory,
      ExtractFileName(LazarusIDE.ActiveProject.LazCompilerOptions.TargetFilename));
    Free;
  end;
  ShowMessage('Create structure Done.');
  log('web structure: ' + TargetDirectory);
  Result := True;
end;

procedure TfWebStructure.FormCreate(Sender: TObject);
begin
  edt_TargetDir.Directory := ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename);
end;

end.

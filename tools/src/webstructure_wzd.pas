unit webstructure_wzd;

{$mode objfpc}{$H+}

interface

uses
  LazIDEIntf, LazFileUtils,
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

uses webstructure_lib, fastplaz_tools_register, IDEExternToolIntf;

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
var
  structure: TWebStructure;
  binaryName: string;
begin
  binaryName := '';
  if (LazarusIDE.ActiveProject <> nil) then
    binaryName := ExtractFileName(LazarusIDE.ActiveProject.LazCompilerOptions.TargetFilename);

  structure := TWebStructure.Create;
  try
    Result := structure.GenerateStructure(TargetDirectory, binaryName);
    if Result then
      ShowMessage('Create structure Done.')
    else
    begin
      log(structure.ErrorMessage, '', mluError);
      MessageDlg('Create structure failed.' + LineEnding + LineEnding +
        structure.ErrorMessage, mtError, [mbOK], 0);
    end;
  finally
    structure.Free;
  end;
end;

procedure TfWebStructure.FormCreate(Sender: TObject);
begin
  if (LazarusIDE.ActiveProject <> nil) and (LazarusIDE.ActiveProject.MainFile <> nil) then
    edt_TargetDir.Directory := ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename)
  else
    edt_TargetDir.Directory := GetUserDir;
end;

end.

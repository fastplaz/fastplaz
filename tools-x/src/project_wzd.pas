unit project_wzd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    edt_ModelName: TEdit;
    Label1: TLabel;
    lbl_Title: TLabel;
    mem: TMemo;
    pnl_Button: TButtonPanel;
    pnl_Top: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.


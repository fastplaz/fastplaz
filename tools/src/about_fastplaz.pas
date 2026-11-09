unit about_fastplaz;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LazFileUtils,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, ComCtrls;

const
  css_CodeName = 'Hero';

type

  { TfAboutFastplaz }

  TfAboutFastplaz = class(TForm)
    lbl_App: TLabel;
    lbl_CodeName: TLabel;
    lbl_URL: TLabel;
    lbl_URL1: TLabel;
    lbl_Version: TLabel;
    mem: TMemo;
    mem1: TMemo;
    mem_build: TMemo;
    page: TPageControl;
    pnl_Button: TButtonPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbl_URLClick(Sender: TObject);
    procedure lbl_URLMouseEnter(Sender: TObject);
    procedure lbl_URLMouseLeave(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
  public
  end;

var
  fAboutFastplaz: TfAboutFastplaz;

implementation

{$R *.lfm}

uses fastplaz_tools_register;

{ TfAboutFastplaz }

procedure TfAboutFastplaz.FormCreate(Sender: TObject);
var
  versionFile, version: string;
  lst: TStringList;
begin
  page.Align := alClient;
  page.ActivePageIndex := 0;

  version := '';
  versionFile := PackageDirectory(FASTPLAZ_TOOLS_PACKAGE);
  if versionFile <> '' then
  begin
    versionFile := versionFile + 'version.txt';
    if FileExistsUTF8(versionFile) then
    begin
      lst := TStringList.Create;
      try
        lst.LoadFromFile(versionFile);
        version := Trim(lst.Text);
      finally
        lst.Free;
      end;
    end;
  end;

  mem_build.Lines.Text :=
    'Target: ' + {$i %FPCTARGET%} + LineEnding +
    'Target OS: ' + {$i %FPCTARGETOS%} + LineEnding +
    'FPC version: ' + {$i %FPCVERSION%} + LineEnding +
    'Build date: ' + {$i %DATE%} + LineEnding + LineEnding +
    mem_build.Lines.Text;
  lbl_Version.Caption := 'Version: ' + version;
  lbl_App.Caption := 'FastPlaz';
  lbl_URL.Caption := 'http://www.fastplaz.com';
  lbl_CodeName.Caption := 'codename: ' + css_CodeName;
end;

procedure TfAboutFastplaz.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure TfAboutFastplaz.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 27 then begin
    key := 0;
    close;
  end;
end;

procedure TfAboutFastplaz.lbl_URLClick(Sender: TObject);
begin
  OpenURL(lbl_URL.Caption);
end;

procedure TfAboutFastplaz.lbl_URLMouseEnter(Sender: TObject);
begin
  lbl_URL.Font.Style:=[fsUnderline];
end;

procedure TfAboutFastplaz.lbl_URLMouseLeave(Sender: TObject);
begin
  lbl_URL.Font.Style:=[];
end;

procedure TfAboutFastplaz.OKButtonClick(Sender: TObject);
begin
  close;
end;


initialization
  fAboutFastplaz := nil;



end.


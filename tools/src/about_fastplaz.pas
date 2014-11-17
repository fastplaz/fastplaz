unit about_fastplaz;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  ShellAPI,
  {$endif}
  PackageIntf,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, ComCtrls;

const
  css_CodeName = 'core';

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

{ TfAboutFastplaz }

function i2s(pI: integer): string;
begin
  Result := '0';
  try
    Result := IntToStr(pI);
  finally
  end;
end;



procedure TfAboutFastplaz.FormCreate(Sender: TObject);
var
  Pkg: TIDEPackage;
  lsS : string;
  lst : TStringList;
begin
  page.Align:= alClient;
  page.ActivePageIndex:=0;

  Pkg := PackageEditingInterface.FindPackageWithName('fastplaz_tools');
  lsS := Pkg.DirectoryExpanded + DirectorySeparator + 'version.txt';
  lst := TStringList.Create;
  if FileExistsUTF8( lsS) then
  begin
    lst.LoadFromFile( lsS);
  end;

  lsS := #13#0'Target ' + {$i %FPCTARGET%};
  lsS := lsS + #13#10'TargetCPU ' + {$i %FPCTARGETCPU%};
  lsS := lsS + #13#10'Target OS ' + {$i %FPCTARGETOS%};
  lsS := lsS + #13#10'FPC Version ' + {$i %FPCVERSION%};
  lsS := lsS + #13#10'Build Date ' + {$i %DATE%};

  lsS := '';
//  lsS := lsS + 'Build width:';
  lsS := lsS + #13#10'Lazarus:';
  lsS := lsS + #13#10'TargetCPU: ' + {$i %FPCTARGETCPU%};
  mem_build.Lines.Text:= lsS + #13#10#13#10 + mem_build.Lines.Text;
  page.ActivePageIndex:=0;
  lbl_Version.Caption := 'Version: ' + lst.Text;
  lbl_App.Caption := 'FastPlaz';
  lbl_URL.Caption := 'http://www.fastplaz.com';
  lbl_CodeName.Caption:= 'codename: ' + css_CodeName;

  lst.Free;
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
var
  lsURL : string;
begin
  lsURL := 'http' + '://app.laz' + 'ex' + '.' + 'pert.com';
  {$ifdef windows}
  ShellExecute(0, PChar('Open'), PChar( lsURL ),
    nil, PChar( ExtractFileDir( Application.ExeName)), 1);
  {$else}
  {$endif}
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


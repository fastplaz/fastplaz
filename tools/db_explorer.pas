unit db_explorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, TreeFilterEdit;

type

  TDBInfo = record
    bProject: boolean;
    Name: string;
    protocol: integer;
    host: string;
    port: string;
    user: string;
    pass: string;
    databasename: string;
    tableprefix: string;
    oNode: TTreeNode;
  end;

  { TfDB }

  TfDB = class(TForm)
    BtnPanel: TPanel;
    DirectoryHierarchyButton: TSpeedButton;
    OpenButton: TSpeedButton;
    SortAlphabeticallyButton: TSpeedButton;
    barBottom: TStatusBar;
    barHeader: TToolBar;
    tvConnectionList: TTreeView;
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvConnectionListClick(Sender: TObject);
    procedure tvConnectionListDblClick(Sender: TObject);
  private
    procedure log(AMessage: string; ACategory: string = '');

  public
    DBIndoAsArray: array of TDBInfo;

  end;

var
  fDB: TfDB;

implementation

{$R *.lfm}

{ TfDB }

procedure TfDB.FormCreate(Sender: TObject);
begin
  barHeader.Hide;

end;

procedure TfDB.btnGoClick(Sender: TObject);
begin

end;

procedure TfDB.FormDestroy(Sender: TObject);
begin

end;

procedure TfDB.tvConnectionListClick(Sender: TObject);
begin

end;

procedure TfDB.tvConnectionListDblClick(Sender: TObject);
begin

end;

procedure TfDB.log(AMessage: string; ACategory: string);
begin

end;

end.


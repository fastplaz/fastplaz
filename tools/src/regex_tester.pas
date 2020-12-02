unit regex_tester;

{$mode objfpc}{$H+}

interface

uses
  regexpr_lib,
  Interfaces, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

type

  { TfRegex }

  TfRegex = class(TForm)
    cbbExpression: TComboBox;
    btnCheck: TBitBtn;
    edtResult: TEdit;
    lbResult: TListBox;
    cbbGroup: TListBox;
    LSLabel1: TLabel;
    LSLabel2: TLabel;
    mem: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    mem_email: TMemo;
    mem_url: TMemo;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure btnCheckClick(Sender: TObject);
    procedure cbbExpressionChange(Sender: TObject);
    procedure cbbExpressionKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fRegex: TfRegex;

implementation

uses
  de_common;

{$R *.lfm}

{ TfRegex }

procedure TfRegex.FormCreate(Sender: TObject);
begin
  mem.Clear;
  cbbExpression.Items.Add(REGEX_EMAIL);
  cbbExpression.Items.Add(REGEX_URL);
  cbbExpression.Items.Add(REGEX_IP);
  //mem.Font.Size:=+1;
end;

procedure TfRegex.btnCheckClick(Sender: TObject);
var
  i: integer;
  s: string;
  re: TRegExpr;
begin
  edtResult.Text := '';
  lbResult.Clear;
  cbbGroup.Clear;
  cbbExpression.SetFocus;
  if cbbExpression.Text = '' then
    Exit;
  if mem.Lines.Text = '' then
    Exit;

  re := TRegExpr.Create;
  try
    re.Expression := cbbExpression.Text;
    if re.Exec(mem.Text) then
    begin
      s := re.Match[0];
      lbResult.Items.Add(s);
      while re.ExecNext() do
      begin
        lbResult.Items.Add(re.Match[0]);
      end;
      for i := 1 to re.GroupCount - 1 do
      begin
        s := re.GroupName[i];
        if s.IsEmpty then
          s := '[noname]';
        cbbGroup.Items.Add(s);
      end;
    end;
    //edtResult.Text := 'OK';
  except
    on E: Exception do
    begin
      edtResult.Text := e.Message;
    end;
  end;
  re.Free;
  //edtResult.SetFocus;
end;

procedure TfRegex.cbbExpressionChange(Sender: TObject);
begin
  if mem.Lines.Text <> '' then
    Exit;
  cbbExpression.Hint := '';
  case cbbExpression.ItemIndex of
    0:
    begin
      mem.Lines.Text := mem_email.Text;
      cbbExpression.Hint := 'Email Regex Tester';
    end;
    1:
    begin
      mem.Lines.Text := mem_url.Text;
      cbbExpression.Hint := 'URL Regex Tester';
    end;
    2:
    begin
      mem.Lines.Text := '127.0.0.1';
      cbbExpression.Hint := 'IP Test';
    end;
  end;
end;

procedure TfRegex.cbbExpressionKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if key = 13 then
  begin
    key := 0;
    btnCheck.Click;
  end;
end;

procedure TfRegex.FormDestroy(Sender: TObject);
begin

end;

procedure TfRegex.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

initialization
  fRegex := nil;

end.

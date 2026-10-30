unit regex_tester;

{$mode objfpc}{$H+}

interface

uses
  LCLType,
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

type

  { TfRegex }

  TfRegex = class(TForm)
    cbbExpression: TComboBox;
    btnCheck: TBitBtn;
    edtResult: TEdit;
    lbResult: TListBox;
    lbGroup: TListBox;
    lblExpression: TLabel;
    lblResult: TLabel;
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
  regex_tester_lib;

{$R *.lfm}

{ TfRegex }

procedure TfRegex.FormCreate(Sender: TObject);
begin
  mem.Clear;
  cbbExpression.Items.Add(REGEX_EMAIL);
  cbbExpression.Items.Add(REGEX_URL);
  cbbExpression.Items.Add(REGEX_IP);
end;

procedure TfRegex.btnCheckClick(Sender: TObject);
var
  matchCount: integer;
begin
  edtResult.Text := '';
  lbResult.Clear;
  lbGroup.Clear;
  cbbExpression.SetFocus;
  if cbbExpression.Text = '' then
    Exit;
  if mem.Lines.Text = '' then
    Exit;

  lbResult.Items.BeginUpdate;
  lbGroup.Items.BeginUpdate;
  try
    try
      matchCount := RegexExecute(cbbExpression.Text, mem.Text,
        lbResult.Items, lbGroup.Items);
      if matchCount > 0 then
        edtResult.Text := Format('%d match(es)', [matchCount])
      else
        edtResult.Text := 'No match';
    except
      on E: Exception do
        edtResult.Text := E.Message;
    end;
  finally
    lbGroup.Items.EndUpdate;
    lbResult.Items.EndUpdate;
  end;
end;

procedure TfRegex.cbbExpressionChange(Sender: TObject);
begin
  case cbbExpression.ItemIndex of
    0: cbbExpression.Hint := 'Email Regex Tester';
    1: cbbExpression.Hint := 'URL Regex Tester';
    2: cbbExpression.Hint := 'IP Test';
    else
      cbbExpression.Hint := '';
  end;
  if mem.Lines.Text <> '' then
    Exit;
  case cbbExpression.ItemIndex of
    0: mem.Lines.Text := mem_email.Text;
    1: mem.Lines.Text := mem_url.Text;
    2: mem.Lines.Text := '127.0.0.1';
  end;
end;

procedure TfRegex.cbbExpressionKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    btnCheck.Click;
  end;
end;

procedure TfRegex.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

initialization
  fRegex := nil;

end.

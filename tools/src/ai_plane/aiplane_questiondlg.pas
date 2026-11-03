unit aiplane_questiondlg;

{
  Dialog for the questions an agent asks (Claude Code's AskUserQuestion).

  Every question is a group: radio buttons for a single choice, check boxes
  when several may be chosen, the option description in grey below each
  choice, and an "Other" choice with a text box for an answer of one's own.
  OK stays in the dialog until every question has an answer.

  The form is built in code (no .lfm). LCL only, no IDEIntf: the IDE window
  and the tester app both use it.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs,
  aiplane_types;

{ Shows the questions. True with one answer per question (several choices
  joined with ", "); False when the user cancelled. }
function AIAskQuestions(const AQuestions: TAIQuestions;
  out AAnswers: TStringArray): boolean;

implementation

const
  OTHER_CAPTION = 'Other';
  SPACING = 6;
  DESC_INDENT = 20;

type
  TQuestionControls = record
    Group: TGroupBox;
    Choices: array of TButtonControl;   // TRadioButton or TCheckBox
    Other: TButtonControl;
    OtherEdit: TEdit;
  end;

  { TAIQuestionForm }

  TAIQuestionForm = class(TForm)
  private
    FQuestions: TAIQuestions;
    FControls: array of TQuestionControls;
    FBox: TScrollBox;
    procedure BuildQuestion(AIndex: integer; APrev: TControl);
    procedure OkClick(Sender: TObject);
    procedure OtherEditChange(Sender: TObject);
    function Checked(AControl: TButtonControl): boolean;
    procedure SetChecked(AControl: TButtonControl; AValue: boolean);
  public
    constructor CreateFor(AOwner: TComponent; const AQuestions: TAIQuestions);
    function Answer(AIndex: integer): string;
  end;

procedure AnchorBelow(AControl, APrev: TControl; AIndent: integer);
begin
  AControl.AnchorSide[akLeft].Control := AControl.Parent;
  AControl.AnchorSide[akLeft].Side := asrTop;
  AControl.BorderSpacing.Left := SPACING + AIndent;
  AControl.AnchorSide[akRight].Control := AControl.Parent;
  AControl.AnchorSide[akRight].Side := asrBottom;
  AControl.BorderSpacing.Right := SPACING;
  if Assigned(APrev) then
  begin
    AControl.AnchorSide[akTop].Control := APrev;
    AControl.AnchorSide[akTop].Side := asrBottom;
  end
  else
  begin
    AControl.AnchorSide[akTop].Control := AControl.Parent;
    AControl.AnchorSide[akTop].Side := asrTop;
  end;
  AControl.BorderSpacing.Top := SPACING;
  AControl.Anchors := [akLeft, akTop, akRight];
end;

{ TAIQuestionForm }

constructor TAIQuestionForm.CreateFor(AOwner: TComponent; const AQuestions: TAIQuestions);
var
  pnlButtons: TPanel;
  btn: TButton;
  i: integer;
  prev: TControl;
begin
  inherited CreateNew(AOwner);
  FQuestions := AQuestions;
  Caption := 'Agent Plane - the agent asks';
  Position := poMainFormCenter;
  BorderStyle := bsSizeable;
  Width := 560;
  Height := 480;
  Constraints.MinWidth := 360;
  Constraints.MinHeight := 240;

  pnlButtons := TPanel.Create(Self);
  pnlButtons.Parent := Self;
  pnlButtons.Align := alBottom;
  pnlButtons.BevelOuter := bvNone;
  pnlButtons.Height := 40;

  btn := TButton.Create(Self);
  btn.Parent := pnlButtons;
  btn.Caption := 'Cancel';
  btn.Cancel := True;
  btn.ModalResult := mrCancel;
  btn.Width := 90;
  btn.Anchors := [akTop, akRight];
  btn.AnchorSide[akRight].Control := pnlButtons;
  btn.AnchorSide[akRight].Side := asrBottom;
  btn.BorderSpacing.Right := 8;
  btn.Top := 8;
  prev := btn;

  btn := TButton.Create(Self);
  btn.Parent := pnlButtons;
  btn.Caption := 'Answer';
  btn.Default := True;
  btn.Width := 90;
  btn.OnClick := @OkClick;
  btn.Anchors := [akTop, akRight];
  btn.AnchorSide[akRight].Control := prev;
  btn.AnchorSide[akRight].Side := asrTop;
  btn.BorderSpacing.Right := 8;
  btn.Top := 8;

  FBox := TScrollBox.Create(Self);
  FBox.Parent := Self;
  FBox.Align := alClient;
  FBox.BorderStyle := bsNone;
  FBox.HorzScrollBar.Visible := False;
  FBox.VertScrollBar.Tracking := True;

  SetLength(FControls, Length(FQuestions));
  prev := nil;
  for i := 0 to High(FQuestions) do
  begin
    BuildQuestion(i, prev);
    prev := FControls[i].Group;
  end;
end;

procedure TAIQuestionForm.BuildQuestion(AIndex: integer; APrev: TControl);
var
  q: TAIQuestion;
  g: TGroupBox;
  lbl: TLabel;
  prev: TControl;
  c: TButtonControl;
  j: integer;
  multi: boolean;

  function NewChoice(const ACaption: string): TButtonControl;
  var
    cb: TCheckBox;
    rb: TRadioButton;
  begin
    if multi then
    begin
      cb := TCheckBox.Create(Self);
      cb.Parent := g;
      cb.Caption := ACaption;
      cb.AutoSize := True;
      Result := cb;
    end
    else
    begin
      rb := TRadioButton.Create(Self);
      rb.Parent := g;
      rb.Caption := ACaption;
      rb.AutoSize := True;
      Result := rb;
    end;
    AnchorBelow(Result, prev, 0);
    prev := Result;
  end;

begin
  q := FQuestions[AIndex];
  multi := q.MultiSelect;

  g := TGroupBox.Create(Self);
  g.Parent := FBox;
  g.AutoSize := True;
  if q.Header <> '' then
    g.Caption := q.Header
  else
    g.Caption := Format('Question %d', [AIndex + 1]);
  AnchorBelow(g, APrev, 0);
  g.BorderSpacing.Left := 8;
  g.BorderSpacing.Right := 8;
  g.BorderSpacing.Top := 8;
  g.BorderSpacing.Bottom := 4;
  g.ChildSizing.TopBottomSpacing := 2;
  FControls[AIndex].Group := g;

  lbl := TLabel.Create(Self);
  lbl.Parent := g;
  lbl.WordWrap := True;
  lbl.AutoSize := True;
  lbl.Caption := q.Question;
  lbl.Font.Style := [fsBold];
  AnchorBelow(lbl, nil, 0);
  prev := lbl;
  if multi then
  begin
    lbl := TLabel.Create(Self);
    lbl.Parent := g;
    lbl.Caption := 'Choose one or more.';
    lbl.Font.Color := clGrayText;
    AnchorBelow(lbl, prev, 0);
    lbl.BorderSpacing.Top := 2;
    prev := lbl;
  end;

  SetLength(FControls[AIndex].Choices, Length(q.Options));
  for j := 0 to High(q.Options) do
  begin
    c := NewChoice(q.Options[j].Caption);
    FControls[AIndex].Choices[j] := c;
    if q.Options[j].Description <> '' then
    begin
      lbl := TLabel.Create(Self);
      lbl.Parent := g;
      lbl.WordWrap := True;
      lbl.AutoSize := True;
      lbl.Caption := q.Options[j].Description;
      lbl.Font.Color := clGrayText;
      AnchorBelow(lbl, prev, DESC_INDENT);
      lbl.BorderSpacing.Top := 0;
      prev := lbl;
    end;
  end;

  c := NewChoice(OTHER_CAPTION);
  FControls[AIndex].Other := c;
  FControls[AIndex].OtherEdit := TEdit.Create(Self);
  with FControls[AIndex].OtherEdit do
  begin
    Parent := g;
    TextHint := 'Type your own answer';
    Tag := AIndex;
    OnChange := @OtherEditChange;
    AnchorBelow(FControls[AIndex].OtherEdit, prev, DESC_INDENT);
    BorderSpacing.Top := 2;
    BorderSpacing.Bottom := SPACING;
  end;
end;

function TAIQuestionForm.Checked(AControl: TButtonControl): boolean;
begin
  if AControl is TCheckBox then
    Result := TCheckBox(AControl).Checked
  else
    Result := TRadioButton(AControl).Checked;
end;

procedure TAIQuestionForm.SetChecked(AControl: TButtonControl; AValue: boolean);
begin
  if AControl is TCheckBox then
    TCheckBox(AControl).Checked := AValue
  else
    TRadioButton(AControl).Checked := AValue;
end;

procedure TAIQuestionForm.OtherEditChange(Sender: TObject);
var
  i: integer;
begin
  // typing an answer picks "Other"
  i := TEdit(Sender).Tag;
  if Trim(TEdit(Sender).Text) <> '' then
    SetChecked(FControls[i].Other, True);
end;

function TAIQuestionForm.Answer(AIndex: integer): string;
var
  j: integer;
  other: string;
begin
  Result := '';
  for j := 0 to High(FControls[AIndex].Choices) do
    if Checked(FControls[AIndex].Choices[j]) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + FQuestions[AIndex].Options[j].Caption;
    end;
  other := Trim(FControls[AIndex].OtherEdit.Text);
  if Checked(FControls[AIndex].Other) and (other <> '') then
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + other;
  end;
end;

procedure TAIQuestionForm.OkClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(FQuestions) do
  begin
    if Checked(FControls[i].Other) and (Trim(FControls[i].OtherEdit.Text) = '') then
    begin
      FBox.ScrollInView(FControls[i].Group);
      FControls[i].OtherEdit.SetFocus;
      MessageDlg('Type an answer for "Other", or pick another choice.', mtInformation, [mbOK], 0);
      Exit;
    end;
    if Answer(i) = '' then
    begin
      FBox.ScrollInView(FControls[i].Group);
      MessageDlg(Format('Please answer: %s', [FQuestions[i].Question]), mtInformation, [mbOK], 0);
      Exit;
    end;
  end;
  ModalResult := mrOk;
end;

function AIAskQuestions(const AQuestions: TAIQuestions;
  out AAnswers: TStringArray): boolean;
var
  form: TAIQuestionForm;
  i: integer;
begin
  AAnswers := nil;
  Result := False;
  if Length(AQuestions) = 0 then
    Exit;
  form := TAIQuestionForm.CreateFor(nil, AQuestions);
  try
    Result := form.ShowModal = mrOk;
    if Result then
    begin
      SetLength(AAnswers, Length(AQuestions));
      for i := 0 to High(AQuestions) do
        AAnswers[i] := form.Answer(i);
    end;
  finally
    form.Free;
  end;
end;

end.

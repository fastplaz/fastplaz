unit json_tools;
{
  enhancement from existing jsonviewer example
}

{$mode objfpc}{$H+}

interface

uses
  fpjson, jsonparser, jsonscanner,
  Interfaces, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  SynEdit, SynHighlighterJScript, SynEditKeyCmds, LCLType;

const
  VK_SPACE = 32;
  VK_KEYLEFT = 37;
  VK_KEYRIGHT = 39;
  VK_KEYUP = 38;
  VK_KEYDOWN = 40;
  VK_HOME = 36;
  VK_END = 35;

type

  { TLocalJSONParser }

  TLocalJSONParser = class(TJSONParser)
  public
    property Scanner;
  end;

  { TfJSONTools }

  TfJSONTools = class(TForm)
    barBottom: TStatusBar;
    edt: TSynEdit;
    ilJSON: TImageList;
    Images: TImageList;
    pgMain: TPageControl;
    SynJScriptSyn1: TSynJScriptSyn;
    tbsRaw: TTabSheet;
    tbsVisual: TTabSheet;
    ToolBar1: TToolBar;
    btnGo: TToolButton;
    btnClear: TToolButton;
    ToolButton2: TToolButton;
    btnOpenFile: TToolButton;
    ToolButton5: TToolButton;
    tvJson: TTreeView;
    procedure btnClearClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure edtKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edtKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edtMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure edtMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fEndOfLine: boolean;
    iLastPosX, iLastPosY: integer;
    jsonData: TJSONData;
    procedure setupScheme;
    procedure updateCursorPosition(AControl: TSynEdit; ACurrentPostX: integer);
    procedure showVisualTreeView();
    procedure showJSONData(AParent: TTreeNode; AData: TJSONData);
  public

  end;

var
  fJSONTools: TfJSONTools;

implementation

uses de_common;

{$R *.lfm}

const
  ImageTypeMap: array[TJSONtype] of integer =
    //      jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject
    (-1, 8, 9, 7, 6, 5, 4);

{ TfJSONTools }

procedure TfJSONTools.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TfJSONTools.FormCreate(Sender: TObject);
begin
  pgMain.Align := alClient;
  pgMain.ActivePage := tbsRaw;
  tvJson.Align := alClient;
  edt.Align := alClient;
  edt.Clear;
  setupScheme();
end;

procedure TfJSONTools.FormDestroy(Sender: TObject);
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

procedure TfJSONTools.setupScheme;
begin
  edt.Font.Name := 'Courier New';
  edt.Font.Size := 12;
  edt.Font.color := $3A4546;
  edt.Color := $222827;
  edt.LineHighlightColor.Background := $3A4546;
  edt.Gutter.Color := $3A4546;
  edt.Font.Color := $f0f0f0;
  edt.Gutter.Parts[1].MarkupInfo.Background := $3A4546;
  edt.Gutter.Parts[1].MarkupInfo.Foreground := $f0f0f0;
end;

procedure TfJSONTools.updateCursorPosition(AControl: TSynEdit; ACurrentPostX: integer);
var
  iRow, lineLength, currentPostX: integer;
  s: string;
begin
  currentPostX := AControl.CaretX;
  iRow := AControl.CaretY - 1;
  s := AControl.Lines.Strings[iRow];
  lineLength := length(s) + 1;
  if fEndOfLine then
  begin
    AControl.CaretX := lineLength;
    exit;
  end;
  if currentPostX > lineLength then
    AControl.CaretX := lineLength;
end;

procedure TfJSONTools.showVisualTreeView();
begin
  try
    tvJson.Items.Clear;
    if Assigned(jsonData) then
      jsonData.Free;
    jsonData := GetJSON(edt.Text);
    showJSONData(nil, jsonData);
    tvJson.FullExpand;
  except
  end;
end;

procedure TfJSONTools.showJSONData(AParent: TTreeNode; AData: TJSONData);
var
  N, N2: TTreeNode;
  I: integer;
  D: TJSONData;
  C: string;
  S: TStringList;

begin
  if not Assigned(AData) then
    exit;
  if (AParent <> nil) then
    N := AParent
  else
    N := tvJson.Items.AddChild(AParent, '');
  case AData.JSONType of
    jtArray,
    jtObject:
    begin
      if (AData.JSONType = jtArray) then
        C := rsJsonSArray
      else
        C := rsJsonSObject;
      C := Format(C, [AData.Count]);
      S := TStringList.Create;
      try
        for I := 0 to AData.Count - 1 do
          if AData.JSONtype = jtArray then
            S.AddObject(IntToStr(I), AData.items[i])
          else
            S.AddObject(TJSONObject(AData).Names[i], AData.items[i]);
        if (AData.JSONType = jtObject) then
          S.Sort;
        for I := 0 to S.Count - 1 do
        begin
          N2 := TVJSON.Items.AddChild(N, S[i]);
          D := TJSONData(S.Objects[i]);
          N2.ImageIndex := ImageTypeMap[D.JSONType];
          N2.SelectedIndex := ImageTypeMap[D.JSONType];
          showJSONData(N2, D);
        end
      finally
        S.Free;
      end;
    end;
    jtNull:
      C := rsJsonSNull;
    else
      C := AData.AsString;
      if (AData.JSONType = jtString) then
        C := '"' + C + '"';
  end;
  if Assigned(N) then
  begin
    if N.Text = '' then
      N.Text := C
    else
      N.Text := N.Text + ': ' + C;
    N.ImageIndex := ImageTypeMap[AData.JSONType];
    N.SelectedIndex := ImageTypeMap[AData.JSONType];
    N.Data := AData;
  end;
end;

procedure TfJSONTools.btnGoClick(Sender: TObject);
var
  VJSONData: TJSONData = nil;
  VJSONParser: TLocalJSONParser;
  posX, posY: integer;
begin
  tvJson.Items.Clear;
  barBottom.Panels[0].Text := '';
  barBottom.Panels[2].Text := '';
  edt.Text := edt.Text.Trim;
  if edt.Text = '' then
    Exit;

  VJSONParser := TLocalJSONParser.Create(edt.Text);
  try
    try
      VJSONData := VJSONParser.Parse;
      edt.Text := VJSONData.FormatJSON([], 2);
      //edt.Color := $00C2EFE7;
      showVisualTreeView;
      barBottom.Panels[0].Text := 'OK';
    except
      on E: Exception do
      begin
        barBottom.Panels[0].Text := 'ERR';
        barBottom.Panels[2].Text := E.Message;
        posX := VJSONParser.Scanner.CurColumn;
        posY := VJSONParser.Scanner.CurRow;
        edt.CaretX := posX;
        edt.CaretY := posY;
        //edt.SelectedColor.Background := $004080FF;
        edt.SelectedColor.Background := clRed;
        edt.SelectedColor.Foreground := clYellow;
        edt.SelectLine;
        edt.SetFocus;
      end;
    end;
  finally
    VJSONData.Free;
    VJSONParser.Free;
  end;

end;

procedure TfJSONTools.btnClearClick(Sender: TObject);
begin
  edt.Clear;
end;

procedure TfJSONTools.btnOpenFileClick(Sender: TObject);
begin
  barBottom.Panels[0].Text := '';
  barBottom.Panels[2].Text := '';
  with TOpenDialog.Create(Application) do
  begin
    Title := 'Open JSON file';
    Filter := 'JSON files (*.json)|*.json';
    //InitialDir := '';
    Options := [ofReadOnly, ofEnableSizing];
    if Execute then
    begin
      edt.Lines.LoadFromFile(FileName);
    end;
    barBottom.Panels[2].Text := FileName;
  end;
end;

procedure TfJSONTools.edtKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  {$IFDEF DARWIN}
  if TSynEdit(Sender).Focused then
  begin
    if (((ssShift in Shift)) and (ssMeta in Shift)) then
    begin
      case Key of
        VK_KEYUP:
        begin
          TSynEdit(Sender).CommandProcessor(TSynEditorCommand(ecEditorTop), ' ', nil);
          key := 0;
        end;
        VK_KEYDOWN:
        begin
          TSynEdit(Sender).CommandProcessor(TSynEditorCommand(ecEditorBottom), ' ', nil);
          key := 0;
        end;
        VK_KEYLEFT:
        begin
          TSynEdit(Sender).CommandProcessor(TSynEditorCommand(ecSelWordLeft), ' ', nil);
          Key := 0;
        end;
        VK_KEYRIGHT:
        begin
          TSynEdit(Sender).CommandProcessor(TSynEditorCommand(ecLineEnd), ' ', nil);
          key := 0;
        end;
      end;
    end;
    if (Shift = [ssMeta]) then
    begin
      case Key of
        VK_C:
        begin
          TSynEdit(Sender).CopyToClipboard;
          Key := 0;
        end;
        VK_X:
        begin
          TSynEdit(Sender).CutToClipboard;
          Key := 0;
        end;
        VK_V:
        begin
          TSynEdit(Sender).PasteFromClipboard;
          Key := 0;
        end;
        VK_R:
        begin
          btnGo.Click;
          Key := 0;
        end;
        VK_A:
        begin
          TSynEdit(Sender).SelectAll;
          Key := 0;
        end;
      end;

    end;
  end;
  {$ENDIF DARWIN}
end;

procedure TfJSONTools.edtKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if ((not (ssShift in Shift)) and (not (ssCtrl in Shift))) then
  begin
    case Key of
      VK_HOME:
      begin
        fEndOfLine := False;
        iLastPosX := 1;
      end;
      VK_END:
      begin // end
        fEndOfLine := True;
      end;
      VK_KEYUP:
      begin // up
        //UpdateCursorPosition(0);
      end;
      VK_KEYDOWN:
      begin // down
        //UpdateCursorPosition(0);
      end;
      VK_KEYLEFT:
      begin
        iLastPosX := TSynEdit(Sender).CaretX;
      end;
      VK_KEYRIGHT:
      begin
        iLastPosX := TSynEdit(Sender).CaretX;
      end;

    end;
  end;
end;

procedure TfJSONTools.edtMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
end;

procedure TfJSONTools.edtMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  fEndOfLine := False;
  if TSynEdit(Sender).SelAvail then
    Exit;
  updateCursorPosition(TSynEdit(Sender), TSynEdit(Sender).CaretX);
end;

initialization
  fJSONTools := nil;

end.

unit fastcode_configuration;

{$mode ObjFPC}{$H+}

interface

uses
  LazFileUtils, fpjson, openai_integration, thread_custom,
  IDEMsgIntf, IDEExternToolIntf, SrcEditorIntf, SynEdit,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

  { TfFastCodeConfiguration }

  TfFastCodeConfiguration = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    edt_OpeningChar: TEdit;
    edt_ClosingChar: TEdit;
    edt_Language: TEdit;
    edt_OpenAIKey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    mem_Prompt: TMemo;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    sourceEditor: TSourceEditorInterface;
    selectedEditor: TSynEdit;
    procedure log(const Msg: string; AFileName: string = '';
      ATheUrgency: TMessageLineUrgency = mluNote);
  public
    Question: string;
    ResponseAsString: string;
    ResponseAsJson: TJSONData;
    procedure Prepare;
    procedure DoFastCode(Sender: TObject);
    procedure OnSuccessFastCode(Sender: TObject);
  end;

var
  fFastCodeConfiguration: TfFastCodeConfiguration;

function GetFastCodeSuggestion(AQuestion: string): string;

implementation

uses config_lib, de_common;

function GetFastCodeSuggestion(AQuestion: string): string;
var
  openaiKey, prompt: string;
  msg: TJSONObject;
begin
  Result := '';

  if AQuestion.IsEmpty then Exit;
  LoadConfiguration;
  openaiKey := GlobalConfig['fastcode/openai_api_key'];
  prompt := GlobalConfig['fastcode/prompt'];
  if openaiKey.IsEmpty then
  begin
    Result := 'OpenAI not found';
    ShowMessage('OpenAI Key not found. '#13'Please fill in the key from the "FastPlaz/Fast Code Configuration" menu');
    Exit;
  end;

  fFastCodeConfiguration.Question := AQuestion;
  fFastCodeConfiguration.edt_OpenAIKey.Text := openaiKey;
  fFastCodeConfiguration.mem_Prompt.Text := prompt;
  Call(@fFastCodeConfiguration.DoFastCode, @fFastCodeConfiguration.OnSuccessFastCode);

end;

{$R *.lfm}

{ TfFastCodeConfiguration }

procedure TfFastCodeConfiguration.FormCreate(Sender: TObject);
begin
end;

procedure TfFastCodeConfiguration.FormKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

procedure TfFastCodeConfiguration.FormShow(Sender: TObject);
begin
end;

procedure TfFastCodeConfiguration.log(const Msg: string; AFileName: string;
  ATheUrgency: TMessageLineUrgency);
begin
  if IDEMessagesWindow <> nil then
    IDEMessagesWindow.AddCustomMessage(ATheUrgency, Msg, AFileName, 0, 0, 'FastPlaz');
end;

procedure TfFastCodeConfiguration.Prepare;
begin
  LoadConfiguration;
  edt_OpenAIKey.Text := GlobalConfig['fastcode/openai_api_key'];
  edt_Language.Text := GlobalConfig['fastcode/language'];
  mem_Prompt.Lines.Text := GlobalConfig['fastcode/prompt'];
  edt_OpeningChar.Text := GlobalConfig['fastcode/opening_char'];
  edt_ClosingChar.Text := GlobalConfig['fastcode/closing_char'];
end;

procedure TfFastCodeConfiguration.DoFastCode(Sender: TObject);
var
  prompt: string;
  msg: TJSONObject;
begin
  sourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if sourceEditor = nil then Exit;
  if not (sourceEditor.EditorControl is TSynEdit) then Exit;
  selectedEditor := TSynEdit(sourceEditor.EditorControl);

  log('thingking ....');
  if Assigned(ResponseAsJson) then ResponseAsJson.Free;
  with TOpenAIIntegration.Create do
  begin
    Key := edt_OpenAIKey.Text;
    prompt := mem_Prompt.Text;
    prompt := prompt.Replace('%language%', edt_Language.Text).Trim;
    if not prompt.IsEmpty then
    begin
      Messages.Clear;
      msg := TJSONObject.Create;
      msg.Add('role', 'system');
      msg.Add('content', prompt);
      Messages.Add(msg);
    end;

    // OnSyncStatus := @openAISync;
    ResponseAsString := '';
    if ChatCompletion(Question) then
    begin
      ResponseAsString := Response.AsJSON;
      ResponseAsJson := GetJSON(ResponseAsString, False);
    end;

    log('I get a response.');
    Free;
  end;

  // pengulangan dari 1 sampai 5

end;

procedure TfFastCodeConfiguration.OnSuccessFastCode(Sender: TObject);
var
  responseAsText: string;
begin
  log('formatting response...');
  if ResponseAsString.IsEmpty then
  begin
    log('Sorry, I can''t figure this out yet');
    Exit;
  end;

  try
    responseAsText := ResponseAsJson.GetPath('choices[0].message.content').AsJSON;
    responseAsText := responseAsText.Trim('"');
    responseAsText := responseAsText.Replace('```', '', [rfReplaceAll]);
    responseAsText := responseAsText.Replace('\n', #13, [rfReplaceAll]);
    responseAsText := responseAsText.Replace('\"', '"', [rfReplaceAll]);
    selectedEditor.InsertTextAtCaret(#13 + edt_OpeningChar.Text + #13 +
      responseAsText + #13 + edt_ClosingChar.Text + #13);
    log('done...');
  except
    on E: Exception do
    begin
      log('Failed, I got exception: ' + E.Message);
    end;
  end;

end;

procedure TfFastCodeConfiguration.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;

procedure TfFastCodeConfiguration.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfFastCodeConfiguration.btnSaveClick(Sender: TObject);
begin
  GlobalConfig['fastcode/openai_api_key'] := edt_OpenAIKey.Text;
  GlobalConfig['fastcode/language'] := edt_Language.Text;
  GlobalConfig['fastcode/prompt'] := mem_Prompt.Text;
  GlobalConfig['fastcode/opening_char'] := edt_OpeningChar.Text;
  GlobalConfig['fastcode/closing_char'] := edt_ClosingChar.Text;
  GlobalConfig.Flush;
  Close;
end;

end.

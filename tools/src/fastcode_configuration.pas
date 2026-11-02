unit fastcode_configuration;

{$mode ObjFPC}{$H+}

interface

uses
  LazFileUtils, fpjson, openai_integration, thread_custom, IDEMsgIntf,
  IDEExternToolIntf, SrcEditorIntf, SynEdit, Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, aiplane_settingsframe;

type

  { TfFastCodeConfiguration }

  TfFastCodeConfiguration = class(TForm)
    btnCancel: TButton;
    btnSave: TButton;
    btnTestConnection: TButton;
    cbbModel: TComboBox;
    edt_OpenAIBaseURL: TEdit;
    edt_OpeningChar: TEdit;
    edt_ClosingChar: TEdit;
    edt_Language: TEdit;
    edt_OpenAIKey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    mem_Prompt: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    tsAIPlane: TTabSheet;
    tsFastCode: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnTestConnectionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    selectedEditor: TSynEdit;
    FAIPlane: TAIPlaneSettingsFrame;
    FTestBaseURL: string;
    FTestKey: string;
    FTestError: string;
    FTestModels: TStringList;
    // snapshot of the configuration, read by the worker thread
    FCodeBaseURL: string;
    FCodeKey: string;
    FCodeModel: string;
    FCodePrompt: string;
    FCodeError: string;
    FCodeBusy: boolean;
    procedure LoadAIPlane;
    procedure SaveAIPlane;
    procedure DoTestConnection(Sender: TObject);
    procedure OnTestConnectionDone(Sender: TObject);
    procedure log(const Msg: string; AFileName: string = '';
      ATheUrgency: TMessageLineUrgency = mluNote);
  public
    Question: string;
    ResponseAsString: string;
    destructor Destroy; override;
    procedure Prepare;
    procedure ShowAIPlaneTab;
    // asks the model for AQuestion and inserts the answer into the active
    // editor; the request runs in a worker thread
    function AskFastCode(const AQuestion: string): boolean;
    procedure DoFastCode(Sender: TObject);
    procedure OnSuccessFastCode(Sender: TObject);
  end;

var
  fFastCodeConfiguration: TfFastCodeConfiguration;

function GetFastCodeSuggestion(AQuestion: string): string;
function NormalizeOpenAIBaseURL(const AUrl: string): string;

implementation

uses config_lib, de_common, tools_lib, aiplane_ide;

function NormalizeOpenAIBaseURL(const AUrl: string): string;
begin
  Result := AUrl.Trim;
  if Result.IsEmpty then
    Result := OPENAI_BASEURL
  else if not Result.EndsWith('/') then
    Result := Result + '/';
end;

function GetFastCodeSuggestion(AQuestion: string): string;
begin
  Result := '';
  if AQuestion.IsEmpty then
    Exit;
  if not Assigned(fFastCodeConfiguration) then
    fFastCodeConfiguration := TfFastCodeConfiguration.Create(nil);
  if fFastCodeConfiguration.AskFastCode(AQuestion) then
    Result := 'asking';
end;

{$R *.lfm}

{ TfFastCodeConfiguration }

procedure TfFastCodeConfiguration.FormCreate(Sender: TObject);
begin
  FTestModels := TStringList.Create;
  FTestModels.Sorted := True;
  FTestModels.Duplicates := dupIgnore;
  FAIPlane := TAIPlaneSettingsFrame.Create(Self);
  FAIPlane.Parent := tsAIPlane;
  FAIPlane.Align := alClient;
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
  edt_OpenAIBaseURL.Text := GlobalConfig['fastcode/openai_base_url'];
  edt_OpenAIKey.Text := GlobalConfig['fastcode/openai_api_key'];
  cbbModel.Text := GlobalConfig['fastcode/openai_model'];
  edt_Language.Text := GlobalConfig['fastcode/language'];
  mem_Prompt.Lines.Text := GlobalConfig['fastcode/prompt'];
  edt_OpeningChar.Text := GlobalConfig['fastcode/opening_char'];
  edt_ClosingChar.Text := GlobalConfig['fastcode/closing_char'];
  LoadAIPlane;
end;

procedure TfFastCodeConfiguration.LoadAIPlane;
var
  settings: TAIPlaneSettings;
begin
  settings := AIPlaneLoadSettings;
  FAIPlane.Runtime := settings.Runtime;
  FAIPlane.Command := settings.Command;
  FAIPlane.Arguments := settings.Arguments;
  FAIPlane.Model := settings.Model;
  FAIPlane.Effort := settings.Effort;
  FAIPlane.McpEnabled := settings.McpEnabled;
  FAIPlane.McpPort := settings.McpPort;
  FAIPlane.WorkingDir := AIPlaneProjectDirectory;
end;

procedure TfFastCodeConfiguration.SaveAIPlane;
var
  settings: TAIPlaneSettings;
begin
  settings.Runtime := FAIPlane.Runtime;
  settings.Command := FAIPlane.Command;
  settings.Arguments := FAIPlane.Arguments;
  settings.Model := FAIPlane.Model;
  settings.Effort := FAIPlane.Effort;
  settings.McpEnabled := FAIPlane.McpEnabled;
  settings.McpPort := FAIPlane.McpPort;
  AIPlaneSaveSettings(settings);
  AIPlaneSettingsChanged;
end;

procedure TfFastCodeConfiguration.ShowAIPlaneTab;
begin
  PageControl1.ActivePage := tsAIPlane;
end;

function TfFastCodeConfiguration.AskFastCode(const AQuestion: string): boolean;
var
  sourceEditor: TSourceEditorInterface;
  language: string;
begin
  Result := False;
  if FCodeBusy then
  begin
    log('Fast Code is still waiting for the previous answer');
    Exit;
  end;

  sourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if (sourceEditor = nil) or not (sourceEditor.EditorControl is TSynEdit) then
    Exit;
  selectedEditor := TSynEdit(sourceEditor.EditorControl);

  // everything the worker thread needs is read here, in the main thread
  LoadConfiguration;
  FCodeKey := Trim(GlobalConfig['fastcode/openai_api_key']);
  FCodeBaseURL := NormalizeOpenAIBaseURL(GlobalConfig['fastcode/openai_base_url']);
  FCodeModel := Trim(GlobalConfig['fastcode/openai_model']);
  language := GlobalConfig['fastcode/language'];
  FCodePrompt := string(GlobalConfig['fastcode/prompt']).Replace('%language%', language).Trim;
  if FCodeKey.IsEmpty and (FCodeBaseURL = OPENAI_BASEURL) then
  begin
    ShowMessage('OpenAI Key not found. '#13'Please fill in the key in "FastPlaz/Configuration", Direct LLM tab');
    Exit;
  end;

  Question := AQuestion;
  ResponseAsString := '';
  FCodeError := '';
  log('thinking ...');
  FCodeBusy := Call(@DoFastCode, @OnSuccessFastCode);
  if not FCodeBusy then
    log('Unable to start the Fast Code thread', '', mluError);
  Result := FCodeBusy;
end;

// runs in a worker thread: no LCL, no IDE calls
procedure TfFastCodeConfiguration.DoFastCode(Sender: TObject);
var
  msg: TJSONObject;
begin
  ResponseAsString := '';
  FCodeError := '';
  with TOpenAIIntegration.Create do
  try
    try
      BaseURL := FCodeBaseURL;
      Key := FCodeKey;
      if not FCodeModel.IsEmpty then
        Model := FCodeModel;
      if not FCodePrompt.IsEmpty then
      begin
        Messages.Clear;
        msg := TJSONObject.Create;
        msg.Add('role', 'system');
        msg.Add('content', FCodePrompt);
        Messages.Add(msg);
      end;

      if ChatCompletion(Question) then
        ResponseAsString := Response.AsJSON
      else if Assigned(HttpResult) then
        FCodeError := Format('HTTP %d', [HttpResult.ResultCode])
      else
        FCodeError := 'No response from server';
    except
      on E: Exception do
        FCodeError := E.Message;
    end;
  finally
    Free;
  end;
end;

procedure TfFastCodeConfiguration.OnSuccessFastCode(Sender: TObject);
var
  responseAsText: string;
begin
  FCodeBusy := False;
  if not FCodeError.IsEmpty then
  begin
    log('Fast Code failed: ' + FCodeError, '', mluError);
    Exit;
  end;

  responseAsText := FastCodeResponseText(ResponseAsString);
  if responseAsText.IsEmpty then
  begin
    log('Sorry, I can''t figure this out yet');
    Exit;
  end;

  if selectedEditor = nil then
    Exit;
  try
    selectedEditor.InsertTextAtCaret(LineEnding + edt_OpeningChar.Text + LineEnding +
      responseAsText + LineEnding + edt_ClosingChar.Text + LineEnding);
    log('done...');
  except
    on E: Exception do
      log('Failed, I got exception: ' + E.Message, '', mluError);
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
  GlobalConfig['fastcode/openai_base_url'] := Trim(edt_OpenAIBaseURL.Text);
  GlobalConfig['fastcode/openai_api_key'] := edt_OpenAIKey.Text;
  GlobalConfig['fastcode/openai_model'] := Trim(cbbModel.Text);
  GlobalConfig['fastcode/language'] := edt_Language.Text;
  GlobalConfig['fastcode/prompt'] := mem_Prompt.Text;
  GlobalConfig['fastcode/opening_char'] := edt_OpeningChar.Text;
  GlobalConfig['fastcode/closing_char'] := edt_ClosingChar.Text;
  GlobalConfig.Flush;
  SaveAIPlane;
  Close;
end;

procedure TfFastCodeConfiguration.btnTestConnectionClick(Sender: TObject);
begin
  // snapshot the UI values here; the worker thread must not touch controls
  FTestBaseURL := NormalizeOpenAIBaseURL(edt_OpenAIBaseURL.Text);
  FTestKey := Trim(edt_OpenAIKey.Text);
  FTestError := '';
  FTestModels.Clear;

  btnTestConnection.Enabled := False;
  btnTestConnection.Caption := 'Testing ...';
  log('testing connection to ' + FTestBaseURL);

  if not Call(@DoTestConnection, @OnTestConnectionDone) then
  begin
    FTestError := 'Unable to start the connection test thread';
    OnTestConnectionDone(nil);
  end;
end;

procedure TfFastCodeConfiguration.DoTestConnection(Sender: TObject);
var
  detail: string;
begin
  // runs in a worker thread
  with TOpenAIIntegration.Create do
  try
    BaseURL := FTestBaseURL;
    Key := FTestKey;
    try
      if not ListModels(FTestModels) then
      begin
        if not Assigned(HttpResult) then
          FTestError := 'No response from server'
        else
        begin
          detail := Trim(HttpResultAsText);
          if detail.Length > 300 then
            detail := detail.Substring(0, 300) + ' ...';
          FTestError := Format('HTTP %d', [HttpResult.ResultCode]);
          if not detail.IsEmpty then
            FTestError := FTestError + ': ' + detail;
        end;
      end;
    except
      on E: Exception do
        FTestError := E.Message;
    end;
  finally
    Free;
  end;
end;

procedure TfFastCodeConfiguration.OnTestConnectionDone(Sender: TObject);
var
  currentModel: string;
begin
  // runs in the main thread (synchronized)
  btnTestConnection.Enabled := True;
  btnTestConnection.Caption := '&Test Connection';

  if not FTestError.IsEmpty then
  begin
    log('connection failed: ' + FTestError, '', mluError);
    ShowMessage('Connection failed.'#13 + FTestError);
    Exit;
  end;

  currentModel := Trim(cbbModel.Text);
  cbbModel.Items.Assign(FTestModels);
  if currentModel.IsEmpty then
  begin
    if cbbModel.Items.Count > 0 then
      cbbModel.ItemIndex := 0;
  end
  else
    cbbModel.Text := currentModel;

  log(Format('connection ok, %d model(s) found', [FTestModels.Count]));
  ShowMessage(Format('Connection OK.'#13'%d model(s) found.', [FTestModels.Count]));
end;

destructor TfFastCodeConfiguration.Destroy;
begin
  FTestModels.Free;
  inherited Destroy;
end;

end.

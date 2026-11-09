unit remotedataset_component;

{$mode objfpc}{$H+}

interface

uses
  LResources, fpjson, fpjsondataset, jsonparser,
  {$if FPC_FULLVERSION >= 30200}
  extjsdataset,
  {$endif}
  http_lib, json_lib,
  Classes, SysUtils;

const
  ERR_CODE_STANDAR_ERROR = 500;
  ERR_CODE_REMOTE_FAILED = 400;
  ERR_INVALID_PARAMETER = 'Invalid Parameters.';
  ERR_INVALID_JSON = 'Invalid Json String';

type

  TUpdateHandlerEvent = procedure of object;

  { TRemoteDataset }

  TRemoteDataset = class(TExtjsJSONObjectDataset)
  private
    FCount: integer;
    FData: TJSONArray;
    FErrorCode: integer;
    FIsSuccess: boolean;
    FLastErrorMessage: string;
    FOnUpdate: TUpdateHandlerEvent;
    FResultAsJson: TJSONUtil;
    FResultAsText: TStringList;
    FSQL: TStringList;
    FToken: string;
    FURL: string;
    httpResponse: IHTTPResponse;
    function isRequirementsComplete: boolean;
    procedure hitQuery;
    procedure onGetUpdatesExecuteHandler;
    procedure onSyncUpdateHandler( AResponse: String);
    function extractData(const AJsonString: string): TJSONObject;
  protected
    procedure loadRemoteData(const AJsonString: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenX: boolean;
    function ExecSQL(ASQL: string): boolean; overload;
    function ExecSQL: boolean; overload;
  published
    property OnUpdate: TUpdateHandlerEvent read FOnUpdate write FOnUpdate;
    property URL: string read FURL write FURL;
    property Token: string read FToken write FToken;
    property SQL: TStringList read FSQL write FSQL;
    property IsSuccess: boolean read FIsSuccess;
    property ErrorCode: integer read FErrorCode;
    property ResultAsText: TStringList read FResultAsText;
    property ResultAsJson: TJSONUtil read FResultAsJson;
    // reference into ResultAsJson, owned by it
    property Data: TJSONArray read FData;
    property Count: integer read FCount;
    property LastErrorMessage: string read FLastErrorMessage;
  end;

procedure Register;

implementation

type

  TSynchEvent = procedure(AStatus: string) of object;
  TExecuteHandlerEvent = procedure of object;

  { TRemoteDatasetSimpleThread }

  TRemoteDatasetSimpleThread = class(TThread)
  private
    contentResult: string;
    FOnExecuteHandler: TExecuteHandlerEvent;
    FOnSynch: TSynchEvent;
    procedure syncUpdate;
  protected
    procedure Execute; override;
  public
    property OnExecuteHandler: TExecuteHandlerEvent
      read FOnExecuteHandler write FOnExecuteHandler;
    property OnSync: TSynchEvent read FOnSynch write FOnSynch;
  end;


procedure Register;
begin
  RegisterComponents('FastPlaz', [TRemoteDataset]);
end;

{ TRemoteDatasetSimpleThread }

procedure TRemoteDatasetSimpleThread.syncUpdate;
begin
  if Assigned(FOnSynch) then
  begin
    FOnSynch(contentResult);
  end;
end;

procedure TRemoteDatasetSimpleThread.Execute;
begin
  if Assigned(FOnExecuteHandler) then
  begin
    FOnExecuteHandler();
  end;
  Synchronize(@syncUpdate);
end;

{ TRemoteDataset }

function TRemoteDataset.isRequirementsComplete: boolean;
begin
  Result := False;
  FLastErrorMessage := ERR_INVALID_PARAMETER;
  if FToken.IsEmpty or FURL.IsEmpty or FSQL.Text.IsEmpty then
    Exit;

  FLastErrorMessage := '';
  Result := True;
end;

procedure TRemoteDataset.hitQuery;
begin
  FResultAsText.Clear;
  FCount := 0;
  FErrorCode := ERR_CODE_STANDAR_ERROR;
  with TRemoteDatasetSimpleThread.Create(True) do
  begin
    FreeOnTerminate := True;
    OnExecuteHandler := @onGetUpdatesExecuteHandler;
    OnSync := @onSyncUpdateHandler;
    Start;
  end;
end;

procedure TRemoteDataset.onGetUpdatesExecuteHandler;
var
  http: THTTPLib;
begin
  FResultAsText.Clear;
  FResultAsJson.Clear;
  FData := nil;
  FCount := 0;
  FIsSuccess := False;
  FErrorCode := ERR_CODE_STANDAR_ERROR;
  http := THTTPLib.Create(FURL);
  try
    http.AddHeader('Token', FToken);
    http.FormData['query'] := FSQL.Text;
    httpResponse := http.Post;
  finally
    http.Free;
  end;
  if httpResponse.ResultCode <> 200 then
    Exit;

  FErrorCode := 0;
  FResultAsText.Text := httpResponse.ResultText;
  try
    if FResultAsJson.LoadFromJsonString(FResultAsText.Text, False) then
    begin
      FCount := FResultAsJson['count'];
      FData := FResultAsJson.ValueArray['data'];
      loadRemoteData(FResultAsText.Text);
      FIsSuccess := True;
    end
    else
    begin
      FErrorCode := ERR_CODE_REMOTE_FAILED;
      FLastErrorMessage := ERR_INVALID_JSON;
    end;
  except
    on E:Exception do
    begin
      FErrorCode := ERR_CODE_REMOTE_FAILED;
      FLastErrorMessage := E.Message;
    end;
  end;
end;

procedure TRemoteDataset.onSyncUpdateHandler(AResponse: String);
begin
  if Assigned(FOnUpdate) then
  begin
    FOnUpdate();
  end;
end;

// Hand the "data" array and "metaData" object of the response over to the
// dataset. Both are detached from the parsed document first, so the dataset
// owns them (OwnsData) and frees them on Close/Destroy.
procedure TRemoteDataset.loadRemoteData(const AJsonString: string);
var
  D: TJSONObject;
  ARows: TJSONArray;
  AMeta: TJSONObject;
begin
  D := extractData(AJsonString);
  if D = nil then
    raise EJSON.Create(ERR_INVALID_JSON);
  try
    ARows := D.Arrays['data'];
    AMeta := D.Objects['metaData'];
    D.Extract('data');
    D.Extract('metaData');
  finally
    D.Free;
  end;
  Close;
  Rows := ARows;
  MetaData := AMeta;
  Open;
end;

function TRemoteDataset.extractData(const AJsonString: string): TJSONObject;
var
  D: TJSONData;
begin
  Result := nil;
  D := GetJSON(AJsonString, False);
  if D = nil then
    Exit;
  if D.JSONType = jtObject then
    Result := TJSONObject(D)
  else
    D.Free;
end;

constructor TRemoteDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FResultAsText := TStringList.Create;
  FResultAsJson := TJSONUtil.Create;
  FIsSuccess := False;
  FLastErrorMessage := '';
end;

destructor TRemoteDataset.Destroy;
begin
  FResultAsJson.Free;
  FResultAsText.Free;
  FSQL.Free;
  inherited Destroy;
end;

function TRemoteDataset.OpenX: boolean;
begin
  Result := False;
  FIsSuccess := False;

  if not isRequirementsComplete then
    Exit;

  if (pos('select', SQL.Text.ToLower) = 0) then
  begin
    //Result := ExecSQL;
  end
  else
  begin
    hitQuery;
    Result := True;
  end;
end;

function TRemoteDataset.ExecSQL(ASQL: string): boolean;
begin
  FSQL.Text := ASQL;
  Result := ExecSQL();
end;

function TRemoteDataset.ExecSQL: boolean;
begin
  Result := False;
  FIsSuccess := False;
  if not isRequirementsComplete then
    Exit;

  //todo:
end;

initialization
  {$I remotedataset_component_icon.lrs}

end.

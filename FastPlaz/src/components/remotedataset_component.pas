unit remotedataset_component;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, cthreads, fpjson, fpjsondataset, jsonparser,
  fphttpclient, http_lib, json_lib,
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
    //FDataset: TExtjsJSONObjectDataset;
    FErrorCode: integer;
    FIsSuccess: boolean;
    FLastErrorMessage: string;
    FOnUpdate: TUpdateHandlerEvent;
    FResultAsJson: TJSONUtil;
    FResultAsText: TStringList;
    FSQL: TStringList;
    FToken: string;
    FURL: string;
    httpClient: TFPHTTPClient;
    httpResponse: IHTTPResponse;
    function isRequirementsComplete: boolean;
    procedure hitQuery;
    procedure onGetUpdatesExecuteHandler;
    procedure onSyncUpdateHandler( AResponse: String);
    function extractData(const AJsonString: string): TJSONObject;
  protected

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
    property Data: TJSONArray read FData;
    property Count: integer read FCount;
    property LastErrorMessage: string read FLastErrorMessage;
    //property Dataset: TExtjsJSONObjectDataset read FDataset;
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
  { $I remotedataset.lrs}
  { $R remotedataset.res} //ulil

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
  D: TJSONObject;
begin
  FResultAsText.Clear;
  FResultAsJson.Clear;
  FCount := 0;
  FErrorCode := ERR_CODE_STANDAR_ERROR;
  with THTTPLib.Create(FURL) do
  begin
    AddHeader('Token', FToken);
    FormData['query'] := FSQL.Text;
    httpResponse := Post;
    if httpResponse.ResultCode = 200 then
    begin
      FErrorCode := 0;
      FResultAsText.Text := httpResponse.ResultText;
      try
        if FResultAsJson.LoadFromJsonString(FResultAsText.Text, False) then
        begin
          FCount := FResultAsJson['count'];
          FData := FResultAsJson.ValueArray['data'];

          D := extractData(FResultAsText.Text);

          Rows := D.Arrays['data'];
          MetaData := D.Objects['metaData'];
          OwnsData := False;
          Open;

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
    Free;
  end;
end;

procedure TRemoteDataset.onSyncUpdateHandler(AResponse: String);
begin
  if Assigned(FOnUpdate) then
  begin
    FOnUpdate();
  end;
end;

function TRemoteDataset.extractData(const AJsonString: string): TJSONObject;
var
  S: TStringStream;
  P: TJSONParser;
  D: TJSONData;
begin
  Result := nil;
  try
    S := TStringStream.Create(AJsonString);
    P := TJSONParser.Create(S);
    D := P.Parse;
    if (D.JSONType=jtObject) then
      Result := D as TJSONObject
    else
      FreeAndNil(D);
  finally
    P.Free;
    S.Free;
  end;
end;

constructor TRemoteDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  //FDataset := TExtjsJSONObjectDataset.Create(Nil);
  FResultAsText := TStringList.Create;
  FResultAsJson := TJSONUtil.Create;
  //FData := TJSONArray.Create;
  FIsSuccess := False;
  FLastErrorMessage := '';
end;

destructor TRemoteDataset.Destroy;
begin
  if Assigned(httpClient) then
    httpClient.Free;
  if Assigned(FData) then
    FData.Free;
  ResultAsJson.Free;
  FResultAsText.Free;
  FSQL.Free;
  //FDataset.Free;
  inherited Destroy;
end;

function TRemoteDataset.OpenX: boolean;
begin
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
  Result := ExecSQL;
end;

function TRemoteDataset.ExecSQL: boolean;
begin
  Result := False;
  FIsSuccess := False;
  if not isRequirementsComplete then
    Exit;

  //todo:
end;

end.

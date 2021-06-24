unit http_lib;

{
  USAGE

  [x] Result:
    Response.ResultCode
    Response.ResultText
    Response.ResponseTime

  [x]
  var
    Response: IHTTPResponse;

  [x] GET
  with THTTPLib.Create do
  begin
    OnSyncStatus:= @SyncStatus;
    URL:= 'http://address';
    Response := Get();
    ...

    Free;
  end;


  [x] POST
  with THTTPLib.Create do
  begin
    OnSyncStatus:= @SyncStatus;
    URL:= 'http://address';
    FormData['var'] := 'value';
    Response := Post();
    ...

    Free;
  end;

  [x] UPLOAD FILE
  with THTTPLib.Create do
  begin
    OnSyncStatus:= @SyncStatus;
    URL:= 'http://address';
    FormData['var'] := 'value';
    AddFile( 'path/filename-01', 'files[]');
    AddFile( 'path/filename-02', 'files[]');
    Response := Post();
    ...

    Free;
  end;

  [x] POST RAW BODY
  with THTTPLib.Create do
  begin
    URL:= 'http://address';
    RequestBody := TStringStream.Create(s);
    Response := Post();
    ...

    Free;
  end;

  [x] SIMPLE UPLOAD FILE
  Response := UploadFile( [ 'fullpath/filename', 'otherfile', 'otherfiletoo' ] );

}

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  fpcgi,
  {$ifndef win32}
  //cthreads,
  {$endif}
  //cmem,
  fphttpclient,
  {$if fpc_fullversion >= 20701}
  //ghashmap,
  //fgl,
  {$else fpc_fullversion >= 20701}
  fgl,
  {$endif fpc_fullversion >= 20701}
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets, fpopenssl,
  {$endif}
  strutils, Classes, SysUtils;

const
  FASTPLAZ_USERAGENT = 'fastplaz;fpc/'+{$i %FPCVERSION%}+';'+{$i %FPCTARGETOS%}+'/'+{$i %FPCTARGET%};

type

  IHTTPResponse = interface(IInterface)
    ['{6406773F-F7C6-D893-D841-93BF62E94D47}']
    function ResultCode: integer;
    function ResultHeader: string;
    function ResultText: string;
    function ResultStream: TMemoryStream;
    function ResponseTime: Dword;
  end;

  { THTTPResponse }

  THTTPResponse = class(TInterfacedObject, IHTTPResponse)
  private
    FResultCode: integer;
    FResultHeader: string;
    FResultText: string;
    FStream: TMemoryStream;
    FResponseTime: Dword;
  public
    constructor Create(ResultCode: integer; const ResultHeader, ResultText: string;
      Stream: TStream; ElapsedTime: Dword);
    constructor Create(ResultCode: integer; const ResultHeader, ResultText: string;
      Stream: TStream);
    constructor Create(ResultCode: integer; const ResultHeader, ResultText: string);
    destructor Destroy; override;
    function ResultCode: integer;
    function ResultHeader: string;
    function ResultText: string;
    function ResultStream: TMemoryStream;
    function ResponseTime: Dword;
  end;

  { TWorkerHTTP }

  TWorkerHTTP = class(TInterfacedObject)
  private
    FIsSuccesfull: boolean;
    FMethod: string;
    FURL: string;
    HTTPClient: TFPHTTPClient;
    function GetRequestHeaders: string;
    function getResponseHeaders: string;
    function getResponseStatusCode: integer;
    function getURL: string;
    procedure setURL(AValue: string);
    procedure prepareRequestBody;
  protected
    procedure Execute;
  public
    ResultStream: TStringStream;
    FailedMessage: string;
    PostFormData: TStrings;
    FileList: TStrings;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    procedure AddFile(FileName: string; VarName: string = 'files[]');
  published
    property URL: string read getURL write setURL;
    property Method: string read FMethod write FMethod;
    property ResponseStatusCode: integer read getResponseStatusCode;
    property ResponseHeaders: string read getResponseHeaders;
    property RequestHeaders:string read GetRequestHeaders;
    property IsSuccessfull: boolean read FIsSuccesfull;
  end;

  { THTTPLib }

  THTTPLib = class(TInterfacedObject)
  private
    FContentType: string;
    FOnSyncStatus: TNotifyEvent;
    FRequestBody: TStream;
    FWorker: TWorkerHTTP;
    FResponseTime: Dword;
    FStopProcessing: boolean;
    function GetAllowRedirect: boolean;
    function GetCookies: TStrings;
    function GetPostFormData(variable: string): string;
    function getIsSuccessfull: boolean;
    function GetRequestHeaders: string;
    function getStreamSize: int64;
    function getURL: string;
    procedure SetAllowRedirect(AValue: boolean);
    procedure SetCookies(AValue: TStrings);
    procedure SetPostFormData(variable: string; AValue: string);
    procedure setURL(AValue: string);
    function CustomSubmit(MethodSubmit: string): IHTTPResponse;
    procedure ThreadTerminated(Sender: TObject);
    procedure SyncStatus;
    procedure Prepare;
    function __GetTickCount: DWord;
  public
    constructor Create(URL: string);
    constructor Create;
    destructor Destroy; override;
    function Get: IHTTPResponse;
    function Post: IHTTPResponse;
    function Put: IHTTPResponse;
    function Patch: IHTTPResponse;
    function Delete: IHTTPResponse;
    function UploadFile(Files: array of string;
      VarName: string = 'files[]'): IHTTPResponse;
    function UrlEncode(const DecodedStr: string; Pluses: boolean = True): string;
    procedure Clear;
    procedure AddFile(FileName: string; VarName: string = 'files[]');
    procedure AddHeader(const HeaderKey, HeaderValue: string);
    property FormData[variable: string]: string
      read GetPostFormData write SetPostFormData; default;
  published
    property URL: string read getURL write setURL;
    property AllowRedirect: boolean read GetAllowRedirect write SetAllowRedirect;
    property ContentType: string read FContentType write FContentType;
    property RequestBody: TStream read FRequestBody write FRequestBody;
    property Cookies: TStrings read GetCookies write SetCookies;
    property Size: int64 read getStreamSize;
    property IsSuccessfull: boolean read getIsSuccessfull;
    property OnSyncStatus: TNotifyEvent read FOnSyncStatus write FOnSyncStatus;
    property RequestHeaders: string read GetRequestHeaders;
  end;


implementation

const
  CRLF = #13#10;

{ TWorkerHTTP }

function TWorkerHTTP.getURL: string;
begin
  Result := FURL;
end;

function TWorkerHTTP.getResponseHeaders: string;
begin
  Result := HTTPClient.ResponseHeaders.Text;
end;

function TWorkerHTTP.GetRequestHeaders: string;
begin
  Result := HTTPClient.RequestHeaders.Text;
end;

function TWorkerHTTP.getResponseStatusCode: integer;
begin
  Result := HTTPClient.ResponseStatusCode;
end;

procedure TWorkerHTTP.setURL(AValue: string);
begin
  FURL := Avalue;
end;

procedure TWorkerHTTP.prepareRequestBody;
var
  S, Sep, postFormDataVar, fileVar: string;
  SS: TStringStream;
  F: TFileStream;
  i: integer;
begin
  s := '';
  Sep := Format('%.8x_multipart_boundary', [Random($ffffff)]);
  //HTTPClient.AddHeader('Content-Type','application/x-www-form-urlencoded');
  //HTTPClient.AddHeader('Content-Type', 'multipart/form-data; boundary=' + Sep);
  HTTPClient.AddHeader('Content-Type', 'multipart/form-data');
  if PostFormData.Count > 0 then
  begin
    for i := 1 to PostFormData.Count do
    begin
      postFormDataVar := ExtractDelimited(1, PostFormData[i - 1], ['=']);
      s := s + '--' + Sep + CRLF;
      s := s + 'Content-Disposition: form-data; name=' + AnsiQuotedStr(
        postFormDataVar, '"') + CRLF + 'Content-Type: text/plain' +
        CRLF + CRLF + ExtractDelimited(2, PostFormData[i - 1], ['=']) + CRLF;
    end;
  end;

  SS := TStringStream.Create(s);
  try

    // file upload
    SS.Seek(0, soFromEnd);
    if FileList.Count > 0 then
    begin
      for i := 1 to FileList.Count do
      begin
        fileVar := ExtractDelimited(1, FileList[i - 1], ['=']);
        s := '--' + Sep + CRLF;
        s := s + Format('Content-Disposition: form-data; name="%s"; filename="%s"' +
          CRLF, [fileVar, ExtractFileName(ExtractDelimited(
          2, FileList[i - 1], ['=']))]);
        s := s + 'Content-Type: application/octet-string' + CRLF + CRLF;
        SS.WriteBuffer(S[1], Length(S));

        //SS.Seek(0,soFromEnd);
        F := TFileStream.Create(ExtractDelimited(2, FileList[i - 1], ['=']),
          fmOpenRead or fmShareDenyWrite);
        try
          SS.CopyFrom(F, F.Size);
        finally
          F.Free;
        end;

        S := CRLF + '--' + Sep + '--' + CRLF;
        SS.WriteBuffer(S[1], Length(S));
      end;
    end;
    // file upload - end

    SS.Position := 0;
    HTTPClient.RequestBody := SS;
  except
    on e: Exception do
    begin
      ResultStream.WriteString(e.Message);
    end;
  end;
  exit;

end;

constructor TWorkerHTTP.Create(CreateSuspended: boolean);
begin
  FIsSuccesfull := False;
  PostFormData := TStringList.Create;
  FileList := TStringList.Create;
  HTTPClient := TFPHTTPClient.Create(nil);
  inherited Create;
end;

destructor TWorkerHTTP.Destroy;
begin
  FileList.Free;
  PostFormData.Free;
  HTTPClient.Free;
  inherited Destroy;
end;

procedure TWorkerHTTP.AddFile(FileName: string; VarName: string);
begin
  if not FileExists(FileName) then
    exit;
  FileList.Add(VarName + '=' + FileName);
end;

procedure TWorkerHTTP.Execute;
begin
  FailedMessage := '';
  FIsSuccesfull := False;
  try
    ResultStream := TStringStream.Create('');
    case FMethod of
      'GET': HTTPClient.Get(FURL, ResultStream);
      'FORMPOST':
      begin
        if not Assigned( HTTPClient.RequestBody) then
           prepareRequestBody;
        HTTPClient.Post(FURL, ResultStream);
      end;
      'DELETE':
      begin
        if not Assigned( HTTPClient.RequestBody) then
           prepareRequestBody;
        HTTPClient.Delete(FURL, ResultStream);
      end;
      'PUT':
      begin
        if not Assigned( HTTPClient.RequestBody) then
           prepareRequestBody;
        HTTPClient.Put(FURL, ResultStream);
      end;
      'PATCH':
      begin
        if not Assigned( HTTPClient.RequestBody) then
           prepareRequestBody;
        HTTPClient.Options(FURL, ResultStream);
      end;
      'POST':
      begin
        ResultStream.WriteString(
          HTTPClient.Post(FURL)
          );
      end;
    end;
    FIsSuccesfull := True;
    HTTPClient.RequestHeaders.Clear;
  except
    on e: Exception do
    begin
      ResultStream.WriteString(e.Message);
      FailedMessage := e.Message;
    end;
  end;

end;


{ THTTPResponse }

constructor THTTPResponse.Create(ResultCode: integer;
  const ResultHeader, ResultText: string; Stream: TStream; ElapsedTime: Dword);
begin
  FResultCode := ResultCode;
  FResultHeader := ResultHeader;
  FResultText := ResultText;
  FStream := TMemoryStream.Create;
  if assigned(Stream) then
    FStream.LoadFromStream(Stream);
  FResponseTime := ElapsedTime;
end;

constructor THTTPResponse.Create(ResultCode: integer;
  const ResultHeader, ResultText: string; Stream: TStream);
begin
  Create(ResultCode, ResultHeader, ResultText, Stream, 0);
end;

constructor THTTPResponse.Create(ResultCode: integer;
  const ResultHeader, ResultText: string);
begin
  Create(ResultCode, ResultHeader, ResultText);
end;

destructor THTTPResponse.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function THTTPResponse.ResultCode: integer;
begin
  Result := FResultCode;
end;

function THTTPResponse.ResultHeader: string;
begin
  Result := FResultHeader;
end;

function THTTPResponse.ResultText: string;
begin
  Result := FResultText;
end;

function THTTPResponse.ResultStream: TMemoryStream;
begin
  Result := FStream;
end;

function THTTPResponse.ResponseTime: Dword;
begin
  Result := FResponseTime;
end;

{ THTTPLib }

function THTTPLib.getURL: string;
begin
  Result := FWorker.URL;
end;

procedure THTTPLib.SetAllowRedirect(AValue: boolean);
begin
  FWorker.HTTPClient.AllowRedirect := AValue;
end;

procedure THTTPLib.SetCookies(AValue: TStrings);
begin
  if getCookies = AValue then
    exit;
  getCookies.Assign(AValue);
end;

procedure THTTPLib.SetPostFormData(variable: string; AValue: string);
begin
  if trim(variable) <> '' then
    FWorker.PostFormData.Values[variable] := AValue;
end;

function THTTPLib.getStreamSize: int64;
begin
  try
    Result := FWorker.ResultStream.Size;
  except
  end;
end;

function THTTPLib.GetCookies: TStrings;
begin
  Result := FWorker.HTTPClient.Cookies;
end;

function THTTPLib.GetAllowRedirect: boolean;
begin
  Result := FWorker.HTTPClient.AllowRedirect;
end;

function THTTPLib.GetPostFormData(variable: string): string;
begin
  try
    Result := FWorker.PostFormData.Values[variable];
  except
    Result := '';
  end;
end;

function THTTPLib.getIsSuccessfull: boolean;
begin
  Result := FWorker.IsSuccessfull;
end;

function THTTPLib.GetRequestHeaders: string;
begin
  Result := FWorker.RequestHeaders;
end;

procedure THTTPLib.setURL(AValue: string);
begin
  FWorker.URL := AValue;
end;

procedure THTTPLib.ThreadTerminated(Sender: TObject);
begin
  //(Sender as TMyThread).

  FStopProcessing := True;
end;

procedure THTTPLib.SyncStatus;
begin
  if assigned(OnSyncStatus) then
    OnSyncStatus(Self);
end;

procedure THTTPLib.Prepare;
begin
  if FContentType <> '' then
    FWorker.HTTPClient.RequestHeaders.Add('Content-Type: ' + FContentType);
  if assigned(FRequestBody) then
    FWorker.HTTPClient.RequestBody := FRequestBody;
end;

function THTTPLib.__GetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;

function THTTPLib.UrlEncode(const DecodedStr: string; Pluses: boolean): string;
var
  I: integer;
begin
  Result := '';
  if Length(DecodedStr) > 0 then
    for I := 1 to Length(DecodedStr) do
    begin
      if not (DecodedStr[I] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ']) then
        Result := Result + '%' + IntToHex(Ord(DecodedStr[I]), 2)
      else if not (DecodedStr[I] = ' ') then
        Result := Result + DecodedStr[I]
      else
      begin
        if not Pluses then
          Result := Result + '%20'
        else
          Result := Result + '+';
      end;
    end;
end;

constructor THTTPLib.Create(URL: string);
begin
  FRequestBody := nil;
  FWorker := TWorkerHTTP.Create(True);
  FWorker.HTTPClient.AddHeader('User-Agent', FASTPLAZ_USERAGENT);
  //FWorker.OnTerminate := @ThreadTerminated; -- prepare for threading
  setURL(URL);
  FStopProcessing := False;
end;

constructor THTTPLib.Create;
begin
  Create('');
end;

destructor THTTPLib.Destroy;
begin
  inherited Destroy;
end;

procedure THTTPLib.Clear;
begin
  FWorker.HTTPClient.RequestHeaders.Clear;
  FWorker.PostFormData.Clear;
  FContentType := '';
end;

procedure THTTPLib.AddFile(FileName: string; VarName: string);
begin
  FWorker.AddFile(FileName, VarName);
end;

procedure THTTPLib.AddHeader(const HeaderKey, HeaderValue: string);
begin
  FWorker.HTTPClient.AddHeader(HeaderKey, HeaderValue);
end;

function THTTPLib.Get: IHTTPResponse;
begin
  Result := CustomSubmit('GET');
end;

function THTTPLib.Post: IHTTPResponse;
begin
  //HTTPClient.RequestBody := TStringStream.Create(PostFormData.Text);

  Result := CustomSubmit('FORMPOST');
end;

function THTTPLib.Put: IHTTPResponse;
begin
  Result := CustomSubmit('PUT');
end;

function THTTPLib.Patch: IHTTPResponse;
begin
  Result := CustomSubmit('PATCH');
end;

function THTTPLib.Delete: IHTTPResponse;
begin
  Result := CustomSubmit('DELETE');
end;

function THTTPLib.UploadFile(Files: array of string; VarName: string): IHTTPResponse;
var
  i: integer;
begin
  for i := Low(Files) to High(Files) do
  begin
    AddFile(Files[i], VarName);
  end;
  Result := Post;
end;

function THTTPLib.CustomSubmit(MethodSubmit: string): IHTTPResponse;
begin
  Prepare;
  FResponseTime := __GetTickCount();
  FWorker.Method := MethodSubmit;

  FWorker.Execute;
  SyncStatus;
  FResponseTime := __GetTickCount - FResponseTime;

  Result := THTTPResponse.Create(FWorker.ResponseStatusCode,
    FWorker.ResponseHeaders, FWorker.ResultStream.DataString,
    FWorker.ResultStream, FResponseTime);

{
  Prepare;
  FResponseTime := __GetTickCount();
  FWorker.Method := MethodSubmit;
  FWorker.Start;
  while not FStopProcessing do
  begin
    SyncStatus;

    //Application.ProcessMessages;
  end;
  SyncStatus;
  FResponseTime := __GetTickCount - FResponseTime;

  Result := THTTPResponse.Create(FWorker.ResponseStatusCode,
    FWorker.ResponseHeaders, FWorker.ResultStream.DataString,
    FWorker.ResultStream, FResponseTime);
}
end;


end.


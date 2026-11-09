unit solr_lib;

{
  USAGE

  with TSOLRModel.Create do
  begin
    Host := 'localhost';
    Port := '8389';
    Address := '/solr';
    Core := 'yourcorename';
    Query.Text := '*:*';
    //Indentation := True;

    //Index := 0;
    //Limit := 10;

    if Open then
    begin

      while not EOF do
      begin
        yourvariable := Field['yourfieldname'];

        Next;
      end;

    end;

    Free;
  end;

  [x] RESULT VARIABLE
    RecordCount
    TotalCount
    ResultText   -> result as json text
    ResultJSON   -> result as json object

  Open returns False (and EOF is True) when the server cannot be reached,
  answers with a non-200 status, returns a Solr error or invalid JSON.
  Field[] returns '' for a missing field and joins multi-valued fields
  with ', '.
}

{$mode objfpc}{$H+}

interface

uses
  http_lib, fpjson, jsonparser,
  Classes, SysUtils;

const
  SOLR_PORT_DEFAULT = '8389';

type

  { TSOLRModel }

  TSOLRModel = class
  private
    FDebugQuery: boolean;
    FFields: string;
    FRowIndex: integer;
    FEOF: boolean;
    FFormat: string;
    FIndentation: boolean;
    FIndex: integer;
    FLimit: integer;
    FRecordCount: integer;
    FSort: string;
    FSSL: boolean;
    FTotalCount: integer;
    FResultCode: integer;
    FResultText: string;
    FErrorMessage: string;
    FAddress: string;
    FCore: string;
    FHost: string;
    FPort: string;
    FQuery: TStringList;
    FURL: string;
    FResponseJSON: TJSONData;
    function GenerateURL: string;
    function GetValue(variable: string): string;
    function GetDocs: TJSONArray;
    procedure init;
  protected
    // Performs the HTTP GET; overridable for tests. Returns False when the
    // request could not be made at all.
    function DoRequest(const AURL: string; out ACode: integer;
      out AText: string): boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Open: boolean;
    function Open(SORLURL: string): boolean;
    // Interprets a Solr JSON response body without making a request.
    function LoadFromJson(const AJson: string): boolean;
    function ResultText: string;
    function ResultCode: integer;

    procedure First;
    procedure Next;
    property Field[FieldName: string]: string read GetValue; default;
  published
    property URL: string read FURL;
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property Post: string read FPort write FPort; deprecated 'use Port';
    property Address: string read FAddress write FAddress;
    property Core: string read FCore write FCore;
    property SSL: boolean read FSSL write FSSL;
    property Format: string read FFormat write FFormat;
    property Indentation: boolean read FIndentation write FIndentation;
    property Query: TStringList read FQuery;
    property Index: integer read FIndex write FIndex;
    property Limit: integer read FLimit write FLimit;

    property Sort: string read FSort write FSort;
    property Fields: string read FFields write FFields;
    property DebugQuery: boolean read FDebugQuery write FDebugQuery;

    property EOF: boolean read FEOF;
    property TotalCount: integer read FTotalCount;
    property RecordCount: integer read FRecordCount;
    property ResultJSON: TJSONData read FResponseJSON;
    property ErrorMessage: string read FErrorMessage;
  end;

// Percent-encode a query parameter value (RFC 3986 unreserved characters
// are kept). Spaces become %20, which Solr accepts.
function SolrEncodeParam(const S: string): string;

implementation

function SolrEncodeParam(const S: string): string;
const
  HexChars = '0123456789ABCDEF';
var
  i: integer;
  c: char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    if c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'] then
      Result := Result + c
    else
      Result := Result + '%' + HexChars[(Ord(c) shr 4) + 1] + HexChars[(Ord(c) and 15) + 1];
  end;
end;

// JSON floats render as "9.5000000000000000E+000" through AsString;
// give scalars a plain representation with a dot decimal separator.
function ScalarToString(const V: TJSONData): string;
var
  fs: TFormatSettings;
begin
  case V.JSONType of
    jtNull: Result := '';
    jtNumber:
      if TJSONNumber(V).NumberType = ntFloat then
      begin
        fs := DefaultFormatSettings;
        fs.DecimalSeparator := '.';
        fs.ThousandSeparator := #0;
        Result := FloatToStr(V.AsFloat, fs);
      end
      else
        Result := V.AsString;
    jtArray, jtObject: Result := V.AsJSON;
    else
      Result := V.AsString;
  end;
end;

{ TSOLRModel }


// format example:
//   http://192.168.1.7:8080/solr-master/area/select?start=5&rows=2&wt=json&indent=true&q=*:*
function TSOLRModel.GenerateURL: string;
begin
  Result := 'http://';
  if FSSL then
    Result := 'https://';
  Result := Result + FHost + ':' + FPort + FAddress + '/' + SolrEncodeParam(FCore) + '/select?';
  Result := Result + 'wt=' + SolrEncodeParam(FFormat) + '&';
  if FIndentation then
    Result := Result + 'indent=true&';
  if FDebugQuery then
    Result := Result + 'debugQuery=true&';
  if FIndex <> -1 then
    Result := Result + 'start=' + IntToStr(FIndex) + '&';
  if FLimit <> -1 then
    Result := Result + 'rows=' + IntToStr(FLimit) + '&';
  if FSort <> '' then
    Result := Result + 'sort=' + SolrEncodeParam(FSort) + '&';
  if FFields <> '' then
    Result := Result + 'fl=' + SolrEncodeParam(FFields) + '&';

  // a query spanning several lines is joined with spaces; every byte is
  // encoded so '&', '+' or '#' in a term cannot inject other parameters
  Result := Result + 'q=' + SolrEncodeParam(Trim(StringReplace(
    StringReplace(FQuery.Text, #13, '', [rfReplaceAll]), #10, ' ', [rfReplaceAll])));
end;

function TSOLRModel.GetDocs: TJSONArray;
var
  d: TJSONData;
begin
  Result := nil;
  if not Assigned(FResponseJSON) then
    Exit;
  d := FResponseJSON.FindPath('response.docs');
  if Assigned(d) and (d is TJSONArray) then
    Result := TJSONArray(d);
end;

function TSOLRModel.GetValue(variable: string): string;
var
  docs: TJSONArray;
  v: TJSONData;
  i: integer;
begin
  Result := '';
  docs := GetDocs;
  if (docs = nil) or (FRowIndex < 0) or (FRowIndex >= docs.Count) then
    Exit;
  v := docs.Items[FRowIndex].FindPath(variable);
  if not Assigned(v) then
    Exit;
  if v.JSONType = jtArray then
  begin
    // multi-valued field
    for i := 0 to v.Count - 1 do
    begin
      if i > 0 then
        Result := Result + ', ';
      Result := Result + ScalarToString(v.Items[i]);
    end;
  end
  else
    Result := ScalarToString(v);
end;

procedure TSOLRModel.init;
begin
  // nothing loaded: EOF is True so "while not EOF" never runs
  FEOF := True;
  FRecordCount := -1;
  FTotalCount := -1;
  FRowIndex := -1;
  FResultCode := 0;
  FResultText := '';
  FErrorMessage := '';
  FreeAndNil(FResponseJSON);
end;

function TSOLRModel.LoadFromJson(const AJson: string): boolean;
var
  d: TJSONData;
  docs: TJSONArray;
begin
  Result := False;
  FEOF := True;
  FRecordCount := -1;
  FTotalCount := -1;
  FRowIndex := -1;
  FreeAndNil(FResponseJSON);
  try
    FResponseJSON := GetJSON(AJson);
  except
    on E: Exception do
    begin
      FErrorMessage := 'invalid JSON: ' + E.Message;
      Exit;
    end;
  end;
  if not Assigned(FResponseJSON) then
  begin
    FErrorMessage := 'empty response';
    Exit;
  end;
  d := FResponseJSON.FindPath('responseHeader.status');
  if Assigned(d) and (d.JSONType = jtNumber) and (d.AsInteger <> 0) then
  begin
    FErrorMessage := 'solr status ' + IntToStr(d.AsInteger);
    d := FResponseJSON.FindPath('error.msg');
    if Assigned(d) then
      FErrorMessage := FErrorMessage + ': ' + d.AsString;
    Exit;
  end;
  docs := GetDocs;
  if docs = nil then
  begin
    FErrorMessage := 'response.docs missing';
    Exit;
  end;
  d := FResponseJSON.FindPath('response.numFound');
  if Assigned(d) and (d.JSONType = jtNumber) then
    FTotalCount := d.AsInteger
  else
    FTotalCount := docs.Count;
  FRecordCount := docs.Count;
  FRowIndex := 0;
  FEOF := FRecordCount = 0;
  FErrorMessage := '';
  Result := True;
end;

function TSOLRModel.DoRequest(const AURL: string; out ACode: integer;
  out AText: string): boolean;
var
  http: THTTPLib;
  response: IHTTPResponse;
begin
  http := THTTPLib.Create(AURL);
  try
    response := http.Get;
    ACode := response.ResultCode;
    AText := response.ResultText;
    Result := http.IsSuccessfull;
  finally
    http.Free;
  end;
end;

constructor TSOLRModel.Create;
begin
  inherited Create;
  FSSL := False;
  FURL := '';
  FHost := 'localhost';
  FPort := SOLR_PORT_DEFAULT;
  FCore := '';
  FAddress := '';
  FFormat := 'json';
  FIndentation := False;
  FQuery := TStringList.Create;
  FResponseJSON := nil;

  FIndex := -1;
  FLimit := -1;
  FDebugQuery := False;

  init;
end;

destructor TSOLRModel.Destroy;
begin
  FResponseJSON.Free;
  FQuery.Free;
  inherited Destroy;
end;

function TSOLRModel.Open: boolean;
begin
  FURL := GenerateURL;
  Result := Open(FURL);
end;

function TSOLRModel.Open(SORLURL: string): boolean;
var
  code: integer;
  text: string;
begin
  init;
  Result := False;
  FURL := SORLURL;
  if not DoRequest(SORLURL, code, text) then
  begin
    FResultCode := code;
    FResultText := text;
    FErrorMessage := 'request failed: ' + text;
    Exit;
  end;
  FResultCode := code;
  FResultText := text;
  if code <> 200 then
  begin
    FErrorMessage := 'http status ' + IntToStr(code);
    Exit;
  end;
  Result := LoadFromJson(text);
end;

function TSOLRModel.ResultText: string;
begin
  Result := FResultText;
end;

function TSOLRModel.ResultCode: integer;
begin
  Result := FResultCode;
end;

procedure TSOLRModel.First;
begin
  if FRecordCount > 0 then
  begin
    FRowIndex := 0;
    FEOF := False;
  end
  else
  begin
    FRowIndex := -1;
    FEOF := True;
  end;
end;

procedure TSOLRModel.Next;
begin
  if FEOF then
    Exit;
  if FRowIndex >= FRecordCount - 1 then
  begin
    FEOF := True;
    Exit;
  end;
  Inc(FRowIndex);
end;

end.

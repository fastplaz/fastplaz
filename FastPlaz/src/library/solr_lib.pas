unit solr_lib;

{
  USAGE

  with TSOLRModel.Create do
  begin
    Host := 'localhost';
    Post := '8389';
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
    httpResponse: IHTTPResponse;
    FAddress: string;
    FCore: string;
    FHost: string;
    FPort: string;
    FQuery: TStringList;
    FURL: string;
    FResponseJSON: TJSONData;
    function GenerateURL: string;
    function GetValue(variable: string): string;
    function isValidResult: boolean;
    procedure init;
    procedure GenerateDataset;
  public
    constructor Create;
    destructor Destroy; override;
    function Open: boolean;
    function Open(SORLURL: string): boolean;
    function ResultText: string;
    function ResultCode: integer;

    procedure First;
    procedure Next;
    property Field[FieldName: string]: string read GetValue; default;
  published
    property URL: string read FURL;
    property Host: string read FHost write FHost;
    property Post: string read FPort write FPort;
    property Address: string read FAddress write FAddress;
    property Core: string read FCore write FCore;
    property SSL: boolean read FSSL write FSSL;
    property Format: string read FFormat write FFormat;
    property Indentation: boolean read FIndentation write FIndentation;
    property Query: TStringList read FQuery write FQuery;
    property Index: integer read FIndex write FIndex;
    property Limit: integer read FLimit write FLimit;

    property Sort: string read FSort write FSort;
    property Fields: string read FFields write FFields;
    property DebugQuery: boolean read FDebugQuery write FDebugQuery;

    property EOF: boolean read FEOF;
    property TotalCount: integer read FTotalCount;
    property RecordCount: integer read FRecordCount;
    property ResultJSON: TJSONData read FResponseJSON;

  end;


implementation

{ TSOLRModel }


// format example:
//   http://192.168.1.7:8080/solr-master/area/select?start=5&rows=2&wt=json&indent=true&q=*:*
function TSOLRModel.GenerateURL: string;
begin
  Result := 'http://';
  if FSSL then
    Result := 'https://';
  Result := Result + FHost + ':' + FPort + FAddress + '/' + FCore + '/select?';
  Result := Result + 'wt=' + FFormat + '&';
  if FIndentation then
    Result := Result + 'indent=true&';
  if FDebugQuery then
    Result := Result + 'debugQuery=true&';
  if FIndex <> -1 then
    Result := Result + 'start=' + IntToStr(FIndex) + '&';
  if FLimit <> -1 then
    Result := Result + 'rows=' + IntToStr(FLimit) + '&';
  if FSort <> '' then
    Result := Result + 'sort=' + FSort + '&';
  if FFields <> '' then
    Result := Result + 'fl=' + FFields + '&';

  Result := Result + 'q=' + trim(FQuery.Text);
end;

function TSOLRModel.GetValue(variable: string): string;
var
  docs: TJSONData;
begin
  Result := '';
  if FRowIndex = -1 then
    Exit;

  docs := FResponseJSON.FindPath('response.docs');
  Result := docs.Items[FRowIndex].FindPath(variable).AsString;
end;

procedure TSOLRModel.init;
begin
  FEOF := False;
  FRecordCount := -1;
  FTotalCount := -1;
  FRowIndex := -1;
end;

function TSOLRModel.isValidResult: boolean;
begin
  Result := False;
  try
    FResponseJSON := GetJSON(httpResponse.ResultText);
    if FResponseJSON.FindPath('responseHeader.status').AsInteger <> 0 then
      Exit;
    FTotalCount := FResponseJSON.FindPath('response.numFound').AsInteger;
    FRecordCount := FResponseJSON.FindPath('response.docs').Count;
    FRowIndex := 0;
    Result := True;
  except
  end;
end;

procedure TSOLRModel.GenerateDataset;
var
  docs, item: TJSONData;
  i, j: integer;
begin
  docs := FResponseJSON.FindPath('response.docs');
  for i := 0 to docs.Count - 1 do
  begin
    item := docs.Items[i];

    // collect field name
    if i = 0 then
    begin
      for j := 0 to item.Count - 1 do
      begin
        // fieldname := TJSONObject(item).Names[j];

        // TODO: save field name to dataset

      end;
    end;

  end; // i

end;

constructor TSOLRModel.Create;
begin
  FSSL := False;
  FURL := '';
  FHost := 'localhost';
  FPort := SOLR_PORT_DEFAULT;
  FCore := '';
  FAddress := '';
  FFormat := 'json';
  FIndentation := False;
  FQuery := TStringList.Create;

  FIndex := -1;
  FLimit := -1;
  FDebugQuery := False;

  init;
end;

destructor TSOLRModel.Destroy;
begin
  FQuery.Destroy;
  inherited Destroy;
end;

function TSOLRModel.Open: boolean;
begin
  Result := False;
  FURL := GenerateURL;
  Result := Open(FURL);
end;

function TSOLRModel.Open(SORLURL: string): boolean;
begin
  init;
  Result := True;

  with THTTPLib.Create do
  begin
    URL := SORLURL;
    httpResponse := Get();

    if httpResponse.ResultCode = 200 then
    begin
      if isValidResult then
      begin
        GenerateDataset;
        Result := True;
      end;
    end;

    Free;
  end;

end;

function TSOLRModel.ResultText: string;
begin
  Result := httpResponse.ResultText;
end;

function TSOLRModel.ResultCode: integer;
begin
  Result := httpResponse.ResultCode;
end;

procedure TSOLRModel.First;
begin
  FRowIndex := 0;
  FEOF := False;
end;

procedure TSOLRModel.Next;
begin
  if FRowIndex = FRecordCount - 1 then
  begin
    FEOF := True;
    Exit;
  end;

  if FRowIndex < FRecordCount - 1 then
    FRowIndex := FRowIndex + 1;
end;

end.

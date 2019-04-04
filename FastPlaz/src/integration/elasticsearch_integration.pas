unit elasticsearch_integration;

{
  // USAGE:

  // Submit Data
  with TElasticSearchIntegration.Create do
  begin
    Endpoint  := 'your_elastic_endpoint';
    IndexName := 'the_index';
    TypeName  := 'the_type';

    Data.Text := 'your json content';
    Send;

    Free;
  end;

  // Query Data
  with TElasticSearchIntegration.Create do
  begin
  Endpoint  := 'your_elastic_endpoint';
    IndexName := 'the_index';
    TypeName  := 'the_type';

    Data.Text := '{"query" : {"query_string" : {"query" : "tes*"}}}';
    Query;

    if IsSuccessfull then
      QueryResult := ResultText;

    Free;
  end;

}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  http_lib, common,
  Classes, SysUtils;

{$ifdef ELASTICSEARCH_INTEGRATION}
{$endif}

type

  { TElasticSearchIntegration }

  TElasticSearchIntegration = class(TInterfacedObject)
  private
    FData: TStringList;
    FEndpoint: string;
    FIsSuccessfull: boolean;
    FResultCode: integer;
    FResultText: string;
    FURL: string;
    FIndexName: string;
    FTypeName: string;
    function getEndpoint: string;
    function getIndexName: string;
    function getTypeName: string;
    procedure setEndpoint(AValue: string);
    procedure setIndexName(AValue: string);
    procedure setTypeName(AValue: string);
  public
    constructor Create;
    destructor Destroy;
    function CustomCommand(AFunction: string = ''): boolean;
    function Send: boolean;
    function Query: boolean;
  published
    property URL: string read FURL write FURL;
    property Endpoint: string read getEndpoint write setEndpoint;
    property IndexName: string read getIndexName write setIndexName;
    property TypeName: string read getTypeName write setTypeName;
    property Data: TStringList read FData write FData;
    property IsSuccessfull: boolean read FIsSuccessfull;

    property ResultText: string read FResultText;
    property ResultCode: integer read FResultCode;
  end;

implementation

var
  Response: IHTTPResponse;

{ TElasticSearchIntegration }

function TElasticSearchIntegration.getIndexName: string;
begin
  Result := FIndexName;
end;

function TElasticSearchIntegration.getEndpoint: string;
begin
  Result := FEndpoint;
end;

function TElasticSearchIntegration.getTypeName: string;
begin
  Result := FTypeName;
end;

procedure TElasticSearchIntegration.setEndpoint(AValue: string);
begin
  FEndpoint := ExcludeTrailingPathDelimiter(AValue);
  FURL := FEndpoint;
  if not FIndexName.IsEmpty then
    FURL := FEndpoint + '/' + FIndexName;
  if not FTypeName.IsEmpty then
    FURL := FURL + '/' + FTypeName;
end;

procedure TElasticSearchIntegration.setIndexName(AValue: string);
begin
  FIndexName := AValue.Replace('/', '');
  FURL := FEndpoint;
  if not FIndexName.IsEmpty then
    FURL := FEndpoint + '/' + FIndexName;
  if not FTypeName.IsEmpty then
    FURL := FURL + '/' + FTypeName;
end;

procedure TElasticSearchIntegration.setTypeName(AValue: string);
begin
  FTypeName := AValue.Replace('/', '');
  FURL := FEndpoint;
  if not FIndexName.IsEmpty then
    FURL := FEndpoint + '/' + FIndexName;
  if not FTypeName.IsEmpty then
    FURL := FURL + '/' + FTypeName;
end;

constructor TElasticSearchIntegration.Create;
begin
  FIsSuccessfull := False;
  FIndexName := '';
  FTypeName := '';

  FData := TStringList.Create;
end;

destructor TElasticSearchIntegration.Destroy;
begin
  FData.Free;
end;

function TElasticSearchIntegration.CustomCommand(AFunction: string): boolean;
var
  httpResponse: IHTTPResponse;
begin
  FIsSuccessfull := False;
  if FEndpoint.IsEmpty then
    Exit;

  URL := URL + AFunction;
  with THTTPLib.Create(URL) do
  begin
    ContentType := 'application/json';
    AddHeader('Cache-Control', 'no-cache');
    RequestBody := TStringStream.Create(FData.Text);

    httpResponse := Post;
    FResultCode := httpResponse.ResultCode;
    FResultText := httpResponse.ResultText;
    if httpResponse.ResultCode = 200 then
      FIsSuccessfull := True;
    Free;
  end;

  Result := FIsSuccessfull;
end;

function TElasticSearchIntegration.Send: boolean;
begin
  Result := CustomCommand();
end;

function TElasticSearchIntegration.Query: boolean;
begin
  Result := CustomCommand('/_search');
end;


end.



unit graphql_integration;

{
  // USAGE:

  // SIMPLE GITHUB API
  with TGraphQLIntegration.Create do
  begin
    URL := 'https://api.github.com/graphql';
    Authorization:= 'your token';
    return := Query('{viewer {login}}', '');
    if IsSuccessfull then
    begin
      ...
    end;

    Free;
  end;

  // WITH VARIABLEs - get number of repo in github

  json := TJSONUtil.Create;
  json['number_of_repos'] := 3;

  with TGraphQLIntegration.Create do
  begin
    URL := 'https://api.github.com/graphql';
    Authorization:= 'your token';
    return := Query('query($number_of_repos:Int) {viewer {name repositories(last: $number_of_repos) {nodes {name}}}} ', json);

    if IsSuccessfull then
    begin
      ;
    end;

    Free;
  end;
  json.Free



}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  Dialogs,
  http_lib, json_lib,
  fpjson, Classes, SysUtils;

{$ifdef EXAMPLE_INTEGRATION}
{$endif}

type

  { TGraphQLIntegration }

  TGraphQLIntegration = class(TInterfacedObject)
  private
    FAuthorization: string;
    FIsSuccessfull: boolean;
    FURL: string;
  public
    constructor Create;
    destructor Destroy;
    function Query(const AQuery: string; const Variables: TJSONUtil = nil): string; overload;
    function Query(const AQuery: string; const Variables: string): string; overload;
  published
    property URL: string read FURL write FURL;
    property Authorization: string read FAuthorization write FAuthorization;
    property IsSuccessfull: boolean read FIsSuccessfull;
  end;

implementation

{ TGraphQLIntegration }

constructor TGraphQLIntegration.Create;
begin
  FURL := '';
  FAuthorization := '';
  FIsSuccessfull := False;
end;

destructor TGraphQLIntegration.Destroy;
begin
end;

function TGraphQLIntegration.Query(const AQuery: string;
  const Variables: TJSONUtil): string;
begin
  if Variables = nil then
    Result := Query(AQuery, '')
  else
    Result := Query(AQuery, Variables.AsJSON);
end;

function TGraphQLIntegration.Query(const AQuery: string;
  const Variables: string): string;
var
  request: string;
  httpResult: IHTTPResponse;
  requestBody: TStream;
begin
  Result := '{}';
  FIsSuccessfull := False;
  if FURL.IsEmpty then
    Exit;
  if Variables.IsEmpty then
    request := '{"query": "' + AQuery + '" }'
  else
    request := '{"query": "' + AQuery + '",' + '"variables": ' + Variables + ' }';

  with THTTPLib.Create(FURL) do
  begin
    ContentType := 'application/json';
    RequestBody := TStringStream.Create(request);
    if not FAuthorization.IsEmpty then
      AddHeader('Authorization', 'bearer ' + FAuthorization);
    httpResult := Post;

    if httpResult.ResultCode = 200 then
      FIsSuccessfull := True;;
    Result := httpResult.ResultText;
    Free;
  end;

end;


end.





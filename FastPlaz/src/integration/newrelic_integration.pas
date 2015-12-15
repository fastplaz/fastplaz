unit newrelic_integration;

{$mode objfpc}{$H+}
{$include ../../define.inc}

interface

uses
  http_lib,
  Classes, SysUtils;

{$ifdef NEWRELIC_INTEGRATION}
const
  NEWRELIC_URL = 'https://api.newrelic.com/deployments.xml';

type

  { TNewrelicIntegration }

  TNewrelicIntegration = class(TInterfacedObject)
  private
    FAPIKey: string;
    FApplicationID: string;
    FAppName: string;
    FChangeLog: string;
    FDescription: string;
    FIsSuccessfull: boolean;
    FRevision: string;
    FURL: string;
    FUser: string;
    procedure setURL(AValue: string);
    function prepare: boolean;
  public
    constructor Create(URL: string);
    constructor Create;
    function Send: IHTTPResponse;
  published
    property URL: string read FURL write setURL;
    property APIKey: string read FAPIKey write FAPIKey;
    property AppName: string read FAppName write FAppName;
    property ApplicationID: string read FApplicationID write FApplicationID;
    property Description: string read FDescription write FDescription;
    property Revision: string read FRevision write FRevision;
    property ChangeLog: string read FChangeLog write FChangeLog;
    property User: string read FUser write FUser;
    property IsSuccessfull: boolean read FIsSuccessfull;
  end;

{$endif}

implementation

{$ifdef NEWRELIC_INTEGRATION}

{ TNewrelicIntegration }

procedure TNewrelicIntegration.setURL(AValue: string);
begin
  if FURL = AValue then
    Exit;
  FURL := AValue;
end;

function TNewrelicIntegration.prepare: boolean;
begin

  Result := True;
end;

constructor TNewrelicIntegration.Create(URL: string);
begin
  setURL(URL);
  FIsSuccessfull := False;
end;

constructor TNewrelicIntegration.Create;
begin
  FIsSuccessfull := False;
  setURL(NEWRELIC_URL);
end;

function TNewrelicIntegration.Send: IHTTPResponse;
begin
  if not prepare then
    Exit;

  FIsSuccessfull := False;
  with THTTPLib.Create(URL) do
  begin
    ContentType := 'application/x-www-form-urlencoded';
    AddHeader('Connection', 'keep-alive');
    AddHeader('Cache-Control', 'no-cache');
    AddHeader('Accept', '*/*');
    AddHeader('x-api-key', FAPIKey);
    FormData['deployment[app_name]'] := UrlEncode( FAppName);
    FormData['deployment[application_id]'] := UrlEncode( FApplicationID);
    FormData['deployment[description]'] := UrlEncode( FDescription);
    FormData['deployment[revision]'] := UrlEncode( FRevision);
    FormData['deployment[changeLog]'] := UrlEncode( FChangeLog);
    FormData['deployment[user]'] := UrlEncode( FUser);
    Result := Post;
    FIsSuccessfull := IsSuccessfull;
    Free;
  end;

end;

{$endif}

end.

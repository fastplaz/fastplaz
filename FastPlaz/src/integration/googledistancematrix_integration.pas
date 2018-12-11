{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
{
REF:
  https://developers.google.com/maps/documentation/distance-matrix/
  // USAGE:

}
unit googledistancematrix_integration;

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  fpjson,
  common, http_lib,
  Classes, SysUtils;

{$ifdef GOOGLEDISTANCE_INTEGRATION}
{$endif}

type

  { TGoogleDistanceIntegration }

  TGoogleDistanceIntegration = class(TInterfacedObject)
  private
    FKey: String;
    jsonData: TJSONData;
    FDestinations: string;
    FDistance: integer;
    FDistanceAsText: string;
    FDuration: integer;
    FDurationAsText: string;
    FIsSuccessfull: boolean;
    FOrigins: string;
    FURL: string;
  public
    constructor Create;
    destructor Destroy;

    function SendExample(FileName: string; Public_Id: string = ''): IHTTPResponse;
    function GetDistance(AOrigins, ADestionations: string): boolean;
  published
    property URL: string read FURL write FURL;
    property Key: String read FKey write FKey;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property Origins: string read FOrigins write FOrigins;
    property Destinations: string read FDestinations write FDestinations;

    property Distance: integer read FDistance;
    property DistanceAsText: string read FDistanceAsText;
    property Duration: integer read FDuration;
    property DurationAsText: string read FDurationAsText;
  end;

implementation

const
  GOOGLEDISTANCE_URL =
    'https://maps.googleapis.com/maps/api/distancematrix/json?origins=%s&destinations=%s&language=id-ID&key=';

var
  Response: IHTTPResponse;

{ TGoogleDistanceIntegration }

constructor TGoogleDistanceIntegration.Create;
begin
  FIsSuccessfull := False;
  FOrigins := '';
  FDestinations := '';
  FDistance := 0;
  FDistanceAsText := '';
  FDuration := 0;
  FDurationAsText := '';
end;

destructor TGoogleDistanceIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
  inherited Destroy;
end;

function TGoogleDistanceIntegration.SendExample(FileName: string;
  Public_Id: string): IHTTPResponse;
begin
  FIsSuccessfull := False;
  with THTTPLib.Create(URL) do
  begin
    ContentType := 'application/x-www-form-urlencoded';
    //AddHeader('Connection', 'keep-alive');
    //AddHeader('Cache-Control', 'no-cache');
    //AddHeader('Accept', '*/*');
    if Public_Id = '' then
      FormData['public_id'] := ChangeFileExt(ExtractFileName(FileName), '');
    FormData['api_key'] := 'apikey';
    FormData['api_secret'] := 'apisecret';
    AddFile(FileName, 'file');

    Result := Post;
    FIsSuccessfull := IsSuccessfull;
    Free;
  end;
end;

function TGoogleDistanceIntegration.GetDistance(AOrigins, ADestionations:
  string): boolean;
var
  s, urlTarget: string;
begin
  Result := False;
  if (AOrigins = '') or (ADestionations = '') then
    Exit;
  urlTarget := Format(GOOGLEDISTANCE_URL, [UrlEncode(AOrigins), UrlEncode(ADestionations)])
    + FKey;

  with THTTPLib.Create(urlTarget) do
  begin
    Response := Get;
    if Response.ResultCode <> 200 then
    begin
      Free;
      Exit;
    end;
    jsonData := GetJSON(Response.ResultText);
    s := jsonGetData(jsonData, 'rows[0]/elements[0]/status');
    if s <> 'OK' then
    begin
      Free;
      Exit;
    end;

    // Detail
    FOrigins := jsonGetData(jsonData, 'origin_addresses[0]');
    FDestinations := jsonGetData(jsonData, 'destination_addresses[0]');
    FDistanceAsText := jsonGetData(jsonData, 'rows[0]/elements[0]/distance/text');
    FDurationAsText := jsonGetData(jsonData, 'rows[0]/elements[0]/duration/text');

    FDistance := s2i(jsonGetData(jsonData, 'rows[0]/elements[0]/distance/value'));
    FDuration := s2i(jsonGetData(jsonData, 'rows[0]/elements[0]/duration/value'));

    // TODO: multiple destinations

    Result := True;
    Free;
  end;

end;


end.

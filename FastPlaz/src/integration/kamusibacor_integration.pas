{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
{
  // USAGE:

  kamus := TKamusIbacorIntegration.Create;
  kamus.Token := 'ibacortoken';
  Result := kamus.Find('gadis');
  kamus.Free;

}
unit kamusibacor_integration;

{$mode objfpc}{$H+}

interface

uses
  {$IFNDEF Windows}
  cthreads,
  {$ENDIF}
  common, http_lib,
  fpjson, Classes, SysUtils;

type

  { TKamusIbacorIntegration }

  TKamusIbacorIntegration = class(TInterfacedObject)
  private
    FToken: string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;
    property Token: string read FToken write FToken;
    function Find(Text: string): string;
  end;



implementation

const
  _KAMUS_IBACOR_URL = 'http://ibacor.com/api/kamus-bahasa?k=%s&kata=%s';

var
  Response: IHTTPResponse;

{ TKamusIbacorIntegration }

constructor TKamusIbacorIntegration.Create;
begin
  FToken := '';
end;

destructor TKamusIbacorIntegration.Destroy;
begin

end;

function TKamusIbacorIntegration.Find(Text: string): string;
var
  s: string;
  httpClient: THTTPLib;
  jsonData: TJSONData;
begin
  Result := '';
  Text := trim(Text);
  if Text = '' then
    Exit;
  if FToken = '' then
    Exit;

  s := Format(_KAMUS_IBACOR_URL, [FToken, Text]);
  httpClient := THTTPLib.Create;
  httpClient.URL := s;
  Response := httpClient.Get;
  httpClient.Free;

  if Response.ResultCode <> 200 then
    Exit;

  try
    jsonData := GetJSON(Response.ResultText);
    if jsonData.GetPath('status').AsString = 'success' then
    begin
      Result := Response.ResultText;
    end;
    jsonData.Free;
  except
  end;

end;

end.


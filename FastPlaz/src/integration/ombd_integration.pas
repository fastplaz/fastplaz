{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit ombd_integration;

{
  ref:
    https://www.omdbapi.com/

}

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, json_lib, logutil_lib,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets,
  {$endif}
  Classes, SysUtils;

type

  { TOmdbIntegration }

  TOmdbIntegration = class
  private
    FKey: string;
    FResponseText: string;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(MovieTitle: string): string;
  published
    property Key:string read FKey write FKey;
    property ResponseText: string read FResponseText;
  end;

implementation

const
  _OMDB_URL = 'http://www.omdbapi.com/?y=&plot=short&r=json&apikey=%s&t=%s';

  _OMDB_MSG_NOTFOUND = 'Film "%s" tidak ditemukan';

var
  Response: IHTTPResponse;

{ TOmdbIntegration }

constructor TOmdbIntegration.Create;
begin
  FKey := '';
  FResponseText := '';
end;

destructor TOmdbIntegration.Destroy;
begin

end;

function TOmdbIntegration.Find(MovieTitle: string): string;
var
  s: string;
  _http: THTTPLib;
  _json: TJSONUtil;
begin
  Result := Format(_OMDB_MSG_NOTFOUND, [MovieTitle]);
  s := Format( _OMDB_URL, [FKey, UrlEncode(MovieTitle)]);

  _http := THTTPLib.Create;
  _http.URL := Trim( s);
  Response := _http.Get;
  FResponseText := Response.ResultText;
  _http.Free;
  if Response.ResultCode <> 200 then
    Exit;

  _json := TJSONUtil.Create;
  try
    _json.LoadFromJsonString(Response.ResultText);
    if _json['Response'] <> 'True' then
    begin
      Exit;
    end;

    Result := '*Info Film "' + MovieTitle + '"*';
    Result := Result + '\nRilis: ' + _json['Released'];
    Result := Result + '\nGenre: ' + _json['Genre'];
    Result := Result + '\nAktor: ' + _json['Actors'];
    Result := Result + '\nIMDB Rating: ' + _json['imdbRating'];
    Result := Result + '\nDirector: ' + _json['Director'];
    Result := Result + '\n_Plot: ' + _json['Plot'] + '_';
  except
    on E:Exception do
    begin
    end;
  end;

end;

end.

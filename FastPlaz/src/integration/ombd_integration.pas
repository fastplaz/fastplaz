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
  Classes, SysUtils;

type

  { TOmdbIntegration }

  TOmdbIntegration = class
  private
  public
    constructor Create;
    destructor Destroy; override;

    function Find(MovieTitle: string): string;
  end;

implementation

const
  _OMDB_URL = 'http://www.omdbapi.com/?y=&plot=short&r=json&t=';

  _OMDB_MSG_NOTFOUND = 'Film "%s" tidak ditemukan';

var
  Response: IHTTPResponse;

{ TOmdbIntegration }

constructor TOmdbIntegration.Create;
begin

end;

destructor TOmdbIntegration.Destroy;
begin

end;

function TOmdbIntegration.Find(MovieTitle: string): string;
var
  _http: THTTPLib;
  _json: TJSONUtil;
begin
  Result := Format(_OMDB_MSG_NOTFOUND, [MovieTitle]);

  _http := THTTPLib.Create;
  _http.URL := _OMDB_URL + UrlEncode(MovieTitle);
  Response := _http.Get;
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

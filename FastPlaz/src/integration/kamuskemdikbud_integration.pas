{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
{
  // USAGE:

  kamus := TKamusIntegration.Create;
  kamus.Token := 'ibacortoken';
  Result := kamus.Find('gadis');
  kamus.Free;

}
unit kamuskemdikbud_integration;

{$mode objfpc}{$H+}

interface

uses
  common, http_lib,
  fpjson, cthreads, Classes, SysUtils;

type

  { TKamusIntegration }

  TKamusIntegration = class(TInterfacedObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; virtual;
    function Find(Text: string): string;
  end;



implementation

const
  _KAMUS_URL = 'http://kbbi.kemdikbud.go.id/entri/';
  __TAG_START = '\<([\.\$A-Za-z0-9=_ "]+)\>';
  __TAG_END = '\</([\.\$A-Za-z0-9=_ "]+)\>';


var
  Response: IHTTPResponse;

{ TKamusIntegration }

constructor TKamusIntegration.Create;
begin
end;

destructor TKamusIntegration.Destroy;
begin

end;

function TKamusIntegration.Find(Text: string): string;
var
  s, return, urlTarget: string;
  httpClient: THTTPLib;
  jsonData: TJSONData;
begin
  Result := '';
  Text := trim(LowerCase(Text));
  if Text = '' then
    Exit;

  urlTarget := _KAMUS_URL + Text;
  httpClient := THTTPLib.Create;
  httpClient.URL := urlTarget;
  Response := httpClient.Get;
  httpClient.Free;

  if Response.ResultCode <> 200 then
    Exit;

  return := copy(Response.ResultText, pos('<ol class="">', Response.ResultText) + 13);
  return := copy(return, 1, pos('</ol>', return) - 1);

  s := Format('%s(.*?)%s', [__TAG_START, __TAG_END]);
  //return:= preg_replace( __TAG_END, '', return, True);

  return := StringReplace(return, '<li>', '- ', [rfReplaceAll]);
  return := StringReplace(return, '</li>', '\n', [rfReplaceAll]);
  return := preg_replace(s, '', return, True);
  return := StringReplace(return, '</i>', '', [rfReplaceAll]);
  return := StringReplace(return, '</font>', '', [rfReplaceAll]);
  return := StringReplace(return, '   ', '', [rfReplaceAll]);
  return := StringReplace(return, '  ', '', [rfReplaceAll]);
  return := StringReplace(return, #13, '', [rfReplaceAll]);
  return := StringReplace(return, #10, '', [rfReplaceAll]);
  //return := StringReplace(return, '\n', #13, [rfReplaceAll]);

  return := trim(return);
  Result := return;
end;

end.




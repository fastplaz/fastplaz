unit kamussunda_integration;
{
  Kamus Sunda
  http://kamus-sunda.com/

  [x] USAGE
  with TKamusSundaIntegration.Create do
  begin

    // Indonesian to Sunda
    Result := TranslateIndonesianToSunda('bagaimana kabarmu');

    // Sunda to Indonesian
    Result := TranslateSundaToIndonesian('kumaha wartos anjeun');

    Free;
  end;

}

{$mode objfpc}{$H+}

interface

uses
  common, json_lib, http_lib, logutil_lib,
  Classes, SysUtils;

type

  { TKamusSundaIntegration }

  TKamusSundaIntegration = class(TInterfacedObject)
  private
    FResultCode: integer;
    FResultText: string;
    FStyle: string;

    function getHTML(AText: string): string;
    function getResult(AHTML: string): string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property Style: string read FStyle write FStyle;

    function TranslateIndonesianToSunda(AText: string): string;
    function TranslateSundaToIndonesian(AText: string): string;
  end;

implementation

const
  _KAMUSSUNDA_URL = 'http://kamus-sunda.com/penerjemah.html';

var
  Response: IHTTPResponse;

{ TKamusSundaIntegration }

function TKamusSundaIntegration.getHTML(AText: string): string;
begin
  Result := '';
  with THTTPLib.Create(_KAMUSSUNDA_URL) do
  begin
    AddHeader('Cache-Control', 'no-cache');
    FormData['text'] := AText;
    FormData['md'] := FStyle;
    Response := Post;
    FResultCode := Response.ResultCode;
    FResultText := Response.ResultText;
    if FResultCode = 200 then
    begin
      Result := FResultText;
    end;
    Free;
  end;
end;

function TKamusSundaIntegration.getResult(AHTML: string): string;
var
  s: string;
begin
  s := '<!-- post hasil -->';
  Result := copy(AHTML, Pos(s, AHTML) + Length(s));

  s := '<div class=''result3''>';
  Result := copy(Result, Pos(s, Result) + Length(s));
  Result := Copy(Result, 0, Pos('</div>', Result)-1);
end;

constructor TKamusSundaIntegration.Create;
begin
  FStyle := 'ish';
end;

destructor TKamusSundaIntegration.Destroy;
begin

end;

function TKamusSundaIntegration.TranslateIndonesianToSunda(AText: string
  ): string;
begin
  FStyle := 'ish';
  Result := getHTML(AText);
  Result := getResult(Result);
end;

function TKamusSundaIntegration.TranslateSundaToIndonesian(AText: string
  ): string;
begin
  FStyle := 'sih';
  Result := getHTML(AText);
  Result := getResult(Result);
end;

end.

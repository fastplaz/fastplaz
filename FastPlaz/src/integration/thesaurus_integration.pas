unit thesaurus_integration;

{
  // USAGE:


}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  common,
  Classes, SysUtils;

{$ifdef THESAURUS_INTEGRATION}
{$endif}

type

  { TThesaurusIntegration }

  TThesaurusIntegration = class(TInterfacedObject)
  private
    FData: TStringList;
    FMultiResult: Boolean;
    FPathName: String;
  public
    constructor Create;
    destructor Destroy;
    function Find(AText: string): string;
  published
    property PathName: String read FPathName write FPathName;
    property MultiResult: Boolean read FMultiResult write FMultiResult;
  end;

implementation

const
  THESAURUS_PATH = 'files/thesaurus/';
  THESAURUS_EXTENSION = '.txt';

{ TThesaurusIntegration }

constructor TThesaurusIntegration.Create;
begin
  FPathName := THESAURUS_PATH;
  FMultiResult := True;
end;

destructor TThesaurusIntegration.Destroy;
begin
  if Assigned(FData) then
    FData.Free;
end;

function TThesaurusIntegration.Find(AText: string): string;
var
  s, LFileName, LSearch, LRow: String;
  i: Integer;
begin
  Result := '';
  LFileName := UpperCase(Copy(Trim(AText),1,1));
  LFileName := FPathName + LFileName + THESAURUS_EXTENSION;
  if not FileExists( LFileName) then
    Exit;;

  if not Assigned(FData) then
    FData := TStringList.Create;
  FData.LoadFromFile( LFileName);

  LSearch := Trim(AText)+' ';
  for i:=0 to FData.Count-1 do
  begin
    LRow := FData[i];
    if Pos( LSearch, LRow) = 1 then
    begin
      LRow := StringReplace( LRow, ';', ';'#13, [rfReplaceAll]);
      Result := Result + LRow + #13;
      if not FMultiResult then
        Break;
      Result := Result + #13;
    end;
  end;
  Result := Trim( Result);
end;



end.



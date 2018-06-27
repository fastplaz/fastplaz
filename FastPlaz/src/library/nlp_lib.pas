unit nlp_lib;

{
  // USAGE:

  [x] Sentences Similiraty
  NLP := TNLP.Create;
  Score := NLP.Similarity('your sentences', 'reference sentences');
  .
  .
  NLP.Free;

}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  common,
  Classes, SysUtils;

{$ifdef NLP_LIB}
{$endif}

type

  TNLPSimilarityMethod = (smNone, smCosinus);

  { TNLP }

  TNLP = class(TInterfacedObject)
  private
  public
    constructor Create;
    destructor Destroy;
    function Similarity(AText, AReference: string;
      AMethod: TNLPSimilarityMethod = smCosinus): double;
    function IsStopWords(AWord: string): boolean;
  published
  end;

implementation

const
  STOPWORDS: array[0..3] of string =
    (
    'di', 'yang', 'ke', 'pada'
    );

{ TNLP }

constructor TNLP.Create;
begin
end;

destructor TNLP.Destroy;
begin
end;

function TNLP.Similarity(AText, AReference: string;
  AMethod: TNLPSimilarityMethod): double;
var
  i: integer;
  LOutput: double;
  s, LText, LReference: string;
  LTextA, LReferenceA: TStrings;
begin
  Result := 0;
  LOutput := 0;
  LText := LowerCase(AText);
  LReference := LowerCase(AReference);
  LTextA := Explode(LText, ' ');
  LReferenceA := Explode(LReference, ' ');
  for i := 0 to LTextA.Count - 1 do
  begin
    s := LTextA[i];
    if IsStopWords(s) then
      Continue;
    if LReferenceA.IndexOf(s) <> -1 then
      LOutput := LOutput + 1;
  end;

  Result := LOutput / sqrt(LTextA.Count * LReferenceA.Count);

  LReferenceA.Free;
  LTextA.Free;
end;

function TNLP.IsStopWords(AWord: string): boolean;
begin
  Result := False;
  if StrInArray(AWord, STOPWORDS) then
    Result := True;
end;


end.

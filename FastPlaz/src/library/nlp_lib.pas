unit nlp_lib;

{
  // USAGE:

  [x] Sentences Similiraty
  NLP := TNLP.Create;
  NLP.StopWords.LoadFromFile('your/stopword_file');
  Score := NLP.Similarity('your sentences', 'reference sentences');
  .
  . Score is between 0 and 1
  .
  NLP.Free;

}

{$mode objfpc}{$H+}
{ $ include ../../define.inc}

interface

uses
  common,
  Classes, SysUtils;

type

  TNLPSimilarityMethod = (smNone, smCosine);

  { TNLP }

  TNLP = class(TInterfacedObject)
  private
    FStopWords: TStringList;
    function similarityCosine(AText, AReference: string): double;
  public
    constructor Create;
    destructor Destroy;
    function Similarity(AText, AReference: string;
      AMethod: TNLPSimilarityMethod = smCosine): double;
    function IsStopWords(AWord: string): boolean;
  published
    property StopWords: TStringList read FStopWords write FStopWords;
  end;

implementation

const
  STOPWORDS_LOCAL: array[0..5] of string =
    (
    'di', 'yang', 'ke', 'pada', 'jika', 'seperti'
    );

{ TNLP }

constructor TNLP.Create;
begin
  FStopWords := TStringList.Create;
end;

destructor TNLP.Destroy;
begin
  FStopWords.Free;
end;

function TNLP.similarityCosine(AText, AReference: string): double;
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

  LText := preg_replace('[^a-z ]', '', LText);
  LReference := preg_replace('[^a-z ]', '', LReference);

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

function TNLP.Similarity(AText, AReference: string;
  AMethod: TNLPSimilarityMethod): double;
begin
  Result := 0;
  if AMethod = smCosine then
    Result := similarityCosine(AText, AReference);
end;

function TNLP.IsStopWords(AWord: string): boolean;
begin
  Result := False;
  if StrInArray(AWord, STOPWORDS_LOCAL) then
    Result := True;
  if FStopWords.IndexOf(AWord) <> -1 then
    Result := True;
end;


end.

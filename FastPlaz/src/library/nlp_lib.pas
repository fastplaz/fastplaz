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

  Similarity is the cosine similarity of the two texts seen as sets of
  distinct terms (letters only, lower case, stop words removed):
  |A and B| / sqrt(|A| * |B|). Identical term sets score 1, texts without
  a common term (or without any term at all) score 0.
}

{$mode objfpc}{$H+}

interface

uses
  common,
  Classes, SysUtils;

type

  TNLPSimilarityMethod = (smNone, smCosine);

  { TNLP }

  TNLP = class
  private
    FStopWords: TStringList;
    procedure SetStopWords(AValue: TStringList);
    function similarityCosine(AText, AReference: string): double;
    function Terms(const AText: string): TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function Similarity(AText, AReference: string;
      AMethod: TNLPSimilarityMethod = smCosine): double;
    function IsStopWords(AWord: string): boolean;
  published
    // assigning copies the list; TNLP keeps owning its own
    property StopWords: TStringList read FStopWords write SetStopWords;
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
  inherited Create;
  FStopWords := TStringList.Create;
end;

destructor TNLP.Destroy;
begin
  FStopWords.Free;
  inherited Destroy;
end;

procedure TNLP.SetStopWords(AValue: TStringList);
begin
  if (AValue = nil) or (AValue = FStopWords) then
    Exit;
  FStopWords.Assign(AValue);
end;

// distinct, non-empty, non-stop-word terms of AText
function TNLP.Terms(const AText: string): TStringList;
var
  tokens: TStrings;
  i: integer;
  s: string;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  tokens := Explode(preg_replace('[^a-z ]', '', LowerCase(AText)), ' ');
  try
    for i := 0 to tokens.Count - 1 do
    begin
      s := Trim(tokens[i]);
      if (s = '') or IsStopWords(s) then
        Continue;
      Result.Add(s);
    end;
  finally
    tokens.Free;
  end;
end;

function TNLP.similarityCosine(AText, AReference: string): double;
var
  i, common: integer;
  LTextA, LReferenceA: TStringList;
begin
  Result := 0;
  LTextA := Terms(AText);
  LReferenceA := Terms(AReference);
  try
    // an empty side used to divide by zero
    if (LTextA.Count = 0) or (LReferenceA.Count = 0) then
      Exit;
    common := 0;
    for i := 0 to LTextA.Count - 1 do
      if LReferenceA.IndexOf(LTextA[i]) <> -1 then
        Inc(common);
    Result := common / sqrt(LTextA.Count * LReferenceA.Count);
  finally
    LReferenceA.Free;
    LTextA.Free;
  end;
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

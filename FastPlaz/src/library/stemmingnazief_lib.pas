unit stemmingnazief_lib;

{$mode objfpc}{$H+}

{

  Stemming Word based on Nazief & Adriani Method
  include more than 28.000 word in indonesia

  [x] USAGE

  // stemming word
  Stemming := TStemmingNazief.Create;
  Stemming.LoadDictionaryFromFile('dictionary.csv');
  StringVariable := Stemming.ParseWord( 'mencintai');

  // sentences
  // result in json
  Stemming := TStemmingNazief.Create;
  Stemming.LoadDictionaryFromFile('dictionary.csv');
  StringVariable := Stemming.ParseSentence( 'kalimat dalam bahasa indonesia');

  // non-standard-word check (kata baku)
  .
  Stemming.StandardWordCheck := True;
  Stemming.LoadNonStandardWordFromFile('nonstandardword.txt');
  .

  [x] Word Type
    0 -
    1 Adjektiva
    2 Nomina
    3 Pronomina
    4 Verba
    5 Adverbia
    6 Numeralia
    7 Interjeksi
    8 Konjungsi
    9 Preposisi
   10 partikel

}


interface

uses
  common, IniFiles, fpjson,
  RegExpr, Classes, SysUtils;

const
  WORD_NONSTANDARD_FILE = 'word-nonstandard.txt';
  STEMMINGNAZIEF_DICTIONARY_FILE = 'dictionary.csv';
  STEMMINGNAZIEF_WORDTYPE: array [0..10] of string =
    ('-', 'Adjektiva', 'Nomina', 'Pronomina', 'Verba', 'Adverbia',
    'Numeralia', 'Interjeksi', 'Konjungsi', 'Preposisi', 'Partikel');

type

  { TStemmingNazief }

  TStemmingNazief = class
  private
    FDictionary: TStringList;
    FDictionaryFile: string;
    FIsDictionaryLoaded: boolean;
    FNonStandardWordCount: integer;
    FStandardWordFile: string;
    FStemmedText: string;
    FUnknownWordCount: integer;
    FWordCount: integer;
    FWordType: integer;
    FWordTypeInString: string;
    FStandardWordCheck: boolean;
    FNonStandardWord: TIniFile;
    FParseWord_nonstandard: boolean;
    FDepth: integer; // ParseWord recursion depth

    function GetStandardWordCheck: boolean;
    procedure SetStandardWordCheck(AValue: boolean);
    function _exist(Text: string; SkipCheck: boolean = False): boolean;
    function _delInflectionSuffixes(Text: string): string;
    function _delDerivationSuffixes(Text: string): string;
    function _delDerivationPrefix(Text: string): string;
    function _pluralWord(Text: string): string;
    function _additionalRule(Text: string): string;
    function Explode(Str, Delimiter: string): TStrings;

    // regex
    function preg_match(const RegexExpression: string; SourceString: string): boolean;
    function preg_replace(const RegexExpression, ReplaceString, SourceString: string;
      UseSubstitution: boolean): string;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadDictionaryFromFile(FileName: string = STEMMINGNAZIEF_DICTIONARY_FILE);
      virtual;
    procedure LoadNonStandardWordFromFile(AFileName: string = WORD_NONSTANDARD_FILE);
      virtual;

    function ParseWord(Text: string): string;
    function ParseSentence(Text: string): string;
  published
    property WordType: integer read FWordType;
    property WordTypeInString: string read FWordTypeInString;

    property Dictionary: TStringList read FDictionary;
    property DictionaryFile: string read FDictionaryFile write FDictionaryFile;
    property NonStandardWord: TIniFile read FNonStandardWord;
    property NonStandardWordFile: string read FStandardWordFile write FStandardWordFile;
    property IsDictionaryLoaded: boolean read FIsDictionaryLoaded
      write FIsDictionaryLoaded;

    property Text: string read FStemmedText;
    property WordCount: integer read FWordCount;
    property NonStandardWordCount: integer read FNonStandardWordCount;
    property UnknownWordCount: integer read FUnknownWordCount;
    property StandardWordCheck: boolean read GetStandardWordCheck
      write SetStandardWordCheck;
  end;

implementation

{ TStemmingNazief }

constructor TStemmingNazief.Create;
begin
  FIsDictionaryLoaded := False;
  DictionaryFile := STEMMINGNAZIEF_DICTIONARY_FILE;

  // hashed lookups: IndexOfName on a plain TStringList is a linear scan
  // over the 28k dictionary entries, done 10-30 times per word
  FDictionary := THashedStringList.Create;
  Dictionary.NameValueSeparator := ',';
  FNonStandardWord := nil;
  FWordType := 0;
  FWordTypeInString := '';
  FStemmedText := '';
  FStandardWordCheck := False;
  FNonStandardWordCount := 0;
  FDepth := 0;
end;

destructor TStemmingNazief.Destroy;
begin
  FDictionary.Free;
  FNonStandardWord.Free;
  inherited Destroy;
end;


function TStemmingNazief._exist(Text: string; SkipCheck: boolean): boolean;
begin
  Result := False;
  FWordType := 0;
  FWordTypeInString := '-';
  if Text = '' then
    Exit;

  if Dictionary.IndexOfName(Text) = -1 then
    Exit;

  if TryStrToInt(Trim(Dictionary.Values[Text]), FWordType) and
    (FWordType >= Low(STEMMINGNAZIEF_WORDTYPE)) and
    (FWordType <= High(STEMMINGNAZIEF_WORDTYPE)) then
    FWordTypeInString := STEMMINGNAZIEF_WORDTYPE[FWordType]
  else
    FWordType := 0;
  Result := True;

  {
  try
    with TRegExpr.Create do
    begin
      //Expression := '^.*(' + Text + '),(.).*';
      Expression := '^.*(' + Text + '),(.)\s';
      if Exec(Dictionary.Text) then
      begin
        Result := True;
        FWordType := StrToInt(Match[2]);
        FWordTypeInString := STEMMINGNAZIEF_WORDTYPE[FWordType];
      end;
      Free;
    end;
  except
    Result := False;
  end;
  }

end;

function TStemmingNazief.GetStandardWordCheck: boolean;
begin
  Result := FStandardWordCheck;
end;

procedure TStemmingNazief.SetStandardWordCheck(AValue: boolean);
begin
  FStandardWordCheck := AValue;

end;

function TStemmingNazief.preg_match(const RegexExpression: string;
  SourceString: string): boolean;
var
  re: TRegExpr;
begin
  Result := False;
  re := TRegExpr.Create;
  try
    try
      re.Expression := RegexExpression;
      Result := re.Exec(SourceString);
    except
    end;
  finally
    re.Free;
  end;
end;

function TStemmingNazief.preg_replace(
  const RegexExpression, ReplaceString, SourceString: string;
  UseSubstitution: boolean): string;
var
  re: TRegExpr;
begin
  Result := SourceString;
  re := TRegExpr.Create;
  try
    try
      re.Expression := RegexExpression;
      Result := re.Replace(SourceString, ReplaceString, UseSubstitution);
    except
      Result := SourceString;
    end;
  finally
    re.Free;
  end;
end;

procedure TStemmingNazief.LoadDictionaryFromFile(FileName: string);
begin
  if not FIsDictionaryLoaded then
  begin
    if FileExists(FileName) then
    begin
      FDictionary.LoadFromFile(FileName);
      FDictionary.NameValueSeparator := ',';
      // skip the CSV header ("word,type_id"); it is not a word
      if (FDictionary.Count > 0) and
        not TryStrToInt(Trim(FDictionary.ValueFromIndex[0]), FWordType) then
        FDictionary.Delete(0);
      FWordType := 0;
      FDictionaryFile := FileName;
      FIsDictionaryLoaded := True;
    end;
  end;
end;

procedure TStemmingNazief.LoadNonStandardWordFromFile(AFileName: string);
begin
  if not FileExists(AFileName) then
    Exit;
  if not Assigned(FNonStandardWord) then
    FNonStandardWord := TIniFile.Create(AFileName);
end;

// #2. Remove Infection suffixes (-lah, -kah, -ku, -mu, -tah, -nya)
function TStemmingNazief._delInflectionSuffixes(Text: string): string;
begin
  Result := Text;
  if preg_match('([km]u|nya|[klt]ah|pun)\Z', Text) then
  begin
    if Result = 'pelaku' then
      Exit;
    ;
    if preg_match('(sekolah)\Z', Text) then // except: sekolah
    begin
      Exit;
    end;
    Result := preg_replace('([km]u|nya|[klt]ah|pun)\Z', '', Text, True);

    // pelangganmukah, pelakunyalah
    if preg_match('([km]u|nya)\Z', Result) then
    begin
      if Result = 'buku' then
        Exit;
      Result := preg_replace('([km]u|nya)\Z', '', Result, True);
    end;
  end;
end;

//#3 - Remove Derivation suffix (-i, -an or -kan (-in))
function TStemmingNazief._delDerivationSuffixes(Text: string): string;
begin
  Result := Text;
  if preg_match('(i|an|in)\Z', Text) then
  begin

    Result := preg_replace('(i|an|in)\Z', '', Text, True);
    if _exist(Result) then
    begin
      Exit;
    end;
    if preg_match('(kan)\Z', Text) then
    begin
      Result := preg_replace('(kan)\Z', '', Text, True);
      if _exist(Result) then
        Exit;
    end;

    // todo: jk tidak ditemukan di kamus
    Result := Text;
  end;
  //Result := Text;
end;

//#4 - Remove Derivation prefix (di-, ke-, se-, te-, be-, me-, or pe-)
function TStemmingNazief._delDerivationPrefix(Text: string): string;
begin
  Result := Text;

  // prefix: di-,ke-,se-
  if preg_match('^(di|[ks]e)', Text) then
  begin
    Result := preg_replace('^(di|[ks]e)', '', Text, True);
    if _exist(Result) then
      Exit;
    Result := _delDerivationSuffixes(Result);
    if _exist(Result) then
      Exit;
    Result := _delDerivationPrefix(Result);
    if _exist(Result) then
      Exit;

    // prefix: diper-
    if preg_match('^(diper)', Text) then
    begin
      Result := preg_replace('^(diper)', '', Text, True);
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
    end;

    // prefix: keber- dan keter-
    if preg_match('^(ke[bt]er)', Text) then
    begin
      Result := preg_replace('^(ke[bt]er)', '', Text, True);
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
    end;

  end; // if preg_match('^(di|[ks]e)', Text)


  // prefix: te-,ter-, be-, ber-
  if preg_match('^([bt]e)', Text) then
  begin
    Result := preg_replace('^([bt]e)', '', Text, True);
    if _exist(Result) then
      Exit;
    Result := preg_replace('^([bt]e[lr])', '', Text, True);
    if _exist(Result) then
      Exit;
    Result := _delDerivationSuffixes(Result);
    if _exist(Result) then
      Exit;

    // berpelanggan, berpengalaman
    // only recurse when a prefix was really removed, otherwise ParseWord
    // calls back into here with the same word forever (bento, tesla, ...)
    if Length(Result) < Length(Text) then
    begin
      Result := ParseWord(Result);
      if _exist(Result) then
        Exit;
    end;
  end;

  // prefix: me- pe-
  if preg_match('^([mp]e)', Text) then
  begin
    Result := preg_replace('^([mp]e)', '', Text, True);
    if _exist(Result) then
      Exit;
    Result := _delDerivationSuffixes(Result);
    if _exist(Result) then
      Exit;

    // memperbarui, mempelajari
    if preg_match('^(mempe[lr])', Text) then
    begin
      Result := preg_replace('^(mempe[lr])', '', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
    end;

    if preg_match('^([mp]eng)', Text) then
    begin
      Result := preg_replace('^([mp]eng)', '', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
      if Result = 'ebom' then //except: pengebom, mengebom
      begin
        Result := 'bom';
        Exit;
      end;

      Result := preg_replace('^([mp]eng)', 'k', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
    end;

    if preg_match('^([mp]eny)', Text) then
    begin
      Result := preg_replace('^([mp]eny)', 's', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
    end;

    if preg_match('^([mp]e[lr])', Text) then
    begin
      Result := preg_replace('^([mp]e[lr])', '', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
    end;

    if preg_match('^([mp]en)', Text) then
    begin
      Result := preg_replace('^([mp]en)', 't', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
      Result := preg_replace('^([mp]en)', '', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
    end;

    if preg_match('^([mp]em)', Text) then
    begin
      Result := preg_replace('^([mp]em)', '', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
      Result := preg_replace('^([mp]em)', 'p', Text, True);
      if _exist(Result) then
        Exit;
      Result := _delDerivationSuffixes(Result);
      if _exist(Result) then
        Exit;
    end;

  end; // if preg_match('^([mp]e)', Text)

end;

function TStemmingNazief._pluralWord(Text: string): string;
var
  str: TStrings;
begin
  Result := Text;

  // buku-bukunya
  if preg_match('([km]u|nya|[klt]ah|pun)\Z', Text) then
  begin
    Result := preg_replace('([km]u|nya|[klt]ah|pun)\Z', '', Text, True);
  end;

  // berbalas-balasan -> balas
  // meniru-nirukan masih salah
  // bersenang-senang

  str := Explode(Result, '-');
  Result := str[0];
  str.Free;
end;

function TStemmingNazief._additionalRule(Text: string): string;
begin
  Result := '';

  // kupukul, kaupukul, kupadamkan
  if preg_match('^(ku|kau)', Text) then
  begin
    Result := preg_replace('^(ku|kau)', '', Text, True);
    Result := ParseWord(Result);
    if FWordType = 0 then
      Result := '';
  end;

end;

function TStemmingNazief.Explode(Str, Delimiter: string): TStrings;
var
  i: integer;
  p: string;
begin
  Result := TStringList.Create;
  while Pos(Delimiter, Str) <> 0 do
  begin
    p := '';
    for i := 1 to Pos(Delimiter, Str) - 1 do
      p := p + Str[i];
    Result.Add(p);
    //Delete(s,1,Pos(Delimiter,Str));
    Delete(Str, 1, Pos(Delimiter, Str) + Length(Delimiter) - 1);
  end;
  //result.Add(s);
  if (Length(Str) <> 0) then
    Result.Add(Str);
end;


function TStemmingNazief.ParseWord(Text: string): string;
const
  MAX_DEPTH = 16;
var
  s: string;
begin
  Text := LowerCase(Text);
  Text := ReplaceAll(Text, [' ', ',', '?', '!', '.', '''', '+', '^',
    '"', #13, #10, '/', '\', '(', ')', '[', ']', '*', '$', '!'], '');

  Result := Text;

  if not IsDictionaryLoaded then
    LoadDictionaryFromFile(FDictionaryFile);
  if not IsDictionaryLoaded then
  begin
    Result := '?';
    Exit;
  end;

  // the prefix/plural rules call ParseWord recursively; a bounded depth
  // guarantees termination whatever the input
  if FDepth >= MAX_DEPTH then
    Exit;
  Inc(FDepth);
  try
    // the non-standard flag belongs to the top level call; nested calls
    // must not clear what the outer word already set
    if FDepth = 1 then
      FParseWord_nonstandard := False;
    if FStandardWordCheck and Assigned(FNonStandardWord) then
    begin
      s := FNonStandardWord.ReadString('base', Text, Text);
      if s <> Text then
      begin
        Text := s;
        Result := s;
        Inc(FNonStandardWordCount);
        FParseWord_nonstandard := True;
      end;
    end;

    if _exist(Text) then
      Exit;

    // if number (',' and '.' were stripped above)
    if preg_match('^-?[0-9]+$', Text) then
    begin
      FWordType := 6;
      FWordTypeInString := STEMMINGNAZIEF_WORDTYPE[6];
      Exit;
    end;

    // if pluralWord
    if preg_match('^(.*)-(.*)$', Text) then
    begin
      Result := _pluralWord(Text);
      if Length(Result) < Length(Text) then
        Result := ParseWord(Result);
      if _exist(Result) then
        Exit;
      Result := Text;
    end;

    Result := _additionalRule(Result);
    if not (Result = '') then
      Exit;

    //#2 - Remove Infection suffixes (-lah, -kah, -ku, -mu, or -nya)
    Result := _delInflectionSuffixes(Text);
    if _exist(Result) then
      Exit;

    //#3 - Remove Derivation suffix (-i, -an or -kan)
    Result := _delDerivationSuffixes(Result);
    if _exist(Result) then
      Exit;

    //#4 - Remove Derivation prefix (di-, ke-, se-, te-, be-, me-, or pe-)
    Result := _delDerivationPrefix(Result);
  finally
    Dec(FDepth);
  end;
end;

function TStemmingNazief.ParseSentence(Text: string): string;
var
  original_word, word: string;
  str: TStrings;
  items: TJSONArray;
  item: TJSONObject;
  i: integer;
begin
  FStemmedText := '';
  FWordCount := 0;
  FNonStandardWordCount := 0;
  FUnknownWordCount := 0;
  str := Explode(Text, ' ');
  items := TJSONArray.Create;
  try
    for i := 0 to str.Count - 1 do
    begin
      original_word := str[i];
      if Trim(original_word) = '' then
        Continue;
      Inc(FWordCount);
      word := ParseWord(original_word);
      item := TJSONObject.Create;
      items.Add(item);
      item.Add('word', word);
      item.Add('wordtype', WordTypeInString);
      item.Add('type', IntToStr(WordType));
      if FParseWord_nonstandard then
      begin
        item.Add('non_standard', 'yes');
        item.Add('original_word', original_word);
      end;
      if WordType = 0 then
        Inc(FUnknownWordCount);
      item.Add('score', '0');
      FStemmedText := FStemmedText + word + ' ';
    end;
    FStemmedText := Trim(FStemmedText);
    Result := items.AsJSON;
  finally
    items.Free;
    str.Free;
  end;
end;


end.

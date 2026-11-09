unit test_stemmingnazief_lib;

{$mode objfpc}{$H+}

interface

uses
  stemmingnazief_lib, fpjson, jsonparser, Classes, SysUtils, fpcunit, testregistry;

type

  { TStemmingNaziefTest }

  TStemmingNaziefTest = class(TTestCase)
  private
    FDir: string;
    FDictFile: string;
    FNonStdFile: string;
    FStem: TStemmingNazief;
    procedure WriteFile(const AName, AContent: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDictionaryLoaded;
    procedure TestHeaderLineSkipped;
    procedure TestBaseWord;
    procedure TestWordTypeOutOfRange;
    procedure TestNumber;
    procedure TestNotANumber;
    procedure TestPrefixSuffix;
    procedure TestInflectionSuffix;
    procedure TestPlural;
    procedure TestKuKau;
    procedure TestUnknownPrefixWordTerminates;
    procedure TestNoDictionary;
    procedure TestNonStandardWord;
    procedure TestNonStandardFlagSurvivesNestedCall;
    procedure TestStandardCheckWithoutFileIsHarmless;
    procedure TestSentenceIsValidJson;
    procedure TestSentenceEscapesOriginalWord;
    procedure TestSentenceCounts;
    procedure TestSentenceSkipsEmptyTokens;
  end;

implementation

const
  DICT = 'word,type_id' + LineEnding +
    'cinta,4' + LineEnding +
    'main,4' + LineEnding +
    'buku,2' + LineEnding +
    'teman,2' + LineEnding +
    'pukul,4' + LineEnding +
    'sekolah,2' + LineEnding +
    'makan,4' + LineEnding +
    'saya,3' + LineEnding +
    'zzz,99' + LineEnding;

  NONSTD = '[base]' + LineEnding +
    'temen=teman' + LineEnding +
    'temen-temen=teman-teman' + LineEnding;

procedure TStemmingNaziefTest.WriteFile(const AName, AContent: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FDir + AName, fmCreate);
  try
    if AContent <> '' then
      f.WriteBuffer(AContent[1], Length(AContent));
  finally
    f.Free;
  end;
end;

procedure TStemmingNaziefTest.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_stem_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FDictFile := FDir + 'dictionary.csv';
  FNonStdFile := FDir + 'nonstandard.txt';
  WriteFile('dictionary.csv', DICT);
  WriteFile('nonstandard.txt', NONSTD);
  FStem := TStemmingNazief.Create;
  FStem.LoadDictionaryFromFile(FDictFile);
end;

procedure TStemmingNaziefTest.TearDown;
var
  rec: TSearchRec;
begin
  FStem.Free;
  if FindFirst(FDir + '*', faAnyFile, rec) = 0 then
  begin
    repeat
      if (rec.Attr and faDirectory) = 0 then
        DeleteFile(FDir + rec.Name);
    until FindNext(rec) <> 0;
    FindClose(rec);
  end;
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
end;

procedure TStemmingNaziefTest.TestDictionaryLoaded;
begin
  AssertTrue(FStem.IsDictionaryLoaded);
  AssertEquals(FDictFile, FStem.DictionaryFile);
  AssertEquals(9, FStem.Dictionary.Count);
end;

procedure TStemmingNaziefTest.TestHeaderLineSkipped;
begin
  // "word,type_id" used to be loaded as a dictionary entry
  AssertEquals(-1, FStem.Dictionary.IndexOfName('word'));
  AssertEquals('word', FStem.ParseWord('word'));
  AssertEquals(0, FStem.WordType);
end;

procedure TStemmingNaziefTest.TestBaseWord;
begin
  AssertEquals('cinta', FStem.ParseWord('Cinta'));
  AssertEquals(4, FStem.WordType);
  AssertEquals('Verba', FStem.WordTypeInString);
  AssertEquals('buku', FStem.ParseWord('buku.'));
  AssertEquals(2, FStem.WordType);
end;

procedure TStemmingNaziefTest.TestWordTypeOutOfRange;
begin
  AssertEquals('zzz', FStem.ParseWord('zzz'));
  AssertEquals(0, FStem.WordType);
  AssertEquals('-', FStem.WordTypeInString);
end;

procedure TStemmingNaziefTest.TestNumber;
begin
  AssertEquals('12345', FStem.ParseWord('12345'));
  AssertEquals(6, FStem.WordType);
  AssertEquals('Numeralia', FStem.WordTypeInString);
  AssertEquals('35', FStem.ParseWord('3,5'));
  AssertEquals(6, FStem.WordType);
end;

procedure TStemmingNaziefTest.TestNotANumber;
begin
  // StrToFloat accepted these as numbers
  FStem.ParseWord('inf');
  AssertEquals(0, FStem.WordType);
  FStem.ParseWord('nan');
  AssertEquals(0, FStem.WordType);
  FStem.ParseWord('1e5');
  AssertEquals(0, FStem.WordType);
end;

procedure TStemmingNaziefTest.TestPrefixSuffix;
begin
  AssertEquals('cinta', FStem.ParseWord('mencintai'));
  AssertEquals(4, FStem.WordType);
  AssertEquals('main', FStem.ParseWord('bermain'));
  AssertEquals('makan', FStem.ParseWord('dimakan'));
  AssertEquals('pukul', FStem.ParseWord('memukul'));
  AssertEquals('buku', FStem.ParseWord('bukunya'));
end;

procedure TStemmingNaziefTest.TestInflectionSuffix;
begin
  AssertEquals('sekolah', FStem.ParseWord('sekolahku'));
  AssertEquals('sekolah', FStem.ParseWord('sekolah'));
  AssertEquals('teman', FStem.ParseWord('temanmulah'));
end;

procedure TStemmingNaziefTest.TestPlural;
begin
  AssertEquals('buku', FStem.ParseWord('buku-bukunya'));
  AssertEquals(2, FStem.WordType);
  AssertEquals('teman', FStem.ParseWord('teman-teman'));
end;

procedure TStemmingNaziefTest.TestKuKau;
begin
  AssertEquals('pukul', FStem.ParseWord('kupukul'));
  AssertEquals(4, FStem.WordType);
  AssertEquals('pukul', FStem.ParseWord('kaupukul'));
end;

procedure TStemmingNaziefTest.TestUnknownPrefixWordTerminates;
begin
  // be-/te- words outside the dictionary used to recurse forever
  AssertEquals('bento', FStem.ParseWord('bento'));
  AssertEquals(0, FStem.WordType);
  AssertEquals('tesla', FStem.ParseWord('tesla'));
  AssertEquals('teflon', FStem.ParseWord('teflon'));
  // the stripped form comes back when nothing matches; what matters is
  // that these return at all
  AssertEquals('bento', FStem.ParseWord('sebento'));
  AssertEquals(0, FStem.WordType);
  AssertTrue(FStem.ParseWord('kubento') <> '');
  AssertTrue(FStem.ParseWord('diberterbentokan') <> '');
  AssertEquals('', FStem.ParseWord(''));
  AssertEquals('be', FStem.ParseWord('be'));
end;

procedure TStemmingNaziefTest.TestNoDictionary;
var
  s: TStemmingNazief;
begin
  s := TStemmingNazief.Create;
  try
    s.DictionaryFile := FDir + 'missing.csv';
    AssertEquals('?', s.ParseWord('cinta'));
    AssertFalse(s.IsDictionaryLoaded);
  finally
    s.Free;
  end;
end;

procedure TStemmingNaziefTest.TestNonStandardWord;
begin
  FStem.StandardWordCheck := True;
  FStem.LoadNonStandardWordFromFile(FNonStdFile);
  AssertEquals('teman', FStem.ParseWord('temen'));
  AssertEquals(2, FStem.WordType);
end;

procedure TStemmingNaziefTest.TestNonStandardFlagSurvivesNestedCall;
var
  j: TJSONData;
begin
  FStem.StandardWordCheck := True;
  FStem.LoadNonStandardWordFromFile(FNonStdFile);
  // temen-temen -> teman-teman -> (plural, nested ParseWord) teman
  j := GetJSON(FStem.ParseSentence('temen-temen'));
  try
    AssertEquals(1, j.Count);
    AssertEquals('teman', j.Items[0].FindPath('word').AsString);
    AssertNotNull('non_standard present', j.Items[0].FindPath('non_standard'));
    AssertEquals('temen-temen', j.Items[0].FindPath('original_word').AsString);
    AssertEquals(1, FStem.NonStandardWordCount);
  finally
    j.Free;
  end;
end;

procedure TStemmingNaziefTest.TestStandardCheckWithoutFileIsHarmless;
begin
  FStem.StandardWordCheck := True;
  FStem.LoadNonStandardWordFromFile(FDir + 'missing.txt');
  AssertNull(FStem.NonStandardWord);
  AssertEquals('cinta', FStem.ParseWord('mencintai'));
  AssertEquals(0, FStem.NonStandardWordCount);
end;

procedure TStemmingNaziefTest.TestSentenceIsValidJson;
var
  j: TJSONData;
begin
  j := GetJSON(FStem.ParseSentence('saya mencintai buku-bukunya bento'));
  try
    AssertTrue(j.JSONType = jtArray);
    AssertEquals(4, j.Count);
    AssertEquals('saya', j.Items[0].FindPath('word').AsString);
    AssertEquals('Pronomina', j.Items[0].FindPath('wordtype').AsString);
    AssertEquals('3', j.Items[0].FindPath('type').AsString);
    AssertEquals('0', j.Items[0].FindPath('score').AsString);
    AssertEquals('cinta', j.Items[1].FindPath('word').AsString);
    AssertEquals('buku', j.Items[2].FindPath('word').AsString);
    AssertEquals('bento', j.Items[3].FindPath('word').AsString);
    AssertEquals('0', j.Items[3].FindPath('type').AsString);
    AssertNull('no non_standard without check', j.Items[0].FindPath('non_standard'));
  finally
    j.Free;
  end;
  AssertEquals('saya cinta buku bento', FStem.Text);
end;

procedure TStemmingNaziefTest.TestSentenceEscapesOriginalWord;
var
  j: TJSONData;
begin
  FStem.StandardWordCheck := True;
  FStem.LoadNonStandardWordFromFile(FNonStdFile);
  // quotes and backslashes are stripped from the word but kept in
  // original_word, which used to produce invalid JSON
  j := GetJSON(FStem.ParseSentence('te"m\en'));
  try
    AssertEquals('teman', j.Items[0].FindPath('word').AsString);
    AssertEquals('te"m\en', j.Items[0].FindPath('original_word').AsString);
  finally
    j.Free;
  end;
end;

procedure TStemmingNaziefTest.TestSentenceCounts;
begin
  FStem.ParseSentence('saya bento tesla');
  AssertEquals(3, FStem.WordCount);
  AssertEquals(2, FStem.UnknownWordCount);
  AssertEquals(0, FStem.NonStandardWordCount);
end;

procedure TStemmingNaziefTest.TestSentenceSkipsEmptyTokens;
var
  j: TJSONData;
begin
  j := GetJSON(FStem.ParseSentence('  saya   buku '));
  try
    AssertEquals(2, j.Count);
    AssertEquals(2, FStem.WordCount);
  finally
    j.Free;
  end;
end;

initialization
  RegisterTest(TStemmingNaziefTest);

end.

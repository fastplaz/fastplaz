unit test_language_lib;

{$mode objfpc}{$H+}

interface

uses
  language_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { TLanguageLibTest }

  TLanguageLibTest = class(TTestCase)
  private
    FDir, FOldDir, FOldLang, FOldFallback: string;
    procedure WritePO(const ALang, AContent: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadPOFile;
    procedure TestTranslate;
    procedure TestUnknownReturnsInput;
    procedure TestEmptyMsgstrReturnsInput;
    procedure TestCaseSensitive;
    procedure TestEqualsSignInMsgstr;
    procedure TestEscapes;
    procedure TestMultilineEntries;
    procedure TestLastEntryWithoutMsgstr;
    procedure TestNoFileReturnsInput;
    procedure TestFallbackLanguage;
    procedure TestCacheReloadsChangedFile;
    procedure TestSwitchingLanguage;
  end;

implementation

const
  PO_ID =
    '# comment' + LineEnding +
    'msgid ""' + LineEnding +
    'msgstr ""' + LineEnding +
    '"Content-Type: text/plain; charset=UTF-8\n"' + LineEnding +
    LineEnding +
    'msgid "Save"' + LineEnding +
    'msgstr "Simpan"' + LineEnding +
    LineEnding +
    'msgid "save"' + LineEnding +
    'msgstr "simpan"' + LineEnding +
    LineEnding +
    'msgid "Empty"' + LineEnding +
    'msgstr ""' + LineEnding +
    LineEnding +
    'msgid "a=b"' + LineEnding +
    'msgstr "x=y=z"' + LineEnding +
    LineEnding +
    'msgid "Say \"hi\"\n"' + LineEnding +
    'msgstr "Bilang \"hai\"\n"' + LineEnding +
    LineEnding +
    'msgid ""' + LineEnding +
    '"first part "' + LineEnding +
    '"second part"' + LineEnding +
    'msgstr ""' + LineEnding +
    '"bagian satu "' + LineEnding +
    '"bagian dua"' + LineEnding +
    LineEnding +
    'msgid "Dangling"' + LineEnding;

procedure TLanguageLibTest.WritePO(const ALang, AContent: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FDir + 'locale' + PathDelim + 'fastplaz.' + ALang + '.po', fmCreate);
  try
    if AContent <> '' then
      f.WriteBuffer(AContent[1], Length(AContent));
  finally
    f.Free;
  end;
end;

procedure TLanguageLibTest.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_lang_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir + 'locale');
  FOldDir := GetCurrentDir;
  FOldLang := LANG;
  FOldFallback := FallbackLANG;
  // __() resolves locale/ against the working directory
  SetCurrentDir(ExcludeTrailingPathDelimiter(FDir));
  WritePO('id', PO_ID);
  LANG := 'id';
  FallbackLANG := '';
  ClearLanguageCache;
end;

procedure TLanguageLibTest.TearDown;
var
  rec: TSearchRec;
begin
  ClearLanguageCache;
  LANG := FOldLang;
  FallbackLANG := FOldFallback;
  SetCurrentDir(FOldDir);
  if FindFirst(FDir + 'locale' + PathDelim + '*', faAnyFile, rec) = 0 then
  begin
    repeat
      if (rec.Attr and faDirectory) = 0 then
        DeleteFile(FDir + 'locale' + PathDelim + rec.Name);
    until FindNext(rec) <> 0;
    FindClose(rec);
  end;
  RemoveDir(FDir + 'locale');
  RemoveDir(ExcludeTrailingPathDelimiter(FDir));
end;

procedure TLanguageLibTest.TestLoadPOFile;
var
  ids, strs: TStringList;
begin
  ids := TStringList.Create;
  strs := TStringList.Create;
  try
    AssertTrue(LoadPOFile('locale' + PathDelim + 'fastplaz.id.po', ids, strs));
    AssertEquals(ids.Count, strs.Count);
    AssertEquals('header and dangling entry skipped', 6, ids.Count);
    AssertEquals('Save', ids[0]);
    AssertEquals('Simpan', strs[0]);
    AssertFalse(LoadPOFile('locale' + PathDelim + 'missing.po', ids, strs));
    AssertEquals(0, ids.Count);
  finally
    strs.Free;
    ids.Free;
  end;
end;

procedure TLanguageLibTest.TestTranslate;
begin
  AssertEquals('Simpan', __('Save'));
end;

procedure TLanguageLibTest.TestUnknownReturnsInput;
begin
  AssertEquals('Delete', __('Delete'));
  AssertEquals('', __(''));
end;

procedure TLanguageLibTest.TestEmptyMsgstrReturnsInput;
begin
  AssertEquals('Empty', __('Empty'));
end;

procedure TLanguageLibTest.TestCaseSensitive;
begin
  // TStringList.IndexOf used to be case insensitive and returned "Simpan"
  AssertEquals('simpan', __('save'));
  AssertEquals('Simpan', __('Save'));
  AssertEquals('SAVE', __('SAVE'));
end;

procedure TLanguageLibTest.TestEqualsSignInMsgstr;
begin
  // ValueFromIndex used to cut at the first '='
  AssertEquals('x=y=z', __('a=b'));
end;

procedure TLanguageLibTest.TestEscapes;
begin
  AssertEquals('Bilang "hai"' + #10, __('Say "hi"' + #10));
end;

procedure TLanguageLibTest.TestMultilineEntries;
begin
  AssertEquals('bagian satu bagian dua', __('first part second part'));
end;

procedure TLanguageLibTest.TestLastEntryWithoutMsgstr;
begin
  // a matching msgid on the last line used to index past the list
  AssertEquals('Dangling', __('Dangling'));
end;

procedure TLanguageLibTest.TestNoFileReturnsInput;
begin
  LANG := 'xx';
  AssertEquals('Save', __('Save'));
  LANG := '';
  AssertEquals('Save', __('Save'));
end;

procedure TLanguageLibTest.TestFallbackLanguage;
begin
  LANG := 'xx';
  FallbackLANG := 'id';
  AssertEquals('Simpan', __('Save'));
end;

procedure TLanguageLibTest.TestCacheReloadsChangedFile;
var
  age: longint;
begin
  AssertEquals('Simpan', __('Save'));
  // a rewritten file with a different timestamp is picked up
  age := FileAge('locale' + PathDelim + 'fastplaz.id.po');
  WritePO('id', 'msgid "Save"' + LineEnding + 'msgstr "Rekam"' + LineEnding);
  if FileAge('locale' + PathDelim + 'fastplaz.id.po') = age then
    ClearLanguageCache; // same second: force it, the age check cannot see it
  AssertEquals('Rekam', __('Save'));
end;

procedure TLanguageLibTest.TestSwitchingLanguage;
begin
  WritePO('en', 'msgid "Save"' + LineEnding + 'msgstr "Save it"' + LineEnding);
  AssertEquals('Simpan', __('Save'));
  LANG := 'en';
  AssertEquals('Save it', __('Save'));
  LANG := 'id';
  AssertEquals('Simpan', __('Save'));
end;

initialization
  RegisterTest(TLanguageLibTest);

end.

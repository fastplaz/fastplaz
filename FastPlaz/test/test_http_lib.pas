unit test_http_lib;

{$mode objfpc}{$H+}

interface

uses
  http_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { THTTPLibTest }

  THTTPLibTest = class(TTestCase)
  private
    FDir: string;
    function WriteFile(const AName, AContent: string): string;
    function StreamToString(AStream: TStream): string;
    function CountOf(const ASub, AText: string): integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // multipart body
    procedure TestMultipartFormFieldsOnly;
    procedure TestMultipartValueWithEquals;
    procedure TestMultipartEmptyValue;
    procedure TestMultipartTwoFiles;
    procedure TestMultipartFieldsAndFile;
    procedure TestMultipartQuotesInName;
    procedure TestMultipartMissingFileRaises;
    procedure TestBoundaryIsUnique;
    // THTTPLib
    procedure TestFormDataRoundTrip;
    procedure TestFormDataEmptyValueKept;
    procedure TestFormDataIgnoresBlankName;
    procedure TestSizeBeforeRequestIsZero;
    procedure TestAddFileSkipsMissing;
    procedure TestClearResetsState;
    procedure TestRequestBodyOwnership;
    procedure TestUrlEncode;
    procedure TestCreateAndFree;
    // THTTPResponse
    procedure TestResponseShortConstructor;
    procedure TestResponseCopiesStream;
  end;

implementation

const
  B = 'xyzBoundary';
  CRLF = #13#10;

function THTTPLibTest.WriteFile(const AName, AContent: string): string;
var
  f: TFileStream;
begin
  Result := FDir + AName;
  f := TFileStream.Create(Result, fmCreate);
  try
    if AContent <> '' then
      f.WriteBuffer(AContent[1], Length(AContent));
  finally
    f.Free;
  end;
end;

function THTTPLibTest.StreamToString(AStream: TStream): string;
begin
  Result := '';
  SetLength(Result, AStream.Size);
  AStream.Position := 0;
  if AStream.Size > 0 then
    AStream.ReadBuffer(Result[1], AStream.Size);
end;

function THTTPLibTest.CountOf(const ASub, AText: string): integer;
var
  p: integer;
begin
  Result := 0;
  p := Pos(ASub, AText);
  while p > 0 do
  begin
    Inc(Result);
    p := Pos(ASub, AText, p + Length(ASub));
  end;
end;

procedure THTTPLibTest.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_http_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
end;

procedure THTTPLibTest.TearDown;
var
  rec: TSearchRec;
begin
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

procedure THTTPLibTest.TestMultipartFormFieldsOnly;
var
  form: TStringList;
  body: TStream;
  s: string;
begin
  form := TStringList.Create;
  body := nil;
  try
    form.Add('name=Budi');
    form.Add('city=Jakarta');
    body := HTTPMultipartEncode(form, nil, B);
    AssertEquals('position rewound', 0, body.Position);
    s := StreamToString(body);
    AssertEquals('--' + B + CRLF +
      'Content-Disposition: form-data; name="name"' + CRLF + CRLF +
      'Budi' + CRLF +
      '--' + B + CRLF +
      'Content-Disposition: form-data; name="city"' + CRLF + CRLF +
      'Jakarta' + CRLF +
      '--' + B + '--' + CRLF, s);
  finally
    body.Free;
    form.Free;
  end;
end;

procedure THTTPLibTest.TestMultipartValueWithEquals;
var
  form: TStringList;
  body: TStream;
  s: string;
begin
  form := TStringList.Create;
  body := nil;
  try
    form.Add('token=abc==def=');
    form.Add('url=http://x/?a=1&b=2');
    body := HTTPMultipartEncode(form, nil, B);
    s := StreamToString(body);
    AssertTrue('full token kept', Pos(CRLF + 'abc==def=' + CRLF, s) > 0);
    AssertTrue('full url kept', Pos(CRLF + 'http://x/?a=1&b=2' + CRLF, s) > 0);
  finally
    body.Free;
    form.Free;
  end;
end;

procedure THTTPLibTest.TestMultipartEmptyValue;
var
  form: TStringList;
  body: TStream;
begin
  form := TStringList.Create;
  body := nil;
  try
    form.Add('empty=');
    body := HTTPMultipartEncode(form, nil, B);
    AssertEquals('--' + B + CRLF +
      'Content-Disposition: form-data; name="empty"' + CRLF + CRLF +
      CRLF +
      '--' + B + '--' + CRLF, StreamToString(body));
  finally
    body.Free;
    form.Free;
  end;
end;

procedure THTTPLibTest.TestMultipartTwoFiles;
var
  files: TStringList;
  body: TStream;
  s: string;
begin
  files := TStringList.Create;
  body := nil;
  try
    files.Add('files[]=' + WriteFile('one.txt', 'AAA'));
    files.Add('files[]=' + WriteFile('two.bin', #0#1#2));
    body := HTTPMultipartEncode(nil, files, B);
    s := StreamToString(body);
    AssertEquals('one closing delimiter', 1, CountOf('--' + B + '--', s));
    AssertEquals('two parts', 2, CountOf('--' + B + CRLF, s));
    AssertTrue('ends with closing delimiter',
      Copy(s, Length(s) - Length('--' + B + '--' + CRLF) + 1, MaxInt) =
      '--' + B + '--' + CRLF);
    AssertTrue('first file header', Pos(
      'Content-Disposition: form-data; name="files[]"; filename="one.txt"' + CRLF +
      'Content-Type: application/octet-stream' + CRLF + CRLF + 'AAA' + CRLF, s) > 0);
    AssertTrue('second file header', Pos(
      'filename="two.bin"' + CRLF +
      'Content-Type: application/octet-stream' + CRLF + CRLF + #0#1#2 + CRLF, s) > 0);
    AssertTrue('second file after first',
      Pos('filename="two.bin"', s) > Pos('filename="one.txt"', s));
  finally
    body.Free;
    files.Free;
  end;
end;

procedure THTTPLibTest.TestMultipartFieldsAndFile;
var
  form, files: TStringList;
  body: TStream;
  s: string;
begin
  form := TStringList.Create;
  files := TStringList.Create;
  body := nil;
  try
    form.Add('title=hello');
    files.Add('photo=' + WriteFile('p.jpg', 'JPG'));
    body := HTTPMultipartEncode(form, files, B);
    s := StreamToString(body);
    AssertEquals(1, CountOf('--' + B + '--', s));
    AssertEquals(2, CountOf('--' + B + CRLF, s));
    AssertTrue('field before file', Pos('name="title"', s) < Pos('name="photo"', s));
  finally
    body.Free;
    files.Free;
    form.Free;
  end;
end;

procedure THTTPLibTest.TestMultipartQuotesInName;
var
  form: TStringList;
  body: TStream;
begin
  form := TStringList.Create;
  body := nil;
  try
    form.Add('na"me=v');
    body := HTTPMultipartEncode(form, nil, B);
    AssertTrue(Pos('name="na%22me"', StreamToString(body)) > 0);
  finally
    body.Free;
    form.Free;
  end;
end;

procedure THTTPLibTest.TestMultipartMissingFileRaises;
var
  files: TStringList;
  body: TStream;
  raised: boolean;
begin
  files := TStringList.Create;
  body := nil;
  raised := False;
  try
    files.Add('f=' + FDir + 'does-not-exist.txt');
    try
      body := HTTPMultipartEncode(nil, files, B);
    except
      on E: Exception do
        raised := True;
    end;
    AssertTrue('exception propagated', raised);
    AssertNull(body);
  finally
    body.Free;
    files.Free;
  end;
end;

procedure THTTPLibTest.TestBoundaryIsUnique;
var
  a, b2: string;
begin
  a := HTTPMultipartBoundary;
  b2 := HTTPMultipartBoundary;
  AssertTrue('not empty', a <> '');
  AssertTrue('different per call', a <> b2);
  AssertTrue('no spaces', Pos(' ', a) = 0);
end;

procedure THTTPLibTest.TestFormDataRoundTrip;
var
  h: THTTPLib;
begin
  h := THTTPLib.Create;
  try
    h.FormData['a'] := '1';
    h.FormData['b'] := 'x=y';
    AssertEquals('1', h.FormData['a']);
    AssertEquals('x=y', h.FormData['b']);
    h.FormData['a'] := '2';
    AssertEquals('overwrite', '2', h.FormData['a']);
    AssertEquals('missing', '', h.FormData['zzz']);
  finally
    h.Free;
  end;
end;

procedure THTTPLibTest.TestFormDataEmptyValueKept;
var
  h: THTTPLib;
begin
  h := THTTPLib.Create;
  try
    h.FormData['a'] := '1';
    h.FormData['a'] := '';
    h.FormData['b'] := '';
    // TStrings.Values[] := '' used to drop the field entirely
    AssertEquals('both fields kept', 2, h.FormDataList.Count);
    AssertEquals('a=', h.FormDataList[0]);
    AssertEquals('b=', h.FormDataList[1]);
    AssertEquals('', h.FormData['a']);
  finally
    h.Free;
  end;
end;

procedure THTTPLibTest.TestFormDataIgnoresBlankName;
var
  h: THTTPLib;
begin
  h := THTTPLib.Create;
  try
    h.FormData[''] := '1';
    h.FormData['  '] := '1';
    AssertEquals(0, h.FormDataList.Count);
  finally
    h.Free;
  end;
end;

procedure THTTPLibTest.TestSizeBeforeRequestIsZero;
var
  h: THTTPLib;
begin
  h := THTTPLib.Create('http://127.0.0.1:1/');
  try
    AssertEquals(0, h.Size);
    AssertFalse(h.IsSuccessfull);
  finally
    h.Free;
  end;
end;

procedure THTTPLibTest.TestAddFileSkipsMissing;
var
  h: THTTPLib;
begin
  h := THTTPLib.Create;
  try
    h.AddFile(FDir + 'nope.txt');
    h.AddFile(WriteFile('yes.txt', 'y'), 'doc');
    AssertEquals(1, h.FormFiles.Count);
    AssertEquals('doc=' + FDir + 'yes.txt', h.FormFiles[0]);
    h.Clear;
    AssertEquals('Clear empties files', 0, h.FormFiles.Count);
  finally
    h.Free;
  end;
end;

procedure THTTPLibTest.TestClearResetsState;
var
  h: THTTPLib;
begin
  h := THTTPLib.Create;
  try
    h.ContentType := 'application/json';
    h.AddHeader('X-Test', '1');
    h.FormData['a'] := '1';
    h.RequestBody := TStringStream.Create('body');
    h.Clear;
    AssertEquals('', h.ContentType);
    AssertEquals('', h.FormData['a']);
    AssertNull(h.RequestBody);
    AssertTrue('custom header removed', Pos('X-Test', h.RequestHeaders) = 0);
    AssertTrue('user agent kept', Pos('User-Agent', h.RequestHeaders) > 0);
  finally
    h.Free;
  end;
end;

procedure THTTPLibTest.TestRequestBodyOwnership;
var
  h: THTTPLib;
  s1, s2: TStringStream;
begin
  h := THTTPLib.Create;
  try
    s1 := TStringStream.Create('one');
    s2 := TStringStream.Create('two');
    h.RequestBody := s1;
    // assigning only replaces the reference
    h.RequestBody := s2;
    AssertSame(s2, h.RequestBody);
    AssertEquals('previous stream untouched', 'one', s1.DataString);
    s1.Free;
    // a caller that frees the stream itself detaches first (telegram_integration)
    h.RequestBody.Free;
    h.RequestBody := nil;
    AssertNull(h.RequestBody);
    // the stream still attached at Free time is released by THTTPLib
    h.RequestBody := TStringStream.Create('three');
  finally
    h.Free;
  end;
end;

procedure THTTPLibTest.TestUrlEncode;
var
  h: THTTPLib;
begin
  h := THTTPLib.Create;
  try
    AssertEquals('a+b', h.UrlEncode('a b'));
    AssertEquals('a%20b', h.UrlEncode('a b', False));
    AssertEquals('abc123', h.UrlEncode('abc123'));
    AssertEquals('%26%3D', h.UrlEncode('&='));
    AssertEquals('', h.UrlEncode(''));
  finally
    h.Free;
  end;
end;

procedure THTTPLibTest.TestCreateAndFree;
var
  i: integer;
  h: THTTPLib;
begin
  // used to leak the worker and its TFPHTTPClient on every instance
  for i := 1 to 50 do
  begin
    h := THTTPLib.Create('http://example.invalid/');
    h.FormData['x'] := IntToStr(i);
    h.Free;
  end;
  AssertTrue(True);
end;

procedure THTTPLibTest.TestResponseShortConstructor;
var
  r: IHTTPResponse;
begin
  // used to recurse into itself
  r := THTTPResponse.Create(404, 'H', 'not found');
  AssertEquals(404, r.ResultCode);
  AssertEquals('H', r.ResultHeader);
  AssertEquals('not found', r.ResultText);
  AssertEquals(0, r.ResponseTime);
  AssertNotNull(r.ResultStream);
  AssertEquals(0, r.ResultStream.Size);
end;

procedure THTTPLibTest.TestResponseCopiesStream;
var
  r: IHTTPResponse;
  src: TStringStream;
begin
  src := TStringStream.Create('payload');
  try
    src.Position := 3;
    r := THTTPResponse.Create(200, '', 'payload', src, 12);
    AssertEquals(12, r.ResponseTime);
    AssertEquals('payload', StreamToString(r.ResultStream));
  finally
    src.Free;
  end;
  // the copy must outlive the source
  AssertEquals(7, r.ResultStream.Size);
end;

initialization
  RegisterTest(THTTPLibTest);

end.

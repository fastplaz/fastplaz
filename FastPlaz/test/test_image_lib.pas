unit test_image_lib;

{$mode objfpc}{$H+}

interface

uses
  image_lib, FPimage, FPWritePNG, FPWriteJPEG, FPReadJPEG, FPReadPNG,
  Classes, SysUtils, fpcunit, testregistry;

type

  { TImageLibTest }

  TImageLibTest = class(TTestCase)
  private
    FDir: string;
    FPng: string;
    FJpg: string;
    procedure WriteImage(const AFile: string; W, H: integer);
    procedure WriteText(const AFile, AText: string);
    function ReadSize(const AFile: string; out W, H: integer): boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateAndFree;
    procedure TestLoadPng;
    procedure TestLoadJpg;
    procedure TestLoadMissingFile;
    procedure TestLoadWrongContent;
    procedure TestResizeKeepsAspect;
    procedure TestResizeStretch;
    procedure TestResizeInvalidIgnored;
    procedure TestSaveJpgAndPng;
    procedure TestAsStreamTwice;
    procedure TestAsStreamIsJpeg;
  end;

implementation

procedure TImageLibTest.WriteImage(const AFile: string; W, H: integer);
var
  img: TFPMemoryImage;
  writer: TFPCustomImageWriter;
  x, y: integer;
  c: TFPColor;
begin
  img := TFPMemoryImage.Create(W, H);
  try
    for y := 0 to H - 1 do
      for x := 0 to W - 1 do
      begin
        c.Red := (x * 65535) div W;
        c.Green := (y * 65535) div H;
        c.Blue := 32768;
        c.Alpha := alphaOpaque;
        img.Colors[x, y] := c;
      end;
    if LowerCase(ExtractFileExt(AFile)) = '.png' then
      writer := TFPWriterPNG.Create
    else
      writer := TFPWriterJPEG.Create;
    try
      img.SaveToFile(AFile, writer);
    finally
      writer.Free;
    end;
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.WriteText(const AFile, AText: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(AFile, fmCreate);
  try
    f.WriteBuffer(AText[1], Length(AText));
  finally
    f.Free;
  end;
end;

function TImageLibTest.ReadSize(const AFile: string; out W, H: integer): boolean;
var
  img: TFPMemoryImage;
  reader: TFPCustomImageReader;
begin
  Result := False;
  W := 0;
  H := 0;
  img := TFPMemoryImage.Create(1, 1);
  if LowerCase(ExtractFileExt(AFile)) = '.png' then
    reader := TFPReaderPNG.Create
  else
    reader := TFPReaderJPEG.Create;
  try
    try
      img.LoadFromFile(AFile, reader);
      W := img.Width;
      H := img.Height;
      Result := True;
    except
      Result := False;
    end;
  finally
    reader.Free;
    img.Free;
  end;
end;

procedure TImageLibTest.SetUp;
begin
  FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fastplaz_image_test_' +
    IntToStr(GetProcessID) + PathDelim;
  ForceDirectories(FDir);
  FPng := FDir + 'src.png';
  FJpg := FDir + 'src.jpg';
  WriteImage(FPng, 40, 20);
  WriteImage(FJpg, 30, 60);
end;

procedure TImageLibTest.TearDown;
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

procedure TImageLibTest.TestCreateAndFree;
var
  i: integer;
  img: TImageLib;
begin
  // the images and canvases used to leak on every instance
  for i := 1 to 20 do
  begin
    img := TImageLib.Create;
    AssertFalse(img.Loaded);
    img.Free;
  end;
end;

procedure TImageLibTest.TestLoadPng;
var
  img: TImageLib;
begin
  img := TImageLib.Create;
  try
    AssertTrue(img.LoadFromFile(FPng));
    AssertTrue(img.Loaded);
    AssertEquals(40, img.Width);
    AssertEquals(20, img.Height);
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.TestLoadJpg;
var
  img: TImageLib;
begin
  img := TImageLib.Create;
  try
    AssertTrue(img.LoadFromFile(FJpg));
    AssertEquals(30, img.Width);
    AssertEquals(60, img.Height);
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.TestLoadMissingFile;
var
  img: TImageLib;
begin
  img := TImageLib.Create;
  try
    AssertFalse(img.LoadFromFile(FDir + 'nope.jpg'));
    AssertFalse(img.Loaded);
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.TestLoadWrongContent;
var
  img: TImageLib;
begin
  // a text file named .png (typical upload) used to raise out of LoadFromFile
  WriteText(FDir + 'fake.png', '<html>not an image</html>');
  WriteText(FDir + 'fake.jpg', 'garbage');
  img := TImageLib.Create;
  try
    AssertFalse(img.LoadFromFile(FDir + 'fake.png'));
    AssertFalse(img.LoadFromFile(FDir + 'fake.jpg'));
    AssertFalse(img.Loaded);
    // the object stays usable
    AssertTrue(img.LoadFromFile(FPng));
    AssertTrue(img.Loaded);
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.TestResizeKeepsAspect;
var
  img: TImageLib;
begin
  img := TImageLib.Create;
  try
    AssertTrue(img.LoadFromFile(FPng)); // 40x20
    img.Resize(20);
    AssertEquals(20, img.Width);
    AssertEquals(10, img.Height);
    img.Resize(0, 0);
    AssertEquals('width 0 keeps the source width', 40, img.Width);
    AssertEquals(20, img.Height);
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.TestResizeStretch;
var
  img: TImageLib;
begin
  img := TImageLib.Create;
  try
    AssertTrue(img.LoadFromFile(FJpg)); // 30x60
    img.Resize(15, 15);
    AssertEquals(15, img.Width);
    AssertEquals(15, img.Height);
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.TestResizeInvalidIgnored;
var
  img: TImageLib;
begin
  img := TImageLib.Create;
  try
    AssertTrue(img.LoadFromFile(FPng));
    img.Resize(-5, 10);
    AssertEquals(40, img.Width);
    img.Resize(10, -1);
    AssertEquals(40, img.Width);
    img.Resize(100000);
    AssertEquals(40, img.Width);
    img.Resize(1);
    AssertEquals(1, img.Width);
    AssertEquals('height never drops to 0', 1, img.Height);
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.TestSaveJpgAndPng;
var
  img: TImageLib;
  w, h: integer;
begin
  img := TImageLib.Create;
  try
    AssertTrue(img.LoadFromFile(FPng));
    img.Resize(10);
    img.Quality := 60;
    AssertTrue(img.SaveToFile(FDir + 'out.jpg'));
    AssertTrue(img.SaveToFile(FDir + 'out.png'));
    img.GrayScale := True;
    AssertTrue(img.SaveToFile(FDir + 'gray.jpg'));
    AssertFalse('unwritable path', img.SaveToFile(FDir + 'no' + PathDelim + 'dir' + PathDelim + 'x.jpg'));
  finally
    img.Free;
  end;
  AssertTrue(ReadSize(FDir + 'out.jpg', w, h));
  AssertEquals(10, w);
  AssertEquals(5, h);
  AssertTrue(ReadSize(FDir + 'out.png', w, h));
  AssertEquals(10, w);
  AssertEquals(5, h);
  AssertTrue(ReadSize(FDir + 'gray.jpg', w, h));
end;

procedure TImageLibTest.TestAsStreamTwice;
var
  img: TImageLib;
  s: TStream;
  first: int64;
begin
  img := TImageLib.Create;
  try
    AssertTrue(img.LoadFromFile(FPng));
    s := img.AsStream;
    AssertNotNull(s);
    AssertEquals('rewound', 0, s.Position);
    first := s.Size;
    AssertTrue(first > 0);
    // a second call used to append a second JPEG and leave Position at the end
    s := img.AsStream;
    AssertEquals(0, s.Position);
    AssertEquals(first, s.Size);
  finally
    img.Free;
  end;
end;

procedure TImageLibTest.TestAsStreamIsJpeg;
var
  img: TImageLib;
  s: TStream;
  magic: array[0..1] of byte;
  f: TFileStream;
  w, h: integer;
begin
  img := TImageLib.Create;
  try
    AssertTrue(img.LoadFromFile(FPng));
    img.Resize(8);
    s := img.AsStream;
    s.ReadBuffer(magic, 2);
    AssertEquals($FF, magic[0]);
    AssertEquals($D8, magic[1]);
    s.Position := 0;
    f := TFileStream.Create(FDir + 'stream.jpg', fmCreate);
    try
      f.CopyFrom(s, 0);
    finally
      f.Free;
    end;
  finally
    img.Free;
  end;
  AssertTrue(ReadSize(FDir + 'stream.jpg', w, h));
  AssertEquals(8, w);
end;

initialization
  RegisterTest(TImageLibTest);

end.

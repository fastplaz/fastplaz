{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit image_lib;

{
  USAGE

  [x] resize:
    img := TImageLib.Create;
    if img.LoadFromFile( 'sourceFileName.jpg') then
    begin
      img.Resize( 300); // auto height

      img.Resize(300,200); // force resize - stretch

      img.GrayScale := True; // convert to grayscale

      img.Quality := 70; // custom jpeg quality. default is 80

      img.SaveToFile('newFileName.jpg'); // save to file
    end;
    img.Free;

  [x] Load As Stream

    img := TImageLib.Create;
    if img.LoadFromFile( 'sourceFileName.jpg') then
    begin
      Response.ContentStream := img.AsStream; // JPEG, owned by img
      ...
    end;
    img.Free; // only after the stream has been consumed

  AsStream is owned by TImageLib and freed with it, so keep the object
  alive until the stream has been read (or copy the stream). The reader
  is chosen from the file extension (.png, .gif, else JPEG); a file whose
  content does not match makes LoadFromFile return False.
}
{$mode objfpc}{$H+}

interface

uses
  FPimage, FPCanvas, FPImgCanv, FPReadJPEG, FPReadPNG, FPReadGif,
  FPWriteJPEG, FPWritePNG,
  Classes, SysUtils;

type

  { TImageLib }

  TImageLib = class
  private
    FGrayScale: boolean;
    FQuality: TFPJPEGCompressionQuality;
    FLoaded: boolean;
    sourceImage, targetImage: TFPCustomImage;
    sourceCanvas, targetCanvas: TFPcustomCanvas;
    reader: TFPCustomImageReader;
    writer: TFPCustomImageWriter;
    stream: TMemoryStream;
    r: TRect;
    function getAsStream: TStream;
    function getHeight: integer;
    function getWidth: integer;
    procedure PrepareWriter(const AExt: string);
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(AFile: string): boolean;
    function SaveToFile(AFile: string): boolean;

    procedure Resize(AWidth: integer; AHeight: integer = 0);

    property GrayScale: boolean read FGrayScale write FGrayScale;
    property Quality: TFPJPEGCompressionQuality read FQuality write FQuality;
    // JPEG encoding of the current (resized) image; rewritten on each call
    property AsStream: TStream read getAsStream;
    property Loaded: boolean read FLoaded;
    // size of the current (resized) image
    property Width: integer read getWidth;
    property Height: integer read getHeight;
  end;


implementation

{ TImageLib }

function TImageLib.getWidth: integer;
begin
  Result := targetImage.Width;
end;

function TImageLib.getHeight: integer;
begin
  Result := targetImage.Height;
end;

// JPEG writer by default, PNG for .png; applies GrayScale and Quality
procedure TImageLib.PrepareWriter(const AExt: string);
begin
  FreeAndNil(writer);
  if AExt = '.png' then
  begin
    writer := TFPWriterPNG.Create;
    TFPWriterPNG(writer).GrayScale := FGrayScale;
    TFPWriterPNG(writer).UseAlpha := False;
  end
  else
  begin
    writer := TFPWriterJPEG.Create;
    TFPWriterJPEG(writer).GrayScale := FGrayScale;
    TFPWriterJPEG(writer).CompressionQuality := FQuality;
  end;
end;

function TImageLib.getAsStream: TStream;
begin
  if not Assigned(stream) then
    stream := TMemoryStream.Create;
  // a second call used to append a second JPEG and leave Position at the end
  stream.Clear;
  PrepareWriter('.jpg');
  writer.ImageWrite(stream, targetImage);
  stream.Position := 0;
  Result := stream;
end;

constructor TImageLib.Create;
begin
  inherited Create;
  sourceImage := TFPMemoryImage.Create(1, 1);
  sourceCanvas := TFPImageCanvas.Create(sourceImage);
  targetImage := TFPMemoryImage.Create(1, 1);
  targetCanvas := TFPImageCanvas.Create(targetImage);
  reader := nil;
  writer := nil;
  stream := nil;
  FLoaded := False;

  r := Rect(0, 0, 1, 1);
  FGrayScale := False;
  FQuality := 80;
end;

destructor TImageLib.Destroy;
begin
  // the two images and canvases used to be leaked
  stream.Free;
  reader.Free;
  writer.Free;
  targetCanvas.Free;
  sourceCanvas.Free;
  targetImage.Free;
  sourceImage.Free;
  inherited Destroy;
end;

function TImageLib.LoadFromFile(AFile: string): boolean;
var
  fileExt: string;
begin
  Result := False;
  FLoaded := False;
  if not FileExists(AFile) then
    Exit;

  FreeAndNil(reader);
  fileExt := LowerCase(ExtractFileExt(AFile));
  case fileExt of
    '.png': reader := TFPReaderPNG.Create;
    '.gif': reader := TFPReaderGif.Create;
    else // default is jpeg
      reader := TFPReaderJPEG.Create;
  end;

  try
    sourceImage.LoadFromFile(AFile, reader);
  except
    // wrong format, truncated file, ...: a user upload must not raise
    Exit;
  end;
  r := Rect(0, 0, sourceImage.Width, sourceImage.Height);
  targetImage.SetSize(r.Width, r.Height);
  targetCanvas.CopyRect(0, 0, sourceCanvas, r);
  FLoaded := True;
  Result := True;
end;

function TImageLib.SaveToFile(AFile: string): boolean;
begin
  Result := False;
  PrepareWriter(LowerCase(ExtractFileExt(AFile)));
  try
    targetImage.SaveToFile(AFile, writer);
    Result := True;
  except
    Result := False;
  end;
end;

// Resize to AWidth x AHeight; AHeight = 0 keeps the aspect ratio,
// AWidth = 0 keeps the source width. Negative or oversized values are
// ignored.
procedure TImageLib.Resize(AWidth: integer; AHeight: integer);
const
  MAX_SIDE = 32767;
var
  w, h: int64;
begin
  if (AWidth < 0) or (AHeight < 0) or (AWidth > MAX_SIDE) or (AHeight > MAX_SIDE) then
    Exit;
  w := AWidth;
  if w = 0 then
    w := sourceImage.Width;
  h := AHeight;
  if h = 0 then
    h := (w * sourceImage.Height) div sourceImage.Width;
  if h = 0 then
    h := 1;
  if (w > MAX_SIDE) or (h > MAX_SIDE) then
    Exit;
  r := Rect(0, 0, w, h);
  targetImage.SetSize(w, h);
  targetCanvas.StretchDraw(0, 0, w, h, sourceImage);
end;

end.

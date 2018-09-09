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
    if not img.LoadFromFile( 'sourceFileName.jpg') then
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
    img.LoadFromFile( 'sourceFileName.jpg');
    ContentStream := img.AsStream;

    img.Free;


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
    sourceImage, targetImage: TFPCustomImage;
    sourceCanvas, targetCanvas: TFPcustomCanvas;
    reader: TFPCustomImageReader;
    writer: TFPCustomImageWriter;
    stream: TMemoryStream;
    r: TRect;
    function getAsStream: TStream;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(AFile: string): boolean;
    function SaveToFile(AFile: string): boolean;

    procedure Resize(AWidth: integer; AHeight: integer = 0);

    property GrayScale: boolean read FGrayScale write FGrayScale;
    property Quality: TFPJPEGCompressionQuality read FQuality write FQuality;
    property AsStream: TStream read getAsStream;
  end;


implementation

uses common;

{ TImageLib }

function TImageLib.getAsStream: TStream;
begin
  if not Assigned(stream) then
    stream := TMemoryStream.Create;
  if not Assigned(writer) then
    writer := TFPWriterJPEG.Create;
  writer.ImageWrite(stream, targetImage);
  Result := stream;
end;

constructor TImageLib.Create;
begin
  inherited Create;
  sourceImage := TFPMemoryImage.Create(1, 1);
  sourceCanvas := TFPImageCanvas.Create(sourceImage);
  targetImage := TFPMemoryImage.Create(1, 1);
  targetCanvas := TFPImageCanvas.Create(targetImage);

  r.Left := 0;
  r.Top := 0;
  FGrayScale := False;
  FQuality := 80;
end;

destructor TImageLib.Destroy;
begin
  inherited Destroy;
  if Assigned(stream) then
    stream.Free;
  if Assigned(reader) then
    reader.Free;
  if Assigned(writer) then
    writer.Free;
end;

function TImageLib.LoadFromFile(AFile: string): boolean;
var
  fileExt: string;
begin
  Result := False;
  if not FileExists(AFile) then
    Exit;

  if Assigned(reader) then
    reader.Free;

  fileExt := LowerCase(ExtractFileExt(AFile));
  case fileExt of
    '.png': reader := TFPReaderPNG.Create;
    '.gif': reader := TFPReaderGif.Create;
    else // default is jpeg
      reader := TFPReaderJPEG.Create;
  end;

  sourceImage.LoadFromFile(AFile, reader);
  r := Rect(0, 0, sourceCanvas.Width, sourceCanvas.Height);
  targetCanvas.Width := r.Width;
  targetCanvas.Height := r.Height;
  targetCanvas.CopyRect(0, 0, sourceCanvas, r);
  Result := True;
end;

function TImageLib.SaveToFile(AFile: string): boolean;
var
  fileExt: string;
begin
  if Assigned(writer) then
    writer.Free;

  fileExt := LowerCase(ExtractFileExt(AFile));
  case fileExt of
    '.png':
    begin
      writer := TFPWriterPNG.Create;
      with writer as TFPWriterPNG do
      begin
        GrayScale := FGrayScale;
      end;
    end
    else
    begin // default is jpeg
      writer := TFPWriterJPEG.Create;
      with writer as TFPWriterJPEG do
      begin
        GrayScale := FGrayScale;
        CompressionQuality := FQuality;
      end;
    end;
  end;

  targetImage.SaveToFile(AFile, writer);
  Result := True;
end;

procedure TImageLib.Resize(AWidth: integer; AHeight: integer);
begin
  r.Width := AWidth;
  if AWidth = 0 then
    r.Width := sourceCanvas.Width;
  if AHeight = 0 then
    AHeight := (r.Width * sourceCanvas.Height) div sourceCanvas.Width;
  r.Height := AHeight;
  targetCanvas.Width := r.Width;
  targetCanvas.Height := r.Height;
  targetCanvas.StretchDraw(0, 0, r.Width, r.Height, sourceImage);
end;

end.

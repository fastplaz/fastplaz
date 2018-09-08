unit image_lib;

{$mode objfpc}{$H+}

interface

uses
  dateutils,
  FPimage, FPReadJPEG, FPWriteJPEG, FPImgCanv, FPCanvas,
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

{ TImageLib }

function TImageLib.getAsStream: TStream;
begin
  if not Assigned(stream) then
    stream := TMemoryStream.Create;
  writer.ImageWrite(stream,targetImage);
  Result := stream;
end;

constructor TImageLib.Create;
begin
  inherited Create;
  sourceImage := TFPMemoryImage.Create(1, 1);
  sourceCanvas := TFPImageCanvas.Create(sourceImage);
  targetImage := TFPMemoryImage.Create(1, 1);
  targetCanvas := TFPImageCanvas.Create(targetImage);

  reader := TFPReaderJPEG.Create;
  writer := TFPWriterJPEG.Create;

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
end;

function TImageLib.LoadFromFile(AFile: string): boolean;
begin
  Result := False;
  if not FileExists(AFile) then
    Exit;

  sourceImage.LoadFromFile(AFile, reader);
  r := Rect(0, 0, sourceCanvas.Width, sourceCanvas.Height);
  targetCanvas.Width := r.Width;
  targetCanvas.Height := r.Height;
  targetCanvas.CopyRect(0, 0, sourceCanvas, r);
  Result := True;
end;

function TImageLib.SaveToFile(AFile: string): boolean;
begin
  with writer as TFPWriterJPEG do
  begin
    GrayScale := FGrayScale;
    Quality := FQuality;
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


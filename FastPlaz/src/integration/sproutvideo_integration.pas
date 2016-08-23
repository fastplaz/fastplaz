unit sproutvideo_integration;

{
  Sprout Video Hosting API
  https://sproutvideo.com/

  // USAGE:

  [x] Get List of Videos

  with TSproutVideoIntegration.Create do
  begin
    Key:= 'yourkeyhere';
    VarOfString := VideoLists;

    Free;
  end;

  [x] Upload Video
  with TSproutVideoIntegration.Create do
  begin
    Key := 'yourkeyhere';
    Title := 'Ini Title';
    Description := 'Ini Description';
    if not UploadVideo('/your/video/path.mp4') then
    begin

    end;
    videoID := ID;
    result := ResultText;

    Free;
  end;

  [x] Delete Video
  with TSproutVideoIntegration.Create do
  begin
    Key := 'yourkeyhere';
    if not Delete( 'yourvideoid') then
    begin

    end;
    result := ResultText;

    Free;
  end;


  [x] Result Error example
  {"error":"Unauthorized"}

}
{$mode objfpc}{$H+}

interface

uses
  http_lib, fpjson, jsonparser,
  Classes, SysUtils;

type

  { TSproutVideoIntegration }

  TSproutVideoIntegration = class(TInterfacedObject)
  private
    FDescription: string;
    FID: string;
    FIsSuccessfull: boolean;
    FKey: string;
    FParseMode: string;
    FResultCode: integer;
    FResultText: string;
    FTitle: string;
    procedure setKey(AValue: string);
  public
    constructor Create;
    function ListVideos: string;
    function UploadVideo(FileName: string): boolean;
    function UploadVideoCustom(FileName: string;
      Title, Description, Options: string): boolean;
    function Delete(VideoID: string): boolean;
  published
    property ID: string read FID;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Key: string read FKey write setKey;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property ParseMode: string read FParseMode write FParseMode;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;
  end;

implementation

const
  SPROUTVIDEO_BASEURL = 'https://api.sproutvideo.com/v1/';

var
  Response: IHTTPResponse;

{ TSproutVideoIntegration }

procedure TSproutVideoIntegration.setKey(AValue: string);
begin
  if FKey = AValue then
    Exit;
  FKey := AValue;
end;

constructor TSproutVideoIntegration.Create;
begin

end;

function TSproutVideoIntegration.ListVideos: string;
var
  urlTarget: string;
begin
  FIsSuccessfull := False;
  Result := '{"err":-1}';

  urlTarget := SPROUTVIDEO_BASEURL + 'videos';
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('SproutVideo-Api-Key', FKey);
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      FIsSuccessfull := IsSuccessfull;

      Result := FResultText;
    except
    end;
    Free;
  end;

end;

function TSproutVideoIntegration.UploadVideo(FileName: string): boolean;
begin
  Result := UploadVideoCustom(FileName, Title, Description, '');
end;

function TSproutVideoIntegration.UploadVideoCustom(FileName: string;
  Title, Description, Options: string): boolean;
var
  j: TJSONData;
  i: integer;
  urlTarget: string;
begin
  FID := '';
  FIsSuccessfull := False;

  urlTarget := SPROUTVIDEO_BASEURL + 'videos';
  with THTTPLib.Create(urlTarget) do
  begin
    try
      //ContentType := 'application/x-www-form-urlencoded';
      {
      bodyPost := '{'
        + '"title":"'+Title+'",'
        + '"deskripsi":"'+Description+'",'
        + '"null":""'
        + '}';
      RequestBody := TStringStream.Create( bodyPost);
      }
      AddHeader('SproutVideo-Api-Key', FKey);
      FormData['title'] := Title;
      FormData['description'] := Description;
      AddFile(FileName, 'file');
      Response := Post;
      if IsSuccessfull then
      begin
        FResultCode := Response.ResultCode;
        FResultText := Response.ResultText;

        j := GetJSON(FResultText);
        i := TJSONObject(j).IndexOfName('id');
        FID := j.Items[i].AsString;
        FIsSuccessfull := True;
      end;
    except
    end;
    Free;
  end;
  Result := FIsSuccessfull;
end;

function TSproutVideoIntegration.Delete(VideoID: string): boolean;
var
  urlTarget: string;
begin
  FIsSuccessfull := False;

  urlTarget := SPROUTVIDEO_BASEURL + 'videos/' + VideoID;
  with THTTPLib.Create(urlTarget) do
  begin
    try
      AddHeader('SproutVideo-Api-Key', FKey);
      Response := Delete;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      // TODO: check result
      FIsSuccessfull := True;
    except
    end;
    Free;
  end;

  Result := FIsSuccessfull;
end;

end.

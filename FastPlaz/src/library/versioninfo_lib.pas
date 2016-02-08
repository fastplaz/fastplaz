unit versioninfo_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, resource, versiontypes, versionresource;

type

  TRVersion = record
    Major: word;
    Minor: word;
    Revision: word;
    BuldNumber: word;
    Version: string;
    FullVersion: string;
  end;

  { TVersionInfo }

  TVersionInfo = class
  private
    FVersResource: TVersionResource;
    function GetFixedInfo: TVersionFixedInfo;
    function GetStringFileInfo: TVersionStringFileInfo;
    function GetVarFileInfo: TVersionVarFileInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(Instance: THandle);
    property FixedInfo: TVersionFixedInfo read GetFixedInfo;
    property StringFileInfo: TVersionStringFileInfo read GetStringFileInfo;
    property VarFileInfo: TVersionVarFileInfo read GetVarFileInfo;
  end;

var
  VersionInfo: TRVersion;

implementation


{ TVersionInfo }

function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
begin
  Result := FVersResource.FixedInfo;
end;

function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
begin
  Result := FVersResource.StringFileInfo;
end;

function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
begin
  Result := FVersResource.VarFileInfo;
end;

constructor TVersionInfo.Create;
begin
  inherited Create;
  FVersResource := TVersionResource.Create;
end;

destructor TVersionInfo.Destroy;
begin
  FVersResource.Free;
  inherited Destroy;
end;

procedure TVersionInfo.Load(Instance: THandle);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, 1, PChar(RT_VERSION));
  try
    FVersResource.SetCustomRawDataStream(Stream);
    // access some property to load from the stream
    FVersResource.FixedInfo;
    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);
  finally
    Stream.Free;
  end;
end;

end.



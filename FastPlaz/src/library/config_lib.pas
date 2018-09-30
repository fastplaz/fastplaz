unit config_lib;

{$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 5) and (FPC_PATCH >= 1))}
  {$DEFINE LSNEWFPC}
{$ENDIF}
{$DEFINE LSNEWFPC} // force bypass

{$mode objfpc}{$H+}

interface

uses
  fpcgi, jsonConf, fpjson, jsonparser, jsonscanner, variants,
  {$IFDEF LSNEWFPC}
  {$ELSE}
  jsonscanner,
  {$ENDIF}
  Classes, SysUtils;

const
  _ERR_CONFIG_FILENOTEXIST = 'File config.json not exist.';
  _ERR_CONFIG_INVALID = 'Invalid Config File.';

type

  TLocalJSONParser = class(TJSONParser)
  public
{$IFDEF LSNEWFPC}
    property Scanner;
{$ELSE}
    property Scanner: TJSONScanner read FScanner;
{$ENDIF}
  end;

  { TMyConfig }

  TMyConfig = class(TJSONConfig)
  private
    FIsValid: boolean;
    function GetConfigValue(KeyName: String): variant;
    procedure SetConfigValue(KeyName: String; AValue: variant);
  public
    Status: integer;
    Message: string;
    constructor Create(AOwner: TComponent); override;
    property Value[KeyName: String]: variant
      read GetConfigValue write SetConfigValue; default;
    property IsValid: boolean read FIsValid;

    function GetObject( AKeyName:UnicodeString):TJSONObject;

    function ValidateFile(ConfigFileName: string): boolean;
  end;

implementation

uses common;

{ TMyConfig }

constructor TMyConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsValid := False;
  Status := 1;
  Message := _ERR_CONFIG_FILENOTEXIST;
end;

function TMyConfig.GetObject(AKeyName: UnicodeString): TJSONObject;
begin
  try
    If (AKeyName[Length(AKeyName)]<>'/') then
      AKeyName := AKeyName+'/';
    Result := FindObject(AKeyName, False);
  except
  end;
end;

function TMyConfig.GetConfigValue(KeyName: String): variant;
var
  El : TJSONData;
begin
  Result := '';
  try
    El:=FindElement(StripSlash( UnicodeString( KeyName)),False);
    if El.JSONType = jtArray then
    begin
      Result := El.AsJSON;
    end
    else
    begin
      Result := El.AsString;
    end;
  except
//    on e: Exception do
//      die(e.Message);
  end;
end;

procedure TMyConfig.SetConfigValue(KeyName: String; AValue: variant);
var
  s: WideString;
begin
  try
    s := AValue;
    SetValue( UnicodeString( KeyName), s);
  except
    on e: Exception do
      die(e.Message);
    ;
  end;
end;

function TMyConfig.ValidateFile(ConfigFileName: string): boolean;
var
  lst: TStringList;
  VJSONData: TJSONData = nil;
  VJSONParser: TLocalJSONParser;
begin
  Result := False;
  if not FileExists(ConfigFileName) then
    Exit;

  lst := TStringList.Create;
  lst.LoadFromFile(ConfigFileName);

  VJSONParser := TLocalJSONParser.Create(lst.Text, [joStrict, joUTF8]);
  try
    try
      //VJSONParser.Strict := True; //deprecated, use joStrict
      VJSONData := VJSONParser.Parse;
      lst.Text := VJSONData.FormatJSON([], 2);
    except
      on E: Exception do
      begin
        Status := 2;
        Message := 'Config Error at line ' + i2s(VJSONParser.Scanner.CurRow) +
          ',' + i2s(VJSONParser.Scanner.CurColumn + 1);
        //if AppData.debug then
        //  Message:= Message + ': ' + VJSONParser.Scanner.CurLine ;
        FreeAndNil(VJSONData);
        FreeAndNil(VJSONParser);
        FreeAndNil(lst);
        Exit;
      end;
    end;
  finally
    FreeAndNil(VJSONData);
    FreeAndNil(VJSONParser);
  end;

  FreeAndNil(lst);
  Status := 0;
  Filename := ConfigFileName;
end;

end.

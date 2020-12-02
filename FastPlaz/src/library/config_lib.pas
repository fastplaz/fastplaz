unit config_lib;

{$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 5) and (FPC_PATCH >= 1))}
  {$DEFINE LSNEWFPC}
{$ENDIF}
{$DEFINE LSNEWFPC}// force bypass

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
    function StripSlash(const DataString: UnicodeString): UnicodeString;
    function GetConfigValue(KeyName: string): variant;
    procedure SetConfigValue(KeyName: string; AValue: variant);
  public
    Status: integer;
    Message: string;
    constructor Create(AOwner: TComponent); override;
    property Value[KeyName: string]: variant read GetConfigValue write SetConfigValue;
      default;
    property IsValid: boolean read FIsValid;

    function GetObject(AKeyName: UnicodeString): TJSONObject;

    function ValidateFile(ConfigFileName: string): boolean;
    procedure SetDataValue(const APath: UnicodeString; const AValue: TJSONData);

  end;

implementation

//uses common;

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
    if (AKeyName[Length(AKeyName)] <> '/') then
      AKeyName := AKeyName + '/';
    Result := FindObject(AKeyName, False);
  except
  end;
end;

function TMyConfig.StripSlash(const DataString: UnicodeString): UnicodeString;
var
  L: integer;
begin
  L := Length(DataString);
  if (L > 0) and (DataString[l] = '/') then
    Result := Copy(DataString, 1, L - 1)
  else
    Result := DataString;
end;

function TMyConfig.GetConfigValue(KeyName: string): variant;
var
  El: TJSONData;
begin
  Result := '';
  try
    El := FindElement(StripSlash(UnicodeString(KeyName)), False);
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

procedure TMyConfig.SetConfigValue(KeyName: string; AValue: variant);
var
  s: WideString;
begin
  try
    s := AValue;
    SetValue(UnicodeString(KeyName), s);
  except
    on e: Exception do
    begin
      //die(e.Message);
    end;
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
        Message := 'Config Error at line ' + IntToStr(VJSONParser.Scanner.CurRow) +
          ',' + IntToStr(VJSONParser.Scanner.CurColumn + 1);
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

procedure TMyConfig.SetDataValue(const APath: UnicodeString;
  const AValue: TJSONData);
var
  lst: TStrings;
  El: TJSONData;
  elName: UnicodeString;
  A : TJSONArray;
  O: TJSONObject;
begin
  El :=FindNodeForValue(aPath,TJSONObject,O,elName);
  if not Assigned(el) then
  begin
    El := GetJSON(AValue.AsJSON);
    O.Add(UTF8Encode(elName), El);
  end
  else
  begin
    A := El as TJSONArray;
    A.Add(AValue);
  end;
  FModified := True;
end;

end.

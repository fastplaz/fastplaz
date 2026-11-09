unit config_lib;

{$mode objfpc}{$H+}

interface

uses
  jsonConf, fpjson, jsonparser, jsonscanner, variants,
  Classes, SysUtils;

const
  _ERR_CONFIG_FILENOTEXIST = 'File config.json not exist.';
  _ERR_CONFIG_INVALID = 'Invalid Config File.';

type

  TLocalJSONParser = class(TJSONParser)
  public
    property Scanner;
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

    // String read/write without going through the Variant default property
    // (Value[]), which costs a Variant conversion per call. GetString
    // returns '' for a missing key, containers as their JSON text.
    function GetString(const AKeyName: string): string;
    procedure SetString(const AKeyName, AValue: string);

    // nil when the path does not exist or is not an object
    function GetObject(AKeyName: UnicodeString): TJSONObject;

    // Parses ConfigFileName; on success Status = 0, IsValid = True and the
    // file becomes the active config. On a parse error Status = 2 and
    // Message holds the position.
    function ValidateFile(ConfigFileName: string): boolean;
    // Replaces the node at APath with a copy of AValue. The caller keeps
    // ownership of AValue.
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
  Result := nil;
  if AKeyName = '' then
    Exit;
  try
    if (AKeyName[Length(AKeyName)] <> '/') then
      AKeyName := AKeyName + '/';
    Result := FindObject(AKeyName, False);
  except
    Result := nil;
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
begin
  Result := GetString(KeyName);
end;

function TMyConfig.GetString(const AKeyName: string): string;
var
  El: TJSONData;
begin
  Result := '';
  try
    El := FindElement(StripSlash(UnicodeString(AKeyName)), False, True);
    if not Assigned(El) then
      Exit;
    // containers are returned as their JSON text, scalars as text
    if El.JSONType in [jtArray, jtObject] then
      Result := El.AsJSON
    else
      Result := El.AsString;
  except
  end;
end;

procedure TMyConfig.SetString(const AKeyName, AValue: string);
begin
  try
    SetValue(UnicodeString(AKeyName), UnicodeString(AValue));
  except
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
  FIsValid := False;
  if not FileExists(ConfigFileName) then
  begin
    Status := 1;
    Message := _ERR_CONFIG_FILENOTEXIST;
    Exit;
  end;

  lst := TStringList.Create;
  VJSONParser := nil;
  try
    lst.LoadFromFile(ConfigFileName);
    VJSONParser := TLocalJSONParser.Create(lst.Text, [joStrict, joUTF8]);
    try
      VJSONData := VJSONParser.Parse;
      if not (VJSONData is TJSONObject) then
      begin
        Status := 2;
        Message := _ERR_CONFIG_INVALID;
        Exit;
      end;
    except
      on E: Exception do
      begin
        Status := 2;
        Message := 'Config Error at line ' + IntToStr(VJSONParser.Scanner.CurRow) +
          ',' + IntToStr(VJSONParser.Scanner.CurColumn + 1);
        Exit;
      end;
    end;
  finally
    FreeAndNil(VJSONData);
    FreeAndNil(VJSONParser);
    FreeAndNil(lst);
  end;

  Status := 0;
  Message := '';
  Filename := ConfigFileName;
  FIsValid := True;
  Result := True;
end;

procedure TMyConfig.SetDataValue(const APath: UnicodeString;
  const AValue: TJSONData);
var
  elName: UnicodeString;
  O: TJSONObject;
  i: integer;
begin
  if not Assigned(AValue) then
    Exit;
  // locate (and create) the parent object; the element itself is looked up
  // by name so that a node of any type gets replaced
  FindElement(StripSlash(APath), True, O, elName, True);
  if (not Assigned(O)) or (elName = '') then
    Exit;
  i := O.IndexOfName(UTF8Encode(elName));
  if i <> -1 then
    O.Delete(i);
  O.Add(UTF8Encode(elName), AValue.Clone);
  FModified := True;
end;

end.

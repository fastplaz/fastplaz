unit config_lib;

{$mode objfpc}{$H+}

interface

uses
  jsonConf, variants,
  Classes, SysUtils;

type

  { TMyConfig }

  TMyConfig = class(TJSONConfig)
  private
    function GetConfigValue(KeyName: WideString): variant;
    procedure SetConfigValue(KeyName: WideString; AValue: variant);
  public
    property Value[KeyName: WideString]: variant read GetConfigValue write SetConfigValue; default;
  end;

implementation

uses common;

{ TMyConfig }

function TMyConfig.GetConfigValue(KeyName: WideString): variant;
var
  s: WideString;
begin
  s := GetValue(KeyName, '');
  Result := s;
end;

procedure TMyConfig.SetConfigValue(KeyName: WideString; AValue: variant);
var
  s: WideString;
begin
  s := AValue;
  SetValue(KeyName, s);
end;

end.

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
    function GetConfigValue(KeyName: widestring): variant;
    procedure SetConfigValue(KeyName: widestring; AValue: variant);
  public
    property Value[KeyName: widestring]: variant read GetConfigValue write SetConfigValue;
      default;
  end;


implementation

uses common;

{ TMyConfig }

function TMyConfig.GetConfigValue(KeyName: widestring): variant;
var
  s: widestring;
begin
  s := GetValue(KeyName, '');
  Result := s;
end;

procedure TMyConfig.SetConfigValue(KeyName: widestring; AValue: variant);
var
  s: widestring;
begin
  s := AValue;
  SetValue(KeyName, s);
end;

end.

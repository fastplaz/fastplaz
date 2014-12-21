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
    function GetConfigValue(KeyName: string): variant;
    procedure SetConfigValue(KeyName: string; AValue: variant);
  public
    property Value[KeyName: string]: variant read GetConfigValue write SetConfigValue;
      default;
  end;


implementation

uses common;

{ TMyConfig }

function TMyConfig.GetConfigValue(KeyName: string): variant;
var
  s: string;
begin
  s := GetValue(KeyName, '');
  Result := s;
end;

procedure TMyConfig.SetConfigValue(KeyName: string; AValue: variant);
var
  s: string;
begin
  s := AValue;
  SetValue(KeyName, s);
end;

end.

unit modvar_util;

{$mode objfpc}{$H+}

{
  USAGE:

    GET:
    s := ModVar['modulename/varname'];

    SET:
    ModVar['modulename/varname'] := 'value';

}

interface

uses
  common, modvar_model, serialize_lib,
  Classes, SysUtils;

const
  {$include '../../../define_cms.inc'}

type

  { TModvarUtil }

  TModvarUtil = class(TModvarModel)
  private
    function GetValue(Name: string): variant;
    procedure SetValue(Name: string; AValue: variant);
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;

    property Values[Name: string]: variant read GetValue write SetValue; default;
  end;

var
  ModVar: TModvarUtil;

implementation

{ TModvarUtil }

function TModvarUtil.GetValue(Name: string): variant;
var
  lst : TStrings;
  modname, varname, varvalue : string;
  varresult : variant;
begin
  Result := '';
  modname:= 'system';
  varname := Name;
  lst := Explode( Name, '/');
  if lst.Count > 1 then
  begin
    modname:= lst[0];
    varname:= lst[1];
  end;
  varvalue := GetCustom( modname, varname, '');
  Result := unserialize( varvalue);
  FreeAndNil( lst);
end;

procedure TModvarUtil.SetValue(Name: string; AValue: variant);
var
  lst : TStrings;
  modname, varname, varvalue : string;
begin
  modname:= 'system';
  varname := Name;
  lst := Explode( Name, '/');
  if lst.Count > 1 then
  begin
    modname:= lst[0];
    varname:= lst[1];
  end;
  varvalue := serialize( AValue);
  SetCustom( modname, varname, varvalue);
  FreeAndNil( lst);
end;

constructor TModvarUtil.Create(const DefaultTableName: string);
begin
  inherited Create(DefaultTableName); // table name = users
end;

destructor TModvarUtil.Destroy;
begin
  inherited Destroy;
end;


initialization
  ModVar := TModvarUtil.Create();

finalization
  FreeAndNil(ModVar);

end.

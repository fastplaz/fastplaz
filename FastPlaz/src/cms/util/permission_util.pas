unit permission_util;

{$mode objfpc}{$H+}

interface

uses
  grouppermission_model,
  Classes, SysUtils;

const
  {$include define_cms.inc}


type

  { TPermissionUtil }

  TPermissionUtil = class(TGroupPermissionModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;


    function checkPermission(Component: string = ''; Instance: string = '';
      Level: integer = ACCESS_NONE; UserID: integer = 0): boolean;
  end;

implementation

uses
  common, fastplaz_handler, security_util;


{ TPermissionUtil }

constructor TPermissionUtil.Create(const DefaultTableName: string);
begin
  inherited Create(DefaultTableName); // table name = users
end;

destructor TPermissionUtil.Destroy;
begin
  inherited Destroy;
end;


function TPermissionUtil.checkPermission(Component: string; Instance: string;
  Level: integer; UserID: integer): boolean;
begin
  Result := False;
  if UserID = 0 then
  begin
    // next: get default userid from session

    Exit;
  end;

  Result := (getSecurityLevel(UserID, Component, Instance) >= Level);
end;

end.


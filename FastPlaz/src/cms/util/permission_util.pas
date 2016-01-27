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
    function getLoggedInUserID: longint;
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

function TPermissionUtil.getLoggedInUserID: longint;
var
  uid: string;
begin
  Result := 0;
  if SessionController.IsTerminated then
    Exit;
  if SessionController.IsExpired then
  begin
    Exit;
  end;

  uid := _SESSION['uid'];
  if uid <> '' then  //-- simple check
    Result := s2i(uid);
end;

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
    UserID:= getLoggedInUserID;
  end;

  Result := (getSecurityLevel(UserID, Component, Instance) >= Level);
end;

end.


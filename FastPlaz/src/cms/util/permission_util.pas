unit permission_util;

{$mode objfpc}{$H+}

interface

uses
  grouppermission_model,
  Classes, SysUtils;

type

  { TPermissionUtil }

  TPermissionUtil = class(TGroupPermissionModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;
  end;

implementation

{ TPermissionUtil }

constructor TPermissionUtil.Create(const DefaultTableName: string);
begin
  inherited Create(DefaultTableName); // table name = users
end;

destructor TPermissionUtil.Destroy;
begin
  inherited Destroy;
end;

end.


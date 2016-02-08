unit group_util;

{$mode objfpc}{$H+}

interface

uses
  group_model,
  Classes, SysUtils;

const
  GROUP_TYPE_CORE = 0;
  GROUP_TYPE_PUBLIC = 1;
  {$include '../../../define_cms.inc'}

type

  { TGroupsUtil }

  TGroupsUtil = class(TGroupModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;
    function Add(Name: string; Description: string = '';
      GroupType: integer = GROUP_TYPE_CORE; Prefix: string = ''): boolean;
  end;

implementation

uses
  fastplaz_handler;

{ TGroupsUtil }

constructor TGroupsUtil.Create(const DefaultTableName: string);
begin
  inherited Create(DefaultTableName); // table name = users
end;

destructor TGroupsUtil.Destroy;
begin
  inherited Destroy;
end;

function TGroupsUtil.Add(Name: string; Description: string; GroupType: integer;
  Prefix: string): boolean;
begin
  New;
  SetFieldValue(GROUP_FIELD_NAME, Name);
  SetFieldValue(GROUP_FIELD_TYPE, GroupType);
  SetFieldValue(GROUP_FIELD_DESCRIPTION, Description);
  SetFieldValue(GROUP_FIELD_PREFIX, Prefix);
  SetFieldValue(GROUP_FIELD_STATE, 0);
  SetFieldValue(GROUP_FIELD_NUMBERUSER, 0);
  SetFieldValue(GROUP_FIELD_NUMBERUSER_MAX, 0);
  SetFieldValue(GROUP_FIELD_LINK, 0);
  SetFieldValue(GROUP_FIELD_MASTER, 0);
  Result := Save();
end;

end.


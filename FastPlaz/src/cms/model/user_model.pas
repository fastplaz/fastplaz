unit user_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

type

  { TUserModel }

  TUserModel = class(TSimpleModel)
  private
  public
    constructor Create(const DefaultTableName: string = '');
  end;

implementation

constructor TUserModel.Create(const DefaultTableName: string = '');
begin
  inherited Create(DefaultTableName); // table name = users
end;

end.


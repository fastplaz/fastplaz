unit fastplaz_tools_register;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf,
  Classes, SysUtils;

const
  FastPlaz = 'FastPlaz';

Procedure Register;
Procedure log( const Msg:string);
function ucwords( const str:string):string;

var
  bCreateProject : boolean = false;

implementation

uses modsimple_lib, model_lib, project_lib;

function ucwords( const str:string):string;
var
  i : integer;
  s : string;
begin
  s := ' ' + lowerCase( str);
  for i:=1 to Length(s) do
  begin
    if s[i] = ' ' then
      s[i+1] := upcase(s[i+1]);
  end;
  Result := trim( s);
end;

procedure log(const Msg: string);
begin
  IDEMessagesWindow.AddMsg('FastPlaz : '+Msg,'',0,Nil);
end;

Procedure Register;
begin
  //RegisterUnit('fastplaz_tools_register', @fastplaz_tools_register.Register);
  RegisterNewItemCategory(TNewIDEItemCategory.Create( FastPlaz));
  RegisterProjectDescriptor(TFileDescProject.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescDefaultModule.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescModel.Create, FastPlaz);
end;


initialization
  //RegisterPackage('FastPlaz', @Register);


end.


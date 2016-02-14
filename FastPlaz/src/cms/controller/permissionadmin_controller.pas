unit permissionadmin_controller;

{$mode objfpc}{$H+}

interface

uses
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, user_util,
  database_lib, security_util, permission_util;

const
  ADMIN_PERMISSION_ROUTE =
    '^(admin)-(permission)-(list|view|add|edit|delete|data|activate|deactivate|suspend|resetpassword)/?$';
  PERMISSIONADMIN_TITLE = 'Permission';
  PERMISSIONADMIN_DESCRIPTION = 'Permission Rule List';

type

  { TPermissionAdminModule }

  TPermissionAdminModule = class(TMyCustomWebModule)
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    function UpdateSequence( const Order:string):string;
    function Tag_LevelInfo_Handler(const TagName: string;
      Params: TStringList): string;
    function Tag_ModInfo_Handler(const TagName: string;
      Params: TStringList): string;
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    User: TUserUtil;
    Permission: TPermissionUtil;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

  end;

implementation

uses theme_controller, common, logutil_lib;

constructor TPermissionAdminModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  VisibleModuleName := 'permissionadmin';

  User := TUserUtil.Create();
  Permission := TPermissionUtil.Create();
  OnRequest := @RequestHandler;
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TPermissionAdminModule.Destroy;
begin
  if Assigned(User) then
    FreeAndNil(User);
  FreeAndNil(Permission);
  inherited Destroy;
end;

procedure TPermissionAdminModule.RequestHandler(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  DataBaseInit;
  LanguageInit;
  QueryExec('SET CHARACTER SET utf8;');

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
  Tags['levelinfo'] := @Tag_LevelInfo_Handler;
  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

function TPermissionAdminModule.UpdateSequence(const Order: string): string;
const
  SQL_ORDER_UPDATE = 'UPDATE group_perms SET sequence=%d WHERE pid=%s';
var
  lst1, lst2 : TStrings;
  sql : string;
  i : integer;
begin
  if Order = '' then
    Exit;
  Result := 'OK';
  lst1 := Explode( Order, ',');
  for i:=0 to lst1.Count-1 do
  begin
    lst2 := Explode( lst1[i], '-');
    sql := Format( SQL_ORDER_UPDATE, [ i+2, lst2[1]]);
    QueryExec( sql);
    lst2.Free;
  end;
  lst1.Free;
end;

function TPermissionAdminModule.Tag_LevelInfo_Handler(const TagName: string;
  Params: TStringList): string;
var
  s: string;
  level: integer;
begin
  Result := '-';
  s := Params.Values['var'];
  s := ThemeUtil.VarValue[s];
  level := s2i(s);
  case level of
    ACCESS_NONE: Result := 'No Access';
    ACCESS_READ: Result := 'Read';
    ACCESS_ADD: Result := 'Add';
    ACCESS_EDIT: Result := 'Edit';
    ACCESS_DELETE: Result := 'Delete';
    ACCESS_COMMENT: Result := 'Comment';
    ACCESS_ADMIN: Result := 'Adnin';
  end;
end;

function TPermissionAdminModule.Tag_ModInfo_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case Params.Values['type'] of
    'title': Result := PERMISSIONADMIN_TITLE;
    'description': Result := PERMISSIONADMIN_DESCRIPTION;
  end;
end;

function TPermissionAdminModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
var
  action: string;
begin
  Result := '';

  if isPost then
  begin
    die( UpdateSequence( _POST['order']) );
  end;

  Permission.AddJoin('groups', 'gid', 'group_perms.gid', ['name']);
  Permission.Find(['1=1'], 'sequence');
  ThemeUtil.Assign('$Permission', @Permission.Data);

  if not isPost then
    Result := ThemeUtil.RenderFromContent(@TagController, Result,
      'modules/permission/view/' + _REQUEST['$3'] + '.html');
end;

procedure TPermissionAdminModule.BeforeRequestHandler(Sender: TObject;
  ARequest: TRequest);
begin
end;

initialization
  // -> http://yourdomainname/user
  // The following line should be moved to a file "routes.pas"
{
  Add this to route.pas
  Route.Add( ADMIN_PERMISSION_ROUTE, TPermissionAdminModule);
}
end.

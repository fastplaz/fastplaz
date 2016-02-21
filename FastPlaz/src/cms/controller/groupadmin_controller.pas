unit groupadmin_controller;

{$mode objfpc}{$H+}

interface

uses
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib,
  user_util, group_util, modvar_util,
  database_lib, security_util, user_controller;

const
  ADMIN_GROUP_ROUTE_REGEX = '^(admin)-(group)-(list|view|add|edit|data|delete)/?$';
  ADMIN_GROUP_URL = 'admin-group-list';
  ADMIN_GROUP_TITLE = 'Groups';
  ADMIN_GROUP_DESCRIPTION = 'Administration';
//{$include '../../../define_cms.inc'}

type

  { TGroupAdminModule }

  TGroupAdminModule = class(TMyCustomWebModule)
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    function setOutput(code: integer; message: string; url: string = ''): string;
    function Tag_ModInfo_Handler(const TagName: string;
      Params: TStringList): string;
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    function Tag_GroupList_Handler(const TagName: string;
      Params: TStringList): string;

    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function SearchHandler(Keyword: string; RequestHeader: TRequest): string;

    function Data: string;
  public
    Group: TGroupsUtil;
    User: TUserUtil;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    function Add: string;
    function Edit: string;
    function Delete: string;
  end;

implementation

uses theme_controller, common, logutil_lib;

constructor TGroupAdminModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  VisibleModuleName := 'groupadmin';

  DataBaseInit;
  LanguageInit;
  QueryExec('SET CHARACTER SET utf8;');

  Group := TGroupsUtil.Create();
  User := TUserUtil.Create();
  OnRequest := @RequestHandler;
  OnSearch := @SearchHandler;
  BeforeRequest := @BeforeRequestHandler;
  Tags['grouplist'] := @Tag_GroupList_Handler;
end;

destructor TGroupAdminModule.Destroy;
begin
  inherited Destroy;
  if Assigned(Group) then
    Group.Free;
end;

function TGroupAdminModule.Add: string;
var
  groupId: integer;
  groupName, description: string;
begin
  Result := '';
  if not isPost then
    Exit;

  groupName := trim(_POST['name']);
  description := trim(_POST['description']);
  if (groupName = '') or (description = '') then
  begin
    Result := setOutput(1, MSG_VALUE_INVALID);
    Exit;
  end;

  groupId := Group.Add(groupName, description);
  if groupId = 0 then
    Result := setOutput(3, MSG_GROUP_ADD_FAILED)
  else
  begin
    Result := setOutput(0, OK);
  end;
end;

function TGroupAdminModule.Edit: string;
var
  gid: integer;
begin
  if isPost then
  begin
    gid := s2i(_POST['id']);
    if gid = 0 then
      Result := setOutput(1, MSG_ID_INVALID);
    if not Group.Find(gid) then
      Result := setOutput(1, MSG_GROUP_NOTEXISTS);

    Group[GROUP_FIELD_NAME] := _POST['name'];
    Group[GROUP_FIELD_DESCRIPTION] := _POST['description'];
    if Group.Save(GROUP_FIELD_ID + '=' + i2s(gid)) then
      Result := setOutput(0, OK)
    else
      Result := setOutput(1, MSG_GROUP_EDIT_FAILED);
  end
  else
  begin //-- GET
    Result := '';
    gid := s2i(_GET['id']);
    if gid = 0 then
      Redirect(BaseURL + 'admin-group-list');

    if not Group.Find(gid) then
      Redirect(BaseURL + 'admin-group-list', MSG_GROUP_NOTEXISTS);

    ThemeUtil.Assign('id', Group['gid']);
    ThemeUtil.Assign('name', Group['name']);
    ThemeUtil.Assign('description', Group['description']);
  end;

end;

function TGroupAdminModule.Delete: string;
var
  gid: integer;
begin
  Result := '';
  if not isPost then
    Exit;

  gid := s2i(_POST['gid']);
  if gid = 2 then
  begin
    Result := setOutput(-1, MSG_USER_DELETE_FAILED);
    Exit;
  end;

  if Group.SafeDelete(gid, User.UserIdLoggedIn) then
  begin
    Result := setOutput(0, OK);
  end
  else
  begin
    Result := setOutput(-1, MSG_GROUP_DELETE_FAILED);
  end;
end;

procedure TGroupAdminModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  if not User.isLoggedIn then
  begin
    FreeAndNil(User);
    Redirect(BaseURL + USER_URL_LOGIN + '?url=' + ADMIN_GROUP_URL);
  end;

  if not User.checkPermission('group', 'group', ACCESS_ADD) then
  begin
    FreeAndNil(User);
    Redirect(BaseURL + 'admin');
  end;

  if _GET['$3'] = 'data' then
  begin
    if not isAjax then
    begin
      Handled := True;
      Exit;
    end;
    Response.ContentType := 'application/json';
    Response.Content := Data;
    Handled := True;
    Exit;
  end;

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

function TGroupAdminModule.setOutput(code: integer; message: string;
  url: string): string;
var
  o, oData: TJSONObject;
  csrf: string;
begin
  o := TJSONObject.Create();
  o.Add('code', code);
  o.Add('msg', message);
  if url <> '' then
    o.Add('url', url);
  o.Add('loadtime', ThemeUtil.GetDebugInfo('time'));
  with TSecurityUtil.Create do
  begin
    csrf := GenerateCSRF('usertable');
    o.Add('token', csrf);
    _SESSION[CSRFTOKEN_KEY] := csrf;
    Free;
  end;
  Result := JsonFormatter(o.AsJSON);
  FreeAndNil(o);
end;

function TGroupAdminModule.Tag_ModInfo_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case Params.Values['type'] of
    'title': Result := ADMIN_GROUP_TITLE;
    'description': Result := ADMIN_GROUP_DESCRIPTION;
  end;
end;

function TGroupAdminModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case _GET['act'] of
    'add': Result := Add;
    'edit': Result := Edit;
    'delete': Result := Delete;
  end;

  if not isPost then
    Result := ThemeUtil.RenderFromContent(@TagController, Result,
      'modules/groups/view/' + _REQUEST['$3'] + '.html');
  if isAjax then
  begin
    Response.ContentType := 'application/json';
    die(Result);
  end;
end;

function TGroupAdminModule.Tag_GroupList_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  if not Group.Find(['isnull(deleted_by)']) then
    Exit;

  Result := HTMLUtil.AddSelectLTE(Params.Values['id'], Params.Values['label'],
    Group.Data, 'gid', 'name');
end;

procedure TGroupAdminModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
end;

function TGroupAdminModule.SearchHandler(Keyword: string;
  RequestHeader: TRequest): string;
const
  ADMIN_GROUP_SEARCHRESULT = '<li><a href="%s">%s</a></li>';
var
  result_content: TStringList;
  url: string;
begin
  Result := '';
  if not User.checkPermission('group', 'group', ACCESS_ADD) then
    Exit;
  if Group.Find(['name LIKE "%' + Keyword + '%"']) then
  begin
    result_content := TStringList.Create;
    result_content.Add('<ul>');
    repeat
      url := Group['gid'];
      url := BaseURL + 'admin/group/view/' + url;
      result_content.Add(Format(ADMIN_GROUP_SEARCHRESULT, [url, Group['name']]));
      Group.Next;
    until Group.EOF;
    result_content.Add('</ul>');

    Result := ThemeUtil.RenderFromContent(@TagController, result_content.Text,
      'modules/groups/view/search_result.html');
    result_content.Free;
  end;
end;

function TGroupAdminModule.Data: string;
var
  o, oData: TJSONObject;
  sql: string;

  index, limit, search, where: string;
  dataCount: integer;
begin
  where := '1=1 AND isnull( deleted_by) ';
  index := _GET['start'];
  limit := _GET['length'];
  search := _GET['search[value]'];
  if search <> '' then
  begin
    where := where + ' AND (name LIKE "%' + search + '%")';
  end;
  if limit <> '' then
  begin
    limit := ' LIMIT ' + index + ', ' + limit;
  end;

  o := TJSONObject.Create;
  oData := TJSONObject.Create;

  o.Add('code', 0);
  o.Add('draw', 0);

  sql := 'SELECT gid, name, gtype, description, state, nbuser, "" as action_col FROM groups u WHERE '
    + where + ' ORDER BY name ' + limit;

  if QueryOpenToJson(sql, oData, False) then
  begin
    dataCount := oData['count'].AsInteger;
    o.Add('recordsFiltered', dataCount);
    o.Add('recordsTotal', dataCount);
    o.Add('data', oData['data']);
  end;
  {$IFDEF DEBUG}
  if ((AppData.debug) and (AppData.debugLevel <= 2)) then
  begin
    o.Add('sql', sql);
  end;
  {$ENDIF}

  Result := JsonFormatter(o.AsJSON);
  FreeAndNil(o);
end;


initialization
  // -> http://yourdomainname/user
  // The following line should be moved to a file "routes.pas"
{
  Add this to route.pas
  Route.Add( ADMIN_GROUP_ROUTE_REGEX, TGroupAdminModule);
}
end.

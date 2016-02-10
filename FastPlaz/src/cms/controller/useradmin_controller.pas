unit useradmin_controller;

{$mode objfpc}{$H+}

interface

uses
  fpjson, logutil_lib, datetime_lib, security_util, mailer_lib,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib,
  database_lib, user_util, user_model;

const
  ADMIN_USER_ROUTE =
    '^(admin)-(user)-(list|view|add|edit|delete|data|activate|deactivate|suspend|resetpassword)/?$';
  ADMIN_USER_TITLE = 'User Management';
  ADMIN_USER_DESCRIPTION = '';
  ADMIN_USER_URL = 'admin-user-list';
  ADMIN_USER_PENDING_URL = 'admin-user-pending';
  ADMIN_USER_MENU_TITLE = 'User & Permission';
//{$include '../../../define_cms.inc'}

type

  { TUserAdminModule }

  TUserAdminModule = class(TMyCustomWebModule)
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    demo: boolean;
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    function Tag_ModInfo_Handler(const TagName: string;
      Params: TStringList): string;

    function setOutput(code: integer; message: string; url: string = ''): string;
    function defaultPostCheck(Level: integer = ACCESS_NONE;
      ForceDie: boolean = True): string;

    function OnMenuHandler(ARequest: TRequest): string;
    function OnSearchHandler(Keyword: string; RequestHeader: TRequest): string;
    function OnNotificationHandler(NotifType: string; ARequest: TRequest): string;

    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    User: TUserUtil;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    function Add: string;
    function ResetPassword: string;
    function Edit: string;
    function Data: string;

    function Delete: string;
    function Activate: string;
    function DeActivate: string;
    function Suspend: string;
  end;

implementation

uses theme_controller, common, modvar_util;

constructor TUserAdminModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);

  DataBaseInit;
  LanguageInit;
  QueryExec('SET CHARACTER SET utf8;');

  demo := s2b(ModVar['system/demo']);

  User := TUserUtil.Create();
  OnRequest := @RequestHandler;
  OnMenu := @OnMenuHandler;
  OnSearch := @OnSearchHandler;
  OnNotification := @OnNotificationHandler;
  BeforeRequest := @BeforeRequestHandler;
  ThemeUtil.Layout := 'admin';
end;

destructor TUserAdminModule.Destroy;
begin
  inherited Destroy;
  FreeAndNil(User);
end;

function TUserAdminModule.Add: string;
var
  code: integer;
  token, email, pass1, pass2, group: string;
  active: boolean;
  params: TStringList;
begin
  Result := '';
  if not isPost then
    Exit;

  //--
  code := -1;
  email := trim(_POST['email']);
  pass1 := trim(_POST['password']);
  pass2 := trim(_POST['password2']);
  group := trim(_POST['group']);
  if _POST['active'] = 'on' then
    active := True
  else
    active := False;

  if (email = '') or (pass1 = '') or (pass2 = '') then
  begin
    Result := setOutput(1, MSG_VALUE_INVALID);
    Exit;
  end;
  if pass1 <> pass2 then
  begin
    Result := setOutput(3, MSG_PASSWORD_INVALID);
    Exit;
  end;

  if User.isEmailExists(email) then
  begin
    Result := setOutput(2, MSG_USER_EXISTS);
    Exit;
  end;

  params := TStringList.Create;
  if active then
    params.Add(USER_FIELDNAME_ACTIVATED + '=1');
  code := User.Add(email, pass1, params);
  if code = 0 then
    Result := setOutput(3, MSG_USER_ADD_FAILED)
  else
  begin
    Result := setOutput(0, OK);
    if s2i(group) > 0 then
      User.AssignToGroup(code, s2i(group));

    //-- TODO: send welcome email
    with TMailer.Create() do
    begin
      AddTo(email);
      Subject := 'Welcome Email - ' + AppData.sitename;

      //---

      Free;
    end;

  end;
  FreeAndNil(params);

end;

function TUserAdminModule.Delete: string;
var
  uid: integer;
  email: string;
begin
  Result := defaultPostCheck(ACCESS_ADD);
  if Result <> '' then
    Exit;

  uid := s2i(_POST['uid']);

  //-- for demo only
  if demo then
  begin
    if uid = 2 then
    begin
      Result := setOutput(-1, MSG_DEMO_FAILEDDELETEUSERADMIN);
      exit;
    end;
  end;


  if User.SafeDelete(uid, User.UserIdLoggedIn) then
  begin
    Result := setOutput(0, OK);
  end
  else
    Result := setOutput(-1, MSG_USER_DELETE_FAILED);
end;

function TUserAdminModule.ResetPassword: string;
var
  uid: integer;
begin
  Result := '';

  if isPost then
  begin
    uid := s2i(_POST['uid']);
    if User.ChangePassword(uid, _POST['password']) then
      Result := setOutput(0, 'OK', BaseURL + ADMIN_USER_URL)
    else
      Result := setOutput(-1, MSG_USER_RESETPASSWORD_FAILED);

    if _POST['sendemail'] = 'on' then
    begin
      // -- TODO: send reset password email
      with TMailer.Create() do
      begin
        AddTo(_POST['email']);
        Subject := 'Password Change - ' + AppData.sitename;

        // ....

        //Send;
        Free;
      end;
    end;
  end
  else
  begin // _GET
    uid := s2i(_GET['id']);

    if uid < 2 then
      Redirect(BaseURL + 'admin-user-list');

    //-- for demo only
    if demo then
    begin
      if uid = 2 then
        Redirect(BaseURL + 'admin-user-list', MSG_DEMO_NORESETPASSWORD);
    end;


    if not User.Find(uid) then
      Redirect(BaseURL + 'admin-user-list', MSG_USER_NOTEXISTS);

    ThemeUtil.Assign('uid', User['uid']);
    ThemeUtil.Assign('email', User['email']);
  end;
end;

function TUserAdminModule.Edit: string;
begin

end;

function TUserAdminModule.Data: string;
var
  o, oData, oTmp: TJSONObject;
  sql: string;

  index, limit, search, where: string;
  recordsFiltered, recordsTotal: integer;

begin
  Result := '';
  if not isAjax then
    Exit;
  where := 'uid<>1 AND isnull( deleted_by)';
  index := _GET['start'];
  limit := _GET['length'];
  search := _GET['search[value]'];
  if search <> '' then
  begin
    where := where + ' AND (email LIKE "%' + search + '%")';
  end;
  if limit <> '' then
  begin
    limit := ' LIMIT ' + index + ', ' + limit;
  end;

  o := TJSONObject.Create;
  oData := TJSONObject.Create;

  o.Add('code', 0);
  o.Add('draw', 0);

  sql := 'SELECT uid, name, uname, email, user_regdate, lastlogin, activated, "" as action_col FROM users u WHERE ' + where + limit;
  if QueryOpenToJson(sql, oData, False) then
  begin

    sql := 'SELECT count( uid) as recordstotal FROM users u WHERE uid<>1 AND isnull( deleted_by)';
    recordsTotal := User.RecordsTotalFiltered(sql);
    recordsFiltered := oData['count'].AsInteger;

    o.Add('recordsFiltered', recordsFiltered);
    o.Add('recordsTotal', recordsTotal);
    o.Add('data', oData['data']);
    o.Add('loadtime', ThemeUtil.GetDebugInfo('time'));
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

function TUserAdminModule.Activate: string;
var
  uid: integer;
  email: string;
begin
  Result := defaultPostCheck(ACCESS_ADD);
  if Result <> '' then
    Exit;

  uid := s2i(_POST['uid']);
  email := User.UserInfo[USER_FIELDNAME_EMAIL];

  if User.Activate(uid) then
  begin
    Result := setOutput(0, 'OK');

    // -- TODO: send activation email
    with TMailer.Create() do
    begin
      AddTo(email);
      Subject := 'Activation Email - ' + AppData.sitename;

      // ....

      //Send;
      Free;
    end;

  end
  else
    Result := setOutput(-1, MSG_USER_ACTIVATE_FAILED);
end;

function TUserAdminModule.DeActivate: string;
var
  uid: integer;
begin
  Result := defaultPostCheck(ACCESS_ADD);
  if Result <> '' then
    Exit;

  uid := s2i(_POST['uid']);
  if User.DeActivate(uid) then
  begin
    Result := setOutput(0, 'OK');
  end
  else
    Result := setOutput(-1, MSG_USER_DEACTIVATE_FAILED);
end;

function TUserAdminModule.Suspend: string;
var
  uid: integer;
  email: string;
begin
  Result := defaultPostCheck(ACCESS_ADD);
  if Result <> '' then
    Exit;

  uid := s2i(_POST['uid']);

  //-- for demo only
  if demo then
  begin
    if uid = 2 then
    begin
      Result := setOutput(-1, MSG_DEMO_FAILEDSUSPEND);
      exit;
    end;
  end;


  if User.Suspend(uid) then
  begin
    Result := setOutput(0, 'OK');
  end
  else
    Result := setOutput(-1, MSG_USER_SUSPEND_FAILED);
end;

procedure TUserAdminModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  if not User.isLoggedIn then
  begin
    FreeAndNil(User);
    if not isAjax then
      Redirect(BaseURL + USER_URL_LOGIN + '?url=' + ADMIN_USER_URL);
    FlashMessages := MSG_NOLOGIN_OR_SESSIONEXPIRED;
    Response.Content := setOutput(ERR_REDIRECT, MSG_NOLOGIN_OR_SESSIONEXPIRED,
      BaseURL + USER_URL_LOGIN + '?url=' + ADMIN_USER_URL);
    Die;
  end;

  if not User.checkPermission('user', 'user', ACCESS_ADD) then
  begin
    FreeAndNil(User);
    if not isAjax then
      Redirect(BaseURL + 'admin');
    Response.Content := setOutput(ERR_REDIRECT, MSG_NOPERMISSION, BaseURL + 'admin');
    Die;
  end;

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
  ThemeUtil.TrimWhiteSpace := False;
  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

function TUserAdminModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case _GET['act'] of
    'add': Result := Add;
    'resetpassword': Result := ResetPassword;
    'activate': Result := Activate;
    'deactivate': Result := DeActivate;
    'delete': Result := Delete;
    'suspend': Result := Suspend;
    'data':
    begin
      Response.ContentType := 'application/json';
      Die(Data);
    end;
  end;

  if not isPost then
    Result := ThemeUtil.RenderFromContent(@TagController, Result,
      'modules/users/view/' + _REQUEST['$3'] + '.html');
  if isAjax then
  begin
    Response.ContentType := 'application/json';
    die(Result);
  end;

end;

function TUserAdminModule.Tag_ModInfo_Handler(const TagName: string;
  Params: TStringList): string;
begin
  case Params.Values['type'] of
    'title': Result := ADMIN_USER_TITLE;
    'description': Result := ADMIN_USER_DESCRIPTION;
    else
      Result := '';
  end;
end;

function TUserAdminModule.setOutput(code: integer; message: string;
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

function TUserAdminModule.defaultPostCheck(Level: integer; ForceDie: boolean): string;
var
  uid: integer;
begin
  Result := setOutput(1, 'Failed.');
  ;
  if (not isAjax) or (not isPost) then
    Exit;

  if not User.checkPermission('user', 'user', Level) then
  begin
    Result := setOutput(1, MSG_NOPERMISSION);
    Exit;
  end;

  //-- TODO: check token first

  //-- check post uid
  uid := s2i(_POST['uid']);
  if uid = 0 then
  begin
    Result := setOutput(1, MSG_ID_INVALID);
    Exit;
  end;

  if not User.Find(uid) then
  begin
    Result := setOutput(1, MSG_USER_NOTEXISTS);
    Exit;
  end;

  Result := '';
end;

function TUserAdminModule.OnMenuHandler(ARequest: TRequest): string;
var
  o, u, subitems: TJSONObject;
  items: TJSONArray;
  pending_user: integer;

begin
  Result := '';
  if not User.isLoggedIn then
    Exit;
  if not User.checkPermission('user', 'user', ACCESS_ADD) then
    Exit;

  pending_user := User.PendingCount;

  o := TJSONObject.Create;
  items := TJSONArray.Create;
  u := TJSONObject.Create;
  subitems := TJSONObject.Create;
  o.Add('title', ADMIN_USER_MENU_TITLE);
  o.Add('icon', 'fa-user-secret');
  if pending_user > 0 then
    o.Add('right-label', '<span class="label pull-right bg-yellow">' +
      i2s(pending_user) + '</span>');
  if isActive then
    o.Add('active', 1);

  if User.checkPermission('user', 'user', ACCESS_ADD) then
  begin
    u.Add('title', 'Users');
    u.Add('icon', 'fa-user');
    subitems.Add('list', User.AddMenu('List', 'fa:fa-users',
      'admin-user-list', '', True, '#user-content'));

    if User.checkPermission('user', 'user', ACCESS_ADD) then
      subitems.Add('add', User.AddMenu('Add', 'fa:fa-plus',
        'admin-user-add', '', True, '#user-content'));

    if pending_user > 0 then
      subitems.Add('pending', User.AddMenu('Pending', 'fa:fa-list',
        ADMIN_USER_PENDING_URL, '<span class="label pull-right bg-yellow">' +
        i2s(pending_user) + '</span>'));
    u.Add('items', subitems);
    items.Add(u);
  end;

  if User.checkPermission('user', 'group', ACCESS_READ) then
    items.Add(User.AddMenu('Groups', 'fa:fa-users', 'admin-group-list', '',
      True, '#userlist-content'));
  if User.checkPermission('user', 'permission', ACCESS_READ) then
    items.Add(User.AddMenu('Permission', 'fa:fa-key', 'admin-permission-list'));

  o.Add('items', items);

  //  Result :=
  //    '{ "title" : "User & Permission", "icon" : "fa-user-secret", "right-label" : "3<\/span>", "active" : 1, "items" : [{ "title" : "Users", "icon" : "fa-user", "items" : { "list" : { "title" : "List", "icon" : "fa:fa-users", "url" : "admin-user-list", "ajax" : "1", "rel" : "#userlist-content" }, "add" : { "title" : "Add", "icon" : "fa:fa-plus", "url" : "admin-user-list", "ajax" : "1", "rel" : "#userlist-content" }, "pending" : { "title" : "Pending", "icon" : "fa:fa-list", "url" : "admin\/user\/pending", "right-label" : "3<\/span>" } } }, { "title" : "Groups", "icon" : "fa:fa-users", "url" : "admin\/group\/list" }, { "title" : "Permission", "icon" : "fa:fa-key", "url" : "admin\/permission\/list" }] }'

  Result := (o.AsJSON);
  FreeAndNil(o);
end;

function TUserAdminModule.OnSearchHandler(Keyword: string;
  RequestHeader: TRequest): string;
const
  ADMIN_USER_SEARCHRESULT = '<li><a href="%s">%s</a>, last login %s</li>';
var
  result_content: TStringList;
  url: string;
  regDate, loginDate: string;
begin
  Result := '';
  if not User.checkPermission('user', 'user', ACCESS_ADD) then
    Exit;

  if User.Find(['email LIKE "%' + Keyword + '%"']) then
  begin
    result_content := TStringList.Create;
    result_content.Add('<ul>');
    repeat
      url := User['uid'];
      url := BaseURL + 'admin/user/view/' + url;
      regDate := DateTimeHuman(User[USER_FIELDNAME_REGDATE]);
      loginDate := UNKNOWN;
      try
        loginDate := DateTimeHuman(User[USER_FIELDNAME_LASTLOGIN]);
      except
      end;
      result_content.Add(Format(ADMIN_USER_SEARCHRESULT,
        [url, User[USER_FIELDNAME_EMAIL], loginDate]));
      User.Next;
    until User.EOF;
    result_content.Add('</ul>');

    Result := ThemeUtil.RenderFromContent(@TagController, result_content.Text,
      'modules/users/view/search_result.html');
    result_content.Free;
  end;
end;

function TUserAdminModule.OnNotificationHandler(NotifType: string;
  ARequest: TRequest): string;
var
  notif: TJSONObject;
  items: TJSONArray;
  pendingUser: integer;
begin
  Result := '';
  if not User.checkPermission('user', 'user', ACCESS_ADD) then
    Exit;

  pendingUser := User.PendingCount;
  if pendingUser = 0 then
    Exit;

  notif := TJSONObject.Create;
  items := TJSONArray.Create;
  items.Add(HTMLUtil.AddNotif(0, Format(MSG_PENDINGUSER, [pendingUser]),
    'fa:fa-user', BaseURL + ADMIN_USER_URL));
  notif.Add('items', items);
  Result := (notif.AsJSON);
  notif.Free;
end;

procedure TUserAdminModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin

end;



initialization
{

  Add this to route.pas
  Route.Add(ADMIN_USER_ROUTE, TUserAdminModule);
}

end.

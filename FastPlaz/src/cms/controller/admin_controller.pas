unit admin_controller;

{$mode objfpc}{$H+}

interface

uses
  fphttp, fpjson, jsonparser, httpprotocol,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, modvar_util,
  user_util, user_controller,
  json_lib,
  database_lib, security_util;

const
  ADMINMENU_TITLE = 'System';
  ADMINMENU_FORMAT_TITLE = '<a href="#"><i class="fa %s"></i><span>%s</span>%s</a>';
  ADMINMENU_FORMAT_SUBMENU_FIRST = '<ul class="treeview-menu">';
  ADMINMENU_FORMAT_SUBMENU_ITEM =
    '<li><a href="%s" class="%s" %s><i class="fa %s"></i> %s%s</a>';
  ADMINMENU_FORMAT_SUBMENU_RIGHTLABEL = '<i class="fa fa-angle-left pull-right"></i>';
  ADMINNOTIF_FORMAT_GENERAL =
    '<li><a href="%s"><i class="fa  %s text-aqua"></i>%s</a></li>';
  ADMINNOTIF_FORMAT_NOTIFCOUNT =
    '<script type="text/javascript">var NotificationCount = %d;</script>';

  ADMIN_DASHBOARD_TITLE = 'Admin';
  ADMIN_DASHBOARD_DESCRIPTION = 'Dashboard';

type

  { TAdminModule }

  TAdminModule = class(TMyCustomWebModule)
    function Tag_ModInfo_Handler(const TagName: string; Params: TStringList): string;
    function Tag_ModVar_Handler(const TagName: string; Params: TStringList): string;
    function Tag_MenuAdmin(const TagName: string; Params: TStringList): string;
    function Tag_Notification(const TagName: string; Params: TStringList): string;
    function GenerateMenu(const JsonString: string): string;
    function GenerateMenuItem(J: TJSonData): string;
    function GenerateNotificationMenu(const JsonString: string): string;

    function DashboardAction: string;
    function GeneralSettingAction: string;
    function GeneralSettingSaveAction: string;

    function OnMenuHandler(ARequest: TRequest): string;
  private
    action: string;
    NotifCount: integer;
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);

    procedure Get; override;
    procedure Post; override;
  public
    User: TUserUtil;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses theme_controller, logutil_lib, common;

constructor TAdminModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  User := TUserUtil.Create();
  inherited CreateNew(AOwner, CreateMode);
  OnMenu := @OnMenuHandler;
  BeforeRequest := @BeforeRequestHandler;
  Tags['admin-menu'] := @Tag_MenuAdmin;
  Tags['admin-notification'] := @Tag_Notification;
end;

destructor TAdminModule.Destroy;
begin
  inherited Destroy;
  if Assigned(User) then
    FreeAndNil(User);
end;

function TAdminModule.Tag_ModInfo_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case Params.Values['type'] of
    'title': Result := ADMIN_DASHBOARD_TITLE;
    'description': Result := ADMIN_DASHBOARD_DESCRIPTION;
  end;
end;

function TAdminModule.Tag_ModVar_Handler(const TagName: string;
  Params: TStringList): string;
begin
  try
    Result := ModVar[Params.Values['key']];
    if Result = '' then
      Result := Params.Values['default'];
    if Params.Values['type'] = 'checkbox' then
    begin
      if Result = 'True' then
        Result := 'checked'
      else
        Result := '';
    end;
  except
  end;
end;

function TAdminModule.Tag_MenuAdmin(const TagName: string; Params: TStringList): string;
var
  html: TStringList;
  i: integer;
  s: string;
begin
  html := TStringList.Create;

  //-- loop every loaded modul
  for i := 0 to ModuleLoaded.Count - 1 do
  begin
    if ModuleLoaded[i].ModuleStore.OnMenu <> nil then
    begin
      s := ModuleLoaded[i].ModuleStore.OnMenu(Application.Request);
      s := GenerateMenu(trim(s));
      if (ModuleLoaded[i].ModuleStore.Name = 'admin') and (s <> '') then
      begin
        s := '<li class="header">MAIN SETTING</li>' + s;
      end;
      html.Add(s);
    end;
  end;//-- loop every loaded modul

  Result := html.Text;
  FreeAndNil(html);
end;

function TAdminModule.Tag_Notification(const TagName: string;
  Params: TStringList): string;
var
  html: TStringList;
  i: integer;
  s: string;
begin
  Result := '';

  html := TStringList.Create;
  NotifCount := 0;

  //-- loop every loaded modul
  for i := 0 to ModuleLoaded.Count - 1 do
  begin
    if ModuleLoaded[i].ModuleStore.OnNotification <> nil then
    begin
      s := ModuleLoaded[i].ModuleStore.OnNotification(
        Params.Values['type'], Application.Request);
      s := GenerateNotificationMenu(s);
      html.Add(s);
    end;
  end;//-- loop every loaded modul

  html.Add(Format(ADMINNOTIF_FORMAT_NOTIFCOUNT, [NotifCount]));
  Result := html.Text;
  FreeAndNil(html);
end;

function TAdminModule.GenerateMenu(const JsonString: string): string;
var
  d: TJSONData;
  i: integer;
  s, icon, right_label, divclass: string;
  html: TStringList;
begin
  Result := '';
  if JsonString = '' then
    Exit;
  html := TStringList.Create;
  try
    d := GetJSON(JsonString);
    icon := jsonGetString(d, 'icon');
    right_label := jsonGetString(d, 'right-label');
    if right_label = '' then
      right_label := '<i class="fa fa-angle-left pull-right"></i>';
    if jsonGetString(d, 'active') = '1' then
      divclass := 'active';
    s := Format(ADMINMENU_FORMAT_TITLE, [icon, jsonGetString(
      d, 'title'), right_label]);
    //right-label <span class="label pull-right bg-yellow">...</span>
    html.Add('<li class="treeview ' + divclass + '">');
    html.Add(s);

    i := TJSONObject(d).IndexOfName('items');
    html.Add(GenerateMenuItem(d.Items[i]));

    html.Add('</li>');
    d.Free;
  except
    on E: Exception do
    begin
      //      if AppData.debug then
      //        LogUtil.Add( e.Message, 'AdminMenu');
    end;
  end;

  Result := html.Text;
  html.Free;
end;

function TAdminModule.GenerateMenuItem(J: TJSonData): string;
var
  html: TStringList;
  i, n: integer;
  d: TJSONData;
  s, title, url, icon, right_label, rel, class_menu: string;
  tmp: TStrings;

begin
  Result := '';
  html := TStringList.Create;
  html.Add(ADMINMENU_FORMAT_SUBMENU_FIRST);

  for i := 0 to J.Count - 1 do
  begin
    url := '#';
    icon := '';
    right_label := '';
    class_menu := '';
    d := J.Items[i];
    title := jsonGetString(d, 'title');
    url := jsonGetString(d, 'url');
    rel := jsonGetString(d, 'rel');
    if rel <> '' then
      rel := ' rel="' + rel + '" ';
    if jsonGetString(d, 'ajax') = '1' then
      class_menu := 'ajaxcall';
    right_label := jsonGetString(d, 'right-label');

    //-- icon
    icon := jsonGetString(d, 'icon');
    tmp := Explode(icon, ':');
    if tmp.Count > 1 then
    begin
      if tmp[0] = 'fa' then
        icon := tmp[1];
    end;
    tmp.Free;

    if url = '' then //-- subsubmenu
    begin
      s := '';
      right_label := ADMINMENU_FORMAT_SUBMENU_RIGHTLABEL;
      url := '#';

      n := TJSONObject(d).IndexOfName('items');
      if n <> -1 then
      begin
        s := s + #13#10#13#10;
        s := s + GenerateMenuItem(d.Items[n]);
        s := s + #13#10#13#10;

        //s:='';
      end;

      s := Format(ADMINMENU_FORMAT_SUBMENU_ITEM, [url, class_menu,
        rel, icon, title, right_label]) + s + '</li>';
    end
    else // without subsubmenu
    begin
      url := BaseURL + url;
      s := Format(ADMINMENU_FORMAT_SUBMENU_ITEM, [url, class_menu,
        rel, icon, title, right_label]) + '</li>';
    end; //-- subsubmenu
    html.add(s);

    ;
  end;

  html.Add('</ul>');
  Result := html.Text;
  html.Free;
end;

function TAdminModule.GenerateNotificationMenu(const JsonString: string): string;
var
  d, items, item: TJSONData;
  i: integer;
  title, icon, url, s: string;
  html: TStringList;
  tmp: TStrings;
begin
  Result := '';
  html := TStringList.Create;
  try
    d := GetJSON(JsonString);
    items := d.Items[0];
    NotifCount := NotifCount + items.Count;
    for i := 0 to items.Count - 1 do
    begin
      item := items.Items[i];
      title := jsonGetString(item, 'title');
      icon := jsonGetString(item, 'icon');
      url := jsonGetString(item, 'url');

      tmp := Explode(icon, ':');
      if tmp.Count > 1 then
      begin
        if tmp[0] = 'fa' then
          icon := tmp[1];
      end;
      tmp.Free;

      s := Format(ADMINNOTIF_FORMAT_GENERAL, [url, icon, title]);
      html.Add(s);
    end;

    d.Free;
  except
    on E: Exception do
    begin
      //      if AppData.debug then
      //        LogUtil.Add( e.Message, 'AdminMenu');
    end;
  end;

  Result := html.Text;
  html.Free;
end;

function TAdminModule.DashboardAction: string;
begin
  Result := 'Admin Dashboard';
end;

function TAdminModule.GeneralSettingAction: string;
begin

  Result := '';
end;

function TAdminModule.GeneralSettingSaveAction: string;
var
  b: boolean;
  i: integer;
begin
  // main setting
  ModVar['system/sitename'] := _POST['sitename'];
  ModVar['system/description'] := _POST['description'];
  ModVar['system/admin_email'] := _POST['admin_email'];
  ModVar['system/demo'] := s2b(_POST['demo']);

  // error & debug
  ModVar['system/redirect_if_error'] := s2b(_POST['redirect_if_error']);
  ModVar['system/debug'] := s2b(_POST['debug']);
  ModVar['system/error_url'] := string(_POST['error_url']);

  // others
  ModVar['system/ajax_timeout'] := s2i(_POST['ajax_timeout']);

  Result := OK;
end;

function TAdminModule.OnMenuHandler(ARequest: TRequest): string;
var
  o, u, t, subitems: TJSONObject;
  items: TJSONArray;
begin
  Result := '';
  if not User.checkPermission('admin', 'admin', ACCESS_ADMIN) then
    Exit;

  o := TJSONObject.Create;
  items := TJSONArray.Create;
  o.Add('title', ADMINMENU_TITLE);
  o.Add('icon', 'fa-th');
  if isActive then
    o.Add('active', 1);

  items.Add(User.AddMenu('General Settings', 'fa:fa-circle-o',
    'admin?act=generalsettings'));
  items.Add(User.AddMenu('Mailer', 'fa:fa-circle-o', 'admin#'));
  items.Add(User.AddMenu('System Information', 'fa:fa-circle-o', 'admin-systeminfo'));


  // Tools Menu
  t := TJSONObject.Create;
  t.Add('title', 'Tools');
  t.Add('icon', 'fa-server');
  subitems := TJSONObject.Create;
  subitems.Add('cachecleanup', User.AddMenu('Cache CleanUp',
    'fa:fa-trash', 'admin?act=cachecleanup', '', True, 'section.content'));
  subitems.Add('sessioncleanup', User.AddMenu('Session CleanUp',
    'fa:fa-trash', 'admin?act=sessioncleanup', '', True, 'section.content'));
  t.Add('items', subitems);
  items.Add(t);


  o.Add('items', items);
  Result := o.AsJSON;
  FreeAndNil(o);
end;

function TAdminModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin

  case _GET['act'] of
    '': Result := DashboardAction;
    'generalsettings':
    begin
      if not User.checkPermission('admin', 'generalsettings', ACCESS_ADMIN) then
      begin
        FreeAndNil(User);
        Redirect(BaseURL + 'admin');

      end;
      Result := GeneralSettingAction;
    end
    else
      Result := 'No Handler for "' + _GET['act'] + '" action.';
  end;

  Result := ThemeUtil.RenderFromContent(@TagController, Result,
    'modules/admin/view/' + _REQUEST['act'] + '.html');
  if isAjax then
    die(Result);

end;

procedure TAdminModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  if not User.isLoggedIn then
  begin
    Redirect(BaseURL + USER_URL_LOGIN + '?url=admin');
  end;
  action := _GET['act'];
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
  Tags['modvar'] := @Tag_ModVar_Handler;
  ThemeUtil.Layout := 'admin';
end;

procedure TAdminModule.Get;
begin
  Response.Content := ThemeUtil.Render();
end;

procedure TAdminModule.Post;
var
  o: TJSONUtil;
  csrf: string;
begin
  o := TJSONUtil.Create;
  o['code'] := 500;

  case action of
    'generalsettings':
    begin
      if ((_POST['do'] = 'save') and (isAjax) and isValidCSRF) then
      begin
        csrf := HTMLUtil.CSRF( 'generalsettings', false);
        o['data'] := GeneralSettingSaveAction;
        o['token'] := csrf;
        o['time'] := TimeUsage;
        o['code'] := Int16( 0);
        Response.SetCustomHeader( 'TimeUsage', i2s( TimeUsage));
      end;
    end;
    else
    begin

    end;
  end;

  Response.ContentType := 'application/json';
  Response.Content := o.AsJSON;
  o.Free;
end;



initialization
{

  Add this to route.pas
  Route.Add('admin', TAdminModule);
}

end.

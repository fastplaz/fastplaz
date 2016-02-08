unit admin_controller;

{$mode objfpc}{$H+}

interface

uses
  fphttp, fpjson, jsonparser,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib,
  user_util, user_controller,
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
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    function Tag_ModInfo_Handler(const TagName: string; Params: TStringList): string;
    function Tag_MenuAdmin(const TagName: string; Params: TStringList): string;
    function Tag_Notification(const TagName: string; Params: TStringList): string;
    function GenerateMenu(const JsonString: string): string;
    function GenerateMenuItem(J: TJSonData): string;
    function GenerateNotificationMenu(const JsonString: string): string;

    function DashboardAction: string;
    function GeneralSettingAction: string;

    function OnMenuHandler(ARequest: TRequest): string;
  private
    NotifCount: integer;
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
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
  OnRequest := @RequestHandler;
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

procedure TAdminModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
  ThemeUtil.Layout := 'admin';
  Response.Content := ThemeUtil.Render();
  Handled := True;
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

function TAdminModule.OnMenuHandler(ARequest: TRequest): string;
var
  o, u, subitems: TJSONObject;
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
end;



initialization
{

  Add this to route.pas
  Route.Add('admin', TAdminModule);
}

end.

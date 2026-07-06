unit systeminfo_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib,
  database_lib, user_util, user_controller;

const
  ADMIN_SYSTEMINFO_TITLE = 'System Information';
  ADMIN_SYSTEMINFO_DESCRIPTION = 'system detail info';

type

  { TSystemInfoModule }

  TSystemInfoModule = class(TMyCustomWebModule)
  private
    User: TUserUtil;
    function Tag_ModInfo_Handler(const TagName: string;
      Params: TStringList): string;
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses theme_controller, common;

constructor TSystemInfoModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  ThemeUtil.Layout := 'admin';
end;

destructor TSystemInfoModule.Destroy;
begin
  FreeAndNil(User);
  inherited Destroy;
end;

procedure TSystemInfoModule.Get;
begin
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
  Response.Content := ThemeUtil.Render();
end;

procedure TSystemInfoModule.Post;
begin
  inherited Post;
end;

function TSystemInfoModule.Tag_ModInfo_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case Params.Values['type'] of
    'title': Result := ADMIN_SYSTEMINFO_TITLE;
    'description': Result := ADMIN_SYSTEMINFO_DESCRIPTION;
  end;
end;

function TSystemInfoModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := ThemeUtil.RenderFromContent(@TagController, '',
    'modules/admin/view/systeminfo.html');
  Result := Result + FastInfo;
end;

procedure TSystemInfoModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  User := TUserUtil.Create();
  if not User.isLoggedIn then
  begin
    FreeAndNil(User);
    Redirect(BaseURL + USER_URL_LOGIN + '?url=admin-systeminfo');
  end;

  if not User.checkPermission('admin', 'systeminfo', ACCESS_ADD) then
  begin
    FreeAndNil(User);
    Redirect(BaseURL + 'admin');
  end;
end;



initialization
  // -> http://yourdomainname/adminuser
  // The following line should be moved to a file "routes.pas"
  Route.Add('^(admin)-(systeminfo)/?$', TSystemInfoModule);

end.

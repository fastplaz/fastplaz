unit notification_controller;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler,
  html_lib, database_lib,
  security_util, user_util,
  Classes, SysUtils, fpcgi, HTTPDefs;

const
  NOTIFICATION_ROUTE_REGEX = '^(notification)/?$'; ---
  NOTIFICATION_TITLE = 'Notification';
  NOTIFICATION_DESCRIPTION = 'Notification Controller';
  NOTIFICATION_URL = 'notification';

type

  { TNotificationModule }

  TNotificationModule = class(TMyCustomWebModule)
  private
    action: string;
    function Tag_ModInfo_Handler(const TagName: string; Params: TStringList): string;
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);

    function GetNotification:string;
  public
    User: TUserUtil;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses theme_controller, common;

constructor TNotificationModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);

  DataBaseInit;
  LanguageInit;
  QueryExec('SET CHARACTER SET utf8;');

  User := TUserUtil.Create();

  BeforeRequest := @BeforeRequestHandler;
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
end;

destructor TNotificationModule.Destroy;
begin
  if Assigned(User) then
    User.Free;
  inherited Destroy;
end;

function TNotificationModule.Tag_ModInfo_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case Params.Values['type'] of
    'title': Result := NOTIFICATION_TITLE;
    'description': Result := NOTIFICATION_DESCRIPTION;
  end;
end;

procedure TNotificationModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  action := _GET['act'];
end;

function TNotificationModule.GetNotification: string;
begin
  Result := 'example notif';
end;

// GET Method Handler
procedure TNotificationModule.Get;
begin
  case action of
    'get':
    begin
      Response.Content:= GetNotification;
    end
    else
    begin
      Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
      Response.Content := ThemeUtil.Render();
    end;
  end;

end;

// POST Method Handler
procedure TNotificationModule.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TNotificationModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin

  // your code here
  Result := 'Notification ....';

end;


initialization
  // -> http://yourdomainname/user
  // The following line should be moved to a file "routes.pas"
{
  Add this to route.pas
  Route.Add( NOTIFICATION_ROUTE_REGEX, TNotificationModule);
}
  Route.Add(NOTIFICATION_ROUTE_REGEX, TNotificationModule);

end.

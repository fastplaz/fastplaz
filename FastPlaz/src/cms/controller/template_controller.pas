unit TEMPLATE_controller;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler,
  html_lib, database_lib,
  security_util, user_util,
  Classes, SysUtils, fpcgi, HTTPDefs;

const
  TEMPLATE_ROUTE_REGEX = '^(template)/?$';
  TEMPLATE_TITLE = 'Template';
  TEMPLATE_DESCRIPTION = 'Template Controller';
  TEMPLATE_URL = 'template';

type

  { TTemplateModule }

  TTemplateModule = class(TMyCustomWebModule)
  private
    function Tag_ModInfo_Handler(const TagName: string; Params: TStringList): string;
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    User: TUserUtil;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses theme_controller, common;

constructor TTemplateModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);

  DataBaseInit;
  LanguageInit;
  QueryExec('SET CHARACTER SET utf8;');

  User := TUserUtil.Create();

  //BeforeRequest := @BeforeRequestHandler;
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
end;

destructor TTemplateModule.Destroy;
begin
  if Assigned(User) then
    User.Free;
  inherited Destroy;
end;

function TTemplateModule.Tag_ModInfo_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case Params.Values['type'] of
    'title': Result := TEMPLATE_TITLE;
    'description': Result := TEMPLATE_DESCRIPTION;
  end;
end;

procedure TTemplateModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TTemplateModule.Get;
begin
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TTemplateModule.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TTemplateModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin

  // your code here
  Result := 'Template ....';

end;


initialization
  // -> http://yourdomainname/user
  // The following line should be moved to a file "routes.pas"
{
  Add this to route.pas
  Route.Add( TEMPLATE_ROUTE_REGEX, TTemplateModule);
}

end.


unit error_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler;

type

  { TErrorinfoModule }

  TErrorinfoModule = class(TMyCustomWebModule)
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses theme_controller, common;

constructor TErrorinfoModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  OnRequest := @RequestHandler;
end;

destructor TErrorinfoModule.Destroy;
begin
  inherited Destroy;
end;

procedure TErrorinfoModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  Tags['maincontent'] := @Tag_MainContent_Handler;
  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

function TErrorinfoModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
var
  s: string;
begin
  //s := Copy(Application.Request.PathInfo, 8, Length(Application.Request.PathInfo) - 8);
  s := Application.Request.GetNextPathInfo;
  if _SESSION['f_err'] <> '' then
  begin
    s := _SESSION['f_err'];
    _SESSION['f_err'] := '';
  end;
  if s <> '' then
  begin
    if Environtment['HTTP_REFERER'] <> '' then
      s := s + '<br>referer: ' + Environtment['HTTP_REFERER'];
    Result := '<hr>' +
      '<div class="bs-example-bg-classes"><p class="bg-danger text-danger">' +
      s + '</p></div>';
  end;
end;


initialization
  // -> http://yourdomainname/error
  // is better to move line below to file "route.pas"
  Route.Add('error', TMyCustomWebModuleClass(TErrorinfoModule));
end.

unit error_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler;

type

  { TErrorinfoModule }

  TErrorinfoModule = class(TMyCustomWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; 
      AResponse: TResponse; var Handled: boolean);
  private
    function TagMainContentHandler(const TagName: string; Params: TStringList): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses theme_controller, common;

procedure TErrorinfoModule.DataModuleRequest(Sender: TObject; 
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  Tags['$maincontent'] := @TagMainContentHandler;
  Response.Content := ThemeUtil.Render(@TagController);
  Handled := True;
end;

function TErrorinfoModule.TagMainContentHandler(const TagName: string;
  Params: TStringList): string;
var
  s : string;
begin
  s := Copy(Application.Request.PathInfo,8,Length(Application.Request.PathInfo)-8);
  if _SESSION['f_err'] <> '' then
  begin
    s := _SESSION['f_err'];
    _SESSION['f_err'] := '';
  end;
  if s <> '' then
    Result := '<hr>' +  '<div class="bs-example-bg-classes"><p class="bg-danger text-danger">'+s+'</p></div>';
end;

constructor TErrorinfoModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  OnRequest := @DataModuleRequest;
end;

destructor TErrorinfoModule.Destroy;
begin
  inherited Destroy;
end;


initialization
  // -> http://yourdomainname/error
  // is better to move line below to file "route.pas"
  AddRoute('error', TErrorinfoModule);
end.


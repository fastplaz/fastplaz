unit docs_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler;

type

  { TDocsModule }

  TDocsModule = class(TMyCustomWebModule)
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

constructor TDocsModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  OnRequest := @RequestHandler;
end;

destructor TDocsModule.Destroy;
begin
  inherited Destroy;
end;

procedure TDocsModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  Tags['maincontent'] := @Tag_MainContent_Handler;
  ThemeUtil.TrimWhiteSpace := False;
  Response.Content := ThemeUtil.Render(@TagController);
  Handled := True;
end;

function TDocsModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
var
  s: string;
begin
  //s := Copy(Application.Request.PathInfo, 7, Length(Application.Request.PathInfo) - 7);
  s := Application.Request.GetNextPathInfo;
  if s = '' then
    s := 'main';
  s := 'modules/docs/view/' + s + '.html';
  Result := ThemeUtil.RenderFromContent(@TagController, '', s);
end;


initialization

end.

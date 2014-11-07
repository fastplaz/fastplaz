unit docs_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler;

type

  { TDocsModule }

  TDocsModule = class(TMyCustomWebModule)
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

procedure TDocsModule.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  Tags['$maincontent'] := @TagMainContentHandler;
  Response.Content := ThemeUtil.Render(@TagController);
  Handled := True;
end;

function TDocsModule.TagMainContentHandler(const TagName: string;
  Params: TStringList): string;
var
  s: string;
begin
  s := Copy(Application.Request.PathInfo, 7, Length(Application.Request.PathInfo) - 7);
  if s = '' then
    s := 'main';
  s := 'modules/docs/' + s + '.html';
  Result := ThemeUtil.RenderFromContent(@TagController, '', s);
end;

constructor TDocsModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  OnRequest := @DataModuleRequest;
end;

destructor TDocsModule.Destroy;
begin
  inherited Destroy;
end;

initialization

end.

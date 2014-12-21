unit info_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler;

type

  { TInfoModule }

  TInfoModule = class(TMyCustomWebModule)
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses theme_controller, common;

constructor TInfoModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  CreateSession := True;
  OnRequest := @RequestHandler;
end;

destructor TInfoModule.Destroy;
begin
  inherited Destroy;
end;

procedure TInfoModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  Application.GetEnvironmentList(lst);

  die('<pre>Your Server Info:<br>' + lst.Text);

  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

initialization

end.


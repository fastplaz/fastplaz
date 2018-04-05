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

uses theme_controller, common, versioninfo_lib;

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
begin
  ThemeUtil.GetVersionInfo();
  //Response.Content := ThemeUtil.Render();
  Response.Content := VersionInfo.FullVersion + FastInfo();
  Handled := True;
end;

initialization

end.


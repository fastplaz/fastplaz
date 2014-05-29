unit info_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler;

type

  { TInfoModule }

  TInfoModule = class(TMyCustomWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; 
      AResponse: TResponse; var Handled: boolean);
  private
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses theme_controller, common;

procedure TInfoModule.DataModuleRequest(Sender: TObject; ARequest: TRequest; 
  AResponse: TResponse; var Handled: boolean);
var
  lst : TStringList;
begin
  lst := TStringList.Create;
  Application.GetEnvironmentList(lst);

  die('Your Server Info:<pre>'+lst.Text);

  Response.Content := ThemeUtil.Render(@TagController);
  Handled := True;
end;

constructor TInfoModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  CreateSession := True;
  OnRequest := @DataModuleRequest;
end;

destructor TInfoModule.Destroy;
begin
  inherited Destroy;
end;

initialization

end.


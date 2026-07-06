unit appstate_util;

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

interface

uses
  Classes, SysUtils, config_lib;

type

  { TMainData }

  TMainData = record
    module, modtype, func: string;
    sitename,
    slogan,
    baseUrl,
    admin_email,
    language,
    tempDir: string;
    logDir: string;
    themeEnable: boolean;
    theme: string;
    cacheType: string;
    cacheWrite: boolean;
    cacheTime: integer;
    tablePrefix: string;
    sessionAutoStart: boolean;
    SessionID: string;
    SessionDir: string;
    SessionStorage: integer;
    hitStorage: string;
    databaseRead,
    databaseWrite: string;
    databaseVersionCheck,
    databaseActive,
    useDatabase,
    initialized,
    debug: boolean;
    debugLevel: integer;
    isReady: boolean;
    cookiePath: string;
  end;

var
  AppData: TMainData;
  Config: TMyConfig;
  _DebugInfo: TStringList;

var
  DisplayErrorHandler: procedure(const Message: string; const Layout: string);
  RedirectHandler: procedure(const URL: string; const FlashMessage: string;
    AStatusCode: Integer);

procedure DisplayError(const Message: string; const Layout: string = 'error');
procedure Redirect(const URL: string; const FlashMessage: string = '';
  AStatusCode: Integer = 302);

implementation

procedure DisplayError(const Message: string; const Layout: string);
begin
  if Assigned(DisplayErrorHandler) then
    DisplayErrorHandler(Message, Layout);
end;

procedure Redirect(const URL: string; const FlashMessage: string;
  AStatusCode: Integer);
begin
  if Assigned(RedirectHandler) then
    RedirectHandler(URL, FlashMessage, AStatusCode);
end;

initialization
  _DebugInfo := TStringList.Create;
  AppData.isReady := False;

finalization
  FreeAndNil(_DebugInfo);

end.

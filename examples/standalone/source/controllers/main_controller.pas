unit main_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_app, HTTPDefs, fastplaz_handler, html_lib;

type

  { TMainController }

  TMainController = class(TMyCustomController)
  private
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
  public
    procedure Get; override;
  end;

  { TGoController }

  // /go : redirect with a flash message; in standalone mode the server must
  // keep running afterwards
  TGoController = class(TMyCustomController)
  public
    procedure Get; override;
  end;

var
  // lives as long as the process: 1 in CGI mode, keeps counting in
  // standalone mode
  RequestCount: integer = 0;

implementation

uses theme_controller, common;

{ TMainController }

procedure TMainController.Get;
begin
  Tags['maincontent'] := @Tag_MainContent_Handler;
  ThemeUtil.Layout := 'home';
  Response.Content := ThemeUtil.Render();
end;

function TMainController.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
var
  visits: integer;
  modeText, visitor: string;
begin
  Inc(RequestCount);
  visits := s2i(_SESSION['visits']) + 1;
  _SESSION['visits'] := visits;

  if IsStandalone then
    modeText := Format('standalone HTTP (%s:%d)', [HTTPAddress, HTTPPort])
  else
    modeText := 'CGI';
  visitor := _GET['name'];
  if visitor = '' then
    visitor := 'guest';

  Result := h3('Hello ' + HTMLEncode(visitor) + ', FastPlaz is running as ' + modeText)
    + '<ul>'
    + '<li>requests handled by this process: ' + i2s(RequestCount) + '</li>'
    + '<li>your visits (session): ' + i2s(visits) + '</li>'
    + '<li>session id: ' + HTMLEncode(SessionID) + '</li>'
    + '<li>remote address: ' + HTMLEncode(_SERVER['REMOTE_ADDR']) + '</li>'
    + '<li>base url: ' + HTMLEncode(BaseURL) + '</li>'
    + '</ul>'
    + '<p><a href="?name=FastPlaz">?name=FastPlaz</a> | '
    + '<a href="go">redirect with a flash message</a> | '
    + '<a href="info">server info (JSON)</a></p>';
end;

{ TGoController }

procedure TGoController.Get;
begin
  Redirect(BaseURL, 'Redirected at ' + FormatDateTime('hh:nn:ss', Now));
end;

end.

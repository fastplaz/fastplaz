unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib;

type
  TMainModule = class(TMyCustomWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; 
      AResponse: TResponse; var Handled: boolean);
  private
    function TagMainContentHandler(const TagName: string; Params: TStringList
      ): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses theme_controller, common, database_lib;

procedure TMainModule.DataModuleRequest(Sender: TObject; ARequest: TRequest; 
  AResponse: TResponse; var Handled: boolean);
begin
  DataBaseInit;
  LanguageInit;

  Tags['$maincontent'] := @TagMainContentHandler; //<<-- tag $maincontent handler
  Response.Content := ThemeUtil.Render(@TagController, 'home');
  Handled := True;
end;

constructor TMainModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  OnRequest := @DataModuleRequest;
end;

destructor TMainModule.Destroy;
begin
  inherited Destroy;
end;

function TMainModule.TagMainContentHandler(const TagName: string; 
  Params: TStringList): string;
begin

  Result:= '<div class="entry-content">'
    + h3('Hello world ... FastPlaz !')
    + '</div>';

end;


end.


unit mainapp;

{$mode objfpc}{$H+}
{$include define.inc}

interface

uses
  fastplaz_handler, httpdefs, fpcgi, fpHTTP,
  SysUtils, Classes;

type

  { TMainWebModule }

  TMainWebModule = class(TMyCustomWebModule)
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    // Handler / Controller
    procedure DoBlockController(Sender: TObject; FunctionName: string;
      Parameter: TStrings; var ResponseString: string);
  end;

var
  MainWebModule: TMainWebModule;

implementation

uses
  logutil_lib, theme_controller, common, database_lib, language_lib;

{ TMainWebModule }

procedure TMainWebModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  //DataBaseInit;  //<<-- if you need database connection
  LanguageInit;

  //==================================== YOUR CUSTOM CMS/FRAMEWORK - START ===

  //==================================== YOUR CUSTOM CMS/FRAMEWORK - END ===

  Tags['maincontent'] := @Tag_MainContent_Handler;
  Response.Content := ThemeUtil.Render(nil, 'home'); // <<-- use home layout
  //Response.Content := ThemeUtil.Render(); <<-- use master layout
  Handled := True;
end;

function TMainWebModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := 'this is home page';
end;

procedure TMainWebModule.DoBlockController(Sender: TObject;
  FunctionName: string; Parameter: TStrings; var ResponseString: string);
begin
  case FunctionName of
    'blocktest':
    begin
      ResponseString := 'This is Block Test with parameter ' + Parameter.Values['var1'];
    end;
  end;
end;


constructor TMainWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  //CreateSession := True;
  OnRequest := @RequestHandler;
  OnBlockController := @DoBlockController;
end;

destructor TMainWebModule.Destroy;
begin
  inherited Destroy;
end;

initialization
  Route.Add('main', TMainWebModule);

end.

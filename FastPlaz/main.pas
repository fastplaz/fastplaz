unit main;

{$mode objfpc}{$H+}
{$include define.inc}

interface

uses
  fastplaz_handler, httpdefs, fpcgi, fpHTTP,
  SysUtils, Classes;

type

  { TMainWebModule }

  TMainWebModule = class(TMyCustomWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    function TagMainContentHandler(const TagName: string; Params: TStringList): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    procedure TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);


    // Handler / Controller
    procedure DoBlockController(Sender: TObject; FunctionName: string;
      Parameter: TStrings; var ResponseString: string);
  end;

var
  MainWebModule: TMainWebModule;
  mc: TCustomHTTPModuleClass;


implementation

uses
  logutil_lib, theme_controller, common, database_lib, language_lib;

{ TMainWebModule }

procedure TMainWebModule.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  //DataBaseInit;
  LanguageInit;

  Tags['$maincontent'] := @TagMainContentHandler;
  Response.Content := ThemeUtil.Render(@TagController, 'home'); // <<-- use home layout
  //Response.Content := ThemeUtil.Render(@TagController); <<-- use master layout

  //==================================== YOUR CUSTOM CMS/FRAMEWORK - START ===



  //==================================== YOUR CUSTOM CMS/FRAMEWORK - END ===

  Handled := True;
end;

procedure TMainWebModule.TagController(Sender: TObject; const TagString: string;
  TagParams: TStringList; out ReplaceText: string);
begin
  inherited TagController(Sender, TagString, TagParams, ReplaceText);

{
  if AppData.debug then
    if ReplaceText = '' then
      ReplaceText := EchoError(__Err_Theme_Tag_NotImplemented, [TagString]);
}
end;

function TMainWebModule.TagMainContentHandler(const TagName: string;
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
  OnRequest := @DataModuleRequest;
  OnBlockController := @DoBlockController;
end;

destructor TMainWebModule.Destroy;
begin
  inherited Destroy;
end;

initialization
  AddRoute('main', TMainWebModule);

end.

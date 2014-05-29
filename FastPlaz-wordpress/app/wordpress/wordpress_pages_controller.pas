unit wordpress_pages_controller;

{$mode objfpc}{$H+}
{$include define.inc}

interface

uses
  wordpress_pages_model,
  fpcgi, fastplaz_handler, httpdefs, fpHTTP,
  Classes, SysUtils;

{$ifdef wordpress}
type

  { TWPPagesWebModule }

  TWPPagesWebModule = class(TMyCustomWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    Pages: TWordpressPages;
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    function View: string;
    procedure TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);

    // Handler / Controller
    procedure DoBlockController(Sender: TObject; FunctionName: string;
      Parameter: TStrings; var ResponseString: string);
  end;

{$endif wordpress}

implementation

uses database_lib, common, html_lib, language_lib, theme_controller,
  wordpress_nggallery_model, wordpress_news_model;

{$ifdef wordpress}

{ TWPPagesWebModule }

// example:
// http://domain/page/about/hubungi-kami/
procedure TWPPagesWebModule.DataModuleRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  DataBaseInit;
  LanguageInit;

  Tags['$maincontent'] := @Tag_MainContent_Handler;
  Response.Content := ThemeUtil.Render(@TagController, '', false);

  //==================================== YOUR CUSTOM CMS/FRAMEWORK - START ===

  {$ifdef wordpress}
    {$ifdef wordpress_nggallery}
    with TWPNGGallery.Create() do
    begin
      Response.Content := Render(Response.Content);
      Free;
    end;
    {$endif}
  {$endif}

  //==================================== YOUR CUSTOM CMS/FRAMEWORK - END ===

  Handled := True;
end;

function TWPPagesWebModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := View;
end;

constructor TWPPagesWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  CreateSession := True;
  OnRequest := @DataModuleRequest;
  OnBlockController := @DoBlockController;
  Pages := TWordpressPages.Create();
end;

destructor TWPPagesWebModule.Destroy;
begin
  FreeAndNil(Pages);
  inherited Destroy;
end;

function TWPPagesWebModule.View: string;
var
  page_name : string;
  str: TStrings;
begin
  str := Explode(Application.Request.PathInfo, '/');
  if str.Count < 3 then
  begin
    Result := H2(__(__Content_Not_Found), 'center');
    FreeAndNil(str);
    Exit;
  end;

  page_name := str[str.Count - 1];
  Pages.Find(['post_status="publish"', 'post_type="page"', 'post_name="' +
    page_name + '"']);
  if Pages.RecordCount = 0 then
  begin
    Result := H2(__(__Content_Not_Found), 'center');
    Exit;
  end;

  //-- old style: Tags['$page'] := @Tag_PageVar_Handler;
  ThemeUtil.AssignVar['$page'] := @Pages.Data;

  Result := ThemeUtil.RenderFromContent(@TagController, '',
    'modules/pages/detail.html');
end;

procedure TWPPagesWebModule.TagController(Sender: TObject;
  const TagString: string; TagParams: TStringList; out ReplaceText: string);
begin
  inherited TagController(Sender, TagString, TagParams, ReplaceText);
end;

procedure TWPPagesWebModule.DoBlockController(Sender: TObject;
  FunctionName: string; Parameter: TStrings; var ResponseString: string);
begin

end;

initialization
  RegisterHTTPModule('pages', TWPPagesWebModule, True);
{$endif wordpress}

end.

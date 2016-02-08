unit search_controller;

{$mode objfpc}{$H+}

interface

uses
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, user_util,
  database_lib, security_util;

const
  SEARCH_ROUTE_REGEX = '^(search)/?$';
  SEARCH_TITLE = 'Search';
  SEARCH_DESCRIPTION = 'Search your keyword';
  SEARCH_URL = 'search';

type

  { TSearchModule }

  TSearchModule = class(TMyCustomWebModule)
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    function Tag_ModInfo_Handler(const TagName: string;
      Params: TStringList): string;
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function SearchToAllModule(KeyWord: string): string;
  public
    User: TUserUtil;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

  end;

implementation

uses theme_controller, common, logutil_lib;

constructor TSearchModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  User := TUserUtil.Create();
  inherited CreateNew(AOwner, CreateMode);
  OnRequest := @RequestHandler;
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TSearchModule.Destroy;
begin
  inherited Destroy;
  if Assigned(User) then
    User.Free;
end;

procedure TSearchModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  DataBaseInit;
  LanguageInit;
  QueryExec('SET CHARACTER SET utf8;');

  if isAjax then
  begin
    echo(SearchToAllModule(_POST['keyword']));
    die;
  end;

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

function TSearchModule.Tag_ModInfo_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '';
  case Params.Values['type'] of
    'title': Result := SEARCH_TITLE;
    'description': Result := SEARCH_DESCRIPTION;
  end;
end;

function TSearchModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
var
  action: string;
begin
  Result := 'search......';
end;

procedure TSearchModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
end;

function TSearchModule.SearchToAllModule(KeyWord: string): string;
var
  i: integer;
  found: TStringList;
  s, resultType, resultData: string;
  searchResult: TJSONData;
begin
  if KeyWord = '' then
    Exit;
  found := TStringList.Create;
  found.Add('<section class="content"><h3>Search Result for "' + KeyWord + '"</h3>');

  for i := 0 to ModuleLoaded.Count - 1 do
  begin
    if ModuleLoaded[i].ModuleStore.OnSearch <> nil then
    begin
      s := ModuleLoaded[i].ModuleStore.OnSearch(KeyWord, Application.Request);
      if s <> '' then
      begin
        found.Add('<SECTION class="search_result">');
        if IsJsonValid(s) then
        begin
          searchResult := GetJSON(s);
          resultType := jsonGetString(searchResult, 'type');
          if resultType = 'html' then
          begin
            resultData := jsonGetString(searchResult, 'data');
            found.Add(resultData);
          end;
          searchResult.Free;
        end
        else
        begin
          found.Add(s);
        end;
        found.Add('</SECTION>');
      end;
    end;
  end;//-- loop every loaded modul

  found.Add('</section>');
  Result := found.Text;
  FreeAndNil(found);
end;


initialization
  // -> http://yourdomainname/user
  // The following line should be moved to a file "routes.pas"
{
  Add this to route.pas
  Route.Add( SEARCH_ROUTE_REGEX, TSearchModule);
}
end.

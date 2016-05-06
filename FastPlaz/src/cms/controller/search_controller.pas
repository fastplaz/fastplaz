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

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses theme_controller, common, logutil_lib;

constructor TSearchModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  User := TUserUtil.Create();
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
end;

destructor TSearchModule.Destroy;
begin
  inherited Destroy;
  if Assigned(User) then
    User.Free;
end;

procedure TSearchModule.Get;
begin
  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['modinfo'] := @Tag_ModInfo_Handler;
  Response.Content := ThemeUtil.Render();
end;

procedure TSearchModule.Post;
begin
  if isAjax then
  begin
    Response.Content := SearchToAllModule(_POST['keyword']);
  end;
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
  DataBaseInit;
  LanguageInit;
  QueryExec('SET CHARACTER SET utf8;');
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

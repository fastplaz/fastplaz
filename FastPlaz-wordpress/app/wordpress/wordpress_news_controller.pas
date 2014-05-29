unit wordpress_news_controller;

{$mode objfpc}{$H+}
{$include define.inc}

interface

uses
  wordpress_news_model, wordpress_terms_model,
  fpcgi, fastplaz_handler, httpdefs, fpHTTP,
  Classes, SysUtils;

type

  { TWPNewsWebModule }

  TWPNewsWebModule = class(TMyCustomWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    NewsTitle: string;
    News : TWordpressNews;
    Terms : TWordpressTerms;
    function GetLastNews(FunctionName: string = ''; Parameter: TStrings = nil): string;
    function GetRandomNews(FunctionName: string = ''; Parameter: TStrings = nil): string;
    function GeneratePagesMenu(ParentMenu: integer; Parameter: TStrings = nil;
      ParentBaseURL: string = ''; IsParent : boolean = true): string;

    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
    function View: string;
    function GetOptions(const KeyName: string): string;
    procedure TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);

    // Handler / Controller
    procedure DoBlockController(Sender: TObject; FunctionName: string;
      Parameter: TStrings; var ResponseString: string);
  end;


implementation

uses database_lib, common, language_lib, html_lib, theme_controller,
  wordpress_options_model, wordpress_nggallery_model, wordpress_pages_model;

{ TWPNewsWebModule }

// example:
// http://domain/2014/04/dialog-keajaiban-silat-silat-untuk-kehidupan/
procedure TWPNewsWebModule.DataModuleRequest(Sender: TObject;
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

function TWPNewsWebModule.GetLastNews(FunctionName: string;
  Parameter: TStrings): string;
var
  lst: TStringList;
  _News: TWordpressNews;
  limit: integer;
  url, title: string;
begin
  limit := 0;
  if Parameter <> nil then
    limit := s2i(Parameter.Values['number']);
  if limit = 0 then
    limit := 20;
  _News := TWordpressNews.Create();
  _News.Find(['post_type="post"','post_status="publish"'], 'post_date desc', limit);
  if _News.RecordCount > 0 then
  begin
    lst := TStringList.Create;
    lst.Add('<div class="news-lastnews">');
    if Parameter <> nil then
      if Parameter.Values['title'] <> '' then
      begin
        title := StringReplace(Parameter.Values['title'], '"', '', [rfReplaceAll]);
        lst.Add('<h3>' + title + '</h3>');
      end;

    lst.Add('<ul>');
    while not _News.Data.EOF do
    begin
      url := '/' + FormatDateTime('YYYY', _News['post_date'].AsDateTime) +
        '/' + FormatDateTime('mm', _News['post_date'].AsDateTime) +
        '/' + _News['post_name'].AsString + '/';
      lst.Add('<li><a href="' + url + '">' + _News['post_title'].AsString + '</a></li>');

      _News.Data.Next;
    end;
    lst.Add('</ul>');
    lst.Add('</div>');

    Result := lst.Text;
    FreeAndNil(lst);
  end;
  FreeAndNil(_News);
end;

function TWPNewsWebModule.GetRandomNews(FunctionName: string;
  Parameter: TStrings): string;
var
  lst: TStringList;
  _News: TWordpressNews;
  limit: integer;
  url, title: string;
begin
  limit := 0;
  if Parameter <> nil then
    limit := s2i(Parameter.Values['number']);
  if limit = 0 then
    limit := 20;
  _News := TWordpressNews.Create();
  _News.Find(['post_type="post"','post_status="publish"'], 'rand()', limit);
  if _News.RecordCount > 0 then
  begin
    lst := TStringList.Create;
    lst.Add('<div class="news-lastnews">');
    if Parameter <> nil then
      if Parameter.Values['title'] <> '' then
      begin
        title := StringReplace(Parameter.Values['title'], '"', '', [rfReplaceAll]);
        lst.Add('<h3>' + title + '</h3>');
      end;

    lst.Add('<ul>');
    while not _News.Data.EOF do
    begin
      url := '/' + FormatDateTime('YYYY', _News['post_date'].AsDateTime) +
        '/' + FormatDateTime('mm', _News['post_date'].AsDateTime) +
        '/' + _News['post_name'].AsString + '/';
      lst.Add('<li><a href="' + url + '">' + _News['post_title'].AsString + '</a></li>');

      _News.Data.Next;
    end;
    lst.Add('</ul>');
    lst.Add('</div>');

    Result := lst.Text;
    FreeAndNil(lst);
  end;
  FreeAndNil(_News);
end;

function TWPNewsWebModule.GeneratePagesMenu(ParentMenu: integer;
  Parameter: TStrings; ParentBaseURL: string; IsParent: boolean): string;
var
  where,
  html, title, submenu, url, page_item_has_children: string;
begin
  with TWordpressPages.Create() do
  begin
    {$ifdef wordpress_polylang}
    if Config.GetValue( _WORDPRESS_PLUGINS_POLYLANG, false) then
    begin
      AddJoin(AppData.table_prefix+'_term_relationships', 'object_id', AppData.table_prefix+'_posts.ID',['term_taxonomy_id']);
      AddJoin(AppData.table_prefix+'_terms', 'term_id', AppData.table_prefix+'_term_relationships.term_taxonomy_id',['slug']);
      where := AppData.table_prefix+'_terms.slug = "'+LANG+'"';
    end;
    {$endif}
    Find(['post_type="page"', 'post_status="publish"', 'post_parent=' +
      i2s(ParentMenu), where], 'post_parent, menu_order', 0,
      'ID,post_title,post_name,post_parent');

    if RecordCount > 0 then
    begin
      if ((Parameter.Values['header'] <> '') and (ParentMenu = 0)) then
        html := Parameter.Values['header'];
      if ((Parameter.Values['type'] = 'nav') and (ParentMenu = 0)) then
        html := html + '<nav>';
      if Parameter.Values['type'] = 'nav' then
      begin
        if IsParent then begin
          html := html + #13#10'<ul id="nav" class="clearfloat">';
          html := html + #13#10'<li><a href="'+BaseURL+'" class="on">'+__('Home')+'</a></li>';
        end
        else begin
          html := html + #13#10'<ul class="children">';
        end;
      end;
      while not Data.EOF do
      begin
        submenu := '';
        title := '';
        url := '#';
        submenu := GeneratePagesMenu(Value['ID'].AsInteger, Parameter,
          ParentBaseURL + Value['post_name'].AsString + '/', false);
        if submenu = '' then page_item_has_children := '' else page_item_has_children := 'page_item_has_children';
        if Parameter.Values['type'] = 'nav' then begin
          if IsParent then begin
            html := html + #13#10'<li class="page_item '+page_item_has_children+'">';
          end else begin
            html := html + #13#10'<li class="'+page_item_has_children+'">';
          end;
        end;
        title := Value['post_title'].AsString;
        url := BaseURL + '/pages/' + ParentBaseURL + Value['post_name'].AsString;
        html := html + '<a href="' + url + '">' + title + '</a>' + submenu;
        if Parameter.Values['type'] = 'nav' then
          html := html + '</li>';
        Data.Next;
      end;
      Parameter.Values['parent_baseurl'] := '';
      if Parameter.Values['type'] = 'nav' then
        html := html + #13#10'</ul>';
      if ((Parameter.Values['type'] = 'nav') and (ParentMenu = 0)) then
        html := html + #13#10'</nav>';
      if ((Parameter.Values['footer'] <> '') and (ParentMenu = 0)) then
        html := html + Parameter.Values['footer'];
    end;
    Free;
    Result := html;
  end;
end;

constructor TWPNewsWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  CreateSession := True;
  OnRequest := @DataModuleRequest;
  OnBlockController := @DoBlockController;
  News:= TWordpressNews.Create();
  Terms := TWordpressTerms.Create();
end;

destructor TWPNewsWebModule.Destroy;
begin
  FreeAndNil(Terms);
  FreeAndNil(News);
  inherited Destroy;
end;

function TWPNewsWebModule.View: string;
begin
  if Application.Request.QueryString = '' then
  begin
    Result := GetLastNews();
    Exit;
  end;

  News.AddJoin(AppData.table_prefix+'_users', 'ID', AppData.table_prefix+'_posts.post_author',['display_name']);
  News.Find(['date_format( post_date, "%Y%m") = "' + _GET['year'] +
    _GET['month'] + '"', 'post_status="publish"', 'post_type="post"',
    'post_name = "' + _GET['permalink'] + '"'], AppData.table_prefix+'_posts.post_date DESC');
  if News.RecordCount = 0 then
  begin
    Result := H2(__(__Content_Not_Found));
    Exit;
  end;

  Terms.GetObjectTerms( News['ID'].AsInteger, ['post_tag']);

  ThemeUtil.Assign('$news', @News.Data);
  ThemeUtil.Assign('$terms', @Terms.Data);
  //or use this
  //ThemeUtil.AssignVar['$news'] := @News.Data;

  Result := ThemeUtil.RenderFromContent(@TagController, '', 'modules/wpnews/detail.html');
  News.AddHit( News['ID'].AsInteger);
end;

function TWPNewsWebModule.GetOptions(const KeyName: string): string;
begin
  with TWordpressOptions.Create() do
  begin
    FindFirst(['option_name="' + KeyName + '"'], '', 'option_value');
    if RecordCount > 0 then
    begin
      Result := Value['option_value'].AsString;
    end;
    Free;
  end;
end;

procedure TWPNewsWebModule.TagController(Sender: TObject; const TagString: string;
  TagParams: TStringList; out ReplaceText: string);
begin
  inherited TagController(Sender, TagString, TagParams, ReplaceText);
end;

function TWPNewsWebModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := View;
end;

procedure TWPNewsWebModule.DoBlockController(Sender: TObject;
  FunctionName: string; Parameter: TStrings; var ResponseString: string);
begin
  case FunctionName of
    'getoption':
    begin
      ResponseString := GetOptions(Parameter.Values['name']);
    end;
    'newstitle':
    begin
      if _GET['permalink'] <> '' then
      begin
        if NewsTitle = '' then
        begin
          with TWordpressNews.Create() do
          begin
            FindFirst(['date_format( post_date, "%Y%m") = "' +
              _GET['year'] + _GET['month'] + '"', 'post_status="publish"',
              'post_type="post"', 'post_name = "' + _GET['permalink'] +
              '"'], '', 'post_title');
            if RecordCount > 0 then
            begin
              NewsTitle := Value['post_title'].AsString;
            end;
            Free;
          end;
        end;
        if NewsTitle <> '' then
          ResponseString := ' - ' + NewsTitle;
      end;
    end; //-- newstitle
    'lastnews':
    begin
      ResponseString := GetLastNews(FunctionName, Parameter);
    end;
    'randomnews':
    begin
      ResponseString := GetRandomNews(FunctionName, Parameter);
    end;
    'pagesmenu':
    begin
      ResponseString := GeneratePagesMenu(0, Parameter);
    end;

  end;

end;

{$ifdef wordpress}
initialization
  RegisterHTTPModule( 'wpnews', TWPNewsWebModule, True);
{$endif}

end.

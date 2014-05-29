unit wordpress_tags_controller;

{$mode objfpc}{$H+}
{$include define.inc}

interface

uses
  fpcgi, fastplaz_handler, httpdefs, fpHTTP,
  Classes, SysUtils;

type

  { TWPTagsWebModule }

  TWPTagsWebModule = class(TMyCustomWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    function GetTagCloud(Parameter: TStrings): string;
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

implementation

uses database_lib, common, language_lib, html_lib, theme_controller,
  wordpress_news_model, wordpress_tags_model;

{ TWPTagsWebModule }

procedure TWPTagsWebModule.DataModuleRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  DataBaseInit;
  LanguageInit;

  Response.Content := ThemeUtil.Render(@TagController);
  Handled := True;
end;

function TWPTagsWebModule.GetTagCloud(Parameter: TStrings): string;
var
  _tags: TWordpressTags;
  lst: TStringList;
  url, title, style: string;
  number: integer;
begin
  number := s2i(Parameter.Values['number']);
  if number = 0 then
    number := 50;
  _tags := TWordpressTags.Create();
  _tags.AddInnerJoin( AppData.table_prefix+'_term_taxonomy', 'term_id', AppData.table_prefix+'_terms.term_id', []);
  _tags.Find([ AppData.table_prefix+'_term_taxonomy.taxonomy IN ("post_tag")'],
    'rand()', number);
  if _tags.RecordCount > 0 then
  begin
    lst := TStringList.Create;
    lst.Add('<div class="tagcloud">');
    if Parameter.Values['title'] <> '' then
    begin
      title := StringReplace(Parameter.Values['title'], '"', '', [rfReplaceAll]);
      lst.Add('<h3>' + title + '</h3>');
    end;
    Randomize;
    while not _tags.Data.EOF do
    begin
      url := BaseURL + '/tag/' + _tags['slug'].AsString;
      title := _tags['name'].AsString;
      style := 'font-size:' + i2s(7 + random(8)) + 'pt;';
      lst.Add('<a href="' + url + '" title="' + title + '" style="' + style + '">' +
        LowerCase(title) + '</a> ');
      //style="font-size: 9.92660550459pt;">

      _tags.Data.Next;
    end;
    lst.Add('</div>');
    Result := lst.Text;
    FreeAndNil(lst);
  end;

  FreeAndNil(_tags);
end;

constructor TWPTagsWebModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  CreateSession := True;
  OnRequest := @DataModuleRequest;
  OnBlockController := @DoBlockController;
end;

destructor TWPTagsWebModule.Destroy;
begin
  inherited Destroy;
end;

function TWPTagsWebModule.View: string;
var
  url, _tags: string;
  News: TWordpressNews;
begin
  _tags := StringReplace(Application.Request.PathInfo, '/tag/', '', [rfReplaceAll]);
  _tags := StringReplace(_tags, '/', '', [rfReplaceAll]);
  if _tags = '' then
  begin
    Result := 'Last Tags Lists';
    exit;
  end;

  News := TWordpressNews.Create();
  if not News.FindByTags(_tags) then
  begin
    Result := H2( format( __( __Tag_Content_Not_Found), [_tags]));
    FreeAndNil(News);
    Exit;
  end;

  Result := '<div class="entry-content">'
    + '<ul>';
  while not News.Data.EOF do
  begin
    url := '/' + FormatDateTime('YYYY', News.Value['post_date'].AsDateTime) +
      '/' + FormatDateTime('mm', News.Value['post_date'].AsDateTime) +
      '/' + News.Value['post_name'].AsString;
    Result := Result + '<li><a href="' + url + '">' + News.Value['post_title'].AsString +
      '</a></li>';
    News.Data.Next;
  end;
  Result := Result + '</ul></div>';


  FreeAndNil(News);
end;

procedure TWPTagsWebModule.TagController(Sender: TObject; const TagString: string;
  TagParams: TStringList; out ReplaceText: string);
var
  _tags: TStringList;
begin
  inherited TagController(Sender, TagString, TagParams, ReplaceText);
  _tags := ExplodeTags(TagString);
  if _tags.Count = 0 then
  begin
    ReplaceText := '[]';
    FreeAndNil(_tags);
    Exit;
  end;
  case _tags[0] of
    '$maincontent':
    begin
      ReplaceText := View;
    end;
  end;

  FreeAndNil(_tags);
end;

procedure TWPTagsWebModule.DoBlockController(Sender: TObject;
  FunctionName: string; Parameter: TStrings; var ResponseString: string);
begin
  case FunctionName of
    'tagcloud':
    begin
      ResponseString := GetTagCloud(Parameter);
    end;
  end;
end;

{$ifdef wordpress}
initialization
  RegisterHTTPModule('tag', TWPTagsWebModule, True);
{$endif}

end.

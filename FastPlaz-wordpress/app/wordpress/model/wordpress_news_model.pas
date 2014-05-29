unit wordpress_news_model;

{$mode objfpc}{$H+}

interface

uses
  database_lib,
  Classes, SysUtils;

type

  { TWordpressNews }

  TWordpressNews = class(TSimpleModel)
  private
    function GetCountPosts: integer;
  public
    constructor Create(const DefaultTableName: string = '');
    function FindByTags(const TagsName: string ):boolean;
    procedure AddHit( const ID:integer);

    Property CountPosts: integer Read GetCountPosts;
  end;


implementation

uses fastplaz_handler, common, wordpress_tags_model;

{ TWordpressNews }

function TWordpressNews.GetCountPosts: integer;
begin
  if Data.Active then Data.Close;
  Data.SQL.Text:= 'SELECT COUNT(*) as countposts FROM '+AppData.table_prefix+'_posts WHERE post_status = "publish" AND post_type = "post"';
  Data.Open;
  if RecordCount > 0 then
    Result := Value['countposts'].AsInteger;
end;

constructor TWordpressNews.Create(const DefaultTableName: string);
begin
  inherited Create(AppData.table_prefix + '_posts');
end;

function TWordpressNews.FindByTags(const TagsName: string): boolean;
var
  tag_id : integer;
begin
  if Data.Active then Data.Close;
  if TagsName = '' then Exit;

  // find tag
  with TWordpressTags.Create() do
  begin
    AddInnerJoin( AppData.table_prefix+'_term_taxonomy', 'term_id', AppData.table_prefix+'_terms.term_id', []);
    Find( [AppData.table_prefix+'_terms.slug = "'+TagsName+'"',AppData.table_prefix+'_term_taxonomy.taxonomy = "post_tag"']);
    if RecordCount = 0 then
    begin
      Result := False;
      Free;
      Exit;
    end;
    tag_id:= Value['term_id'].AsInteger;
    Free;
  end;

  AddInnerJoin( AppData.table_prefix+'_term_relationships', 'object_id', AppData.table_prefix+'_posts.ID', [] );
  GroupBy( AppData.table_prefix+'_posts.ID');
  Find( [
      AppData.table_prefix+'_term_relationships.term_taxonomy_id IN ('+i2s(tag_id)+')',
      AppData.table_prefix+'_posts.post_type = "post"',
      AppData.table_prefix+'_posts.post_status = "publish"'
    ], AppData.table_prefix+'_posts.post_date DESC', 10);

  Result := True;
end;

procedure TWordpressNews.AddHit(const ID: integer);
begin
  // prepare for add hit counter
end;


end.




unit wordpress_terms_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database_lib;

type

  { TWordpressTerms }

  TWordpressTerms = class(TSimpleModel)
  public
    constructor Create(const DefaultTableName: string = '');
    function GetObjectTerms( const ObjectID:integer; const Taxonomy: array of string):boolean;
  end;

implementation

uses common, fastplaz_handler;

{ TWordpressTerms }

constructor TWordpressTerms.Create(const DefaultTableName: string);
begin
  inherited Create(AppData.table_prefix+ '_terms');
end;

{
example:
Terms.GetObjectTerms( News['ID'].AsInteger, ['post_tag']);
}
function TWordpressTerms.GetObjectTerms(const ObjectID: integer;
  const Taxonomy: array of string): boolean;
var
  i:integer;
  where : string;
begin
  Result := False;
  for i:=Low(Taxonomy) to High(Taxonomy) do
  begin
    if where = '' then
      where := '"'+Taxonomy[i]+'"'
    else
      where:=where+',"'+Taxonomy[i]+'"';
  end;
  where := AppData.table_prefix+'_term_taxonomy.taxonomy IN ('+where+')';

  AddInnerJoin(AppData.table_prefix+'_term_taxonomy', 'term_id', AppData.table_prefix+'_terms.term_id', ['count']);
  AddInnerJoin(AppData.table_prefix+'_term_relationships', 'term_taxonomy_id', AppData.table_prefix+'_term_taxonomy.term_taxonomy_id', []);
  Find( [where, AppData.table_prefix+'_term_relationships.object_id='+i2s(ObjectID)], AppData.table_prefix+'_terms.name ASC');
  if RecordCount > 0 then
    Result := True;
end;

end.


unit test_solr_lib;

{$mode objfpc}{$H+}

interface

uses
  solr_lib, Classes, SysUtils, fpcunit, testregistry;

type

  { TFakeSolr - answers Open with a canned status/body }

  TFakeSolr = class(TSOLRModel)
  public
    RequestedURL: string;
    Code: integer;
    Body: string;
    Fails: boolean;
    constructor Create;
  protected
    function DoRequest(const AURL: string; out ACode: integer;
      out AText: string): boolean; override;
  end;

  { TSolrLibTest }

  TSolrLibTest = class(TTestCase)
  private
    FSolr: TFakeSolr;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEncodeParam;
    procedure TestGenerateURL;
    procedure TestGenerateURLEncodesQuery;
    procedure TestInitialStateIsEOF;
    procedure TestOpenIteratesDocs;
    procedure TestOpenEmptyResultIsEOF;
    procedure TestOpenHttpError;
    procedure TestOpenRequestFailed;
    procedure TestOpenSolrError;
    procedure TestOpenInvalidJson;
    procedure TestOpenTwiceReplacesResult;
    procedure TestFieldMissingAndTypes;
    procedure TestFirstAndNext;
    procedure TestLoadFromJsonWithoutNumFound;
    procedure TestDeprecatedPostAlias;
  end;

implementation

const
  TWO_DOCS =
    '{"responseHeader":{"status":0,"QTime":1},' +
    '"response":{"numFound":42,"start":0,"docs":[' +
    '{"id":"1","title":"First","tags":["a","b"],"price":9.5,"flag":true,"nothing":null},' +
    '{"id":"2","title":"Second"}' +
    ']}}';

  NO_DOCS =
    '{"responseHeader":{"status":0},"response":{"numFound":0,"start":0,"docs":[]}}';

{ TFakeSolr }

constructor TFakeSolr.Create;
begin
  inherited Create;
  Code := 200;
  Body := TWO_DOCS;
end;

function TFakeSolr.DoRequest(const AURL: string; out ACode: integer;
  out AText: string): boolean;
begin
  RequestedURL := AURL;
  ACode := Code;
  AText := Body;
  Result := not Fails;
end;

{ TSolrLibTest }

procedure TSolrLibTest.SetUp;
begin
  FSolr := TFakeSolr.Create;
end;

procedure TSolrLibTest.TearDown;
begin
  FSolr.Free;
end;

procedure TSolrLibTest.TestEncodeParam;
begin
  AssertEquals('abc-_.~09', SolrEncodeParam('abc-_.~09'));
  AssertEquals('%2A%3A%2A', SolrEncodeParam('*:*'));
  AssertEquals('a%20b%26c%3Dd%2Be%23f', SolrEncodeParam('a b&c=d+e#f'));
  AssertEquals('', SolrEncodeParam(''));
end;

procedure TSolrLibTest.TestGenerateURL;
begin
  FSolr.Host := 'h';
  FSolr.Port := '8983';
  FSolr.Address := '/solr';
  FSolr.Core := 'core1';
  FSolr.Query.Text := '*:*';
  FSolr.Index := 5;
  FSolr.Limit := 2;
  FSolr.Sort := 'id desc';
  FSolr.Fields := 'id,title';
  FSolr.Indentation := True;
  FSolr.Open;
  AssertEquals('http://h:8983/solr/core1/select?wt=json&indent=true&start=5&rows=2' +
    '&sort=id%20desc&fl=id%2Ctitle&q=%2A%3A%2A', FSolr.RequestedURL);
  AssertEquals(FSolr.RequestedURL, FSolr.URL);
  FSolr.SSL := True;
  FSolr.Open;
  AssertEquals('https://', Copy(FSolr.RequestedURL, 1, 8));
end;

procedure TSolrLibTest.TestGenerateURLEncodesQuery;
begin
  FSolr.Core := 'c';
  // a term with '&' used to inject a second parameter
  FSolr.Query.Text := 'title:"hello world"&rows=100000';
  FSolr.Open;
  AssertEquals('http://localhost:8389/c/select?wt=json&q=title%3A%22hello%20world%22%26rows%3D100000',
    FSolr.RequestedURL);
  FSolr.Query.Clear;
  FSolr.Query.Add('a:1');
  FSolr.Query.Add('b:2');
  FSolr.Open;
  AssertTrue('lines joined with a space', Pos('q=a%3A1%20b%3A2', FSolr.RequestedURL) > 0);
end;

procedure TSolrLibTest.TestInitialStateIsEOF;
begin
  AssertTrue(FSolr.EOF);
  AssertEquals('', FSolr['id']);
  AssertEquals('', FSolr.ResultText);
  AssertEquals(0, FSolr.ResultCode);
  AssertNull(FSolr.ResultJSON);
end;

procedure TSolrLibTest.TestOpenIteratesDocs;
var
  n: integer;
  ids: string;
begin
  AssertTrue(FSolr.Open);
  AssertEquals(200, FSolr.ResultCode);
  AssertEquals(TWO_DOCS, FSolr.ResultText);
  AssertEquals(2, FSolr.RecordCount);
  AssertEquals(42, FSolr.TotalCount);
  AssertEquals('', FSolr.ErrorMessage);
  n := 0;
  ids := '';
  while not FSolr.EOF do
  begin
    Inc(n);
    ids := ids + FSolr['id'];
    FSolr.Next;
    if n > 10 then
      Fail('loop does not terminate');
  end;
  AssertEquals(2, n);
  AssertEquals('12', ids);
end;

procedure TSolrLibTest.TestOpenEmptyResultIsEOF;
begin
  // used to loop forever / raise "list index out of bounds"
  FSolr.Body := NO_DOCS;
  AssertTrue(FSolr.Open);
  AssertTrue(FSolr.EOF);
  AssertEquals(0, FSolr.RecordCount);
  AssertEquals(0, FSolr.TotalCount);
  AssertEquals('', FSolr['id']);
  FSolr.Next;
  AssertTrue(FSolr.EOF);
end;

procedure TSolrLibTest.TestOpenHttpError;
begin
  // Open used to return True whatever happened
  FSolr.Code := 404;
  FSolr.Body := 'not found';
  AssertFalse(FSolr.Open);
  AssertTrue(FSolr.EOF);
  AssertEquals(404, FSolr.ResultCode);
  AssertEquals('not found', FSolr.ResultText);
  AssertEquals('http status 404', FSolr.ErrorMessage);
  AssertEquals(-1, FSolr.RecordCount);
end;

procedure TSolrLibTest.TestOpenRequestFailed;
begin
  FSolr.Fails := True;
  FSolr.Code := 0;
  FSolr.Body := 'connect refused';
  AssertFalse(FSolr.Open);
  AssertTrue(FSolr.EOF);
  AssertTrue(FSolr.ErrorMessage, Pos('request failed', FSolr.ErrorMessage) = 1);
end;

procedure TSolrLibTest.TestOpenSolrError;
begin
  FSolr.Body := '{"responseHeader":{"status":400},"error":{"msg":"undefined field x","code":400}}';
  AssertFalse(FSolr.Open);
  AssertTrue(FSolr.EOF);
  AssertEquals('solr status 400: undefined field x', FSolr.ErrorMessage);
end;

procedure TSolrLibTest.TestOpenInvalidJson;
begin
  FSolr.Body := '<html>oops</html>';
  AssertFalse(FSolr.Open);
  AssertTrue(FSolr.EOF);
  AssertTrue(Pos('invalid JSON', FSolr.ErrorMessage) = 1);
  FSolr.Body := '{"responseHeader":{"status":0}}';
  AssertFalse(FSolr.Open);
  AssertEquals('response.docs missing', FSolr.ErrorMessage);
end;

procedure TSolrLibTest.TestOpenTwiceReplacesResult;
begin
  AssertTrue(FSolr.Open);
  AssertEquals(2, FSolr.RecordCount);
  FSolr.Body := NO_DOCS;
  AssertTrue(FSolr.Open);
  AssertEquals(0, FSolr.RecordCount);
  AssertTrue(FSolr.EOF);
  FSolr.Code := 500;
  AssertFalse(FSolr.Open);
  AssertNull('previous document released on failure', FSolr.ResultJSON);
end;

procedure TSolrLibTest.TestFieldMissingAndTypes;
begin
  AssertTrue(FSolr.Open);
  AssertEquals('First', FSolr['title']);
  AssertEquals('a, b', FSolr['tags']);
  AssertEquals('9.5', FSolr['price']);
  AssertEquals('True', FSolr['flag']);
  AssertEquals('', FSolr['nothing']);
  AssertEquals('missing field', '', FSolr['does-not-exist']);
  FSolr.Next;
  AssertEquals('', FSolr['tags']);
  AssertEquals('Second', FSolr['title']);
end;

procedure TSolrLibTest.TestFirstAndNext;
begin
  AssertTrue(FSolr.Open);
  FSolr.Next;
  AssertEquals('2', FSolr['id']);
  FSolr.Next;
  AssertTrue(FSolr.EOF);
  FSolr.Next;
  AssertTrue('Next after EOF stays EOF', FSolr.EOF);
  FSolr.First;
  AssertFalse(FSolr.EOF);
  AssertEquals('1', FSolr['id']);
end;

procedure TSolrLibTest.TestLoadFromJsonWithoutNumFound;
begin
  AssertTrue(FSolr.LoadFromJson('{"response":{"docs":[{"id":"x"}]}}'));
  AssertEquals(1, FSolr.RecordCount);
  AssertEquals(1, FSolr.TotalCount);
  AssertEquals('x', FSolr['id']);
end;

procedure TSolrLibTest.TestDeprecatedPostAlias;
begin
  {$WARN 5043 off}
  FSolr.Post := '1234';
  {$WARN 5043 on}
  AssertEquals('1234', FSolr.Port);
end;

initialization
  RegisterTest(TSolrLibTest);

end.

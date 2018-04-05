unit example_controller;

{$mode objfpc}{$H+}

interface

uses
  process,
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;

type

  { TExampleWebModule }

  TExampleWebModule = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure dbRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure oneRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure twoRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
  public
    procedure TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);
  end;

var
  ExampleWebModule: TExampleWebModule;

implementation

uses logutil_lib, theme_controller, database_lib, fastplaz_handler, common,
  users_model;

{$R *.lfm}

{ TExampleWebModule }


procedure TExampleWebModule.DataModuleRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  if Actions.FindAction(ARequest.QueryFields.Values[ActionVar]) = nil then
  begin
    Response.Content := ThemeUtil.Render(@TagController);
    Handled := True;
  end;
end;

procedure TExampleWebModule.dbRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
var
  Users: TUsers;
begin
  DataBaseInit;
  AResponse.Content := 'Example App - Connect to Database';
  Users := TUsers.Create('user');

  //-- get all data
  Users.GetAll;
  AResponse.Contents.Add('<hr>');
  while not Users.EOF do
  begin
    AResponse.Contents.Add('<br>' + Users['Host']);
    Users.Next;
  end;

  //-- find some data
  Users.Find(['Host="localhost"']);
  AResponse.Contents.Add('<hr>');
  while not Users.EOF do
  begin
    AResponse.Contents.Add('<br>' + Users['Host']);
    Users.Next;
  end;

  FreeAndNil(Users);

  Handled := True;
end;

procedure TExampleWebModule.oneRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Content := ThemeUtil.Render(@TagController);
  //AResponse.Content := 'Example App - COBA - one';

  Handled := True;
end;

procedure TExampleWebModule.twoRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Content := 'Example App - COBA - two';
  Handled := True;
end;

procedure TExampleWebModule.TagController(Sender: TObject;
  const TagString: string; TagParams: TStringList; out ReplaceText: string);
var
  tags: TStrings;
  content_internal, path, template_file: string;
begin
  // for fcl-web, use direct call to ThemeUtil.TagController
  ThemeUtil.TagController(Sender, TagString, TagParams, ReplaceText);

  path := StringReplace(Request.PathInfo, '/example/', '', [rfReplaceAll]);
  path := Copy(path, 1, Length(path) - 1);
  if path = '' then
    path := 'main';
  template_file := 'modules/example/' + path + '.html';

  content_internal := 'subfunction : ' + path;

  tags := ExplodeTags(TagString);
  case tags[0] of
    'maincontent':
    begin
      ReplaceText := ThemeUtil.RenderFromContent(@TagController,
        content_internal, template_file);
    end;
  end;

  FreeAndNil(tags);
end;

initialization

end.

unit contact_controller;

{$mode objfpc}{$H+}

interface

uses
  contact_model,
  Classes, SysUtils, html_lib, fpcgi, fpjson, json_lib, HTTPDefs, 
  fastplaz_handler, database_lib, string_helpers, dateutils,
  datetime_helpers, array_helpers;

type
  TContactController = class(TMyCustomController)
  private
    Contacts: TContactModel;
    htmlLayout: string;
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList
      ): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses theme_controller, common;

constructor TContactController.CreateNew(AOwner: TComponent; CreateMode: integer
  );
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  Contacts := TContactModel.Create();
end;

destructor TContactController.Destroy;
begin
  Contacts.Free;
  inherited Destroy;
end;

// Init First
procedure TContactController.BeforeRequestHandler(Sender: TObject; 
  ARequest: TRequest);
begin
end;

// GET Method Handler
procedure TContactController.Get;
var
  userName, lastLogin, contactId: String;
  whereAsArray: TStringArray;
begin
  DataBaseInit();

  // membaca session
  username := _SESSION['username'];
  lastLogin := _SESSION['lastLogin'];

  // dapatkan id dari query string
  contactId := _GET['id']; //todo: sanitize

  htmlLayout := 'list';
  whereAsArray.Add('status_id=0');
  if contactId.IsNotEmpty then // jika ada id, tampilkan page detail
  begin
    htmlLayout := 'detail';
    whereAsArray.Add('cid='+contactId);
  end;

  Contacts.Find(whereAsArray);
  ThemeUtil.Assign('$Title', 'Contact List');
  ThemeUtil.Assign('$username', username);
  ThemeUtil.Assign('$lastLogin', lastLogin);
  ThemeUtil.AssignVar['$Contacts'] := @Contacts.Data;

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Response.Content := ThemeUtil.Render();
end;

// POST Method Handler
procedure TContactController.Post;
begin
  Response.Content := 'This is POST Method';
end;

function TContactController.Tag_MainContent_Handler(const TagName: string; 
  Params: TStringList): string;
begin
  Result := ThemeUtil.RenderFromContent(nil, '', 'modules/contact/'+htmlLayout+'.html');
end;



initialization
  // -> http://yourdomainname/contact
  // The following line should be moved to a file "routes.pas"
  Route[ '/contact'] := TContactController;

end.


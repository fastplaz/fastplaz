unit initialize_controller;

{$mode objfpc}{$H+}

interface

uses
  fpjson, jsonparser, jsonscanner, jsonConf,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler;

type

  { TLocalJSONParser }

  TLocalJSONParser = class(TJSONParser)
  private
    FScanner: TJSONScanner;
  public
{$IFDEF LSNEWFPC}
    property Scanner;
{$ELSE}
    property Scanner: TJSONScanner read FScanner;
{$ENDIF}
  end;

  { TInitializeModule }

  TInitializeModule = class(TMyCustomWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    htaccess, ContentFile: TStringList;
    function InitializeApp: boolean;
    function CreateDirCustom(const NewDir: string;
      CreateHtaccess: boolean = False): boolean;
    function TagMainContentHandler(const TagName: string; Params: TStringList): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses theme_controller, common;

function JsonBeautifier(const str: string): string;
var
  VJSONData: TJSONData = nil;
  VJSONParser: TLocalJSONParser;
begin
  if str = '' then
    Exit;
  ;
  Result := str;
  VJSONParser := TLocalJSONParser.Create(str);
  try
    VJSONParser.Strict := True;
    VJSONData := VJSONParser.Parse;
    Result := VJSONData.FormatJSON([], 2);
  except
  end;
  FreeAndNil(VJSONData);
  FreeAndNil(VJSONParser);
end;

procedure TInitializeModule.DataModuleRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  if FileExists('config/config.json') then
    _Redirect(BaseURL);

  InitializeApp;

  Tags['$maincontent'] := @TagMainContentHandler;
  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

function TInitializeModule.InitializeApp: boolean;
begin
  htaccess := TStringList.Create;
  with htaccess do
  begin
    Add('deny from all');
    Add('<FilesMatch "\.(css|js|png|gif|jpg|ico|swf|flv|eot|woff|ttf|svg)$">');
    Add('order allow,deny');
    Add('allow from all');
    Add('</filesmatch>');
  end;
  ContentFile := TStringList.Create;
  ContentFile.Text := _APP_URL;
  CreateDirCustom('config', True);
  CreateDirCustom('files', True);
  CreateDirCustom('modules');
  CreateDirCustom('ztemp', True);
  CreateDirCustom('ztemp/cache');
  CreateDirCustom('ztemp/error_log');
  CreateDirCustom('ztemp/sessions');
  CreateDirCustom('themes', True);
  CreateDirCustom('themes/default', True);
  CreateDirCustom('themes/default/js');
  CreateDirCustom('themes/default/style');
  CreateDirCustom('themes/default/templates', True);
  with TStringList.Create do
  begin
    try
      Text := 'body{background:#eee;}';
      SaveToFile('themes/default/style/style.css');
      Text := '';
    except
    end;
    Add('<html>');
    Add('<head>');
    Add('  <title>FastPlaz - Generated Templates</title>');
    Add('  <link rel="stylesheet" id="fastplaz-style-css"  href="{$themefullpath}/style/style.css" type="text/css" media="all" />');
    Add('</head>');
    Add('<body>');
    Add('{$maincontent}');
    Add('</body>');
    Add('</html>');
    try
      SaveToFile('themes/default/templates/master.html');
      SaveToFile('themes/default/templates/home.html');
    except
    end;
    Free;
  end;

  // initialize configuration file
  Config.SetValue(_SYSTEM_SITENAME, _APP);
  Config.SetValue(_SYSTEM_WEBMASTER_EMAIL, 'webmaster@' +
    GetEnvironmentVariable('SERVER_NAME'));
  Config.SetValue(_SYSTEM_ERROR_URL, '/');
  Config.SetValue(_SYSTEM_ERROR_REDIRECT, False);
  Config.SetValue(_SYSTEM_THEME, 'default');
  Config.SetValue(_SYSTEM_TEMP_DIR, 'ztemp');

  Config.SetValue(_DATABASE_DRIVER, 'MySQL 5.0');
  Config.SetValue(_DATABASE_HOSTNAME, 'localhost');
  Config.SetValue(_DATABASE_USERNAME, 'your_username');
  Config.SetValue(_DATABASE_PASSWORD, 'your_password');
  Config.SetValue(_DATABASE_DATABASENAME, 'your_database');
  Config.SetValue(_DATABASE_TABLE_PREFIX, '');

  // beautifier json
  FreeAndNil(Config);
  ContentFile.LoadFromFile('config/config.json');
  ContentFile.Text := JsonBeautifier(ContentFile.Text);
  ContentFile.SaveToFile('config/config.json');
  Config := TJSONConfig.Create(nil);
  Config.Filename := 'config/config.json';


  FreeAndNil(ContentFile);
  FreeAndNil(htaccess);
  Result := True;
end;

function TInitializeModule.CreateDirCustom(const NewDir: string;
  CreateHtaccess: boolean): boolean;
begin
  try
    Result := ForceDirectories(NewDir);
    ContentFile.SaveToFile(NewDir + '/index.html');
    if CreateHtaccess then
      htaccess.SaveToFile(NewDir + '/.htaccess');
    Result := True;
  except
    Result := False;
  end;
end;

function TInitializeModule.TagMainContentHandler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '<h1>FastPlaz</h1>App structures have been successfully generated !!!';
end;

constructor TInitializeModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  CreateSession := True;
  OnRequest := @DataModuleRequest;
end;

destructor TInitializeModule.Destroy;
begin
  inherited Destroy;
end;

initialization
  Route.Add( 'initialize', TInitializeModule);

end.

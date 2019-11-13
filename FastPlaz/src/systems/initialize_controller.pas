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
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    htaccess, ContentFile: TStringList;
    function InitializeApp: boolean;
    function CreateDirCustom(const NewDir: string;
      CreateHtaccess: boolean = False): boolean;
    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;
  end;

implementation

uses theme_controller, common, config_lib;

function JsonBeautifier(const str: string): string;
var
  VJSONData: TJSONData = nil;
  VJSONParser: TLocalJSONParser;
begin
  if str = '' then
    Exit;
  ;
  Result := str;
  VJSONParser := TLocalJSONParser.Create(str, [joStrict, joUTF8]);
  try
    //VJSONParser.Strict := True; // deprecated
    VJSONData := VJSONParser.Parse;
    Result := VJSONData.FormatJSON([], 2);
  except
  end;
  FreeAndNil(VJSONData);
  FreeAndNil(VJSONParser);
end;

constructor TInitializeModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  CreateSession := True;
  OnRequest := @RequestHandler;
end;

destructor TInitializeModule.Destroy;
begin
  inherited Destroy;
end;

procedure TInitializeModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  if FileExists('config/config.json') then
    Redirect(BaseURL);

  InitializeApp;

  Tags['maincontent'] := @Tag_MainContent_Handler;
  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

function TInitializeModule.InitializeApp: boolean;
var
  s: string;
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
  CreateDirCustom('ztemp/logs');
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
    Add('  <link rel="stylesheet" id="fastplaz-style-css"  href="[$themefullpath]/style/style.css" type="text/css" media="all" />');
    Add('</head>');
    Add('<body>');
    Add('[flashmessages]');
    Add('[maincontent]');
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
  Config.Filename := 'config/config.json';
  Config.SetValue(_SYSTEM_SITENAME, _APP);
  Config.SetValue(_SYSTEM_SLOGAN, _APP_SLOGAN);
  Config.SetValue(_SYSTEM_BASEURL, '');
  Config.SetValue(_SYSTEM_WEBMASTER_EMAIL, 'admin@' +
    WideString(GetEnvironmentVariable('SERVER_NAME')));
  Config.SetValue(_SYSTEM_ERROR_URL, '/');
  Config.SetValue(_SYSTEM_ERROR_REDIRECT, False);
  Config.SetValue(_SYSTEM_DEBUG, True);
  Config.SetValue(_SYSTEM_THEME_ENABLE, True);
  Config.SetValue(_SYSTEM_THEME, 'default');
  Config.SetValue(_SYSTEM_TEMP_DIR, 'ztemp');
  Config.SetValue(_SYSTEM_SESSION_TIMEOUT, 0);

  Config.SetValue(UTF8Decode(format(_DATABASE_DRIVER, ['default'])), 'MySQL 5.6');
  Config.SetValue(UTF8Decode(format(_DATABASE_HOSTNAME, ['default'])), 'localhost');
  Config.SetValue(UTF8Decode(format(_DATABASE_PORT, ['default'])), '');
  Config.SetValue(UTF8Decode(format(_DATABASE_USERNAME, ['default'])), 'your_username');
  Config.SetValue(UTF8Decode(format(_DATABASE_PASSWORD, ['default'])), 'your_password');
  Config.SetValue(UTF8Decode(format(_DATABASE_DATABASENAME, ['default'])), 'your_database');
  Config.SetValue(UTF8Decode(format(_DATABASE_TABLE_PREFIX, ['default'])), '');
  //Config.SetValue(UTF8Decode(format(_DATABASE_LIBRARY, ['default'])), '../libs/win/libmysql.dll');
  Config.SetValue(UTF8Decode(format(_DATABASE_LIBRARY, ['default'])), '');

  Config.SetValue(UTF8Decode(format(_MAIL_MAILSERVER, ['default'])), 'your.mail.server');
  Config.SetValue(UTF8Decode(format(_MAIL_USERNAME, ['default'])), 'your-username');
  Config.SetValue(UTF8Decode(format(_MAIL_PASSWORD, ['default'])), 'your-password');
  Config.SetValue(UTF8Decode(format(_MAIL_SMTPPORT, ['default'])), '465');
  Config.SetValue(UTF8Decode(format(_MAIL_SSL, ['default'])), True);
  Config.SetValue(UTF8Decode(format(_MAIL_TLS, ['default'])), True);

  FreeAndNil(Config);

  // beautifier json
  ContentFile.LoadFromFile('config/config.json');
  ContentFile.Text := JsonBeautifier(ContentFile.Text);
  ContentFile.SaveToFile('config/config.json');
  Config := TMyConfig.Create(nil);
  Config.Filename := 'config/config.json';

  // root .htaccess
  s := Application.EnvironmentVariable['SCRIPT_NAME'];
  s := ExtractFileName(s);
  with TStringList.Create do
  begin
    Add('# Generated by FastPlaz');
    Add('ErrorDocument 404 404.html');
    Add('ErrorDocument 500 500.html');
    Add('');
    Add('RewriteEngine On');
    Add('AddHandler cgi-script .cgi');
    Add('AddHandler cgi-script .bin');
    Add('AddHandler cgi-script .pas');
    Add('AddHandler cgi-script .exe');
    Add('Options +ExecCGI');
    Add('');
    Add('DirectoryIndex ' + s);
    Add('');
    Add('RewriteCond %{REQUEST_FILENAME} -d [OR]');
    Add('RewriteCond %{REQUEST_FILENAME} -f [OR]');
    Add('RewriteCond %{REQUEST_FILENAME} -l');
    Add('RewriteRule ^(.*)$ - [NC,L]');
    Add('#RewriteRule ^(.*)$ 404.php [QSA,L]');
    Add('RewriteRule ^(.*)$ ' + s + '/$1 [QSA,L]');
    Add('');
    try
      SaveToFile('.htaccess');
    except
    end;
    Free;
  end;

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

function TInitializeModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
begin
  Result := '<h1>FastPlaz</h1>Application Structure successfully generated !!!';
end;


initialization
  //Route.Add('initialize', TMyCustomWebModuleClass(TInitializeModule));

end.

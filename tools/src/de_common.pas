unit de_common;

{$mode objfpc}{$H+}

interface

uses
  config_lib, regexpr_lib, SQLDB, fpjson,
  SynEdit, Classes, SysUtils, LazFileUtils,
  //INTF
  PackageIntf, IDEWindowIntf, SrcEditorIntf;

const
  FASTPLAZ_CONFIG_FILENAME = 'config.json';
  FDE_WINDOW_NAME = 'IDEFDEWindow';
  FDE_BROWSER_WINDOW_NAME = 'IDEFDEBrowserWindow';
  FDE_CONFIG_PATH = '.fastplaz';
  FDE_CONFIG_FILE_NAME = 'fastplaz_dbe.json';

  FASTCODE_WINDOW_NAME = 'IDEFASTCODEWindow';

  FASTPLAZ_EXPERT_DBE_MAINMENU_NAME = 'mnu_FastPlazExpertMainMenu';
  FASTPLAZ_EXPERT_DBE_MAINMENU_CAPTION = 'FastPla&z';

  VK_SPACE = 32;
  VK_KEYLEFT = 37;
  VK_KEYRIGHT = 39;
  VK_KEYUP = 38;
  VK_KEYDOWN = 40;
  VK_HOME = 36;
  VK_END = 35;

  ICO_CONNECTION_ON = 10;
  ICO_CONNECTION_OFF = 11;
  ICO_TABLE = 16;
  ICO_VIEW = 8;
  ICO_MYSQL = 1;
  ICO_POSTGRES = 2;
  ICO_ORACLE = 3;
  ICO_ODBC = 4;
  ICO_INTERBASE = 5;
  ICO_FIREBIRD = 5;
  ICO_MSSQL = 6;
  ICO_SQLITE = 7;

  QUERY_SELECT_REGEX = '(?is)(SELECT|VIEW|SHOW|DESCRIBE) (.*)';
  QUERY_DELETE_REGEX = '(?is)(DROP|DELETE) (.*)';
  TAB_PREFIX = 'tbs_';
  TAB_EDITOR_INDEX = 1;
  TAB_QUERY_INDEX = 2;
  TAB_GRID_INDEX = 4;


  SQL_SHOW_TABLES_MYSQL = 'show tables';
  SQL_SHOW_TABLES_POSTGRES =
    'SELECT * FROM information_schema.tables WHERE table_schema = ''public''';
  SQL_SHOW_VIEWS_MYSQL = 'SHOW FULL TABLES IN %s WHERE TABLE_TYPE LIKE "VIEW";';
  SQL_SHOW_VIEWS_POSTGRES =
    ' select table_name from INFORMATION_SCHEMA.views WHERE table_schema = ANY (current_schemas(false))';
  SQL_STRUCTURE_TABLE_POSTGRES =
    'SELECT column_name "Field", data_type "Type", is_nullable "Null", '''' "Key" , column_default "Default", udt_name "Extras" '
    + ' FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name =''%s'';';

  REGEX_EMAIL = '([\w\d\-\.]+@[\w\d\-]+(\.[\w\d\-]+)+)';
  REGEX_EMAIL2 = '[^\w\d\-\.]([\w\d\-\.]+@[\w\d\-]+(\.[\w\d\-]+)+)[^\w\d\-\.]';
  REGEX_URL = '((http)|(https)|(ftp)):\/\/([\- \w]+\.)+\w{2,3}(\/ [%\-\w]+(\.\w{2,})?)*';
  REGEX_IP =
    '^(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9])\.(25[0-5]' +
    '|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\.(25[0-5]|2[0-4]' +
    '[0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\.(25[0-5]|2[0-4][0-9]|[' +
    '0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])$';
  REGEX_TELP = '\(([0-9]{2}|0{1}((x|[0-9]){2}[0-9]{2}))\)\s*[0-9]{3,4}[- ]*[0-9]{4}';
  REGEX_TELP2 = '^[01]?[- .]?\(?[2-9]\d{2}\)?[- .]?\d{3}[- .]?\d{4}$';

  // reveal in finder
  {$ifdef windows}
  rsRevealInFinder = 'Reveal in Explorer';
  {$else}
  rsRevealInFinder = 'Reveal in Finder';
  {$endif}

  FASTCODE_PROMPT = 'You are an expert Pascal programmer. Please answer with specific code. Start description with //. Dont use markdown, and a maximum length of 80 characters per line in description. Only give responses in the %language% language.';
  FASTCODE_OPENING_CHAR = '{';
  FASTCODE_CLOSING_CHAR = '}';

  LASTUPDATE_URL = 'https://raw.githubusercontent.com/fastplaz/fastplaz/development/LAST_UPDATED_DEVELOPMENT';
  LASTUPDATE_FILENAME = 'LAST_UPDATED_DEVELOPMENT';

resourcestring
  //rsDBConnector = 'DB Connector';
  RS_FAST_CODE_CONFIGURATION = 'Fast Code Configuration';
  RS_FAST_CODE = 'Fast Code Suggestion';
  RS_FAST_CODE_MARK = '//';
  RS_DATABASE_EXPLORER_MENU = 'Database Explorer';
  RS_JSON_TOOLS_MENU = 'JSON Validator && Formatter';
  RS_REGEX_TESTER_MENU = 'Regex Tester';
  RS_THERE_ARE_DEVELOPMENT_UPDATE = 'There are development update: ';
  rsDBExplorer = 'DB Explorer';
  rsViewDBConnector = 'View DB Connector';
  rsConfirmation = 'Confirmation';
  rsAskCancelDangerOperation = 'I will cancel this danger operation (drop/delete)?';
  rsAskCancelOperation = 'I will cancel this operation (drop/delete)?';

  // json tools
  rsJsonSEmpty   = 'Empty document';
  rsJsonSArray   = 'Array (%d elements)';
  rsJsonSObject  = 'Object (%d members)';
  rsJsonSNull    = 'null';

type
  TOnSQLCallback = procedure(const AStatus: integer; const AMessage: string;
    var AQuery: TSQLQuery) of object;

var
  GlobalConfig,
  Config: TMyConfig;

function preg_match(const ARegexExpression: string; AText: string): boolean;
procedure InserTextToEditor(const AText: string);
function GetCurrentLine(): string;
procedure ViewDBConnector(Sender: TObject);
procedure LoadConfiguration;

implementation

function preg_match(const ARegexExpression: string; AText: string): boolean;
begin
  Result := False;
  try
    with TRegExpr.Create do
    begin
      Expression := ARegexExpression;
      Result := Exec(AText);
      Free;
    end;
  except
  end;

end;

procedure InserTextToEditor(const AText: string);
var
  sourceEditor: TSourceEditorInterface;
  ASynEdit: TSynEdit;
begin
  sourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if sourceEditor <> nil then
  begin
    if sourceEditor.EditorControl is TSynEdit then
    begin
      ASynEdit := TSynEdit(sourceEditor.EditorControl);
      ASynEdit.InsertTextAtCaret(AText, scamAdjust);
      ASynEdit.SetFocus;
    end;
  end;
end;

function GetCurrentLine: string;
var
  sourceEditor: TSourceEditorInterface;
  ASynEdit: TSynEdit;
begin
  Result := '';
  sourceEditor := SourceEditorManagerIntf.ActiveEditor;
  if sourceEditor = nil then Exit;
  if not (sourceEditor.EditorControl is TSynEdit) then Exit;

  ASynEdit := TSynEdit(sourceEditor.EditorControl);
  Result := ASynEdit.SelText;
  if not Result.IsEmpty then
  begin
    Result := RS_FAST_CODE_MARK + Result;
    Exit;
  end;
  Result := ASynEdit.LineText;
end;

procedure ViewDBConnector(Sender: TObject);
var
  Pkg: TIDEPackage;
begin
  IDEWindowCreators.ShowForm(FDE_WINDOW_NAME, True);
  //if IDEFDEWindow <> nil then
  //begin
  //end;
end;

procedure LoadConfiguration;
var
  configurationPath, configurationFileName: string;
  templateData: TJSONObject;
begin
  if GlobalConfig = nil then
  begin
    configurationPath := GetUserDir + FDE_CONFIG_PATH;
    configurationFileName := configurationPath + DirectorySeparator + FASTPLAZ_CONFIG_FILENAME;
    if not DirectoryExistsUTF8(ConfigurationPath) then
      CreateDirUTF8(ConfigurationPath);

    GlobalConfig := TMyConfig.Create(nil);
    GlobalConfig.Filename := configurationFileName;
    GlobalConfig.Formatted := True;

    //initial config
    if not FileExistsUTF8(ConfigurationFileName) then
    begin
      templateData := TJSONObject.Create;
      templateData.Add('openai_api_key', '');
      templateData.Add('openai_model', 'gpt-3.5-turbo');
      templateData.Add('openai_temperature', 0);
      templateData.Add('openai_maxtoken', 250);
      templateData.Add('language', 'english');
      templateData.Add('prompt', FASTCODE_PROMPT);
      templateData.Add('opening_char', FASTCODE_OPENING_CHAR);
      templateData.Add('closing_char', FASTCODE_CLOSING_CHAR);
      GlobalConfig.SetDataValue('fastcode', templateData);
      FreeAndNil(templateData);

      GlobalConfig.Flush;
    end;
  end;
  GlobalConfig.Reload;
end;

end.

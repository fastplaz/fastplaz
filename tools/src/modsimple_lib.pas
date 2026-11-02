unit modsimple_lib;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, Controls, LazarusPackageIntf, ProjectIntf,
  Classes, SysUtils;

resourcestring
  rs_Mod_Default_Name = 'Create Simple Controller';
  rs_Mod_Default_Description = 'Create Simple Controller';

type

  { TFileDescDefaultModule }

  TFileDescDefaultModule = class(TFileDescPascalUnit)
  private
    IsAPI: boolean;
  public
    constructor Create; override;
    constructor Create(AsAPI: boolean);
    function Init(var NewFilename: string; NewOwner: TObject;
      var NewSource: string; Quiet: boolean): TModalResult; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const Filename, SourceName,
      ResourceName: string): string;
      override;
    function GetImplementationSource(const Filename, SourceName,
      ResourceName: string): string; override;
    function CreateSource(const Filename, SourceName, ResourceName: string): string;
      override;
  end;

  { TFileRouteDescModule }

  TFileRouteDescModule = class(TFileDescPascalUnit)
  private
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetImplementationSource(const Filename, SourceName,
      ResourceName: string): string; override;
  end;

var
  _GlobalProjectName: string;

// Shows the module wizard and stores its result in ModulTypeName and
// Permalink. Returns False when the wizard was cancelled.
function AskModuleName(const ADefaultTypeName, ADefaultPermalink: string): boolean;

implementation

uses fastplaz_tools_register, tools_lib, modsimple_wzd;

function AskModuleName(const ADefaultTypeName, ADefaultPermalink: string): boolean;
var
  wizard: TfModuleSimpleWizard;
begin
  Result := False;
  ModulTypeName := ADefaultTypeName;
  Permalink := ADefaultPermalink;
  wizard := TfModuleSimpleWizard.Create(nil);
  try
    if wizard.ShowModal <> mrOk then
      Exit;
    ModulTypeName := ControllerTypeName(wizard.edt_ModuleName.Text);
    Permalink := SanitizePermalink(wizard.edt_Permalink.Text);
    if Permalink = '' then
      Permalink := SanitizePermalink(wizard.edt_ModuleName.Text);
    if Permalink = '' then
      Permalink := ADefaultPermalink;
    Result := True;
  finally
    wizard.Free;
  end;
end;

{ TFileRouteDescModule }

constructor TFileRouteDescModule.Create;
begin
  inherited Create;
  DefaultFilename := 'routes.pas';
  DefaultFileExt := '.pas';
end;

function TFileRouteDescModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', fpjson, fastplaz_handler';
end;

function TFileRouteDescModule.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    str.Add('uses info_controller, ' + LowerCase(_GlobalProjectName) + '_controller;');
    str.Add('');
    str.Add('initialization');
    str.Add('  Route[ ''info''] := TInfoModule;');
    str.Add('  Route[ ''/''] := ' + ControllerTypeName(_GlobalProjectName) +
      '; // Main Controller');
    str.Add('');
    Result := str.Text;
  finally
    str.Free;
  end;
end;

{ TFileDescDefaultModule }

constructor TFileDescDefaultModule.Create;
begin
  inherited Create;
  DefaultFileExt := '.pas';
  VisibleInNewDialog := True;
  IsAPI := False;
end;

constructor TFileDescDefaultModule.Create(AsAPI: boolean);
begin
  Create;
  IsAPI := AsAPI;
end;

function TFileDescDefaultModule.Init(var NewFilename: string;
  NewOwner: TObject; var NewSource: string; Quiet: boolean): TModalResult;
begin
  Result := inherited Init(NewFilename, NewOwner, NewSource, Quiet);
  if Result <> mrOk then
    Exit;
  if bExpert then
    Exit; // the menu already asked
  if bCreateProject then
  begin
    ModulTypeName := ControllerTypeName(_GlobalProjectName);
    Permalink := 'main';
    Exit;
  end;
  if Quiet then
  begin
    ModulTypeName := 'TSampleController';
    Permalink := 'sample';
    Exit;
  end;
  if not AskModuleName('TSampleController', 'sample') then
    Exit(mrCancel);
  if NewFilename = '' then
    NewFilename := SanitizeIdentifier(Permalink) + '_controller.pas';
end;

function TFileDescDefaultModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  if not IsAPI then
    Result := Result + ', html_lib';
  Result := Result + ', fastplaz_app, fpjson, json_lib, HTTPDefs, fastplaz_handler, database_lib, dateutils, string_helpers, datetime_helpers, array_helpers, json_helpers';
end;

function TFileDescDefaultModule.GetLocalizedName: string;
begin
  Result := rs_Mod_Default_Name;
end;

function TFileDescDefaultModule.GetLocalizedDescription: string;
begin
  Result := rs_Mod_Default_Description;
end;

function TFileDescDefaultModule.GetInterfaceSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  with str do
  begin
    Add('type');
    Add('  ' + ModulTypeName + ' = class(TMyCustomController)');
    Add('  private');
    if not IsAPI then
    begin
      Add('    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;');
    end;
    Add('    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);');
    Add('  public');
    Add('    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;');
    Add('    destructor Destroy; override;');
    Add('');
    Add('    procedure Get; override;');
    Add('    procedure Post; override;');
    Add('  end;');
    Add('');
  end;
  Result := str.Text;
  FreeAndNil(str);
end;

function TFileDescDefaultModule.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  Result := inherited GetImplementationSource(FileName, SourceName, ResourceName);
  str := TStringList.Create;
  with str do
  begin
    if IsAPI then
    begin
      Add('uses common;');
    end
    else
    begin
      Add('uses theme_controller, common;');
    end;
    Add('');

    Add('constructor ' + ModulTypeName +
      '.CreateNew(AOwner: TComponent; CreateMode: integer);');
    Add('Begin');
    Add('  inherited CreateNew(AOwner, CreateMode);');
    Add('  BeforeRequest := @BeforeRequestHandler;');
    Add('End;');
    Add('');

    Add('destructor ' + ModulTypeName + '.Destroy;');
    Add('Begin');
    Add('  inherited Destroy;');
    Add('End;');
    Add('');

    Add('// Init First');
    Add('procedure ' + ModulTypeName +
      '.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);');
    Add('Begin');
    if IsAPI then
    begin
      Add('  Response.ContentType := ''application/json'';');
    end;
    Add('End;');
    Add('');

    Add('// GET Method Handler');
    Add('procedure ' + ModulTypeName + '.Get;');
    Add('Begin');
    if IsAPI then
    begin
      Add('  //---');
      Add('  Response.Content := ''{}'';');
    end
    else
    begin
      Add('  Tags[''maincontent''] := @Tag_MainContent_Handler; //<<-- tag maincontent handler');
      Add('  Response.Content := ThemeUtil.Render();');
    end;
    Add('End;');
    Add('');

    Add('// POST Method Handler');
    if IsAPI then
    begin
      Add('// CURL example:');
      Add('//   curl -X POST -H "Authorization: Basic dW5hbWU6cGFzc3dvcmQ=" "yourtargeturl"');
    end;
    Add('procedure ' + ModulTypeName + '.Post;');
    if IsAPI then
    begin
      Add('var');
      Add('  json : TJSONUtil;');
      Add('  authstring : string;');
      Add('Begin');
      Add('  authstring := Header[''Authorization''];');
      Add('  if authstring <> ''YourAuthKey'' then');
      Add('  begin');
      Add('    //');
      Add('  end;');
      Add('  json := TJSONUtil.Create;');
      Add('');
      Add('  json[''code''] := Int16(0);');
      Add('  json[''data''] := ''yourdatahere'';');
      Add('  json[''path01/path02/var01''] := ''value01'';');
      Add('  json[''path01/path02/var02''] := ''value02'';');
      Add('  CustomHeader[ ''ThisIsCustomHeader''] := ''datacustomheader'';');
      Add('');
      Add('  //---');
      Add('  Response.Content := json.AsJSON;');
      Add('  json.Free;');
    end
    else
    begin
      Add('Begin');
      Add('  Response.Content := ''This is POST Method'';');
    end;
    Add('End;');
    Add('');

    if not IsAPI then
    begin
      Add('function ' + ModulTypeName +
        '.Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;');
      Add('Begin');
      Add('');
      Add('  // your code here');
      Add('  Result:=h3(''Hello "' + UcWords(ResourceName) +
        '" Module ... FastPlaz !'');');
      Add('');
      Add('End;');
    end;

    Add('');
    Add('');
  end;
  Result := Result + str.Text;
  FreeAndNil(str);

  if not bCreateProject then
  begin
    Result := Result + LineEnding + 'initialization' + LineEnding +
      '  // -> http://yourdomainname/' + ResourceName + LineEnding +
      '  // The following line should be moved to a file "routes.pas"' +
      LineEnding + '  Route[ ''' + Permalink + '''] := ' + ModulTypeName +
      ';' + LineEnding + LineEnding;
  end;
end;

function TFileDescDefaultModule.CreateSource(
  const Filename, SourceName, ResourceName: string): string;
begin
  Result := inherited CreateSource(Filename, SourceName, Permalink);
  log('module "' + ModulTypeName + '" created', Filename);
end;

end.

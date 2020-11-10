unit modsimple_lib;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, Controls, LazarusPackageIntf, ProjectIntf,
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
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetUnitDirectives: string; virtual;
    function GetInterfaceSource(const Filename, SourceName,
      ResourceName: string): string;
      override;
    function GetImplementationSource(const Filename, SourceName,
      ResourceName: string): string; override;
    function GetResourceSource(const ResourceName: string): string; override;
    function CreateSource(const Filename, SourceName, ResourceName: string): string;
      override;
    procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); override;
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
  _GlobalProjectName : String;

implementation

uses fastplaz_tools_register, modsimple_wzd;

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
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
  str := TStringList.Create;
  with str do
  begin
    Add('uses info_controller, ' + LowerCase(_GlobalProjectName) + '_controller;');
    Add('');
    Add('initialization');
    Add('  Route[ ''info''] := TInfoModule;');
    Add('  Route[ ''/''] := T' + ucwords(_GlobalProjectName) + 'Controller; // Main Controller');
    Add('');
  end;

  Result := str.Text;
  FreeAndNil(str);
end;

{ TFileDescDefaultModule }

constructor TFileDescDefaultModule.Create;
begin
  inherited Create;
  //Name:=rs_Mod_Default_Name;
  DefaultFileExt := '.pas';
  VisibleInNewDialog := True;
  IsAPI := False;
end;

constructor TFileDescDefaultModule.Create(AsAPI: boolean);
begin
  Create;
  IsAPI := AsAPI;
end;

function TFileDescDefaultModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  if not IsAPI then
    Result := Result + ', html_lib';
  Result := Result + ', fpcgi, fpjson, json_lib, HTTPDefs, fastplaz_handler, database_lib, string_helpers, dateutils, datetime_helpers';
end;

function TFileDescDefaultModule.GetLocalizedName: string;
begin
  Result := inherited GetLocalizedName;
  Result := rs_Mod_Default_Name;
end;

function TFileDescDefaultModule.GetLocalizedDescription: string;
begin
  Result := inherited GetLocalizedDescription;
  Result := rs_Mod_Default_Description;
end;

function TFileDescDefaultModule.GetUnitDirectives: string;
begin
  Result := '{$mode objfpc}{$H+}';
  if Owner is TLazProject then
    Result := CompilerOptionsToUnitDirectives(TLazProject(Owner).LazCompilerOptions);
end;

function TFileDescDefaultModule.GetInterfaceSource(
  const Filename, SourceName, ResourceName: string): string;
var
  str: TStringList;
begin
  //Result:=inherited GetInterfaceSource(Filename, SourceName, ResourceName);
  str := TStringList.Create;
  with str do
  begin
    Add('type');
    Add('  ' + ModulTypeName + ' = class(TMyCustomController)');
    //Add('    procedure RequestHandler(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
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
    //Add('  OnRequest := @RequestHandler;');
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

    //Add('procedure ' + ModulTypeName + '.RequestHandler(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
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
      Add('  Result:=h3(''Hello "' + ucwords(ResourceName) +
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


function TFileDescDefaultModule.GetResourceSource(const ResourceName: string): string;
begin
  Result := inherited GetResourceSource(ResourceName);
end;

function TFileDescDefaultModule.CreateSource(
  const Filename, SourceName, ResourceName: string): string;
begin
  if not bExpert then
  begin
    Permalink := 'sample';
    ModulTypeName := 'TSampleModule';
    if not bCreateProject then
    begin
      with TfModuleSimpleWizard.Create(nil) do
      begin
        if ShowModal = mrOk then
        begin
          if edt_ModuleName.Text <> '' then
            ModulTypeName := 'T' + StringReplace(UcWords(edt_ModuleName.Text),
              ' ', '', [rfReplaceAll]) + 'Controller';
          Permalink := edt_Permalink.Text;
          if Permalink = '' then
          begin
            Permalink := StringReplace(UcWords(edt_ModuleName.Text),
              ' ', '', [rfReplaceAll]);
          end;
        end;
        Free;
      end;
    end
    else
    begin
      ModulTypeName := 'T' + ucwords(_GlobalProjectName) + 'Controller';
      Permalink := 'main';
    end;
  end;
  Result := inherited CreateSource(Filename, SourceName, Permalink);
  log('module "' + ModulTypeName + '" created', Filename);
end;

procedure TFileDescDefaultModule.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  inherited UpdateDefaultPascalFileExtension(DefPasExt);
end;


end.

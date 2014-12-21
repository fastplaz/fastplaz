unit modsimple_lib;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, Controls, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf,
  Classes, SysUtils;

resourcestring
  rs_Mod_Default_Name = 'Module Generator - Simple';
  rs_Mod_Default_Description = 'Create FastPlaz simple module';

type

  { TFileDescDefaultModule }

  TFileDescDefaultModule = class(TFileDescPascalUnit)
  private
  public
    constructor Create; override;
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
  Result := Result + ', fastplaz_handler';
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
    Add('uses info_controller, main;');
    Add('');
    Add('initialization');
    Add('  Route.Add( ''main'', TMainModule);');
    Add('  Route.Add( ''info'', TInfoModule);');
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
end;

function TFileDescDefaultModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', fpcgi, HTTPDefs, fastplaz_handler, html_lib, database_lib';
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
  Result:='{$mode objfpc}{$H+}';
  if Owner is TLazProject then
    Result:=CompilerOptionsToUnitDirectives(TLazProject(Owner).LazCompilerOptions);
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
    Add('  ' + ModulTypeName + ' = class(TMyCustomWebModule)');
    Add('    procedure RequestHandler(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
    Add('  private');
    Add('    function Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;');
    Add('  public');
    Add('    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;');
    Add('    destructor Destroy; override;');
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
    Add('uses theme_controller, common;');
    Add('');

    Add('constructor ' + ModulTypeName +
      '.CreateNew(AOwner: TComponent; CreateMode: integer);');
    Add('Begin');
    Add('  inherited CreateNew(AOwner, CreateMode);');
    Add('  OnRequest := @RequestHandler;');
    Add('End;');
    Add('');

    Add('destructor ' + ModulTypeName + '.Destroy;');
    Add('Begin');
    Add('  inherited Destroy;');
    Add('End;');
    Add('');

    Add('procedure ' + ModulTypeName +
      '.RequestHandler(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
    Add('Begin');
    Add('  Tags[''$maincontent''] := @Tag_MainContent_Handler; //<<-- tag $maincontent handler');
    Add('  Response.Content := ThemeUtil.Render();');
    Add('  Handled := True;');
    Add('End;');
    Add('');

    Add('function ' + ModulTypeName +
      '.Tag_MainContent_Handler(const TagName: string; Params: TStringList): string;');
    Add('Begin');
    Add('');
    Add('  // your code here');
    Add('  Result:=h3(''Hello "' + ucwords(ResourceName) + '" Module ... FastPlaz !'');');
    Add('');
    Add('End;');
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
      LineEnding + '  Route.Add(''' + Permalink + ''',' + ModulTypeName +
      ');' + LineEnding + LineEnding;
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
  begin;
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
              ' ', '', [rfReplaceAll]) + 'Module';
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
      ModulTypeName := 'TMainModule';
      Permalink := 'main';
    end;
  end;
  Result := inherited CreateSource(Filename, SourceName, Permalink);
  log('module "' + ModulTypeName + '" created');
end;

procedure TFileDescDefaultModule.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  inherited UpdateDefaultPascalFileExtension(DefPasExt);
end;


end.

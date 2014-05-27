unit modsimple_lib;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, Controls, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf,
  Classes, SysUtils;

resourcestring
  rs_Mod_Default_Name = 'FastPlaz - simple default module';
  rs_Mod_Default_Description = 'create fastplaz simple module';

type

  { TFileDescDefaultModule }

  TFileDescDefaultModule = class(TFileDescPascalUnit)
  private
    ModulTypeName,
    Permalink : string;
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
    function GetResourceSource(const ResourceName: string): string; override;
    function CreateSource(const Filename, SourceName, ResourceName: string): string; override;
    procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); override;
  end;

implementation

uses fastplaz_tools_register, modsimple_wzd;

{ TFileDescDefaultModule }

constructor TFileDescDefaultModule.Create;
begin
  inherited Create;
  //Name:=rs_Mod_Default_Name;
  DefaultFileExt:='.pas';
  VisibleInNewDialog:=true;
end;

function TFileDescDefaultModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', fpcgi, HTTPDefs, custom_handler';
end;

function TFileDescDefaultModule.GetLocalizedName: string;
begin
  Result:=inherited GetLocalizedName;
  Result:=rs_Mod_Default_Name;
end;

function TFileDescDefaultModule.GetLocalizedDescription: string;
begin
  Result:=inherited GetLocalizedDescription;
  Result:=rs_Mod_Default_Description;
end;

function TFileDescDefaultModule.GetInterfaceSource(const Filename, SourceName,
  ResourceName: string): string;
var
  str : TStringList;
begin
  //Result:=inherited GetInterfaceSource(Filename, SourceName, ResourceName);
  str := TStringList.Create;
  with str do begin
    Add( 'type');
    Add( '  '+ModulTypeName+' = class(TMyCustomWebModule)');
    Add( '    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
    Add( '  private');
    Add( '  public');
    Add( '    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;');
    Add( '    destructor Destroy; override;');
    Add( '    procedure TagController(Sender: TObject; const TagString: string; TagParams: TStringList; Out ReplaceText: string);');
    Add( '  end;');
    Add( '');
  end;
  Result := str.Text;
  FreeAndNil(str);
end;

function TFileDescDefaultModule.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
var
  str : TStringList;
begin
  Result:=Inherited GetImplementationSource(FileName,SourceName,ResourceName);
  str := TStringList.Create;
  with str do begin
    Add('uses theme_controller, common;');
    Add('');
    Add('procedure '+ModulTypeName+'.DataModuleRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse; var Handled: boolean);');
    Add('Begin');
    Add('');
    Add('  // your init code here');
    Add('');
    Add('  Response.Content := ThemeUtil.Render(@TagController);');
    Add('  Handled := True;');
    Add('End;');
    Add('');

    Add('constructor '+ModulTypeName+'.CreateNew(AOwner: TComponent; CreateMode: integer);');
    Add('Begin');
    Add('  inherited CreateNew(AOwner, CreateMode);');
    Add('  CreateSession := True;');
    Add('  OnRequest := @DataModuleRequest;');
    Add('End;');
    Add('');

    Add('destructor '+ModulTypeName+'.Destroy;');
    Add('Begin');
    Add('  inherited Destroy;');
    Add('End;');
    Add('');

    Add('procedure '+ModulTypeName+'.TagController(Sender: TObject; const TagString: string; TagParams: TStringList; out ReplaceText: string);');
    Add('var');
    Add('  thetags: TStringList;');
    Add('Begin');
    Add('  inherited TagController(Sender, TagString, TagParams, ReplaceText);');
    Add('  thetags := ExplodeTags(TagString);');
    Add('  if thetags.Count = 0 then begin FreeAndNil(thetags); Exit; end;');
    Add('');
    Add('  case thetags[0] of');
    Add('    ''$maincontent'':');
    Add('    begin');
    Add('      ReplaceText := ''FastPlaz ..... Hello world !!!'';');
    Add('    end;');
    Add('  end;');
    Add('');
    Add('  // your other custom tag');
    Add('');
    Add('  FreeAndNil(thetags);');
    Add('End;');
  end;
  Result := Result+ str.Text;
  FreeAndNil(str);

  Result:=result
    +LineEnding+'initialization'
    +LineEnding+'  // -> http://yourdomainname/'+ResourceName
    +LineEnding+'  // is better to move line below to file "route.pas"'
    +LineEnding+'  AddRoute('''+Permalink+''','+ModulTypeName+');'+LineEnding;
end;


function TFileDescDefaultModule.GetResourceSource(const ResourceName: string
  ): string;
begin
  Result:=inherited GetResourceSource(ResourceName);
end;

function TFileDescDefaultModule.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Permalink := 'sample';
  ModulTypeName := 'TSampleModule';
  with TfModuleSimpleWizard.Create(nil) do
  begin
    if ShowModal = mrOK then
    begin
      if edt_ModuleName.Text <> '' then
        ModulTypeName:= 'T'+StringReplace( UcWords(edt_ModuleName.Text), ' ', '', [rfReplaceAll])+'Module';
      Permalink:= edt_Permalink.Text;
      if Permalink = '' then
      begin
        Permalink:= StringReplace( UcWords(edt_ModuleName.Text), ' ', '', [rfReplaceAll]);
      end;
    end;
    Free;
  end;
  Result:=inherited CreateSource(Filename, SourceName, Permalink);
  log( 'module "' + ModulTypeName + '" created');
end;

procedure TFileDescDefaultModule.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  inherited UpdateDefaultPascalFileExtension(DefPasExt);
end;


end.


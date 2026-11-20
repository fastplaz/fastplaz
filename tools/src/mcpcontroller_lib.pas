unit mcpcontroller_lib;

{
  "FastPlaz | Create MCP Controller": a controller unit that serves the MCP
  endpoint of an existing project, with one example tool. Also used by the
  "MCP Server Application" project (bCreateProject: route in routes.pas,
  server name from the project). Source text: mcp_lib.
}

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, Controls, LazarusPackageIntf, ProjectIntf, LazIDEIntf,
  Classes, SysUtils;

resourcestring
  rs_Mod_MCP_Name = 'Create MCP Controller';
  rs_Mod_MCP_Description = 'Create an MCP (Model Context Protocol) server controller';

type

  { TFileDescMcpModule }

  TFileDescMcpModule = class(TFileDescPascalUnit)
  public
    constructor Create; override;
    function Init(var NewFilename: string; NewOwner: TObject;
      var NewSource: string; Quiet: boolean): TModalResult; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const Filename, SourceName,
      ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName,
      ResourceName: string): string; override;
    function CreateSource(const Filename, SourceName, ResourceName: string): string;
      override;
  end;

  { TFileDescMcpRoutes: routes.pas of an MCP project }

  TFileDescMcpRoutes = class(TFileDescPascalUnit)
  public
    ControllerUnit: string;
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetImplementationSource(const Filename, SourceName,
      ResourceName: string): string; override;
  end;

var
  { server name written into the controller of a new MCP project }
  McpProjectServerName: string = '';

{ Asks for the controller name (default TMcpController / "mcp") and creates
  "<permalink>_controller.pas" through the IDE. }
function NewMcpControllerUnit(AFlags: TNewFlags; out AFileName: string): TModalResult;

implementation

uses fastplaz_tools_register, tools_lib, modsimple_lib, modsimple_wzd, mcp_lib;

// The controller wizard, preset for MCP. Sets ModulTypeName and Permalink.
function AskMcpModuleName: boolean;
var
  wizard: TfModuleSimpleWizard;
begin
  Result := False;
  wizard := TfModuleSimpleWizard.Create(nil);
  try
    wizard.Caption := rs_Mod_MCP_Name;
    wizard.lbl_Title.Caption := 'MCP Controller Wizard';
    wizard.edt_ModuleName.Text := MCP_DEFAULT_MODULE_NAME;
    wizard.edt_Permalink.Text := MCP_DEFAULT_PERMALINK;
    if wizard.ShowModal <> mrOk then
      Exit;
    ModulTypeName := McpControllerTypeName(wizard.edt_ModuleName.Text);
    Permalink := ControllerPermalink(wizard.edt_ModuleName.Text,
      wizard.edt_Permalink.Text);
    Result := True;
  finally
    wizard.Free;
  end;
end;

function NewMcpControllerUnit(AFlags: TNewFlags; out AFileName: string): TModalResult;
var
  descriptor: TFileDescMcpModule;
begin
  AFileName := '';
  if not AskMcpModuleName then
    Exit(mrCancel);
  AFileName := ControllerFileName(Permalink);
  descriptor := TFileDescMcpModule.Create;
  bCreateProject := False;
  bExpert := True;
  try
    // the IDE does not take ownership of the descriptor
    Result := LazarusIDE.DoNewEditorFile(descriptor, AFileName, '', AFlags);
  finally
    bExpert := False;
    descriptor.Free;
  end;
end;

{ TFileDescMcpModule }

constructor TFileDescMcpModule.Create;
begin
  inherited Create;
  DefaultFileExt := '.pas';
  VisibleInNewDialog := True;
end;

function TFileDescMcpModule.Init(var NewFilename: string; NewOwner: TObject;
  var NewSource: string; Quiet: boolean): TModalResult;
begin
  Result := inherited Init(NewFilename, NewOwner, NewSource, Quiet);
  if Result <> mrOk then
    Exit;
  if bExpert or bCreateProject then
    Exit; // the menu or the project wizard set the names
  ModulTypeName := McpControllerTypeName(MCP_DEFAULT_MODULE_NAME);
  Permalink := MCP_DEFAULT_PERMALINK;
  if Quiet then
    Exit;
  if not AskMcpModuleName then
    Exit(mrCancel);
  if NewFilename = '' then
    NewFilename := ControllerFileName(Permalink);
end;

function TFileDescMcpModule.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection + McpControllerUses;
end;

function TFileDescMcpModule.GetLocalizedName: string;
begin
  Result := rs_Mod_MCP_Name;
end;

function TFileDescMcpModule.GetLocalizedDescription: string;
begin
  Result := rs_Mod_MCP_Description;
end;

function TFileDescMcpModule.GetInterfaceSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result := McpControllerInterfaceSource(ModulTypeName);
end;

function TFileDescMcpModule.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
var
  serverName: string;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
  if bCreateProject then
    serverName := McpProjectServerName
  else
    serverName := '';
  Result := Result + McpControllerImplementationSource(ModulTypeName,
    Permalink, serverName, not bCreateProject);
end;

function TFileDescMcpModule.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result := inherited CreateSource(Filename, SourceName, Permalink);
  log('MCP controller "' + ModulTypeName + '" created, endpoint /' + Permalink,
    Filename);
end;

{ TFileDescMcpRoutes }

constructor TFileDescMcpRoutes.Create;
begin
  inherited Create;
  DefaultFilename := 'routes.pas';
  DefaultFileExt := '.pas';
end;

function TFileDescMcpRoutes.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection + ', fastplaz_handler';
end;

function TFileDescMcpRoutes.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result := McpRoutesImplementationSource(ControllerUnit, ModulTypeName, Permalink);
end;

end.

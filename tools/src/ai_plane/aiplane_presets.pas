unit aiplane_presets;

{
  Known runtimes and how to start them. A preset is only a default: the
  command and its arguments stay editable in the configuration, so a new
  ACP agent can be used without a new release.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, aiplane_types, aiplane_process, aiplane_acp, aiplane_claude,
  aiplane_pi;

type
  TAIRuntimePreset = record
    Id: string;
    Title: string;
    AdapterClass: TAIAdapterClass;
    Command: string;
    Arguments: string;      // space separated
    ModelsCommand: string;  // lists models, one per line ('' = from session)
    Note: string;
  end;

const
  AI_PRESET_CLAUDE = 'claude';
  AI_PRESET_OPENCODE = 'opencode';
  AI_PRESET_PI = 'pi';
  AI_PRESET_CLAUDE_ACP = 'claude-acp';
  { Used when nothing is configured yet. }
  AI_DEFAULT_PRESET = AI_PRESET_OPENCODE;

function AIPresetCount: integer;
function AIPreset(AIndex: integer): TAIRuntimePreset;
function AIFindPreset(const AId: string; out APreset: TAIRuntimePreset): boolean;
{ Full path of the runtime's executable (ACommand, or the preset's command
  when empty), '' when it is not installed / not on PATH. }
function AIRuntimeExecutable(const APresetId: string; const ACommand: string = ''): string;
{ Message for a runtime whose executable was not found. }
function AIRuntimeMissingMessage(const APresetId: string; const ACommand: string = ''): string;
{ Creates an adapter from a preset. ACommand/AArguments override the preset
  when not empty. }
function AICreateAdapter(const APresetId: string; const ACommand: string = '';
  const AArguments: string = ''): TAIAdapter;

implementation

var
  Presets: array of TAIRuntimePreset;

procedure AddPreset(const AId, ATitle: string; AClass: TAIAdapterClass;
  const ACommand, AArguments, AModelsCommand, ANote: string);
var
  n: integer;
begin
  n := Length(Presets);
  SetLength(Presets, n + 1);
  Presets[n].Id := AId;
  Presets[n].Title := ATitle;
  Presets[n].AdapterClass := AClass;
  Presets[n].Command := ACommand;
  Presets[n].Arguments := AArguments;
  Presets[n].ModelsCommand := AModelsCommand;
  Presets[n].Note := ANote;
end;

function AIPresetCount: integer;
begin
  Result := Length(Presets);
end;

function AIPreset(AIndex: integer): TAIRuntimePreset;
begin
  Result := Presets[AIndex];
end;

function AIFindPreset(const AId: string; out APreset: TAIRuntimePreset): boolean;
var
  i: integer;
begin
  for i := 0 to High(Presets) do
    if SameText(Presets[i].Id, AId) then
    begin
      APreset := Presets[i];
      Exit(True);
    end;
  APreset := Default(TAIRuntimePreset);
  Result := False;
end;

function AIRuntimeExecutable(const APresetId: string; const ACommand: string): string;
var
  preset: TAIRuntimePreset;
  command: string;
begin
  Result := '';
  command := ACommand;
  if command = '' then
  begin
    if not AIFindPreset(APresetId, preset) then
      Exit;
    command := preset.Command;
  end;
  Result := AIFindExecutable(command);
end;

function AIRuntimeMissingMessage(const APresetId: string; const ACommand: string): string;
var
  preset: TAIRuntimePreset;
  command: string;
begin
  if not AIFindPreset(APresetId, preset) then
    Exit(Format('Unknown runtime: %s', [APresetId]));
  command := ACommand;
  if command = '' then
    command := preset.Command;
  Result := Format('%s is not available: "%s" was not found. Install it or set ' +
    'its command in FastPlaz/Configuration, AI Harness tab.', [preset.Title, command]);
  if preset.Note <> '' then
    Result := Result + ' (' + preset.Note + ')';
end;

function AICreateAdapter(const APresetId: string; const ACommand: string;
  const AArguments: string): TAIAdapter;
var
  preset: TAIRuntimePreset;
begin
  if not AIFindPreset(APresetId, preset) then
    raise EAIAdapter.CreateFmt('Unknown runtime: %s', [APresetId]);
  Result := preset.AdapterClass.Create;
  if ACommand <> '' then
    Result.Command := ACommand
  else
    Result.Command := preset.Command;
  Result.Arguments.Delimiter := ' ';
  Result.Arguments.StrictDelimiter := True;
  if AArguments <> '' then
    Result.Arguments.DelimitedText := AArguments
  else if preset.Arguments <> '' then
    Result.Arguments.DelimitedText := preset.Arguments;
end;

initialization
  AddPreset(AI_PRESET_OPENCODE, 'OpenCode', TACPAdapter,
    'opencode', 'acp', 'opencode models', 'native ACP');
  AddPreset(AI_PRESET_CLAUDE, 'Claude Code', TClaudeStreamAdapter,
    'claude', '', '', 'stream-json mode, no Node.js needed');
  AddPreset(AI_PRESET_PI, 'Pi', TPiRpcAdapter,
    'pi', '', '', 'native RPC mode (pi --mode rpc)');
  AddPreset(AI_PRESET_CLAUDE_ACP, 'Claude Code (ACP)', TACPAdapter,
    'claude-agent-acp', '', '',
    'ACP bridge: npm install -g @agentclientprotocol/claude-agent-acp');

end.

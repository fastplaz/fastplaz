unit aiplane_presets;

{
  Known runtimes and how to start them. A preset is only a default: the
  command and its arguments stay editable in the configuration, so a new
  ACP agent can be used without a new release.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, aiplane_types, aiplane_acp, aiplane_claude;

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
  AddPreset(AI_PRESET_PI, 'Pi', TACPAdapter,
    'pi-acp', '', '', 'ACP bridge: npm install -g pi-acp (not verified yet)');
  AddPreset(AI_PRESET_CLAUDE_ACP, 'Claude Code (ACP)', TACPAdapter,
    'claude-agent-acp', '', '',
    'ACP bridge: npm install -g @agentclientprotocol/claude-agent-acp');

end.

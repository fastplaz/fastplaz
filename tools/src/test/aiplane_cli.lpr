program aiplane_cli;

{
  Console tester for the AI Plane core: the same adapters the IDE window
  uses, without rebuilding Lazarus.

    aiplane_cli [--runtime claude|opencode|pi|claude-acp|mock]
                [--command <exe>] [--args "<a b>"] [--model <id>]
                [--effort <level>] [--cwd <dir>] [--log] [--yes]
                [--prompt "<text>"]

  With --prompt it sends one message and exits (exit code 1 on error),
  otherwise it is an interactive chat. Commands in the chat:
    /models  /model <id>  /effort <level>  /new  /log  /quit

  --runtime mock uses mock_acp_agent from the current directory.
  --yes answers every permission request with the first "allow" option.

  Build: fpc -FU lib -Fu../ai_plane aiplane_cli.lpr
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, aiplane_types, aiplane_presets;

type

  { TCli }

  TCli = class
  public
    Adapter: TAIAdapter;
    ShowLog: boolean;
    AutoYes: boolean;
    Done: boolean;
    Failed: boolean;
    LastKind: TAIEventKind;
    procedure OnEvent(Sender: TObject; const AEvent: TAIEvent);
    procedure OnPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
    procedure RunTurn(const AText: string);
    procedure ListModels;
  end;

const
  STATUS: array[TAIToolStatus] of string = ('pending', 'running', 'done', 'failed');

procedure TCli.OnEvent(Sender: TObject; const AEvent: TAIEvent);
begin
  if (AEvent.Kind <> LastKind) and (LastKind in [aekText, aekThinking]) then
    WriteLn;
  case AEvent.Kind of
    aekText:
      Write(AEvent.Text);
    aekThinking:
    begin
      if LastKind <> aekThinking then
        Write('(thinking) ');
      Write(AEvent.Text);
    end;
    aekToolCall, aekToolUpdate:
      WriteLn(Format('  [tool %s] %s %s', [STATUS[AEvent.ToolStatus],
        AEvent.ToolTitle, AEvent.ToolPath]));
    aekPlan:
      Write('  [plan]' + LineEnding + AEvent.Text);
    aekStatus:
      WriteLn('  [', AEvent.Text, ']');
    aekUsage:
      WriteLn('  [usage ', AEvent.Text, ']');
    aekError:
    begin
      WriteLn('  [error] ', AEvent.Text);
      Failed := True;
    end;
    aekDone:
    begin
      WriteLn('  [done: ', AEvent.StopReason, ']');
      Done := True;
      if AEvent.StopReason = 'error' then
        Failed := True;
    end;
    aekLog:
      if ShowLog then
        WriteLn('  ', Copy(AEvent.Text, 1, 300));
  end;
  if AEvent.Kind <> aekLog then
    LastKind := AEvent.Kind;
end;

procedure TCli.OnPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
var
  i, choice: integer;
  answer: string;
begin
  WriteLn;
  WriteLn('  [permission] ', ARequest.ToolTitle, ' ', Copy(ARequest.Detail, 1, 200));
  if AutoYes then
  begin
    for i := 0 to High(ARequest.Options) do
      if Pos('allow', ARequest.Options[i].Kind) = 1 then
      begin
        WriteLn('  -> ', ARequest.Options[i].Name);
        Adapter.RespondPermission(ARequest.Key, ARequest.Options[i].Id);
        Exit;
      end;
  end;
  for i := 0 to High(ARequest.Options) do
    WriteLn(Format('    %d) %s', [i + 1, ARequest.Options[i].Name]));
  Write('  choose (empty = cancel): ');
  ReadLn(answer);
  choice := StrToIntDef(Trim(answer), 0) - 1;
  if (choice >= 0) and (choice <= High(ARequest.Options)) then
    Adapter.RespondPermission(ARequest.Key, ARequest.Options[choice].Id)
  else
    Adapter.RespondPermission(ARequest.Key, '');
end;

procedure TCli.RunTurn(const AText: string);
begin
  Done := False;
  LastKind := aekStatus;
  try
    Adapter.Prompt(AText);
  except
    on E: Exception do
    begin
      WriteLn('  [error] ', E.Message);
      Failed := True;
      Exit;
    end;
  end;
  while not Done do
    if not Adapter.Poll then
      Sleep(10);
end;

procedure TCli.ListModels;
var
  i: integer;
begin
  if Length(Adapter.Models) = 0 then
    WriteLn('  no models yet (they arrive with the session)');
  for i := 0 to High(Adapter.Models) do
    WriteLn('  ', Adapter.Models[i].Id, '  ', Adapter.Models[i].Name);
end;

function Option(const AName: string; const ADefault: string = ''): string;
var
  i: integer;
begin
  Result := ADefault;
  for i := 1 to ParamCount - 1 do
    if ParamStr(i) = AName then
      Exit(ParamStr(i + 1));
end;

function HasFlag(const AName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to ParamCount do
    if ParamStr(i) = AName then
      Exit(True);
end;

var
  cli: TCli;
  runtime, command, line, oneShot: string;
begin
  DefaultSystemCodePage := CP_UTF8;
  SetTextCodePage(Output, CP_UTF8);
  runtime := Option('--runtime', AI_DEFAULT_PRESET);
  command := Option('--command');
  if runtime = 'mock' then
  begin
    runtime := AI_PRESET_OPENCODE;
    if command = '' then
      command := ExpandFileName('mock_acp_agent' + ExtractFileExt(ParamStr(0)));
  end;
  cli := TCli.Create;
  try
    cli.Adapter := AICreateAdapter(runtime, command, Option('--args'));
    cli.Adapter.OnEvent := @cli.OnEvent;
    cli.Adapter.OnPermission := @cli.OnPermission;
    cli.Adapter.Model := Option('--model');
    cli.Adapter.Effort := Option('--effort');
    cli.Adapter.WorkingDir := Option('--cwd', GetCurrentDir);
    cli.ShowLog := HasFlag('--log');
    cli.AutoYes := HasFlag('--yes');
    WriteLn('AI Plane CLI - ', cli.Adapter.AdapterName, ': ', cli.Adapter.Command);

    oneShot := Option('--prompt');
    if oneShot <> '' then
    begin
      cli.RunTurn(oneShot);
      cli.Adapter.Stop;
      if cli.Failed then
        ExitCode := 1;
      Exit;
    end;

    repeat
      Write('> ');
      if EOF(Input) then
        Break;
      ReadLn(line);
      line := Trim(line);
      if line = '' then
        Continue;
      if line = '/quit' then
        Break
      else if line = '/models' then
        cli.ListModels
      else if Copy(line, 1, 7) = '/model ' then
        cli.Adapter.Model := Trim(Copy(line, 8, MaxInt))
      else if Copy(line, 1, 8) = '/effort ' then
        cli.Adapter.Effort := Trim(Copy(line, 9, MaxInt))
      else if line = '/new' then
        cli.Adapter.NewSession
      else if line = '/log' then
        cli.ShowLog := not cli.ShowLog
      else
        cli.RunTurn(line);
    until False;
    cli.Adapter.Stop;
  finally
    cli.Adapter.Free;
    cli.Free;
  end;
end.

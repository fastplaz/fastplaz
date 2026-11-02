program aiplane_cli;

{
  Console tester for the Agent Plane core: the same adapters the IDE window
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
  --mcp [port] runs the Agent Plane MCP server (default port 17845) with two
    demo tools and passes it to the runtime, to check that a real agent
    reaches the IDE tools:
      secret_word      returns a random word (auto)
      confirm_action   asks for permission first
    Example: --mcp --prompt "Call the fastplaz secret_word tool and tell me the word"

  Build: fpc -FU lib -Fu../ai_plane aiplane_cli.lpr
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, aiplane_types, aiplane_presets, aiplane_mcpserver;

type

  { TCli }

  TCli = class
  public
    Adapter: TAIAdapter;
    Mcp: TAIMcpServer;
    SecretWord: string;
    ShowLog: boolean;
    AutoYes: boolean;
    Done: boolean;
    Failed: boolean;
    LastKind: TAIEventKind;
    procedure OnEvent(Sender: TObject; const AEvent: TAIEvent);
    procedure OnPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
    procedure OnMcpPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
    procedure OnMcpLog(Sender: TObject; const AText: string);
    procedure ToolSecretWord(ACall: TAIMcpToolCall);
    procedure ToolConfirm(ACall: TAIMcpToolCall);
    function Choose(const ARequest: TAIPermissionRequest): string;
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

// The option id to answer with ('' = cancel).
function TCli.Choose(const ARequest: TAIPermissionRequest): string;
var
  i, choice: integer;
  answer: string;
begin
  Result := '';
  WriteLn;
  WriteLn('  [permission] ', ARequest.ToolTitle, ' ', Copy(ARequest.Detail, 1, 200));
  if AutoYes then
  begin
    for i := 0 to High(ARequest.Options) do
      if Pos('allow', ARequest.Options[i].Kind) = 1 then
      begin
        WriteLn('  -> ', ARequest.Options[i].Name);
        Exit(ARequest.Options[i].Id);
      end;
  end;
  for i := 0 to High(ARequest.Options) do
    WriteLn(Format('    %d) %s', [i + 1, ARequest.Options[i].Name]));
  Write('  choose (empty = cancel): ');
  ReadLn(answer);
  choice := StrToIntDef(Trim(answer), 0) - 1;
  if (choice >= 0) and (choice <= High(ARequest.Options)) then
    Result := ARequest.Options[choice].Id;
end;

procedure TCli.OnPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
begin
  Adapter.RespondPermission(ARequest.Key, Choose(ARequest));
end;

procedure TCli.OnMcpPermission(Sender: TObject; const ARequest: TAIPermissionRequest);
begin
  Mcp.RespondPermission(ARequest.Key, Choose(ARequest));
end;

procedure TCli.OnMcpLog(Sender: TObject; const AText: string);
begin
  WriteLn('  [mcp] ', Copy(AText, 1, 300));
end;

procedure TCli.ToolSecretWord(ACall: TAIMcpToolCall);
begin
  ACall.Finish('The secret word is: ' + SecretWord);
end;

procedure TCli.ToolConfirm(ACall: TAIMcpToolCall);
begin
  ACall.Finish('Confirmed: ' + ACall.Str('action', '(nothing)'));
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
    if not Adapter.Poll and not (Assigned(Mcp) and Mcp.Poll) then
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
    if HasFlag('--mcp') then
    begin
      cli.SecretWord := 'kopi-' + Copy(AIMcpNewToken, 1, 6);
      cli.Mcp := TAIMcpServer.Create;
      cli.Mcp.OnLog := @cli.OnMcpLog;
      cli.Mcp.OnPermission := @cli.OnMcpPermission;
      cli.Mcp.RegisterTool('secret_word', 'Secret word',
        'Returns the secret word of this test session.',
        '{"type":"object","properties":{}}', mtpAuto, @cli.ToolSecretWord);
      cli.Mcp.RegisterTool('confirm_action', 'Confirm an action',
        'Asks the user to confirm an action, then reports it.',
        '{"type":"object","properties":{"action":{"type":"string"}},"required":["action"]}',
        mtpAsk, @cli.ToolConfirm);
      if not cli.Mcp.Start(StrToIntDef(Option('--mcp', ''), AI_MCP_DEFAULT_PORT)) then
      begin
        WriteLn('  [mcp] ', cli.Mcp.LastError);
        ExitCode := 1;
        Exit;
      end;
      cli.Adapter.McpUrl := cli.Mcp.Url;
      cli.Adapter.McpToken := cli.Mcp.Token;
      WriteLn('  [mcp] secret word: ', cli.SecretWord);
    end;
    WriteLn('Agent Plane CLI - ', cli.Adapter.AdapterName, ': ', cli.Adapter.Command);

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
    cli.Mcp.Free;
    cli.Free;
  end;
end.

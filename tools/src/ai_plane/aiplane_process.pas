unit aiplane_process;

{
  Line based pipe to a child process, read by polling.

  Poll reads only what NumBytesAvailable reports, so it never blocks and can
  run from an LCL timer. Stdout is split into lines (the agent protocols are
  newline delimited JSON). Stderr is drained on every poll as well, otherwise
  a chatty runtime fills the stderr pipe and blocks.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, pipes;

type
  TAILineEvent = procedure(Sender: TObject; const ALine: string) of object;
  TAIExitEvent = procedure(Sender: TObject; AExitCode: integer) of object;

  { TAIProcessChannel }

  TAIProcessChannel = class
  private
    FErrBuf: string;
    FExited: boolean;
    FOnExit: TAIExitEvent;
    FOnLine: TAILineEvent;
    FOnStderr: TAILineEvent;
    FOutBuf: string;
    FProcess: TProcess;
    function ReadAvailable(AStream: TStream; var ABuffer: string): boolean;
    procedure FlushLines(var ABuffer: string; AHandler: TAILineEvent; AFinal: boolean);
  public
    destructor Destroy; override;
    { Raises EAIProcess when the executable cannot be found or started. }
    procedure Start(const ACommand: string; AArgs: TStrings; const AWorkingDir: string);
    procedure WriteLine(const ALine: string);
    function Poll: boolean;
    { Closes stdin, waits up to AGraceMs for a clean exit, then kills. }
    procedure Stop(AGraceMs: integer = 1500);
    function Running: boolean;
    property OnLine: TAILineEvent read FOnLine write FOnLine;
    property OnStderr: TAILineEvent read FOnStderr write FOnStderr;
    property OnExit: TAIExitEvent read FOnExit write FOnExit;
  end;

  EAIProcess = class(Exception);

{ Finds ACommand on PATH. On Windows it also tries .exe, .cmd and .bat, since
  npm installs shims as .cmd files. Returns '' when not found. }
function AIFindExecutable(const ACommand: string): string;

implementation

const
  READ_CHUNK = 65536;
  MAX_READ_PER_POLL = 4 * 1024 * 1024;

function AIFindExecutable(const ACommand: string): string;
{$IFDEF WINDOWS}
const
  EXTS: array[0..3] of string = ('', '.exe', '.cmd', '.bat');
var
  ext: string;
{$ENDIF}
begin
  Result := '';
  if ACommand = '' then
    Exit;
  if (ExtractFilePath(ACommand) <> '') then
  begin
    if FileExists(ACommand) then
      Result := ACommand;
    Exit;
  end;
  {$IFDEF WINDOWS}
  for ext in EXTS do
  begin
    if (ext = '') and (ExtractFileExt(ACommand) = '') then
      Continue;
    Result := ExeSearch(ACommand + ext, GetEnvironmentVariable('PATH'));
    if Result <> '' then
      Exit;
  end;
  {$ELSE}
  Result := ExeSearch(ACommand, GetEnvironmentVariable('PATH'));
  {$ENDIF}
end;

{ TAIProcessChannel }

destructor TAIProcessChannel.Destroy;
begin
  if Assigned(FProcess) then
    Stop(500);
  inherited Destroy;
end;

procedure TAIProcessChannel.Start(const ACommand: string; AArgs: TStrings;
  const AWorkingDir: string);
var
  exe: string;
begin
  if Assigned(FProcess) then
  begin
    if Running then
      raise EAIProcess.Create('Process already started');
    FreeAndNil(FProcess);
  end;
  exe := AIFindExecutable(ACommand);
  if exe = '' then
    raise EAIProcess.CreateFmt('Executable not found: %s', [ACommand]);

  FOutBuf := '';
  FErrBuf := '';
  FExited := False;
  FProcess := TProcess.Create(nil);
  {$IFDEF WINDOWS}
  // Batch shims cannot be started directly with CreateProcess.
  if SameText(ExtractFileExt(exe), '.cmd') or SameText(ExtractFileExt(exe), '.bat') then
  begin
    FProcess.Executable := GetEnvironmentVariable('COMSPEC');
    if FProcess.Executable = '' then
      FProcess.Executable := 'cmd.exe';
    FProcess.Parameters.Add('/c');
    FProcess.Parameters.Add(exe);
  end
  else
  {$ENDIF}
    FProcess.Executable := exe;
  if Assigned(AArgs) then
    FProcess.Parameters.AddStrings(AArgs);
  if AWorkingDir <> '' then
    FProcess.CurrentDirectory := AWorkingDir;
  FProcess.Options := [poUsePipes, poNoConsole];
  try
    FProcess.Execute;
  except
    on E: Exception do
    begin
      FreeAndNil(FProcess);
      raise EAIProcess.CreateFmt('Cannot start %s: %s', [exe, E.Message]);
    end;
  end;
end;

procedure TAIProcessChannel.WriteLine(const ALine: string);
var
  s: string;
begin
  if not Running then
    raise EAIProcess.Create('Process is not running');
  s := ALine + #10;
  FProcess.Input.WriteBuffer(s[1], Length(s));
end;

function TAIProcessChannel.ReadAvailable(AStream: TStream; var ABuffer: string): boolean;
var
  available, n, total, oldLen: integer;
begin
  Result := False;
  total := 0;
  while total < MAX_READ_PER_POLL do
  begin
    available := TInputPipeStream(AStream).NumBytesAvailable;
    if available <= 0 then
      Break;
    if available > READ_CHUNK then
      available := READ_CHUNK;
    oldLen := Length(ABuffer);
    SetLength(ABuffer, oldLen + available);
    n := AStream.Read(ABuffer[oldLen + 1], available);
    SetLength(ABuffer, oldLen + n);
    if n <= 0 then
      Break;
    Inc(total, n);
    Result := True;
  end;
end;

procedure TAIProcessChannel.FlushLines(var ABuffer: string;
  AHandler: TAILineEvent; AFinal: boolean);
var
  p: integer;
  line: string;
begin
  repeat
    p := Pos(#10, ABuffer);
    if p = 0 then
      Break;
    line := Copy(ABuffer, 1, p - 1);
    Delete(ABuffer, 1, p);
    if (line <> '') and (line[Length(line)] = #13) then
      SetLength(line, Length(line) - 1);
    if (line <> '') and Assigned(AHandler) then
      AHandler(Self, line);
  until False;
  if AFinal and (ABuffer <> '') then
  begin
    line := ABuffer;
    ABuffer := '';
    if Assigned(AHandler) then
      AHandler(Self, line);
  end;
end;

function TAIProcessChannel.Poll: boolean;
var
  alive: boolean;
begin
  Result := False;
  if not Assigned(FProcess) or FExited then
    Exit;
  // Check before reading so output written right before exit is not lost.
  alive := FProcess.Running;
  if ReadAvailable(FProcess.Output, FOutBuf) then
    Result := True;
  if ReadAvailable(FProcess.Stderr, FErrBuf) then
    Result := True;
  FlushLines(FOutBuf, FOnLine, not alive);
  FlushLines(FErrBuf, FOnStderr, not alive);
  if not alive then
  begin
    FExited := True;
    Result := True;
    if Assigned(FOnExit) then
      FOnExit(Self, FProcess.ExitCode);
  end;
end;

procedure TAIProcessChannel.Stop(AGraceMs: integer);
var
  waited: integer;
begin
  if not Assigned(FProcess) then
    Exit;
  try
    if FProcess.Running then
    begin
      FProcess.CloseInput;
      waited := 0;
      while FProcess.Running and (waited < AGraceMs) do
      begin
        Sleep(20);
        Inc(waited, 20);
      end;
      if FProcess.Running then
        FProcess.Terminate(1);
    end;
  finally
    FreeAndNil(FProcess);
    FExited := True;
  end;
end;

function TAIProcessChannel.Running: boolean;
begin
  Result := Assigned(FProcess) and not FExited and FProcess.Running;
end;

end.

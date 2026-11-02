unit aiplane_types;

{
  AI Plane core types.

  The chat UI only talks to TAIAdapter and receives TAIEvent records, so a
  runtime (Claude Code, OpenCode, Pi, ...) can be swapped without touching the
  UI. Adapters are poll based: the owner calls Poll from a timer (LCL) or a
  loop (console), and every event is raised in the caller's thread. There are
  no worker threads, so handlers may update controls directly.

  This unit must not depend on LCL or IDEIntf: it is shared by the IDE
  package, the standalone tester and the unit tests.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAIEventKind = (
    aekStatus,      // adapter state text (connected, model, session id)
    aekText,        // assistant message chunk
    aekThinking,    // reasoning chunk
    aekToolCall,    // a tool call started
    aekToolUpdate,  // a tool call changed status or produced output
    aekPlan,        // plan entries (Text holds one entry per line)
    aekUsage,       // token usage text
    aekError,       // error text; the turn may still be running
    aekDone,        // the turn finished (StopReason)
    aekLog          // raw protocol traffic, for the debug pane
    );

  TAIToolStatus = (atsPending, atsRunning, atsCompleted, atsFailed);

  TAIEvent = record
    Kind: TAIEventKind;
    Text: string;
    ToolCallId: string;
    ToolTitle: string;
    ToolKind: string;
    ToolStatus: TAIToolStatus;
    ToolPath: string;
    StopReason: string;
  end;

  TAIEventHandler = procedure(Sender: TObject; const AEvent: TAIEvent) of object;

  TAIPermissionOption = record
    Id: string;
    Name: string;
    Kind: string;   // allow_once, allow_always, reject_once, reject_always
  end;

  TAIPermissionRequest = record
    Key: string;    // pass back to TAIAdapter.RespondPermission
    ToolCallId: string;
    ToolTitle: string;
    ToolKind: string;
    Detail: string;
    Options: array of TAIPermissionOption;
  end;

  { The handler may answer at once (RespondPermission inside the handler) or
    later, for example from a button in the chat. Until it is answered the
    agent waits. }
  TAIPermissionHandler = procedure(Sender: TObject;
    const ARequest: TAIPermissionRequest) of object;

  { Lets the IDE serve the unsaved editor buffer instead of the file on disk.
    Return False to fall back to the file. }
  TAIReadFileHandler = function(Sender: TObject; const APath: string;
    out AContent: string): boolean of object;

  TAIModelInfo = record
    Id: string;
    Name: string;
  end;

  TAIModelList = array of TAIModelInfo;

  TAIAdapterState = (aasIdle, aasStarting, aasReady, aasBusy, aasStopped, aasFailed);

  EAIAdapter = class(Exception);

  { TAIAdapter }

  TAIAdapter = class
  private
    FArguments: TStringList;
    FCommand: string;
    FEffort: string;
    FModel: string;
    FOnEvent: TAIEventHandler;
    FOnPermission: TAIPermissionHandler;
    FOnReadFile: TAIReadFileHandler;
    FWorkingDir: string;
    FMcpUrl: string;
    FMcpToken: string;
  protected
    FModels: TAIModelList;
    FState: TAIAdapterState;
    FSessionId: string;
    procedure DoEvent(AKind: TAIEventKind; const AText: string);
    procedure DoEventRec(const AEvent: TAIEvent);
    procedure DoPermission(const ARequest: TAIPermissionRequest);
    function ReadTextFile(const APath: string): string;
    procedure SetState(AValue: TAIAdapterState);
    procedure ModelChanged; virtual;
    procedure EffortChanged; virtual;
    procedure SetEffort(const AValue: string);
    procedure SetModel(const AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    { Spawns the runtime and performs the handshake. Asynchronous: the adapter
      reaches aasReady during later Poll calls. }
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    { Sends a user message. Starts the runtime first when needed. }
    procedure Prompt(const AText: string); virtual; abstract;
    { Asks the runtime to stop the current turn. }
    procedure Cancel; virtual; abstract;
    { Reads pending output and raises events. Returns True when anything was
      processed, so a loop can skip sleeping. }
    function Poll: boolean; virtual; abstract;
    procedure RespondPermission(const AKey, AOptionId: string); virtual; abstract;
    { Starts a new conversation; the runtime keeps running when it can. }
    procedure NewSession; virtual; abstract;

    function IsBusy: boolean;
    class function AdapterName: string; virtual; abstract;

    property Command: string read FCommand write FCommand;
    property Arguments: TStringList read FArguments;
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    { Empty means the runtime default. }
    property Model: string read FModel write SetModel;
    { auto, off, minimal, low, medium, high, xhigh. Empty or auto means the
      runtime default. }
    property Effort: string read FEffort write SetEffort;
    { The IDE's MCP server (http://127.0.0.1:<port>/mcp) and its Bearer
      token. Empty URL: no IDE tools. Read when the runtime starts. }
    property McpUrl: string read FMcpUrl write FMcpUrl;
    property McpToken: string read FMcpToken write FMcpToken;
    property Models: TAIModelList read FModels;
    property State: TAIAdapterState read FState;
    property SessionId: string read FSessionId;

    property OnEvent: TAIEventHandler read FOnEvent write FOnEvent;
    property OnPermission: TAIPermissionHandler read FOnPermission write FOnPermission;
    property OnReadFile: TAIReadFileHandler read FOnReadFile write FOnReadFile;
  end;

  TAIAdapterClass = class of TAIAdapter;

const
  { Name of the IDE's MCP server as the agent sees it (Claude Code shows its
    tools as mcp__lazarus__<tool>). }
  AI_MCP_SERVER_ID = 'lazarus';

  AI_EFFORT_LEVELS: array[0..6] of string =
    ('auto', 'off', 'minimal', 'low', 'medium', 'high', 'xhigh');

function AIEvent(AKind: TAIEventKind; const AText: string = ''): TAIEvent;
function AIToolStatusFromString(const AValue: string): TAIToolStatus;
function AIStateToString(AState: TAIAdapterState): string;
{ Splits message text into prose and ``` code blocks. Objects[i] <> nil marks
  a code block. Prose is trimmed and empty prose is dropped; an unclosed
  fence (still streaming) counts as code. The caller frees the list. }
function AISplitCodeBlocks(const AText: string): TStringList;
{ Copies to AOut the items of AItems that contain every word of AFilter
  (case-insensitive, any order). An empty filter copies everything. }
procedure AIFilterModels(AItems: TStrings; const AFilter: string; AOut: TStrings);

implementation

function AIEvent(AKind: TAIEventKind; const AText: string): TAIEvent;
begin
  Result := Default(TAIEvent);
  Result.Kind := AKind;
  Result.Text := AText;
end;

function AIToolStatusFromString(const AValue: string): TAIToolStatus;
begin
  case LowerCase(AValue) of
    'in_progress', 'running': Result := atsRunning;
    'completed', 'success': Result := atsCompleted;
    'failed', 'error': Result := atsFailed;
    else
      Result := atsPending;
  end;
end;

function AIStateToString(AState: TAIAdapterState): string;
const
  NAMES: array[TAIAdapterState] of string =
    ('idle', 'starting', 'ready', 'busy', 'stopped', 'failed');
begin
  Result := NAMES[AState];
end;

function AISplitCodeBlocks(const AText: string): TStringList;
var
  lines: TStringList;
  i: integer;
  inCode: boolean;
  current: string;

  procedure Flush;
  begin
    // Objects: 1 = code block
    if inCode then
      Result.AddObject(current, TObject(PtrInt(1)))
    else if Trim(current) <> '' then
      Result.AddObject(Trim(current), nil);
    current := '';
  end;

begin
  Result := TStringList.Create;
  lines := TStringList.Create;
  try
    lines.Text := AText;
    inCode := False;
    current := '';
    for i := 0 to lines.Count - 1 do
    begin
      if Copy(TrimLeft(lines[i]), 1, 3) = '```' then
      begin
        Flush;
        inCode := not inCode;
        Continue;
      end;
      if current <> '' then
        current := current + LineEnding;
      current := current + lines[i];
    end;
    // An unclosed fence while streaming is still shown as code.
    Flush;
  finally
    lines.Free;
  end;
end;

procedure AIFilterModels(AItems: TStrings; const AFilter: string; AOut: TStrings);
var
  words: TStringList;
  i, w: integer;
  item: string;
  ok: boolean;
begin
  AOut.BeginUpdate;
  words := TStringList.Create;
  try
    AOut.Clear;
    words.Delimiter := ' ';
    words.StrictDelimiter := True;
    words.DelimitedText := LowerCase(Trim(AFilter));
    for i := 0 to AItems.Count - 1 do
    begin
      item := LowerCase(AItems[i]);
      ok := True;
      for w := 0 to words.Count - 1 do
        if (words[w] <> '') and (Pos(words[w], item) = 0) then
        begin
          ok := False;
          Break;
        end;
      if ok then
        AOut.Add(AItems[i]);
    end;
  finally
    words.Free;
    AOut.EndUpdate;
  end;
end;

{ TAIAdapter }

constructor TAIAdapter.Create;
begin
  FArguments := TStringList.Create;
  FState := aasIdle;
end;

destructor TAIAdapter.Destroy;
begin
  FArguments.Free;
  inherited Destroy;
end;

procedure TAIAdapter.DoEvent(AKind: TAIEventKind; const AText: string);
begin
  DoEventRec(AIEvent(AKind, AText));
end;

procedure TAIAdapter.DoEventRec(const AEvent: TAIEvent);
begin
  if Assigned(FOnEvent) then
    FOnEvent(Self, AEvent);
end;

procedure TAIAdapter.DoPermission(const ARequest: TAIPermissionRequest);
var
  i: integer;
begin
  if Assigned(FOnPermission) then
  begin
    FOnPermission(Self, ARequest);
    Exit;
  end;
  // No handler: reject, never allow silently.
  for i := 0 to High(ARequest.Options) do
    if Pos('reject', ARequest.Options[i].Kind) = 1 then
    begin
      RespondPermission(ARequest.Key, ARequest.Options[i].Id);
      Exit;
    end;
  RespondPermission(ARequest.Key, '');
end;

function TAIAdapter.ReadTextFile(const APath: string): string;
var
  list: TStringList;
begin
  if Assigned(FOnReadFile) and FOnReadFile(Self, APath, Result) then
    Exit;
  list := TStringList.Create;
  try
    list.LoadFromFile(APath);
    Result := list.Text;
  finally
    list.Free;
  end;
end;

procedure TAIAdapter.SetState(AValue: TAIAdapterState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  DoEvent(aekStatus, 'state: ' + AIStateToString(AValue));
end;

procedure TAIAdapter.ModelChanged;
begin
end;

procedure TAIAdapter.EffortChanged;
begin
end;

procedure TAIAdapter.SetEffort(const AValue: string);
begin
  if FEffort = AValue then
    Exit;
  FEffort := AValue;
  EffortChanged;
end;

procedure TAIAdapter.SetModel(const AValue: string);
begin
  if FModel = AValue then
    Exit;
  FModel := AValue;
  ModelChanged;
end;

function TAIAdapter.IsBusy: boolean;
begin
  Result := FState in [aasStarting, aasBusy];
end;

end.

unit thread_custom;

{$mode objfpc}{$H+}
{
  [x] USAGE
  // example #1
  Call(@OnExecuteProc, @OnSuccessProc);

  // example #2
  FThread := TCustomThread.Create(True);
  FThread.OnExecute := @OnExecuteProc;
  FThread.OnSuccess := @OnSuccessProc;
  FThread.Start;

}
interface

uses
  Classes, SysUtils;

type

  { TMessageData }

  TMessageData = record
    Text: string;
  end;
  PMessageData = ^TMessageData;

  { TCustomThread }

  TCustomThread = class(TThread)
  private
    fsStatusText: string;
    FOnExecute: TNotifyEvent;
    FOnSuccess: TNotifyEvent;
    procedure synchronizeStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property StatusText: string read fsStatusText write fsStatusText;
  end;

function Call(ACalledProcedure: TNotifyEvent; ACallbackProcedure: TNotifyEvent): boolean;

implementation

function Call(ACalledProcedure: TNotifyEvent;
  ACallbackProcedure: TNotifyEvent): boolean;
var
  tmpThread: TCustomThread;
begin
  Result := False;
  try
    tmpThread := TCustomThread.Create(True);
    tmpThread.OnExecute := ACalledProcedure;
    tmpThread.OnSuccess := ACallbackProcedure;
    tmpThread.Start;
    Result := True;
  except
  end;
end;

{ TCommandThread }

procedure TCustomThread.synchronizeStatus;
begin
  if fsStatusText.IsEmpty then
    fsStatusText := 'success';
  if FOnSuccess <> nil then
    FOnSuccess(Self);
end;

procedure TCustomThread.Execute;
begin
  fsStatusText := 'start';
  if FOnExecute <> nil then
    FOnExecute(Self);

  Synchronize(@synchronizeStatus);
end;

constructor TCustomThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FOnExecute := nil;
  FOnSuccess := nil;
end;

destructor TCustomThread.Destroy;
begin
  inherited Destroy;
end;

end.

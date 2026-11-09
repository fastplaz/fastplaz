unit logutil_lib;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_app,
  common,
  Classes, SysUtils;

type

  { TLogUtil }

  TLogUtil = class
  private
    FPrefix: string;
    FDateStamp: string;
    FLock: TRTLCriticalSection;
    procedure setPrefix(AValue: string);
    procedure UpdateFileName;
    procedure WriteLine(const AFileName, ALine: string);
    function Sanitize(const S: string): string;
  public
    Dir, FileName, FullName: UnicodeString;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterError(MessageString: string; psHttpCode: integer = 0;
      URL: string = '');
    procedure Add(const message: string; const ModName: string = '';
      const Skip: boolean = False; CustomLogFile: string = '');
  published
    property Prefix: string read FPrefix write setPrefix;
  end;

var
  LogUtil: TLogUtil;

implementation


{ TLogUtil }

procedure TLogUtil.setPrefix(AValue: string);
begin
  if FPrefix=AValue then Exit;
  FPrefix:=AValue;
  FDateStamp := '';
  UpdateFileName;
end;

// Recompute the log file name when the day changes, so a long running
// process does not keep writing to yesterday's file.
procedure TLogUtil.UpdateFileName;
var
  stamp: string;
begin
  stamp := FormatDateTime('YYYYMMDD', Now);
  if stamp = FDateStamp then
    Exit;
  FDateStamp := stamp;
  FileName := 'app-' + UnicodeString(stamp) + '.log';
  FullName := Dir + '/' + UnicodeString(FPrefix) + FileName;
end;

// Keep one entry per line; CR/LF inside a message would allow forged entries.
function TLogUtil.Sanitize(const S: string): string;
begin
  Result := StringReplace(S, #13#10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
end;

// Append one line to AFileName. Logging must never break the request, so
// every I/O failure is swallowed; the file handle is always released.
procedure TLogUtil.WriteLine(const AFileName, ALine: string);
var
  log_file: TextFile;
begin
  EnterCriticalSection(FLock);
  try
    try
      AssignFile(log_file, AFileName);
      if FileExists(AFileName) then
        Append(log_file)
      else
        Rewrite(log_file);
      try
        WriteLn(log_file, FormatDateTime('YYYY-mm-dd hh:nn:ss', Now) + ' | ' + ALine);
      finally
        CloseFile(log_file);
      end;
    except
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TLogUtil.Create;
begin
  InitCriticalSection(FLock);
  Dir := 'ztemp/logs';
  try
    Dir := Config.GetValue('log/dir', 'ztemp/logs');
    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);
  except
  end;
  FPrefix := '';
  FDateStamp := '';
  UpdateFileName;
end;

destructor TLogUtil.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TLogUtil.RegisterError(MessageString: string; psHttpCode: integer;
  URL: string);
begin
  UpdateFileName;
  WriteLine(string(FullName), Sanitize(MessageString) + ' | ' + i2s(psHttpCode) +
    ' | ' + Sanitize(URL));
end;

procedure TLogUtil.Add(const message:string;const ModName:string;const Skip:
  boolean;CustomLogFile:string);
var
  s: string;
begin
  if Skip then
    Exit;
  if CustomLogFile.IsEmpty then
  begin
    UpdateFileName;
    CustomLogFile := string(FullName);
  end;
  s := '';
  if ModName <> '' then
    s := ModName + ': ';
  s := s + message;
  WriteLine(CustomLogFile, Sanitize(s));
end;

initialization
  LogUtil := TLogUtil.Create;

finalization;
  FreeAndNil(LogUtil);

end.

unit logutil_lib;

{$mode objfpc}{$H+}

interface

uses
  fpcgi,
  common,
  Classes, SysUtils;

type

  { TLogUtil }

  TLogUtil = class
  private
    FPrefix: string;
    log_file: TextFile;
    procedure SaveStringToPath(theString, filePath: string);
    procedure setPrefix(AValue: string);
  public
    Dir, FileName, FullName: UnicodeString;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterError(MessageString: string; psHttpCode: integer = 0;
      URL: string = '');
    procedure Add(const message: string; const ModName: string = '';
      const Skip: boolean = False);
  published
    property Prefix: string read FPrefix write setPrefix;
  end;

var
  LogUtil: TLogUtil;

implementation

uses config_lib, fastplaz_handler;


{ TLogUtil }

procedure TLogUtil.SaveStringToPath(theString, filePath: string);
var
  textFile: TFileStream = nil;
  textLength: integer;
  stringBuffer: ^string;
begin
  {$I+}
  {$T+}
  textLength := length(theString);
  try
    textFile := TFileStream.Create(filePath, fmOpenWrite or fmCreate);
    { write string to stream while avoiding to write the initial length }
    stringBuffer := @theString + 1;
    textFile.WriteBuffer(stringBuffer^, textLength);
  finally
    if textFile <> nil then
      textFile.Free;
  end;
end;

procedure TLogUtil.setPrefix(AValue: string);
begin
  if FPrefix=AValue then Exit;
  FPrefix:=AValue;
  FileName := 'app-' + UnicodeString( FormatDateTime('YYYYMMDD', Now)) + '.log';
  FullName := dir + '/' + FPrefix + FileName;
end;

constructor TLogUtil.Create;
begin
  try
    Dir := Config.GetValue('log/dir', 'ztemp/logs');
    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);
  except
  end;
  FPrefix := '';
  FileName := 'app-' + UnicodeString( FormatDateTime('YYYYMMDD', Now)) + '.log';
  FullName := dir + '/' + FileName;
end;

destructor TLogUtil.Destroy;
begin

end;

procedure TLogUtil.RegisterError(MessageString: string; psHttpCode: integer;
  URL: string);
begin
  AssignFile(log_file, FullName);
  { $I+}
  try
    //Rewrite(log_file);
    Append(log_file);
    WriteLn(log_file, FormatDateTime('YYYY-mm-dd hh:nn:ss', now) +
      ' | ' + MessageString + ' | ' + i2s(psHttpCode) + ' | ' + URL
      );
    CloseFile(log_file);
  except
  end;
end;

procedure TLogUtil.Add(const message: string; const ModName: string;
  const Skip: boolean);
var
  s: string;
begin
  if Skip then
    Exit;
  try
    if ModName <> '' then
      s := ModName + ': ';
    s := s + message;
    AssignFile(log_file, FullName);
    { $I+}
    try
      if not FileExists(FullName) then
        Rewrite(log_file)
      else
        Append(log_file);
      WriteLn(log_file, FormatDateTime('YYYY-mm-dd hh:nn:ss', now) + ' | ' + s);
      CloseFile(log_file);
    except
    end;
  except
    on E: Exception do
    begin
      die('cannot logging: ' + E.Message);
    end;
  end;
end;

initialization
  LogUtil := TLogUtil.Create;

finalization;
  FreeAndNil(LogUtil);

end.


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
    log_file: TextFile;
    procedure SaveStringToPath(theString, filePath: string);
  public
    Dir, FileName, FullName: WideString;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterError(MessageString: string; psHttpCode: integer = 0;
      URL: string = '');
    procedure Add(const message: string; const ModName: string = '';
      const Skip: boolean = False);
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

constructor TLogUtil.Create;
begin
  try
    Dir := Config.GetValue('log/dir', 'ztemp/logs');
    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);
  except
  end;
  FileName := 'app-' + FormatDateTime('YYYYMMDD', Now) + '.log';
  FullName := dir + '/' + FileName;
end;

destructor TLogUtil.Destroy;
begin

end;

procedure TLogUtil.RegisterError(MessageString: string; psHttpCode: integer;
  URL: string);
begin
  AssignFile(log_file, fullname);
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
    AssignFile(log_file, fullname);
    { $I+}
    try
      if not FileExists(fullname) then
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


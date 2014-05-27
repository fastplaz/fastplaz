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
    dir, filename, fullname: string;
    constructor Create;
    destructor Destroy; override;
    procedure registerError(psMessage: string; psHttpCode: integer = 0; psURL: string = '');
    procedure add(const message: string);
  end;

var
  LogUtil: TLogUtil;

implementation

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
  dir := Config.GetValue('log/dir', 'ztemp/error_log');
  filename := 'app.log';
  fullname := dir + '/' + filename;
end;

destructor TLogUtil.Destroy;
begin

end;

procedure TLogUtil.registerError(psMessage: string; psHttpCode: integer;
  psURL: string);
begin
  AssignFile(log_file, fullname);
  { $I+}
  try
    //Rewrite(log_file);
    Append(log_file);
    WriteLn(log_file, FormatDateTime('YYYY-mm-dd hh:nn:ss', now) +
      ' | ' + psMessage + ' | ' + i2s(psHttpCode) + ' | ' + psURL
      );
    CloseFile(log_file);
  except
  end;
end;

procedure TLogUtil.add(const message: string);
begin
  AssignFile(log_file, fullname);
  { $I+}
  try
    if not FileExists(fullname) then
      Rewrite(log_file)
    else
      Append(log_file);
    WriteLn(log_file, FormatDateTime('YYYY-mm-dd hh:nn:ss', now) + ' | ' + message
      );
    CloseFile(log_file);
  except
  end;
end;

initialization
  LogUtil := TLogUtil.Create;

finalization;
  FreeAndNil(LogUtil);

end.




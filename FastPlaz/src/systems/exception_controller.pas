unit exception_controller;

{$mode objfpc}{$H+}
{
 REF:
 http://wiki.freepascal.org/Logging_exceptions

}

interface

uses
  Classes, SysUtils;

procedure DumpExceptionCallStack(E: Exception);
procedure DumpCallStack;

implementation

uses common;

{
EXAMPLE:
try
  ...
  raise EAccessViolation.Create('Incorrect password entered');
except
  on e: exception do
  begin
    DumpExceptionCallStack( e);
    ...
  end;
end;
}
procedure DumpExceptionCallStack( E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := '<pre>Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);

  echo(Report);
  //Halt; // End of program execution
end;

procedure DumpCallStack;
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
  Report: string;
const
  MaxDepth = 20;
begin
  Report := '<pre>';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       Report := Report + BackTraceStrFunc(CallerAddress) + LineEnding;
       Inc(I);
       if (I >= MaxDepth) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
  echo(Report);
end;

end.


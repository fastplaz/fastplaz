unit datetime_lib;

{
  Format datetimes in a more Human Readable form
  (like tomorow, 4 days from now, 6 hours ago)

  var := DateTimeHuman( theDate);
  var := DateTimeHuman( theDate, 7);
  var := DateTimeHuman( theDate, 7, 'ddd d mmm yyyy');
  var := DateTimeHuman( '2015/12/30');

}
{$mode objfpc}{$H+}

interface

uses
  dateutils, common, language_lib, Math, Classes, SysUtils;

const
  TDateTimeEpsilon = 2.2204460493e-16;

function DateTimeHuman(TheDate: string; MaxIntervalDate: integer = 7; FormatDate: string = ''): string;
function DateTimeHuman(TheDate: TDateTime; MaxIntervalDate: integer = 7; FormatDate: string = ''): string;

implementation

function _DateTimeDiff(const ANow, AThen: TDateTime): TDateTime;
begin
  Result := ANow - AThen;
  if (ANow > 0) and (AThen < 0) then
    Result := Result - 0.5
  else if (ANow < -1.0) and (AThen > -1.0) then
    Result := Result + 0.5;
end;

function _DaysBetween(const ANow, AThen: TDateTime): extended;
begin
  Result := (trunc(_DateTimeDiff(ANow, AThen)) + TDateTimeEpsilon);
end;

function DateTimeHuman(TheDate: string; MaxIntervalDate: integer; FormatDate: string): string;
var
  dateTmp: TDateTime;
  ts: TFormatSettings;
begin
  GetLocaleFormatSettings(0, ts);
  ts.ShortDateFormat := 'yyyy/MM/dd h:nn';
  try
    dateTmp := StrToDateTime(TheDate, ts);
    Result := DateTimeHuman(dateTmp, MaxIntervalDate, FormatDate);
  except
    on e: Exception do
    begin
      Result := e.Message + ': "' + TheDate + '"';
    end;
  end;
end;

function DateTimeHuman(TheDate: TDateTime; MaxIntervalDate: integer; FormatDate: string): string;
var
  diffDate: double;
  diff, i: integer;
begin
  if MaxIntervalDate = 0 then
    MaxIntervalDate := 2;
  diffDate := _DaysBetween(TheDate, Now);
  if DaysBetween(Now, TheDate) = 0 then // still today
  begin
    diff := HoursBetween(Now, TheDate);
    if diff = 0 then
    begin
      diff := MinutesBetween(Now, TheDate);
      if diff = 0 then
      begin
        diff := SecondsBetween(Now, TheDate);
        Result := Format(__('%d seconds ago'), [diff]);
        Exit;
      end;
      Result := Format(__('%d minutes ago'), [diff]);
      Exit;
    end;
    Result := Format(__('%d hours ago'), [diff]);
    Exit;
  end;
  i := DaysBetween(Now, TheDate);
  if diffDate < 0 then
  begin
    if i <= MaxIntervalDate then
      Result := Format(__('%d days ago'), [i])
    else
    begin
      if FormatDate = '' then
        Result := DateTimeToStr(TheDate)
      else
        DateTimeToString(Result, FormatDate, TheDate);
    end;

    if i = 1 then
      Result := __('yesterday');
  end;
  if diffDate > 0 then
  begin
    if i <= MaxIntervalDate then
      Result := Format(__('%d days from now'), [i])
    else
    begin
      if FormatDate = '' then
        Result := DateTimeToStr(TheDate)
      else
        DateTimeToString(Result, FormatDate, TheDate);
    end;

    if i = 1 then
      Result := __('tomorrow');
  end;

end;

end.

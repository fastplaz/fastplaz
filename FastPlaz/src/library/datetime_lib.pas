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
  dateutils, common, language_lib, Classes, SysUtils;

function DateTimeHuman(TheDate: string; MaxIntervalDate: integer = 30; FormatDate: string = ''): string;
function DateTimeHuman(TheDate: TDateTime; MaxIntervalDate: integer = 30; FormatDate: string = ''): string;

implementation

function _DateTimeDiff(const ANow, AThen: TDateTime): TDateTime;
begin
  Result := ANow - AThen;
  if (ANow > 0) and (AThen < 0) then
    Result := Result - 0.5
  else if (ANow < -1.0) and (AThen > -1.0) then
    Result := Result + 0.5;
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

function _SayDate(TheDate: TDateTime; MaxIntervalDate: integer; FormatDate: string; Text: string = 'ago'): string;
var
  diff, i: integer;
begin
  diff := DaysBetween(Now, TheDate);
  if diff <= MaxIntervalDate then
  begin
    i := HoursBetween(Now, TheDate);
    if i >= 1 then
    begin
      if i > 24 then
        Result := Format(__('%d days ' + Text), [diff])
      else
        Result := Format(__('%d hours ' + Text), [i]);
    end
    else
    begin
      i := MinutesBetween(Now, TheDate);
      if i = 0 then
        Result := Format(__('%d secondss ' + Text), [SecondsBetween(Now, TheDate)])
      else
        Result := Format(__('%d minutes ' + Text), [i]);
    end;
  end
  else
  begin
    if FormatDate = '' then
    begin
      if diff < 31 then
        Result := Format(__('more %d days  ' + Text), [DaysBetween(Now, TheDate)]);
      if diff > 30 then
        Result := Format(__('more %d months ' + Text), [MonthsBetween(Now, TheDate)]);
      if diff > 360 then
        Result := Format(__('more %d years ' + Text), [YearsBetween(Now, TheDate)]);
    end
    else
    begin
      DateTimeToString(Result, FormatDate, TheDate);
    end;
  end;
end;

function DateTimeHuman(TheDate: TDateTime; MaxIntervalDate: integer; FormatDate: string): string;
var
  diff: integer;
  diffDate: TDateTime;
begin
  if MaxIntervalDate = 0 then
    MaxIntervalDate := 30;
  MaxIntervalDate := 7;

  diffDate := _DateTimeDiff(TheDate, Now);
  diff := DaysBetween(Now, TheDate);

  if diffDate <= 0 then
  begin
    if diff = 1 then
      Result := __('yesterday')
    else
      Result := _SayDate(TheDate, MaxIntervalDate, FormatDate, 'ago');
  end
  else
  begin // present
    if diff = 1 then
      Result := __('tomorrow')
    else
      Result := _SayDate(TheDate, MaxIntervalDate, FormatDate, 'from now');
  end;

end;

end.

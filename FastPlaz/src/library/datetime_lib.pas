unit datetime_lib;

{
  Format datetimes in a more Human Readable form
  (like tomorrow, yesterday, 4 days from now, 6 hours ago, 3 years ago)

  example:

  var := DateTimeHuman( theDate);
  var := DateTimeHuman( theDate, 7);
  var := DateTimeHuman( theDate, 7, 'ddd d mmm yyyy');
  var := DateTimeHuman( '2015/12/30');
  var := DateTimeHuman( '2015-12-30 10:20:30');
  var := DateTimeHuman( '1451445630');          // unix timestamp (local time)

  Rules (relative to Now):
  - less than 24 hours      : "%d seconds|minutes|hours ago" / "from now"
  - calendar day +/- 1      : "yesterday" / "tomorrow"
  - up to MaxIntervalDate   : "%d days ago" / "from now"
  - beyond MaxIntervalDate  : FormatDate when given, otherwise
                              "%d months ago" or "%d years ago"

  DateTimeHumanAt() does the same, but relative to an explicit reference
  time instead of Now (useful for testing and for batch processing).

  An input string that can not be parsed is returned unchanged.
}
{$mode objfpc}{$H+}

interface

uses
  dateutils, language_lib, Classes, SysUtils;

function DateTimeHuman(TheDate: string; MaxIntervalDate: integer = 30;
  FormatDate: string = ''): string;
function DateTimeHuman(TheDate: TDateTime; MaxIntervalDate: integer = 30;
  FormatDate: string = ''): string;

function DateTimeHumanAt(TheDate: string; ANow: TDateTime;
  MaxIntervalDate: integer = 30; FormatDate: string = ''): string;
function DateTimeHumanAt(TheDate, ANow: TDateTime; MaxIntervalDate: integer = 30;
  FormatDate: string = ''): string;

// Parse the string forms accepted by DateTimeHuman:
//   yyyy/mm/dd, yyyy-mm-dd, with optional time ("hh:nn" or "hh:nn:ss",
//   separated by a space or "T"), or a unix timestamp (local time).
function TryParseHumanDate(const S: string; out ADateTime: TDateTime): boolean;

implementation

const
  // A plain integer with at least this many digits is treated as a
  // unix timestamp; shorter numbers (e.g. "20151230") are not.
  MIN_UNIX_TIMESTAMP_DIGITS = 9;

function TryParseHumanDate(const S: string; out ADateTime: TDateTime): boolean;
var
  s1: string;
  unixTime: int64;
  fs: TFormatSettings;
begin
  Result := False;
  ADateTime := 0;
  s1 := Trim(S);
  if s1 = '' then
    Exit;

  // unix timestamp, compared against local Now so convert to local time
  if TryStrToInt64(s1, unixTime) and
    (Length(s1) - Ord(s1[1] = '-') >= MIN_UNIX_TIMESTAMP_DIGITS) then
  begin
    ADateTime := UnixToDateTime(unixTime, False);
    Exit(True);
  end;

  // fixed format, independent of the locale settings of the host
  fs := DefaultFormatSettings;
  fs.DateSeparator := '/';
  fs.TimeSeparator := ':';
  fs.ShortDateFormat := 'yyyy/mm/dd';
  fs.LongDateFormat := 'yyyy/mm/dd';
  fs.ShortTimeFormat := 'hh:nn';
  fs.LongTimeFormat := 'hh:nn:ss';
  fs.TimeAMString := '';
  fs.TimePMString := '';

  s1 := StringReplace(s1, '-', '/', [rfReplaceAll]);
  // ISO 8601 "yyyy-mm-ddThh:nn:ss"
  if (Length(s1) > 10) and (s1[11] = 'T') then
    s1[11] := ' ';

  Result := TryStrToDateTime(s1, ADateTime, fs);
end;

function DateTimeHumanAt(TheDate, ANow: TDateTime; MaxIntervalDate: integer;
  FormatDate: string): string;
var
  suffix: string;
  seconds: int64;
  calendarDays, months, years: integer;
begin
  if MaxIntervalDate = 0 then
    MaxIntervalDate := 30;

  if TheDate <= ANow then
    suffix := 'ago'
  else
    suffix := 'from now';

  seconds := SecondsBetween(ANow, TheDate);
  if seconds < SecsPerDay then
  begin
    if seconds < SecsPerMin then
      Result := Format(__('%d seconds ' + suffix), [seconds])
    else if seconds < SecsPerHour then
      Result := Format(__('%d minutes ' + suffix), [seconds div SecsPerMin])
    else
      Result := Format(__('%d hours ' + suffix), [seconds div SecsPerHour]);
    Exit;
  end;

  calendarDays := DaysBetween(DateOf(ANow), DateOf(TheDate));
  if calendarDays = 1 then
  begin
    if TheDate <= ANow then
      Result := __('yesterday')
    else
      Result := __('tomorrow');
    Exit;
  end;

  if calendarDays <= MaxIntervalDate then
    Exit(Format(__('%d days ' + suffix), [calendarDays]));

  if FormatDate <> '' then
  begin
    DateTimeToString(Result, FormatDate, TheDate);
    Exit;
  end;

  years := YearsBetween(ANow, TheDate);
  months := MonthsBetween(ANow, TheDate);
  if years >= 1 then
    Result := Format(__('%d years ' + suffix), [years])
  else if months >= 1 then
    Result := Format(__('%d months ' + suffix), [months])
  else
    Result := Format(__('%d days ' + suffix), [calendarDays]);
end;

function DateTimeHumanAt(TheDate: string; ANow: TDateTime;
  MaxIntervalDate: integer; FormatDate: string): string;
var
  dateTmp: TDateTime;
begin
  if TryParseHumanDate(TheDate, dateTmp) then
    Result := DateTimeHumanAt(dateTmp, ANow, MaxIntervalDate, FormatDate)
  else
    Result := TheDate;
end;

function DateTimeHuman(TheDate: string; MaxIntervalDate: integer;
  FormatDate: string): string;
begin
  Result := DateTimeHumanAt(TheDate, Now, MaxIntervalDate, FormatDate);
end;

function DateTimeHuman(TheDate: TDateTime; MaxIntervalDate: integer;
  FormatDate: string): string;
begin
  Result := DateTimeHumanAt(TheDate, Now, MaxIntervalDate, FormatDate);
end;

end.

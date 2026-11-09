unit test_datetime_lib;

{$mode objfpc}{$H+}

interface

uses
  datetime_lib, dateutils, Classes, SysUtils, fpcunit, testregistry;

type

  { TDateTimeLibTest }

  TDateTimeLibTest = class(TTestCase)
  private
    FNow: TDateTime;
    function Human(ADate: TDateTime; AMaxInterval: integer = 30;
      AFormat: string = ''): string;
  protected
    procedure SetUp; override;
  published
    // relative output
    procedure TestSecondsAgo;
    procedure TestSecondsFromNow;
    procedure TestMinutesAgo;
    procedure TestHoursAgo;
    procedure TestHoursAcrossMidnightIsNotYesterday;
    procedure TestYesterday;
    procedure TestTomorrow;
    procedure TestTwoCalendarDaysIsNotYesterday;
    procedure TestDaysAgo;
    procedure TestDaysFromNow;
    procedure TestDaysBeyondIntervalWithoutFormat;
    procedure TestFormatDateBeyondInterval;
    procedure TestFormatDateNotUsedWithinInterval;
    procedure TestZeroIntervalUsesDefault;
    procedure TestMonthsAgo;
    procedure TestAlmostOneYearIsMonthsNotZeroYears;
    procedure TestOneYearAgo;
    procedure TestYearsFromNow;
    // parsing
    procedure TestParseDateWithDash;
    procedure TestParseDateWithSlash;
    procedure TestParseDateTime;
    procedure TestParseDateTimeWithSeconds;
    procedure TestParseIso8601;
    procedure TestParseIgnoresLocaleSeparator;
    procedure TestParseUnixTimestampIsLocalTime;
    procedure TestParseCompactDateIsNotUnixTimestamp;
    procedure TestParseInvalid;
    procedure TestStringInvalidReturnedUnchanged;
    procedure TestStringEmptyReturnedUnchanged;
    procedure TestStringOverload;
    // public API against the real clock
    procedure TestDateTimeHumanStringWithFormat;
    procedure TestDateTimeHumanNow;
  end;

implementation

function TDateTimeLibTest.Human(ADate: TDateTime; AMaxInterval: integer;
  AFormat: string): string;
begin
  Result := DateTimeHumanAt(ADate, FNow, AMaxInterval, AFormat);
end;

procedure TDateTimeLibTest.SetUp;
begin
  // Sunday 10 March 2024, 09:00:00
  FNow := EncodeDateTime(2024, 3, 10, 9, 0, 0, 0);
end;

procedure TDateTimeLibTest.TestSecondsAgo;
begin
  AssertEquals('0 seconds ago', Human(FNow));
  AssertEquals('5 seconds ago', Human(IncSecond(FNow, -5)));
  AssertEquals('59 seconds ago', Human(IncSecond(FNow, -59)));
end;

procedure TDateTimeLibTest.TestSecondsFromNow;
begin
  AssertEquals('5 seconds from now', Human(IncSecond(FNow, 5)));
end;

procedure TDateTimeLibTest.TestMinutesAgo;
begin
  AssertEquals('1 minutes ago', Human(IncSecond(FNow, -60)));
  AssertEquals('59 minutes ago', Human(IncSecond(FNow, -(59 * 60 + 59))));
end;

procedure TDateTimeLibTest.TestHoursAgo;
begin
  AssertEquals('1 hours ago', Human(IncHour(FNow, -1)));
  AssertEquals('6 hours ago', Human(IncHour(FNow, -6)));
  AssertEquals('23 hours ago', Human(IncSecond(FNow, -(SecsPerDay - 1))));
end;

procedure TDateTimeLibTest.TestHoursAcrossMidnightIsNotYesterday;
begin
  // 09:00 -> 23:00 the day before is 10 hours, even though the calendar day differs
  AssertEquals('10 hours ago', Human(IncHour(FNow, -10)));
end;

procedure TDateTimeLibTest.TestYesterday;
begin
  // 24 hours
  AssertEquals('yesterday', Human(IncHour(FNow, -24)));
  // 09 March 00:30, 32.5 hours ago
  AssertEquals('yesterday', Human(EncodeDateTime(2024, 3, 9, 0, 30, 0, 0)));
end;

procedure TDateTimeLibTest.TestTomorrow;
begin
  AssertEquals('tomorrow', Human(IncHour(FNow, 24)));
  // 11 March 23:30
  AssertEquals('tomorrow', Human(EncodeDateTime(2024, 3, 11, 23, 30, 0, 0)));
end;

procedure TDateTimeLibTest.TestTwoCalendarDaysIsNotYesterday;
begin
  // 08 March 23:00, 34 hours ago: DaysBetween() says 1, but it is two calendar days back
  AssertEquals('2 days ago', Human(EncodeDateTime(2024, 3, 8, 23, 0, 0, 0)));
end;

procedure TDateTimeLibTest.TestDaysAgo;
begin
  AssertEquals('2 days ago', Human(IncDay(FNow, -2)));
  AssertEquals('10 days ago', Human(IncDay(FNow, -10)));
  AssertEquals('30 days ago', Human(IncDay(FNow, -30)));
end;

procedure TDateTimeLibTest.TestDaysFromNow;
begin
  AssertEquals('4 days from now', Human(IncDay(FNow, 4)));
end;

procedure TDateTimeLibTest.TestDaysBeyondIntervalWithoutFormat;
begin
  // interval exceeded but less than a month and no format: still days
  AssertEquals('10 days ago', Human(IncDay(FNow, -10), 7));
end;

procedure TDateTimeLibTest.TestFormatDateBeyondInterval;
begin
  AssertEquals('2024-02-29', Human(IncDay(FNow, -10), 7, 'yyyy-mm-dd'));
  AssertEquals('2023-03-10', Human(IncYear(FNow, -1), 30, 'yyyy-mm-dd'));
end;

procedure TDateTimeLibTest.TestFormatDateNotUsedWithinInterval;
begin
  AssertEquals('3 days ago', Human(IncDay(FNow, -3), 7, 'yyyy-mm-dd'));
end;

procedure TDateTimeLibTest.TestZeroIntervalUsesDefault;
begin
  AssertEquals('20 days ago', Human(IncDay(FNow, -20), 0, 'yyyy-mm-dd'));
  AssertEquals('2024-02-01', Human(IncDay(FNow, -38), 0, 'yyyy-mm-dd'));
end;

procedure TDateTimeLibTest.TestMonthsAgo;
begin
  AssertEquals('1 months ago', Human(IncDay(FNow, -31)));
  AssertEquals('1 months ago', Human(IncDay(FNow, -45)));
  AssertEquals('3 months ago', Human(IncDay(FNow, -100)));
end;

procedure TDateTimeLibTest.TestAlmostOneYearIsMonthsNotZeroYears;
begin
  // used to produce "0 years ago" for 361..365 days
  AssertEquals('11 months ago', Human(IncDay(FNow, -362)));
end;

procedure TDateTimeLibTest.TestOneYearAgo;
begin
  AssertEquals('1 years ago', Human(IncDay(FNow, -366)));
  AssertEquals('1 years ago', Human(IncYear(FNow, -1)));
end;

procedure TDateTimeLibTest.TestYearsFromNow;
begin
  AssertEquals('2 years from now', Human(IncDay(FNow, 800)));
end;

procedure TDateTimeLibTest.TestParseDateWithDash;
var
  dt: TDateTime;
begin
  AssertTrue(TryParseHumanDate('2015-12-30', dt));
  AssertEquals(EncodeDate(2015, 12, 30), dt);
end;

procedure TDateTimeLibTest.TestParseDateWithSlash;
var
  dt: TDateTime;
begin
  AssertTrue(TryParseHumanDate('2015/12/30', dt));
  AssertEquals(EncodeDate(2015, 12, 30), dt);
end;

procedure TDateTimeLibTest.TestParseDateTime;
var
  dt: TDateTime;
begin
  AssertTrue(TryParseHumanDate('2015-12-30 10:20', dt));
  AssertEquals(EncodeDateTime(2015, 12, 30, 10, 20, 0, 0), dt);
end;

procedure TDateTimeLibTest.TestParseDateTimeWithSeconds;
var
  dt: TDateTime;
begin
  AssertTrue(TryParseHumanDate('2015-12-30 22:20:30', dt));
  AssertEquals(EncodeDateTime(2015, 12, 30, 22, 20, 30, 0), dt);
end;

procedure TDateTimeLibTest.TestParseIso8601;
var
  dt: TDateTime;
begin
  AssertTrue(TryParseHumanDate('2015-12-30T10:20:30', dt));
  AssertEquals(EncodeDateTime(2015, 12, 30, 10, 20, 30, 0), dt);
end;

procedure TDateTimeLibTest.TestParseIgnoresLocaleSeparator;
var
  dt: TDateTime;
  saved: TFormatSettings;
begin
  saved := DefaultFormatSettings;
  try
    DefaultFormatSettings.DateSeparator := '.';
    DefaultFormatSettings.TimeSeparator := '.';
    DefaultFormatSettings.ShortDateFormat := 'dd.mm.yyyy';
    AssertTrue(TryParseHumanDate('2015-12-30 10:20', dt));
    AssertEquals(EncodeDateTime(2015, 12, 30, 10, 20, 0, 0), dt);
  finally
    DefaultFormatSettings := saved;
  end;
end;

procedure TDateTimeLibTest.TestParseUnixTimestampIsLocalTime;
var
  dt, expected: TDateTime;
begin
  expected := EncodeDateTime(2015, 12, 30, 10, 20, 30, 0);
  AssertTrue(TryParseHumanDate(IntToStr(DateTimeToUnix(expected, False)), dt));
  AssertEquals(expected, dt);
end;

procedure TDateTimeLibTest.TestParseCompactDateIsNotUnixTimestamp;
var
  dt: TDateTime;
begin
  // "20151230" must not be interpreted as 1970-08-23
  if TryParseHumanDate('20151230', dt) then
    AssertFalse('compact number treated as unix timestamp', YearOf(dt) = 1970);
end;

procedure TDateTimeLibTest.TestParseInvalid;
var
  dt: TDateTime;
begin
  AssertFalse(TryParseHumanDate('not a date', dt));
  AssertFalse(TryParseHumanDate('', dt));
  AssertFalse(TryParseHumanDate('2015-13-45', dt));
end;

procedure TDateTimeLibTest.TestStringInvalidReturnedUnchanged;
begin
  AssertEquals('not a date', DateTimeHumanAt('not a date', FNow));
end;

procedure TDateTimeLibTest.TestStringEmptyReturnedUnchanged;
begin
  AssertEquals('', DateTimeHumanAt('', FNow));
end;

procedure TDateTimeLibTest.TestStringOverload;
begin
  AssertEquals('yesterday', DateTimeHumanAt('2024-03-09 08:00', FNow));
  AssertEquals('2 days from now', DateTimeHumanAt('2024-03-12', FNow));
end;

procedure TDateTimeLibTest.TestDateTimeHumanStringWithFormat;
begin
  // far in the past, so the formatted date is returned regardless of the clock
  AssertEquals('2015-12-30 10:20', DateTimeHuman('2015-12-30 10:20', 30, 'yyyy-mm-dd hh:nn'));
  AssertEquals('2015-12-30', DateTimeHuman('2015/12/30', 30, 'yyyy-mm-dd'));
end;

procedure TDateTimeLibTest.TestDateTimeHumanNow;
var
  s: string;
begin
  s := DateTimeHuman(Now);
  AssertTrue('unexpected: ' + s, Pos(' seconds ago', s) > 0);
  s := DateTimeHuman(IntToStr(DateTimeToUnix(Now, False)));
  AssertTrue('unexpected: ' + s, Pos(' seconds ago', s) > 0);
end;

initialization
  RegisterTest(TDateTimeLibTest);

end.

{
This file is part of the FastPlaz package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit datetime_helpers;

{
  [x] USAGE
  variable
    d : TDateTime;
    s : String;

  d.FromString( '17-08-1945 09:59:00');
  s := d.Format( 'yyyy-mm-dd HH:nn:ss');
  d.AsString;

  if d.IsAM then begin end;
  if d.IsPM then begin end;
  if d.IsToday then begin end;

  if Tomorrow.IsSaturday then begin ... end;
  if d.HourOf = 9 then ....

  d := d.IncMinute(1);

  if d.YearsDiff( Now) > 40 then begin ... end;

  s := d.HumanReadable;
  // 'more than 73 years ago'

}

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  common, datetime_lib,
  dateutils, Classes, SysUtils;

type

  { TDateTimeSmartHelper }

  TDateTimeSmartHelper = Type Helper for TDateTime
  private
  public
    function AsString: String; overload; inline;
    function AsUnixTime(AIsUTC:boolean = False): Int64; overload; inline;
    function Format( AFormat: String = 'yyyy-mm-dd HH:nn:ss'): String; overload; inline;
    function HumanReadable: String; overload; inline;
    function FromString( ADateTimeString: String; AFormat: String = 'yyyy-mm-dd HH:nn:ss'): TDateTime; overload; inline;

    function DayOfWeek: Word; overload; inline;
    function YearOf: Word; overload; inline;
    function MonthOf: Word; overload; inline;
    function DayOf: Word; overload; inline;
    function HourOf: Word; overload; inline;
    function MinuteOf: Word; overload; inline;
    function SecondOf: Word; overload; inline;

    function YearsDiff( ADateTime: TDateTime): Word; overload; inline;
    function MonthsDiff( ADateTime: TDateTime): Word; overload; inline;
    function WeeksDiff( ADateTime: TDateTime): Word; overload; inline;
    function DaysDiff( ADateTime: TDateTime): Word; overload; inline;
    function HoursDiff( ADateTime: TDateTime): Word; overload; inline;
    function MinutesDiff( ADateTime: TDateTime): Word; overload; inline;
    function SecondsDiff( ADateTime: TDateTime): Word; overload; inline;

    function IncYear( AValue: Integer = 1): TDateTime; overload; inline;
    function IncWeek( AValue: Integer = 1): TDateTime; overload; inline;
    function IncDay( AValue: Integer = 1): TDateTime; overload; inline;
    function IncHour( AValue: Integer = 1): TDateTime; overload; inline;
    function IncMinute( AValue: Integer = 1): TDateTime; overload; inline;
    function IncSecond( AValue: Integer = 1): TDateTime; overload; inline;

    function IsAM: boolean; overload; inline;
    function IsPM: boolean; overload; inline;
    function IsToday: boolean; overload; inline;
    function IsMonday: boolean; overload; inline;
    function IsTuesday: boolean; overload; inline;
    function IsWednesday: boolean; overload; inline;
    function IsThursday: boolean; overload; inline;
    function IsFriday: boolean; overload; inline;
    function IsSaturday: boolean; overload; inline;
    function IsSunday: boolean; overload; inline;
  end;


implementation

{ TDateTimeSmartHelper }

function TDateTimeSmartHelper.AsString: String;
begin
  Result := Format;
end;

function TDateTimeSmartHelper.AsUnixTime(AIsUTC: boolean): Int64;
begin
  Result := DateTimeToUnix(Self, AIsUTC);
end;

function TDateTimeSmartHelper.Format(AFormat: String): String;
begin
  Result := FormatDateTime( AFormat, Self);
end;

function TDateTimeSmartHelper.HumanReadable: String;
begin
  Result := DateTimeHuman( Self);
end;

function TDateTimeSmartHelper.FromString(ADateTimeString: String;
  AFormat: String): TDateTime;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.ShortDateFormat := AFormat;
  Self := StrToDateTime( ADateTimeString, FS);
  Result := Self;
end;

function TDateTimeSmartHelper.DayOfWeek: Word;
begin
  Result := DayOfTheWeek( Self);
end;

function TDateTimeSmartHelper.YearOf: Word;
begin
  Result := dateutils.YearOf( Self);
end;

function TDateTimeSmartHelper.MonthOf: Word;
begin
  Result := dateutils.MonthOf( Self);
end;

function TDateTimeSmartHelper.DayOf: Word;
begin
  Result := dateutils.DayOf( Self);
end;

function TDateTimeSmartHelper.HourOf: Word;
begin
  Result := dateutils.HourOf( Self);
end;

function TDateTimeSmartHelper.MinuteOf: Word;
begin
  Result := dateutils.MinuteOf( Self);
end;

function TDateTimeSmartHelper.SecondOf: Word;
begin
  Result := dateutils.SecondOf( Self);
end;

function TDateTimeSmartHelper.YearsDiff(ADateTime: TDateTime): Word;
begin
  Result := YearsBetween( Self, ADateTime);
end;

function TDateTimeSmartHelper.MonthsDiff(ADateTime: TDateTime): Word;
begin
  Result := MonthsBetween( Self, ADateTime);
end;

function TDateTimeSmartHelper.WeeksDiff(ADateTime: TDateTime): Word;
begin
  Result := WeeksBetween( Self, ADateTime);
end;

function TDateTimeSmartHelper.DaysDiff(ADateTime: TDateTime): Word;
begin
  Result := DaysBetween( Self, ADateTime);
end;

function TDateTimeSmartHelper.HoursDiff(ADateTime: TDateTime): Word;
begin
  Result := HoursBetween( Self, ADateTime);
end;

function TDateTimeSmartHelper.MinutesDiff(ADateTime: TDateTime): Word;
begin
  Result := MinutesBetween( Self, ADateTime);
end;

function TDateTimeSmartHelper.SecondsDiff(ADateTime: TDateTime): Word;
begin
  Result := SecondsBetween( Self, ADateTime);
end;

function TDateTimeSmartHelper.IncYear(AValue: Integer): TDateTime;
begin
  Result := dateutils.IncYear( Self, AValue);
end;

function TDateTimeSmartHelper.IncWeek(AValue: Integer): TDateTime;
begin
  Result := dateutils.IncWeek( Self, AValue);
end;

function TDateTimeSmartHelper.IncDay(AValue: Integer): TDateTime;
begin
  Result := dateutils.IncDay( Self, AValue);
end;

function TDateTimeSmartHelper.IncHour(AValue: Integer): TDateTime;
begin
  Result := dateutils.IncHour( Self, AValue);
end;

function TDateTimeSmartHelper.IncMinute(AValue: Integer): TDateTime;
begin
  Result := dateutils.IncMinute( Self, AValue);
end;

function TDateTimeSmartHelper.IncSecond(AValue: Integer): TDateTime;
begin
  Result := dateutils.IncSecond( Self, AValue);
end;

function TDateTimeSmartHelper.IsAM: boolean;
begin
  Result := NOT dateutils.IsPM( Self);
end;

function TDateTimeSmartHelper.IsPM: boolean;
begin
  Result := dateutils.IsPM( Self);
end;

function TDateTimeSmartHelper.IsToday: boolean;
begin
  Result := dateutils.IsToday( Self);
end;

function TDateTimeSmartHelper.IsMonday: boolean;
begin
  if DayOfTheWeek( Self) = 1 Then
    Result := True
  else
    Result := False;
end;

function TDateTimeSmartHelper.IsTuesday: boolean;
begin
  if DayOfTheWeek( Self) = 2 Then
    Result := True
  else
    Result := False;
end;

function TDateTimeSmartHelper.IsWednesday: boolean;
begin
  if DayOfTheWeek( Self) = 3 Then
    Result := True
  else
    Result := False;
end;

function TDateTimeSmartHelper.IsThursday: boolean;
begin
  if DayOfTheWeek( Self) = 4 Then
    Result := True
  else
    Result := False;
end;

function TDateTimeSmartHelper.IsFriday: boolean;
begin
  if DayOfTheWeek( Self) = 5 Then
    Result := True
  else
    Result := False;
end;

function TDateTimeSmartHelper.IsSaturday: boolean;
begin
  if DayOfTheWeek( Self) = 6 Then
    Result := True
  else
    Result := False;
end;

function TDateTimeSmartHelper.IsSunday: boolean;
begin
  if DayOfTheWeek( Self) = 7 Then
    Result := True
  else
    Result := False;
end;

end.




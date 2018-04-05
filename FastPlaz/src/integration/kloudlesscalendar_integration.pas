unit kloudlesscalendar_integration;

{
  The Universal API Platform
  Simplify your integration strategy
  https://kloudless.com

  Calendar
  https://developers.kloudless.com/docs/v1/calendar

  [x] USAGE

  Timestamp ISO 8601 Format:
  2017-03-04T00:00:00+07:00
  date('Y-m-d\TH:i:s.Z\Z', time());

  Klaudless Interactive Docs
  https://developers.kloudless.com/interactive-docs/#!/accounts/cal_calendars_events_list

}
{$mode objfpc}{$H+}

interface

uses
  common, http_lib, logutil_lib,
  fpjson, jsonparser, dateutils,
  Classes, SysUtils;

type

  TimestampISO8601 = string;

  { TKlaudlessCalendarIntegration }

  TKlaudlessCalendarIntegration = class(TInterfacedObject)
  private
    FAppID: string;
    FAPIKey: string;
    FDebug: boolean;
    FEventCountTotal: integer;
    FIsSuccessfull: boolean;
    FResultCode: integer;
    FResultText: string;
    jsonData: TJSONData;
    function isPermitted: boolean;
  public
    constructor Create;
    destructor Destroy;

    property Debug: boolean read FDebug write FDebug;
    property IsSuccessfull: boolean read FIsSuccessfull;
    property ResultCode: integer read FResultCode;
    property ResultText: string read FResultText;

    property AppID: string read FAppID write FAppID;
    property APIKey: string read FAPIKey write FAPIKey;

    property EventCountTotal: integer read FEventCountTotal write FEventCountTotal;

    function GetEventListAsJson(AAccountID, ACalendarID: string;
      AStartTime: TDateTime; AEndTime: TDateTime): string;
    function GetEventListAsJson(AAccountID, ACalendarID: string;
      AStartTime: TimestampISO8601 = ''; AEndTime: TimestampISO8601 = ''): string;
    function EventList(AFilter: string = ''): string;

    function EventCreate(AAccountID, ACalendarID: string; AEventName: string;
      AStartTime: TDateTime; AEndTime: TDateTime): string;
    function EventCreate(AAccountID, ACalendarID: string; AEventName: string;
      AStartTime: TimestampISO8601 = ''; AEndTime: TimestampISO8601 = ''): string;
  end;


implementation

const
  _KLAUDLESS_API_URL = 'https://api.kloudless.com';
  _KLAUDLESS_CALENDAR_EVENTLIST_URL = '/v1/accounts/%s/cal/calendars/%s/events/';
  //_KLAUDLESS_NUMBER = '1️⃣2️⃣3️⃣4️⃣5️⃣6️⃣7️⃣8️⃣9️⃣🔟';
  //_PAGAR = '️⃣';
  //_BINTANG = '*️⃣';

var
  Response: IHTTPResponse;

{ TKlaudlessCalendarIntegration }

function TKlaudlessCalendarIntegration.isPermitted: boolean;
begin
  Result := False;
  if (FAppID = '') or (FAPIKey = '') then
    Exit;
  Result := True;
end;

constructor TKlaudlessCalendarIntegration.Create;
begin
  FAppID := '';
  FAPIKey := '';
  FDebug := True;
  FEventCountTotal := 0;
end;

destructor TKlaudlessCalendarIntegration.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
end;

function TKlaudlessCalendarIntegration.GetEventListAsJson(AAccountID,
  ACalendarID: string; AStartTime: TDateTime; AEndTime: TDateTime): string;
begin
  Result := GetEventListAsJson(AAccountID, ACalendarID,
    DateTimeToISO8601(AStartTime), DateTimeToISO8601(AEndTime));
end;

function TKlaudlessCalendarIntegration.GetEventListAsJson(AAccountID,
  ACalendarID: string;
  AStartTime: TimestampISO8601; AEndTime: TimestampISO8601): string;
var
  urlTarget: string;
begin
  Result := '';
  if not isPermitted then
    Exit;
  if (ACalendarID = '') or (AAccountID = '') then
    Exit;

  urlTarget := _KLAUDLESS_API_URL + Format(_KLAUDLESS_CALENDAR_EVENTLIST_URL,
    [UrlEncode(AAccountID), UrlEncode(ACalendarID)]) + '?_=1';
  if AStartTime <> '' then
    urlTarget := urlTarget + '&start=' + UrlEncode(AStartTime);
  if AEndTime <> '' then
    urlTarget := urlTarget + '&end=' + UrlEncode(AEndTime);

  with THTTPLib.Create(urlTarget) do
  begin
    try
      //ContentType := 'application/json';
      AddHeader('Authorization', 'APIkey ' + FAPIKey);
      Response := Get;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      if Response.ResultCode = 200 then
      begin
        jsonData := GetJSON(FResultText);
        Result := FResultText;
      end;
    except
      on E: Exception do
      begin
        if Debug then
          LogUtil.Add(e.Message, 'KLAUD');
      end;
    end;
    Free;
  end;
end;

function TKlaudlessCalendarIntegration.EventList(AFilter: string): string;
var
  i: integer;
  startTime, endTime: TDateTime;
  s, tgl: string;
  lst: TStrings;
  _eventList: TStringList;
begin
  Result := '';
  if not Assigned(jsonData) then
    Exit;

  FEventCountTotal := jsonData.GetPath('count').AsInteger;
  if FEventCountTotal = 0 then
  begin
    Exit;
  end;

  _eventList := TStringList.Create;
  AFilter := UpperCase( AFilter);
  for i := 0 to FEventCountTotal - 1 do
  begin
    s := jsonGetData(jsonData, 'objects[' + i2s(i) + '].name');
    if Pos( AFilter, UpperCase(s)) = 0 then
      Continue;

    s := jsonGetData(jsonData, 'objects[' + i2s(i) + '].id') + '#' + s;
    s := jsonGetData(jsonData, 'objects[' + i2s(i) + '].end') + '#' + s;
    s := jsonGetData(jsonData, 'objects[' + i2s(i) + '].start') + '#' + s;
    _eventList.Add(s);
  end;
  _eventList.Sort;

  tgl := '';
  Result := '';
  for i := 0 to _eventList.Count - 1 do
  begin
    s := _eventList[i];
    lst := Explode(s, '#');

    s := lst[0];
    startTime := ISO8601ToDateTime(s, 7);
    s := lst[1];
    endTime := ISO8601ToDateTime(s, 7);
    s := '🗓' + '*' + FormatDateTime('d MMM YY', startTime) + '*';
    if s <> tgl then
    begin
      tgl := s;
      Result := Result + tgl + ':'#10;
    end;
    s := lst[3];
    s := copy( s, LastDelimiter('|',s)+1);
    Result := Result + i2s(i + 1) + '. ' + s + #10;
    Result := Result + ' ' + FormatDateTime('HH:nn', startTime) +
      '-' + FormatDateTime('HH:nn', endTime) + #10;
    //Result := Result + lst[2] + #10;

    lst.Free;
    Result := Result + #10;
  end;

  _eventList.Free;
end;

function TKlaudlessCalendarIntegration.EventCreate(AAccountID, ACalendarID: string;
  AEventName: string; AStartTime: TDateTime; AEndTime: TDateTime): string;

begin
  Result := EventCreate(AAccountID, ACalendarID, AEventName,
    DateTimeToISO8601(AStartTime), DateTimeToISO8601(AEndTime));
end;

function TKlaudlessCalendarIntegration.EventCreate(AAccountID, ACalendarID: string;
  AEventName: string; AStartTime: TimestampISO8601; AEndTime: TimestampISO8601): string;
var
  s, urlTarget: string;
begin
  Result := '';
  if not isPermitted then
    Exit;
  if (ACalendarID = '') or (AAccountID = '') then
    Exit;

  urlTarget := _KLAUDLESS_API_URL + Format(_KLAUDLESS_CALENDAR_EVENTLIST_URL,
    [UrlEncode(AAccountID), UrlEncode(ACalendarID)]) + '?_=1';

  s := '{';
  s := s + '"name": "' + AEventName + '",';
  s := s + '"start": "' + AStartTime + '",';
  s := s + '"end": "' + AEndTime + '",';
  s := s + '"nonet": "none"';
  s := s + '}';

  with THTTPLib.Create(urlTarget) do
  begin
    try
      ContentType := 'application/json';
      AddHeader('Authorization', 'APIkey ' + FAPIKey);
      RequestBody := TStringStream.Create(s);
      Response := Post;
      FResultCode := Response.ResultCode;
      FResultText := Response.ResultText;
      if (Response.ResultCode = 200) or (Response.ResultCode = 201) then
      begin
        jsonData := GetJSON(FResultText);
        s := jsonGetData(jsonData, 'message');
        if s = '' then
        begin
          Result := FResultText;
        end;
      end;
    except
      on E: Exception do
      begin
        if Debug then
          LogUtil.Add(e.Message, 'KLAUD');
      end;
    end;
    Free;
  end;
end;

end.

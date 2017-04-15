unit ibacortrainschedule_integration;

{$mode objfpc}{$H+}

interface

uses
  http_lib, fpjson, common,
  Classes, SysUtils;

type

  { TIbacorTrainScheduleController }

  TIbacorTrainScheduleController = class
  private
    FDateAsString: string;
    FDestinationStation: string;
    FStationData: TStringList;
    FStationName: string;
    FToken: string;
    jsonData: TJSONData;
    function getStationData: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function DateKey(AIndex: integer): string;
    function CityKey(ACityName: string): string;
    function StationKey(ACityName, AStationName: string): string;
    function IsValidCity(ACityName: string): boolean;
    function StationListAsString(ACityName: string): string;
    function Schedule(ADateKey, AFromKey, ADestinationKey: string): string;
    function ScheduleStationToStation(ADateKey, AFromKey, ADestinationKey:
      string): string;
  published
    property Token: string read FToken write FToken;
    property StationData: TStringList read FStationData;
    property DateAsString: string read FDateAsString;

    property DestinationStation: string read FDestinationStation;
    property StationName: string read FStationName;

  end;

implementation

const
  _IBACOR_TRAINSCHEDULE_DATA = 'http://ibacor.com/api/kereta-api?k=';
  _IBACOR_TRAINSCHEDULE =
    'http://ibacor.com/api/kereta-api?k=%s&tanggal=%s&asal=%s&tujuan=%s';

var
  Response: IHTTPResponse;

{ TIbacorTrainScheduleController }

function TIbacorTrainScheduleController.getStationData: boolean;
begin
  Result := False;
  if FToken = '' then
    Exit;

  with THTTPLib.Create(_IBACOR_TRAINSCHEDULE_DATA + FToken) do
  begin
    Response := Get;
    try
      if Response.ResultCode = 200 then
      begin
        jsonData := GetJSON(Response.ResultText);
        if jsonGetData(jsonData, 'status') = 'success' then
        begin
          FStationData.Text := Response.ResultText;
          Result := True;
        end;
      end;
    except
    end;
    Free;
  end;
end;

constructor TIbacorTrainScheduleController.Create;
begin
  FStationData := TStringList.Create;
  FStationData.Text := '';
end;

destructor TIbacorTrainScheduleController.Destroy;
begin
  if Assigned(jsonData) then
    jsonData.Free;
  FStationData.Free;
end;

function TIbacorTrainScheduleController.DateKey(AIndex: integer): string;
begin
  Result := '';

  if not Assigned(jsonData) then
    if not getStationData then
      exit;

  FDateAsString := jsonGetData(jsonData, 'data/tanggal[' + i2s(AIndex) + ']/name');
  Result := jsonGetData(jsonData, 'data/tanggal[' + i2s(AIndex) + ']/value');
end;

function TIbacorTrainScheduleController.CityKey(ACityName: string): string;
var
  i: integer;
  s: string;
begin
  Result := '';

  if not Assigned(jsonData) then
    if not getStationData then
      exit;

  for i := 0 to jsonData.GetPath('data.stasiun').Count - 1 do
  begin
    s := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) + ']/kota');
    if s = UpperCase(ACityName) then
    begin
      FStationName := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) + ']/list[0]/name');
      Result := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) + ']/list[0]/value');
    end;
  end;

end;

function TIbacorTrainScheduleController.StationKey(ACityName,
  AStationName: string): string;
var
  i, j: integer;
  s: string;
begin
  Result := '';

  if not Assigned(jsonData) then
    if not getStationData then
      exit;

  ACityName := StringReplace(UpperCase(ACityName), ' ', '', [rfReplaceAll]);
  AStationName := StringReplace(UpperCase(AStationName), ' ', '', [rfReplaceAll]);
  try
    for i := 0 to jsonData.GetPath('data.stasiun').Count - 1 do
    begin
      s := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) + ']/kota');
      s := StringReplace(s, ' ', '', [rfReplaceAll]);
      if s = ACityName then
      begin
        for j := 0 to jsonData.GetPath('data.stasiun[' + i2s(i) + '].list').Count - 1 do
        begin
          s := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) + ']/list[' +
            i2s(j) + ']/name');
          s := StringReplace(s, ' ', '', [rfReplaceAll]);
          if Pos(AStationName, s) > 0 then
          begin
            Result := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) +
              ']/list[' + i2s(j) + ']/value');
          end;
        end;
        exit;
      end;
    end;
  except
  end;
end;

function TIbacorTrainScheduleController.IsValidCity(ACityName: string): boolean;
var
  i: integer;
  s: string;
begin
  Result := False;

  if not Assigned(jsonData) then
    if not getStationData then
      exit;

  try
    for i := 0 to jsonData.GetPath('data.stasiun').Count - 1 do
    begin
      s := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) + ']/kota');
      if s = UpperCase(ACityName) then
      begin
        Result := True;
      end;
    end;
  except
  end;

end;

function TIbacorTrainScheduleController.StationListAsString(ACityName: string): string;
var
  i, j: integer;
  s: string;
begin
  Result := '';

  if not Assigned(jsonData) then
    if not getStationData then
      exit;

  ACityName := StringReplace(UpperCase(ACityName), ' ', '', [rfReplaceAll]);
  Result := '';
  try
    for i := 0 to jsonData.GetPath('data.stasiun').Count - 1 do
    begin
      s := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) + ']/kota');
      s := StringReplace(s, ' ', '', [rfReplaceAll]);
      if s = ACityName then
      begin
        for j := 0 to jsonData.GetPath('data.stasiun[' + i2s(i) + '].list').Count - 1 do
        begin
          s := jsonGetData(jsonData, 'data/stasiun[' + i2s(i) + ']/list[' +
            i2s(j) + ']/name');
          s := StringReplace(s, ' ', '', [rfReplaceAll]);
          Result := Result + s + #10;
        end;
        Result := Trim(Result);
        exit;
      end;
    end;
  except
  end;
end;

function TIbacorTrainScheduleController.Schedule(ADateKey, AFromKey,
  ADestinationKey: string): string;
begin
  Result := '';
end;

function TIbacorTrainScheduleController.ScheduleStationToStation(
  ADateKey, AFromKey, ADestinationKey: string): string;
var
  urlTarget: string;
begin
  Result := '';
  urlTarget := Format(_IBACOR_TRAINSCHEDULE, [FToken, ADateKey, AFromKey,
    ADestinationKey]);
  with THTTPLib.Create(urlTarget) do
  begin
    Response := Get;
    try
      if Response.ResultCode = 200 then
      begin
        Result := Response.ResultText;
      end;
    except
    end;
    Free;
  end;
end;

end.

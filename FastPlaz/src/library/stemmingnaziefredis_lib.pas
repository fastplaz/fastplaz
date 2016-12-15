unit stemmingnaziefredis_lib;

{$mode objfpc}{$H+}

interface

uses
  stemmingnazief_lib, redis_controller, common,
  Classes, SysUtils;

const
  _STEMMINGNAZIEF_DICTIONARY_REDISKEY = 'STEMMING_DICTIONARY';

type

  { TStemmingNaziefRedis }

  TStemmingNaziefRedis = class(TStemmingNazief)
  private
    FKeyName: string;
    FRedis: TRedisConstroller;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Ping:boolean;
    procedure LoadDictionaryFromFile(FileName: string =
      _STEMMINGNAZIEF_DICTIONARY_REDISKEY);
      override;
    procedure LoadDictionaryFromRedis(AKeyName: string =
      _STEMMINGNAZIEF_DICTIONARY_REDISKEY);

  published
    property KeyName: string read FKeyName write FKeyName;
  end;

implementation

{ TStemmingNaziefRedis }

constructor TStemmingNaziefRedis.Create;
begin
  inherited Create;
  FRedis := TRedisConstroller.Create;
  FKeyName := _STEMMINGNAZIEF_DICTIONARY_REDISKEY;
end;

destructor TStemmingNaziefRedis.Destroy;
begin
  FRedis.Free;
  inherited Destroy;
end;

function TStemmingNaziefRedis.Ping: boolean;
begin
  Result := FRedis.Ping;
end;

procedure TStemmingNaziefRedis.LoadDictionaryFromFile(FileName: string);
begin
  inherited LoadDictionaryFromFile;
  if IsDictionaryLoaded then
  begin
    FRedis[KeyName] := Dictionary.Text;
  end;
end;

procedure TStemmingNaziefRedis.LoadDictionaryFromRedis(AKeyName: string);
begin
  FKeyName := AKeyName;
  Dictionary.Text := FRedis[AKeyName];
end;

end.

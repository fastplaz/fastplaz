{

Simple Redis Client Controller
* for set and get data only.
- non-thread
- master-slave support

by: Luri Darmawan ( http://www.fastplaz.com )
requirement:
  - synapse (if any)

USAGE:
[x] set value
Redis := TRedisConstroller.Create;
Redis['thekey'] := 'your string';

[x] get value
variable := Redis['thekey'];

USE MASTER SLAVE
- see example

**********************************************************************}
unit redis_controller;

{$mode objfpc}{$H+}

interface

uses
  cthreads,
  fpcgi, redis_lib, fastplaz_handler, common, Classes, SysUtils;

const
  _REDIS_CONFIG_SERVERADDRESS = 'redis/%s/server';
  _REDIS_CONFIG_PORT = 'redis/%s/port';
  _REDIS_CONFIG_TIMEOUT = 'redis/%s/timeout';
  _REDIS_CONFIG_WRITETOMASTER = 'redis/write_to_master';
  _REDIS_CONFIG_MASTERCONFIG = 'redis/master_config';


type

  { TRedisConstroller }

  TRedisConstroller = class(TRedisLib)
  private
  public
    constructor Create(const ConfigName: string = 'default');
    destructor Destroy; override;
  end;

implementation

//uses config_lib, logutil_lib, language_lib, versioninfo_lib, html_lib, initialize_controller;

{ TRedisConstroller }

constructor TRedisConstroller.Create(const ConfigName: string);
var
  s: string;
begin
  inherited Create(ConfigName);
  ServerAddress := string(Config.GetValue(format(_REDIS_CONFIG_SERVERADDRESS, [ConfigName]), REDIS_DEFAULT_SERVER));
  Port := string(Config.GetValue(format(_REDIS_CONFIG_PORT, [ConfigName]), '6379'));
  TimeOut := (Config.GetValue(format(_REDIS_CONFIG_TIMEOUT, [ConfigName]), REDIS_DEFAULT_TIMEOUT));
  WriteToMaster := (Config.GetValue(_REDIS_CONFIG_WRITETOMASTER, False));
  if WriteToMaster then
  begin
    s := string(Config.GetValue(_REDIS_CONFIG_MASTERCONFIG, 'master'));
    MasterServerAddress := string(Config.GetValue(format(_REDIS_CONFIG_SERVERADDRESS, [s]), REDIS_DEFAULT_SERVER));
    MasterPort := string(Config.GetValue(format(_REDIS_CONFIG_PORT, [s]), '6379'));
    MasterTimeOut := (Config.GetValue(format(_REDIS_CONFIG_TIMEOUT, [s]), REDIS_DEFAULT_TIMEOUT));
  end;
end;

destructor TRedisConstroller.Destroy;
begin
  inherited Destroy;
end;

end.






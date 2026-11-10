unit routes;

{ Routes of the metric test app.

  Patterns are regular expressions matched against PathInfo (which starts
  with "/"), in the order they are added; the first match wins. They are not
  anchored automatically: "/" alone matches every address, so anchor each
  pattern with ^...$ . The trailing slash is optional (/?).

  Keep "notfound" last: it takes every address no other route knows. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses
  app_controller, notfound_controller,
  probe_ping_controller, probe_json_controller, probe_echo_controller,
  probe_state_controller, probe_sleep_controller, probe_raise_controller,
  probe_halt_controller, probe_session_controller, probe_db_controller,
  probe_big_controller, probe_stats_controller;

initialization
  Route.Add('probe_ping', '^/probe/ping/?$', TProbePingController);
  Route.Add('probe_json', '^/probe/json/?$', TProbeJsonController);
  Route.Add('probe_echo', '^/probe/echo(/.*)?$', TProbeEchoController);
  Route.Add('probe_state', '^/probe/state/?$', TProbeStateController);
  Route.Add('probe_sleep', '^/probe/sleep/?$', TProbeSleepController);
  Route.Add('probe_raise', '^/probe/raise/?$', TProbeRaiseController);
  Route.Add('probe_halt', '^/probe/halt/?$', TProbeHaltController);
  Route.Add('probe_session', '^/probe/session/?$', TProbeSessionController);
  Route.Add('probe_db', '^/probe/db/?$', TProbeDbController);
  Route.Add('probe_big', '^/probe/big/?$', TProbeBigController);
  Route.Add('probe_stats', '^/probe/stats/?$', TProbeStatsController);

  Route.Add('main', '^/$', TAppController); // Main Controller

  // keep last: every other address gets a 404
  Route.Add('notfound', '^/.+', TNotFoundController);

end.

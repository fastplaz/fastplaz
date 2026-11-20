unit example_tools;

(*
  Tools and a resource of the MCP example. Everything is registered in the
  initialization section; handlers keep no shared state, so they are safe
  in the threaded standalone server.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, mcp_server;

implementation

procedure DoEcho(ACall: TMcpToolCall);
begin
  ACall.Finish(ACall.Str('text'));
end;

procedure DoAdd(ACall: TMcpToolCall);
begin
  if not ACall.Has('a') or not ACall.Has('b') then
    raise EMcpToolError.Create('Both "a" and "b" are required');
  ACall.FinishJSON(TJSONObject.Create(['sum', ACall.Float('a') + ACall.Float('b')]));
end;

procedure DoServerTime(ACall: TMcpToolCall);
begin
  ACall.Finish(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
end;

function ReadAbout(const AUri: string; out AText: string): boolean;
begin
  AText := '# FastPlaz MCP example' + LineEnding + LineEnding +
    'Tools: echo, add, server_time.';
  Result := True;
end;

initialization
  McpServer.Name := 'fastplaz-mcp-example';
  McpServer.Title := 'FastPlaz MCP Example';
  McpServer.Version := '1.0.0';
  McpServer.Instructions := 'Example tools served by a FastPlaz application.';

  McpServer.RegisterTool('echo', 'Echo', 'Returns the given text.',
    '{"type":"object","properties":{"text":{"type":"string","description":"Text to return"}},"required":["text"]}',
    @DoEcho, [mthReadOnly, mthIdempotent]);
  McpServer.RegisterTool('add', 'Add numbers', 'Adds two numbers.',
    '{"type":"object","properties":{"a":{"type":"number"},"b":{"type":"number"}},"required":["a","b"]}',
    @DoAdd, [mthReadOnly, mthIdempotent],
    '{"type":"object","properties":{"sum":{"type":"number"}},"required":["sum"]}');
  McpServer.RegisterTool('server_time', 'Server time',
    'Current date and time of the server.', '', @DoServerTime, [mthReadOnly]);
  McpServer.RegisterResource('fastplaz://about', 'about', 'About this server',
    'What this example offers', 'text/markdown', @ReadAbout);

end.

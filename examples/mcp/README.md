# FastPlaz MCP Server Example

A FastPlaz application that is an **MCP (Model Context Protocol) server**:
AI agents and MCP clients (Claude Code, MCP Inspector, ...) call its tools
over HTTP. The same binary runs standalone or as CGI behind Apache.

| Item | Value |
| --- | --- |
| Endpoint | `POST /mcp`, one JSON-RPC message, JSON answer |
| Transport | MCP Streamable HTTP without SSE (`GET /mcp` answers `405`) |
| Session | stateless, no `Mcp-Session-Id` |
| Tools | `echo`, `add` (with output schema), `server_time` |
| Resource | `fastplaz://about` |

## Files

```
source/mcp.lpr                  program
source/routes.pas               Route['/mcp'] := TMcpController
source/tools/example_tools.pas  tools and resource, registered in initialization
public_html/config/config.json  server mode/port and the "mcp" section
public_html/.htaccess           CGI setup, passes the bearer token to the program
```

## Build

Open `source/mcp.lpi` in Lazarus (requires the `fastplaz_runtime` package)
and build, or:

```
lazbuild source/mcp.lpi
```

The binary is written to `public_html/mcp.bin`.

## Run standalone

```
cd public_html
./mcp.bin
```

The endpoint is `http://127.0.0.1:17846/mcp` (port in `config.json`,
`systems/server/port`, or `--port=`).

```
curl -s -X POST http://127.0.0.1:17846/mcp -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"add","arguments":{"a":2,"b":3}}}'
```

## Run as CGI

Point an Apache virtual host at `public_html` (`AllowOverride All`,
`mod_rewrite`, `mod_cgi`). The endpoint is `http://yourhost/mcp`.

## Connect a client

Claude Code:

```
claude mcp add --transport http fastplaz-example http://127.0.0.1:17846/mcp
```

With a token, add `--header "Authorization: Bearer <token>"`.

## Configuration

`config/config.json`, read on the first request:

```json
"mcp" : {
  "token" : "env:MCP_TOKEN",
  "allowed_origins" : "app.example.com",
  "exception_details" : false
}
```

| Key | Meaning |
| --- | --- |
| `token` | bearer token every request must send; `env:NAME` reads it from an environment variable; empty = no token |
| `allowed_origins` | browser origins allowed besides loopback and the server's own host (comma separated, `*` = all) |
| `exception_details` | send the text of unexpected exceptions to the client (development only) |

Apache does not pass the `Authorization` header to CGI programs by default;
`public_html/.htaccess` passes it on with a rewrite rule (read by the
controller as `REDIRECT_HTTP_AUTHORIZATION`). `CGIPassAuth On` (Apache
2.4.13+) does the same but needs `AllowOverride AuthConfig`.

## Adding a tool

```pascal
procedure DoGreet(ACall: TMcpToolCall);
begin
  if ACall.Str('name') = '' then
    raise EMcpToolError.Create('name is required');  // tool error, isError = true
  ACall.Finish('Hello ' + ACall.Str('name'));
end;

initialization
  McpServer.RegisterTool('greet', 'Greet', 'Says hello.',
    '{"type":"object","properties":{"name":{"type":"string"}},"required":["name"]}',
    @DoGreet, [mthReadOnly]);
```

Standalone mode serves requests in several threads with one `McpServer`:
register tools in `initialization` only and keep handlers free of shared
state (or protect it).


FastPlaz - MCP Server Project Wizard

This project is an MCP (Model Context Protocol) server built with FastPlaz.
AI agents and MCP clients (Claude Code, MCP Inspector, ...) call its tools
over HTTP:

  POST http://yourhost/mcp           (CGI, behind Apache)
  POST http://127.0.0.1:17845/mcp    (standalone, run the binary directly)

Add tools in the controller unit, initialization section:

  McpServer.RegisterTool('name', 'Title', 'Description',
    '{"type":"object","properties":{...}}', @Handler, [mthReadOnly]);

Standalone mode serves requests in several threads: keep tool handlers free
of shared state.

config/config.json, section "mcp":
  token              bearer token ("env:NAME" reads an environment variable)
  allowed_origins    browser origins allowed besides localhost and own host
  exception_details  send exception texts to the client (development only)

Test with curl:
curl -X POST http://127.0.0.1:17845/mcp -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'

Connect Claude Code:
claude mcp add --transport http myserver http://127.0.0.1:17845/mcp

Example project: examples/mcp

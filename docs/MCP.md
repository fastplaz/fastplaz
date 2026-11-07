# FastPlaz MCP Server (AI Agent Plane)

FastPlaz AI Agent Plane runs a small **MCP (Model Context Protocol) server
inside the Lazarus IDE**. The coding agent started from AI Agent Plane (Claude
Code, Pi, OpenCode, GitHub Copilot CLI or another ACP agent) uses it to work
on the IDE itself: open a unit at a line, read the editor including unsaved
changes, compile the project and read the compiler messages, and generate
FastPlaz controllers and models.

This page describes what is implemented. Design notes and the roadmap:
[AI_PLANE_TOOL_RUNDOWN.md](AI_PLANE_TOOL_RUNDOWN.md). The feature itself:
[AI_AGENT_by_FASTPLAZ.md](AI_AGENT_by_FASTPLAZ.md).

Source: `tools/src/ai_plane/` (`aiplane_httpserver.pas`,
`aiplane_mcpserver.pas`, `aiplane_tools_ide.pas`, `aiplane_tools_fastplaz.pas`).

## Server

| Item | Value |
| --- | --- |
| Server name | `fastplaz` (title "Lazarus IDE (FastPlaz Agent Plane)", version 0.1) |
| Transport | MCP Streamable HTTP, simplest form: `POST /mcp` with one JSON-RPC message, JSON response, no SSE stream |
| URL | `http://127.0.0.1:<port>/mcp` |
| Default port | `17826`; when it is taken, the next free port up to `17835` |
| Protocol versions | `2025-11-25`, `2025-06-18` (default), `2025-03-26`, `2024-11-05` |
| Methods | `initialize`, `ping`, `tools/list`, `tools/call`; notifications (`notifications/initialized`, `notifications/cancelled`) are accepted |
| Session | `Mcp-Session-Id` is issued at `initialize` and checked on later requests |
| Request body limit | 4 MB |
| Result size | Text results are cut at 32 KB |

Tool calls run in the IDE main thread: the server is polled from a timer, no
threads touch IDEIntf. A long call such as a compile keeps the HTTP request
open until it finishes.

### HTTP answers

| Request | Answer |
| --- | --- |
| `POST /mcp` with a JSON-RPC message | `200` with the JSON-RPC response, `202` for notifications |
| `DELETE /mcp` | `200`, ends the session |
| `GET /mcp` and other methods | `405` (no server-initiated SSE stream) |
| Missing or wrong `Authorization: Bearer <token>` | `401` |
| `Origin` header that is not loopback | `403` |
| Unknown `Mcp-Session-Id`, or another path | `404` |

### Security

- Listens on `127.0.0.1` only.
- Every request needs `Authorization: Bearer <token>`. A new random token is
  made each time the server starts.
- Requests with a non-loopback `Origin` header are refused (DNS rebinding
  protection).
- `read_editor` only reads files in the project and its search paths. The
  generators only write inside the project directory.

### Settings

Edited in *FastPlaz → Configuration → AI Harness* and stored in the FastPlaz
configuration:

| Key | Default | Meaning |
| --- | --- | --- |
| `aiplane/mcp_enabled` | `true` | Run the MCP server ("Enable IDE tools (MCP)") |
| `aiplane/mcp_port` | `17826` | First port to try (1024-65535) |

When the server is disabled, runtimes get no MCP entry and the chat works
without IDE tools.

### How agents get connected

AI Agent Plane registers the server with the agent it starts. The token is
never written into the user's own agent configuration.

| Agent | How |
| --- | --- |
| ACP agents (OpenCode, ...) | `mcpServers` entry (type `http`, `Authorization` header) in `session/new`, when the agent advertises `mcpCapabilities.http` |
| GitHub Copilot CLI | ACP as above; the preset adds `--allow-tool=fastplaz` so Copilot does not ask a second time |
| Claude Code | `--mcp-config` pointing to a temporary JSON file, plus `--allowedTools mcp__fastplaz` so Claude Code does not ask a second time. Tools appear as `mcp__fastplaz__<tool>` |
| Pi | Temporary extension loaded with `-e` that calls `pi.registerMcpServer` (needs a Pi version with MCP support, 1.4.0 or later) |

### Permissions

Each tool has a permission level. Tools that need permission show a card in
the AI Agent Plane chat, and the call waits until the user answers.

| Level | Behaviour |
| --- | --- |
| auto | Runs without asking |
| ask once per session | Asks first; "Allow for this session" is remembered until the session is reset |
| always ask | Asks before every call |

The card shows the tool title, a short summary of what will happen and the
arguments.

## Tools

13 tools in two groups. Paths are absolute or relative to the project
directory.

### IDE tools

Source: `tools/src/ai_plane/aiplane_tools_ide.pas`

| Tool | Description | Arguments | Permission |
| --- | --- | --- | --- |
| `open_file` | Opens a file in the Lazarus source editor, optionally at a line and column | `path` (required), `line`, `column` (1-based) | auto |
| `get_active_editor` | File, caret position (1-based line and column), selected text and modified state of the active editor | none | auto |
| `list_open_files` | Files open in the source editor and whether they have unsaved changes | none | auto |
| `read_editor` | Text of a file as shown in the editor, unsaved changes included, with line numbers. Reads the file on disk when it is not open. Project files and search paths only | `path` (required), `start_line`, `end_line` | auto |
| `project_info` | Active project: `.lpi` file, main unit, build modes, target OS/CPU, unit and include search paths, custom compiler options, output file | none | auto |
| `list_project_units` | Files that are part of the active project | none | auto |
| `compile_project` | Compiles the active project (or builds all of it with `rebuild`). Saves modified editor files first | `rebuild`, `build_mode`, `min_urgency` | ask once per session |
| `build_project` | Builds all of the active project, like **Run > Build**: every project unit is compiled again. Saves modified editor files first | `build_mode`, `min_urgency` | ask once per session |
| `clean_and_compile_project` | Like **Run > Clean up and Build**: deletes the compiled files (unit output directory, and `*.ppu`/`*.o` in the source directories, using the project's clean masks), then builds all. Source files (`.pas`, `.pp`, `.inc`, `.lpr`, `.lpi`, `.lfm`, ...) are never deleted | `build_mode`, `min_urgency` | always ask |
| `get_messages` | Messages of the last compile from the IDE Messages window, with file, line and column | `min_urgency` | auto |

Shared arguments:

- `build_mode`: build mode to make active first (see `project_info`).
- `min_urgency`: lowest message level to return, one of `hint`, `note`,
  `warning`, `error`. Default `warning`.

Build results (`compile_project`, `build_project`,
`clean_and_compile_project`):

```json
{"success": true, "duration_ms": 1234,
 "messages": [{"file": "...", "line": 10, "column": 5, "urgency": "error", "text": "..."}]}
```

`clean_and_compile_project` adds `cleanup` with `output_directory`,
`deleted_count`, `deleted`, `failed` and `skipped_sources`.

### FastPlaz generators

Source: `tools/src/ai_plane/aiplane_tools_fastplaz.pas`

These tools write a new unit from the FastPlaz templates, add it to the
project and open it. Agents are told to use them instead of writing a
controller or model by hand. In FastPlaz a *module* is a controller, so
"create a module" means one of the controller generators.

| Tool | Description | Arguments | Permission |
| --- | --- | --- | --- |
| `create_simple_controller` | Web page controller (`TMyCustomController` with Get/Post, rendered through the theme, `maincontent` tag handler). Saves `<permalink>_controller.pas` | `name` (required), `permalink`, `directory` | ask once per session |
| `create_api_controller` | REST controller that answers JSON (Get/Post/Options). Saves `<permalink>_controller.pas` | `name` (required), `permalink`, `directory` | ask once per session |
| `create_model` | `TSimpleModel` unit for a database table. Saves `<name>_model.pas` | `name`, `table`, `directory` | ask once per session |

Arguments:

- `name`: module or model name in words. `"user group"` gives
  `TUserGroupController` / `TUserGroupModel`. A single joined word such as
  `"UserGroup"` gives `TUsergroupController`.
- `permalink`: route of the controller, e.g. `user-group`. Default: the name.
- `table`: database table. When `name` is left out, the model name comes from
  the table (`users` gives `TUserModel`). The table name is written into the
  constructor only when `TSimpleModel` would not work it out from the class
  name.
- `directory`: where to put the unit, relative to the project directory or
  an absolute path inside it. Default: the project directory.

Results:

- Controllers: `file`, `class`, `permalink`, `route`, `saved`, `in_unit_path`.
- Model: `file`, `class`, `table`, `custom_table_name`, `saved`,
  `in_unit_path`.

Rules:

- Nothing is overwritten. If the file already exists, the call fails and
  nothing changes.
- The directory must be inside the project directory. `in_unit_path` says
  whether it is in the project's unit search path.
- A generated controller registers its route in its `initialization`
  section. If the project has a `routes.pas`, move that line there.

## Example

```http
POST /mcp HTTP/1.1
Host: 127.0.0.1:17826
Authorization: Bearer <token>
Mcp-Session-Id: <session id from initialize>
Content-Type: application/json

{"jsonrpc":"2.0","id":1,"method":"tools/call",
 "params":{"name":"open_file","arguments":{"path":"main.pas","line":42}}}
```

## Adding a tool

1. Write a handler method `procedure(ACall: TAIMcpToolCall) of object` that
   ends with `ACall.Finish(text)`, `ACall.FinishJSON(data)` or
   `ACall.Fail(message)`. It can finish later, for example after a compile.
2. Register it in the `Register` method of `TAIIdeTools` or
   `TAIFastPlazTools` with `AServer.RegisterTool(name, title, description,
   inputSchema, permission, handler)`.
3. Optional: `AServer.SetToolDescriber(name, describer)` adds a summary to the
   permission card.
4. Add tests in `tools/src/test/` (see [UNIT_TEST.md](../UNIT_TEST.md)).
5. List the tool in this page and in the catalog of
   [AI_PLANE_TOOL_RUNDOWN.md](AI_PLANE_TOOL_RUNDOWN.md).

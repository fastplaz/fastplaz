# AI Agent Plane IDE Tools — Rundown

Design document. What is implemented today, with the full tool list, is in
[MCP.md](MCP.md); the feature itself is described in
[AI_AGENT_by_FASTPLAZ.md](AI_AGENT_by_FASTPLAZ.md).

Status (2026-11-07):

* **Phase 1 done**: `aiplane_httpserver`, `aiplane_mcpserver`,
  `aiplane_tools_ide`, settings, registration for ACP agents, Claude Code,
  Pi and GitHub Copilot CLI. `build_project` and
  `clean_and_compile_project` were added on top of the phase 1 tools.
* **Phase 3 started**: the FastPlaz generators `create_simple_controller`,
  `create_api_controller` and `create_model` (`aiplane_tools_fastplaz`).
* Phase 2, the rest of phase 3, and phase 4 are still proposals.

Decisions taken on the open questions are listed in section 10.

Goal: let the coding agent running in AI Agent Plane (OpenCode, Claude Code,
Pi, GitHub Copilot CLI, other ACP agents) act on the Lazarus IDE itself — open a unit at a line, read the
active editor, compile the project and read the errors, ask CodeTools where an
identifier is declared, inspect a database through the Database Explorer, and so
on — instead of only reading and writing files on disk and running shell
commands.

Must run on **Windows, macOS and Linux** (all Lazarus widgetsets: win32,
cocoa, gtk2/gtk3/qt5/qt6), like the rest of FastPlaz. See section 7.

---

## 1. Starting point

State before the IDE tools were built (kept for context):

| Area | Before |
| --- | --- |
| Agent tools | Only the runtime's built-in tools (file read/write, shell, search). |
| IDE → agent | `fs/read_text_file` (ACP) and `OnReadFile`: unsaved editor buffers are served to the agent. |
| MCP | `session/new` sent `mcpServers: []` (`aiplane_acp.pas`). Claude Code and Pi got no MCP config. |
| Permissions | Permission cards already existed in the chat view (`OnPermission` / `RespondPermission`) for ACP, Claude (`can_use_tool`) and Pi (extension dialogs). |

Today the IDE serves its own tools over MCP to every runtime; see
[MCP.md](MCP.md).

---

## 2. Architecture: MCP over HTTP, served by the IDE

```
 Lazarus IDE process                                agent process
 ┌──────────────────────────────────┐              ┌────────────────────┐
 │ AIPlaneWindow                    │   spawns     │ opencode / claude /│
 │  └ TAIChatFrame ── adapter ──────┼─────────────▶│ pi                 │
 │                                  │              │                    │
 │ TAIMcpServer                     │  POST /mcp   │  MCP client        │
 │  http://127.0.0.1:17826/mcp      │◀─────────────┤                    │
 │  └ tool handlers (main thread)   │  JSON-RPC    │                    │
 │     TAIIdeTools: editor /        │              └────────────────────┘
 │       project / build            │
 │     TAIFastPlazTools: generators │
 └──────────────────────────────────┘
```

The IDE itself is the MCP server, using the MCP **Streamable HTTP** transport
in its simplest form (JSON responses, no SSE stream). No extra binary, no
bridge process.

Why HTTP only:

* Supported by the runtimes we target (checked 2026-11-02):

  | Runtime | MCP over HTTP | How it was checked |
  | --- | --- | --- |
  | OpenCode 2.2.5 (ACP) | yes | `initialize` → `agentCapabilities.mcpCapabilities: {"http": true, "sse": false}` |
  | Claude Code 2.1.316 | yes | `--mcp-config` entry with `"type": "http"`, `url`, `headers` (to verify live) |
  | other ACP agents | if they advertise `mcpCapabilities.http` | checked at `initialize`; otherwise IDE tools are off for that runtime |
  | Pi 1.4.0 | yes (built-in MCP, checked 2026-11-03) | `pi.registerMcpServer()` from an extension loaded with `-e` |
  | GitHub Copilot CLI 1.0.103 (ACP) | yes (checked 2026-11-03) | `copilot --acp` → `mcpCapabilities: {"http": true, "sse": true}` |

* No locally built binary that Windows Smart App Control / WDAC or macOS
  Gatekeeper could block, no per-OS/CPU build, no executable bit.
* No environment copying into child processes: URL and token travel in the
  MCP server entry.
* One process, one code path on every OS.

### 2.1 HTTP server inside the IDE

`TFPHttpServer` is blocking/threaded, so `TAIHttpServer` is a minimal
HTTP/1.1 handler on a non-blocking socket, **polled from the chat timer**
(the same no-thread pattern as `TAIProcessChannel`). Tool calls therefore run
in the main thread, where IDEIntf and CodeTools are safe to use.

| Request | Answer |
| --- | --- |
| `POST /mcp`, `Content-Type: application/json`, JSON-RPC body | `200`, `application/json`, JSON-RPC response (or `202` for notifications) |
| `GET /mcp` | `405 Method Not Allowed` (no server-initiated SSE stream; allowed by the spec) |
| `DELETE /mcp` | `200` (session end, nothing to clean up) |
| missing / wrong `Authorization: Bearer <token>` | `401` |
| `Origin` header present and not loopback | `403` (DNS rebinding protection required by the MCP spec) |
| anything else | `404` |

* Handled MCP methods: `initialize`, `notifications/initialized`, `ping`,
  `tools/list`, `tools/call`.
* `Mcp-Session-Id` is issued at `initialize` and checked afterwards.
* Request body limit 4 MB (`AI_HTTP_MAX_BODY`); keep-alive supported; several
  connections at once (the agent may open more than one).
* Long tools (compile): the response is held open until the build finishes,
  the timer keeps the IDE responsive. If a runtime's MCP tool timeout turns
  out too short, `compile_project` returns at once and the agent polls
  `get_messages` (to check during phase 1).

### 2.2 Registering the server per runtime

The token is random per IDE session; the URL uses the actual port (section 3).

| Runtime | How |
| --- | --- |
| ACP (OpenCode, GitHub Copilot CLI, `claude-acp`, ...) | `session/new` → `mcpServers: [{"type": "http", "name": "fastplaz", "url": "http://127.0.0.1:17826/mcp", "headers": [{"name": "Authorization", "value": "Bearer <token>"}]}]`, only when the agent advertised `mcpCapabilities.http` |
| Claude Code (stream-json) | `--mcp-config <temp json>`: `{"mcpServers": {"fastplaz": {"type": "http", "url": "...", "headers": {"Authorization": "Bearer <token>"}}}}`; tools appear as `mcp__fastplaz__<tool>`; `--allowedTools mcp__fastplaz` so Claude Code does not ask a second time (the IDE asks for its risky tools itself) |
| Pi | a temporary extension (`aiplane_mcp_<pid>.ts`, loaded with `pi --mode rpc -e <file>`) calls `pi.registerMcpServer("fastplaz", {url, headers, exposure: "direct"})` for that session only; tools appear as `mcp__fastplaz__<tool>` |
| GitHub Copilot CLI | ACP as above; the preset adds `--allow-tool=fastplaz` so Copilot does not ask a second time |

For ACP the IDE should also advertise and implement the client-side
capabilities it already partly has:

* `fs/write_text_file` → write into the editor buffer when the file is open
  (undoable), otherwise to disk.
* `terminal/*` (later) → run commands with output shown in the IDE.

### 2.3 Code layout

| Unit | Purpose | LCL / IDEIntf |
| --- | --- | --- |
| `aiplane_httpserver.pas` | `TAIHttpServer`: minimal non-blocking HTTP/1.1 server on 127.0.0.1, polled | no |
| `aiplane_mcpserver.pas` | `TAIMcpServer`: `/mcp` routing, token, `Origin`, sessions, JSON-RPC/MCP messages, tool registry (`RegisterTool`), permission hook (`OnPermission` / `RespondPermission`), async replies (`TAIMcpToolCall.Finish` / `Fail`), result truncation | no |
| `aiplane_tools_ide.pas` | `TAIIdeTools`: editor, project and build tools backed by IDEIntf (CodeTools later) | yes (IDE package only) |
| `aiplane_tools_fastplaz.pas` | `TAIFastPlazTools`: FastPlaz generators (later: Database Explorer, regex, JSON) | yes |

The server stays free of LCL/IDEIntf like the rest of `ai_plane`, so it is
unit tested with fake tools, and the LCL tester can serve a mock IDE.

---

## 3. Configuration

Stored in the FastPlaz `config.json` under `aiplane`, edited in
*FastPlaz → Configuration → AI Harness*:

| Setting | Key | Default | UI |
| --- | --- | --- | --- |
| MCP Tools enabled | `aiplane/mcp_enabled` | `true` | check box "Enable IDE tools (MCP)" |
| MCP port | `aiplane/mcp_port` | `17826` | spin edit (1024–65535) |

Behaviour:

* **Enabled**: the server starts with the first runtime the AI Agent Plane window
  starts, and is registered with every runtime that supports it.
* **Disabled**: no server, no socket; runtimes get no MCP entry
  (`mcpServers: []`, no `--mcp-config`, Pi without the extension). The chat
  works as it does today.
* **Port in use** (another Lazarus instance, another program): the server
  tries the next ports up to `mcp_port + 9`, uses the first free one and
  passes that URL to the runtime. The protocol log shows the address
  (`MCP server on http://127.0.0.1:17827/mcp`). When none is free, IDE
  tools are off for this session and the chat shows why.
* Changing either setting restarts the server and the runtime (like a runtime
  change today), keeping the conversation where the runtime can resume.
* The status line shows `MCP` when the server runs, the error when it could
  not start, and nothing when it is off.
* Later: per-group switches (editor, build, CodeTools, FastPlaz, database) and
  a button to reset the remembered "allow for this session" choices.

`TAIPlaneSettings` (in `aiplane_ide.pas`) gains `McpEnabled: boolean` and
`McpPort: integer`; `TAIPlaneSettingsFrame` gains the two controls; the LCL
tester stores the same keys in `aiplane_tester.ini`.

---

## 4. Tool catalog

Legend — **Perm**: `auto` runs without asking, `ask` shows a permission card,
`ask*` asks once per session (can be remembered).

All paths are absolute or relative to the project directory. Tools refuse paths
outside the project directory, its packages and the unit search path unless the
user allows it.

### 4.1 Editor and navigation (phase 1–2)

| Tool | Arguments | Returns | Perm | IDE API |
| --- | --- | --- | --- | --- |
| `open_file` | `path`, `line?`, `column?` | `{opened: true}` | auto | `LazarusIDE.DoOpenEditorFile`, `SourceEditorManagerIntf` |
| `get_active_editor` | — | `{path, line, column, selection, modified}` | auto | `SourceEditorManagerIntf.ActiveEditor` |
| `list_open_files` | — | `[{path, modified}]` | auto | `SourceEditorManagerIntf` |
| `read_editor` | `path`, `start_line?`, `end_line?` | text from the buffer (unsaved changes included) | auto | `TSourceEditorInterface.GetText` |
| `apply_edit` | `path`, `edits: [{start_line, start_col, end_line, end_col, text}]` | `{applied, new_text_hash}` | ask | editor buffer, single undo block |
| `save_file` | `path` | `{saved}` | ask* | `LazarusIDE.DoSaveEditorFile` |

### 4.2 Project and packages (phase 1)

| Tool | Arguments | Returns | Perm |
| --- | --- | --- | --- |
| `project_info` | — | `{lpi, main_unit, build_modes, active_build_mode, target_os, target_cpu, unit_paths, include_paths, defines, output}` | auto |
| `list_project_units` | — | `[{unit, path, in_project}]` | auto |
| `list_packages` | — | `[{name, lpk, version, installed}]` (required by the project) | auto |
| `package_info` | `name` | units, paths, dependencies | auto |

### 4.3 Build (phase 1)

| Tool | Arguments | Returns | Perm |
| --- | --- | --- | --- |
| `compile_project` | `build_mode?`, `rebuild?: bool`, `min_urgency?` | `{success, duration_ms, messages: [{file, line, column, urgency, text}]}` | ask* |
| `build_project` | `build_mode?`, `min_urgency?` | same shape (Run → Build) | ask* |
| `clean_and_compile_project` | `build_mode?`, `min_urgency?` | same shape plus `cleanup: {output_directory, deleted_count, deleted, failed, skipped_sources}` (Run → Clean up and Build, source files never deleted) | ask |
| `build_package` | `lpk` | same shape | ask* |
| `get_messages` | `min_urgency?` | last Messages window content, structured | auto |
| `run_project` / `stop_project` | — | `{started}` | ask (later) |

`compile_project` returns the **structured** Messages window, so the agent does
not parse `lazbuild` output, and the usual loop becomes: compile → read errors →
`open_file` at the error → fix → compile.

Installing a package rebuilds the IDE itself; that is **not** exposed as a tool.

### 4.4 CodeTools (phase 2)

The biggest gain: agents understand Pascal only textually; CodeTools resolves
units, search paths, overloads and includes the way the compiler does.

| Tool | Arguments | Returns | Perm |
| --- | --- | --- | --- |
| `find_declaration` | `path`, `line`, `column` | `{path, line, column, snippet}` | auto |
| `find_references` | `path`, `line`, `column`, `scope?: unit\|project` | `[{path, line, column}]` | auto |
| `find_unit_for` | `identifier` | units that declare it (FPC, LCL, packages, project) | auto |
| `resolve_unit_path` | `unit` | file the compiler would use | auto |
| `unit_outline` | `path` | classes, methods, properties, types (Code Explorer view) | auto |
| `identifier_hint` | `path`, `line`, `column` | declaration and fpdoc summary (like the editor hint) | auto |
| `add_to_uses` | `path`, `unit`, `section?: interface\|implementation` | `{changed}` | ask |
| `complete_class` | `path`, `line`, `column` | `{changed}` (Ctrl+Shift+C) | ask |
| `rename_identifier` | `path`, `line`, `column`, `new_name` | `{files_changed}` | ask |

API: `CodeToolBoss` (find declaration, find references, gather identifiers,
add unit to uses, complete code, rename). Exact method names to be checked
against the Lazarus sources during phase 2.

### 4.5 FastPlaz (phase 3)

| Tool | Arguments | Returns | Perm |
| --- | --- | --- | --- |
| `create_simple_controller` (done) | `name`, `permalink?`, `directory?` | `{file, class, permalink, route, saved, in_unit_path}` | ask* |
| `create_api_controller` (done) | `name`, `permalink?`, `directory?` | as above | ask* |
| `create_model` (done) | `name?`, `table?`, `directory?` | `{file, class, table, custom_table_name, saved, in_unit_path}` | ask* |
| `fastplaz_create_structure` | `kind: web\|theme` | created dirs | ask |
| `db_list_connections` | — | names and types from the Database Explorer config (no passwords) | auto |
| `db_list_tables` | `connection` | table names | ask* |
| `db_describe_table` | `connection`, `table` | columns, types, keys, indexes | ask* |
| `db_query` | `connection`, `sql`, `limit?` | rows (read-only: `SELECT` only, row limit) | ask |
| `regex_test` | `pattern`, `text`, `flags?` | matches and groups (Regex Tester engine) | auto |
| `json_validate` | `text` | `{valid, error_line, formatted}` | auto |

Generators reuse the existing menu code (module, JSON module, model, web and
theme structure), so their output matches what the menu produces.

### 4.6 Debugger (later)

`debug_call_stack`, `debug_locals`, `debug_evaluate(expr)`,
`set_breakpoint(path, line)`, `debug_continue`. Only meaningful while the
debugger is paused; every call except reads is `ask`.

---

## 5. Permissions and safety

* Read-only tools run immediately (`auto`). Tools that change files, run
  builds, touch a database or generate code show a permission card in the chat
  (Allow / Allow for this session / Deny), using the existing permission UI.
* `apply_edit` previews the diff in the card; the edit lands in the editor as
  one undo step and is not saved unless `save_file` is approved.
* `db_query` accepts a single `SELECT` statement, adds a row limit, never
  returns connection passwords.
* Path guard: project directory, required packages and unit search paths only;
  anything else asks.
* Server: binds to `127.0.0.1` only, requires the per-session Bearer token,
  rejects non-loopback `Origin` headers, stops when AI Agent Plane stops its last
  runtime or MCP Tools is disabled.
* Output limits: text results are cut at 32 KB (`AI_MCP_MAX_RESULT`).
* Timeouts (proposal, not implemented): 10 s default, compile and build
  without a timeout but cancellable with the chat's Stop button.

---

## 6. Chat UI

* Tool calls arrive back through the runtime as the same foldable tool cards
  the chat already draws for the runtime's own tools; IDE tools are labelled
  (`FastPlaz: compile_project`).
* Status line: `MCP`, the error, or nothing when off; the address is in the
  protocol log.

---

## 7. Cross-platform (Windows, macOS, Linux)

### 7.1 Socket

* Bind to the literal `127.0.0.1`, never `localhost`, and register
  `http://127.0.0.1:<port>/mcp` with runtimes (`localhost` can resolve to
  `::1` first on macOS and some Linux distributions).
* Binding to loopback only avoids the firewall prompt (Windows Defender
  Firewall, macOS application firewall).
* Non-blocking accept/read/write differ per OS. Keep them in one small unit:
  `ioctlsocket(FIONBIO)` / `WinSock2.select` on Windows, `fpfcntl(O_NONBLOCK)`
  / `fpSelect` on Unix (`{$ifdef MSWINDOWS}` / `{$ifdef UNIX}`).
* `SO_REUSEADDR` on Unix so a restarted IDE can bind the same port at once;
  on Windows use `SO_EXCLUSIVEADDRUSE` instead (Windows `SO_REUSEADDR` would
  let two IDEs share the port).
* Ports below 1024 need root on Unix: the setting is limited to 1024–65535.

### 7.2 Processes

* Windows `.cmd` shims (npm) keep going through `cmd.exe /c`, as
  `TAIProcessChannel` does today; on Unix the runtime is started directly.
* Temporary files (Claude `--mcp-config`) go to `GetTempDir` and are deleted
  when the runtime stops.

### 7.3 Paths

* Agents may send `/` or `\` on any OS: normalize with `SetDirSeparators` and
  `ExpandFileName` / `TrimFilename` (LazFileUtils) before use.
* Compare with `CompareFilenames` / `SameFileName`: case-insensitive on
  Windows and the default macOS file system, case-sensitive on Linux.
* The path guard resolves symlinks before checking the project root (macOS
  `/var` → `/private/var`, Linux home directories on other mounts).
* Paths returned to the agent use the OS separator and are absolute, so
  `open_file` round-trips.
* File names are UTF-8 everywhere (`*UTF8` file functions from LazFileUtils).

### 7.4 IDE side

* Polling runs from the chat's `TTimer`, which behaves the same on win32,
  cocoa, gtk and qt; no threads touch IDEIntf.
* Compile and build go through IDEIntf (`LazarusIDE.DoBuildProject`), not a
  shelled-out `lazbuild`, so the IDE's own compiler path and build mode are
  used on every OS.

### 7.5 Pi

Pi 1.4.0 has built-in MCP. The temporary extension only registers the
IDE's server; Pi's own client connects to it, the same on all three systems.
An older Pi without `registerMcpServer` prints `IDE tools off: ...` on
stderr and starts without the tools.

### 7.6 Testing matrix

* `run_tests.bat` (Windows) and `run_tests.sh` (Linux, macOS) run the same
  `tools_test`, including HTTP round trips against the server on a local port
  (port fallback, token, `Origin`, keep-alive, large bodies).
* Live checks per release: Windows 11 (win32), Linux (gtk2 and qt5), macOS
  (cocoa, Apple Silicon).
* CI (if added): a GitHub Actions matrix `windows-latest`, `ubuntu-latest`,
  `macos-latest` running `tools_test`.

---

## 8. Testing

* **Unit tests** (`tools_test`): MCP message helpers, HTTP parsing (chunked
  bodies refused or handled, keep-alive, limits), token and `Origin` checks,
  port fallback, dispatcher with a fake provider (argument validation,
  permission flow, async replies, truncation), settings load/save defaults
  (`mcp_enabled = true`, `mcp_port = 17826`).
* **Mock agent**: extend `mock_acp_agent` to read `mcpServers` from
  `session/new` and call a tool over HTTP (`tool open_file ...`), so the ACP
  adapter's wiring is tested without a real agent.
* **LCL tester** (`aiplane_tester`): a fake IDE provider (opens files in a
  memo, returns canned compiler messages) to check cards and permissions with
  screenshots.
* **Live**: OpenCode and Claude Code against the real IDE with a sample
  project that has a compile error.

---

## 9. Phases

| Phase | Deliverables |
| --- | --- |
| 1 (done) | Settings (`mcp_enabled`, `mcp_port`) and UI; `TAIHttpServer`, `TAIMcpServer`; registration for ACP and Claude Code; tools: `open_file`, `get_active_editor`, `list_open_files`, `read_editor`, `project_info`, `list_project_units`, `compile_project`, `get_messages`; status line; tests. Added later: `build_project`, `clean_and_compile_project`. |
| 2 | `apply_edit` and `save_file`, ACP `fs/write_text_file` into editor buffers; CodeTools tools. (Pi support was done early, see the runtime table.) |
| 3 (started) | FastPlaz generators (done: `create_simple_controller`, `create_api_controller`, `create_model`), Database Explorer tools, regex and JSON tools. |
| 4 | Debugger tools, `run_project`, ACP `terminal/*`; optional stdio bridge for runtimes without HTTP MCP. |

---

## 10. Questions and decisions

Decided:

1. Tool names: plain names (`open_file`). Claude Code and Pi namespace them
   as `mcp__fastplaz__*` anyway.
2. `compile_project` saves modified editor buffers first, like the IDE does
   on F9 (`DoBuildProject`). So do `build_project` and
   `clean_and_compile_project`.
3. Pi: a temporary extension written at start-up registers the IDE's server
   with Pi's built-in MCP client (Pi 1.4.0 or later).
4. Port in use: try the next ports up to `+9`.

Open:

5. Database tools: allow only connections already saved in the Database
   Explorer, or also ad-hoc connection strings?

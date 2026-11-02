# AI Plane IDE Tools — Rundown

Status (2026-11-02): **phase 1 implemented** (`aiplane_httpserver`,
`aiplane_mcpserver`, `aiplane_tools_ide`, settings, ACP and Claude Code
registration). Phases 2-4 are still proposals. Decisions taken on the open
questions: plain tool names (Claude Code namespaces them as
`mcp__lazarus__*` anyway), `compile_project` saves modified buffers first
(that is what `DoBuildProject` does, like F9), port in use → next free port up
to `+9`.

Goal: let the coding agent running in AI Plane (OpenCode, Claude Code, Pi, other
ACP agents) act on the Lazarus IDE itself — open a unit at a line, read the
active editor, compile the project and read the errors, ask CodeTools where an
identifier is declared, inspect a database through the Database Explorer, and so
on — instead of only reading and writing files on disk and running shell
commands.

Must run on **Windows, macOS and Linux** (all Lazarus widgetsets: win32,
cocoa, gtk2/gtk3/qt5/qt6), like the rest of FastPlaz. See section 7.

---

## 1. Current state

| Area | Today |
| --- | --- |
| Agent tools | Only the runtime's built-in tools (file read/write, shell, search). |
| IDE → agent | `fs/read_text_file` (ACP) and `OnReadFile`: unsaved editor buffers are served to the agent. |
| MCP | `session/new` sends `mcpServers: []` (`aiplane_acp.pas`). Claude Code and Pi get no MCP config. |
| Permissions | Permission cards already exist in the chat view (`OnPermission` / `RespondPermission`) for ACP, Claude (`can_use_tool`) and Pi (extension dialogs). |

---

## 2. Architecture: MCP over HTTP, served by the IDE

```
 Lazarus IDE process                                agent process
 ┌──────────────────────────────────┐              ┌────────────────────┐
 │ AIPlaneWindow                    │   spawns     │ opencode / claude /│
 │  └ TAIChatFrame ── adapter ──────┼─────────────▶│ pi                 │
 │                                  │              │                    │
 │ TAIToolServer                    │  POST /mcp   │  MCP client        │
 │  http://127.0.0.1:17845/mcp      │◀─────────────┤  (or Pi extension) │
 │  └ TAIToolDispatcher (main thr.) │  JSON-RPC    │                    │
 │     └ tool providers             │              └────────────────────┘
 │        editor / project / build /│
 │        codetools / fastplaz      │
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
  | Pi | no MCP client | a small Pi extension calls the same endpoint with `fetch()` |

* No locally built binary that Windows Smart App Control / WDAC or macOS
  Gatekeeper could block, no per-OS/CPU build, no executable bit.
* No environment copying into child processes: URL and token travel in the
  MCP server entry.
* One process, one code path on every OS.

### 2.1 HTTP server inside the IDE

`TFPHttpServer` is blocking/threaded, so `TAIToolServer` is a minimal
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
* Request size limit (for example 1 MB); keep-alive supported; several
  connections at once (the agent may open more than one).
* Long tools (compile): the response is held open until the build finishes,
  the timer keeps the IDE responsive. If a runtime's MCP tool timeout turns
  out too short, `compile_project` returns at once and the agent polls
  `get_messages` (to check during phase 1).

### 2.2 Registering the server per runtime

The token is random per IDE session; the URL uses the actual port (section 3).

| Runtime | How |
| --- | --- |
| ACP (OpenCode, `claude-acp`, ...) | `session/new` → `mcpServers: [{"type": "http", "name": "lazarus", "url": "http://127.0.0.1:17845/mcp", "headers": [{"name": "Authorization", "value": "Bearer <token>"}]}]`, only when the agent advertised `mcpCapabilities.http` |
| Claude Code (stream-json) | `--mcp-config <temp json>`: `{"mcpServers": {"lazarus": {"type": "http", "url": "...", "headers": {"Authorization": "Bearer <token>"}}}}`; tools appear as `mcp__lazarus__<tool>`; `--allowedTools mcp__lazarus` so Claude Code does not ask a second time (the IDE asks for its risky tools itself) |
| Pi | Pi extension `aiplane-lazarus.ts` registers the tools and POSTs `tools/call` to the same URL; URL and token passed to Pi when AI Plane starts it |

For ACP the IDE should also advertise and implement the client-side
capabilities it already partly has:

* `fs/write_text_file` → write into the editor buffer when the file is open
  (undoable), otherwise to disk.
* `terminal/*` (later) → run commands with output shown in the IDE.

### 2.3 Code layout (proposal)

| Unit | Purpose | LCL / IDEIntf |
| --- | --- | --- |
| `aiplane_mcpproto.pas` | JSON-RPC/MCP message helpers, tool schema records | no |
| `aiplane_httpserver.pas` | Minimal non-blocking HTTP/1.1 server, polled | no |
| `aiplane_toolserver.pas` | `TAIToolServer`: `/mcp` routing, token, `Origin`, sessions | no |
| `aiplane_tooldispatch.pas` | `TAIToolDispatcher`: tool registry, argument validation, permission hook, async replies, result shaping | no |
| `aiplane_tools_ide.pas` | Tool providers backed by IDEIntf / CodeTools | yes (IDE package only) |
| `aiplane_tools_fastplaz.pas` | Providers for generators, Database Explorer, regex, JSON | yes |

The core stays free of LCL/IDEIntf like the rest of `ai_plane`, so the server
and dispatcher are unit tested with a fake provider, and the LCL tester can
serve a mock IDE.

---

## 3. Configuration

Stored in the FastPlaz `config.json` under `aiplane`, edited in
*FastPlaz → Configuration → AI Harness*:

| Setting | Key | Default | UI |
| --- | --- | --- | --- |
| MCP Tools enabled | `aiplane/mcp_enabled` | `true` | check box "Enable IDE tools (MCP)" |
| MCP port | `aiplane/mcp_port` | `17845` | spin edit (1024–65535) |

Behaviour:

* **Enabled**: the server starts with the first runtime the AI Plane window
  starts, and is registered with every runtime that supports it.
* **Disabled**: no server, no socket; runtimes get no MCP entry
  (`mcpServers: []`, no `--mcp-config`, Pi without the extension). The chat
  works as it does today.
* **Port in use** (another Lazarus instance, another program): the server
  tries the next ports up to `mcp_port + 9`, uses the first free one and
  passes that URL to the runtime. The status line shows
  `MCP: 127.0.0.1:17846` so the user can see it moved. When none is free, IDE
  tools are off for this session and the chat shows why.
* Changing either setting restarts the server and the runtime (like a runtime
  change today), keeping the conversation where the runtime can resume.
* The status line shows `MCP: off`, `MCP: 127.0.0.1:<port>` or the error.
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
| `compile_project` | `build_mode?`, `rebuild?: bool` | `{success, duration_ms, messages: [{file, line, column, urgency, text}]}` | ask* |
| `build_package` | `lpk` | same shape | ask* |
| `get_messages` | `urgency_min?` | last Messages window content, structured | auto |
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
| `fastplaz_create_module` | `name`, `kind: default\|json`, `route?` | created files | ask |
| `fastplaz_create_model` | `connection`, `table` | created unit | ask |
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
  rejects non-loopback `Origin` headers, stops when AI Plane stops its last
  runtime or MCP Tools is disabled.
* Output limits: tool results are truncated (for example 32 KB or 500 lines)
  with a note, like Pi tool output today (`MAX_TOOL_OUTPUT`).
* Timeouts: 10 s default, compile and build without a timeout but cancellable
  with the chat's Stop button.

---

## 6. Chat UI

* Tool calls arrive back through the runtime as the same foldable tool cards
  the chat already draws for the runtime's own tools; IDE tools are labelled
  (`Lazarus: compile_project`).
* Status line: `MCP: 127.0.0.1:<port>`, `MCP: off` or the error.

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

### 7.5 Pi extension

Written in TypeScript and run by Pi's Node runtime; `fetch()` to
`127.0.0.1` is the same on all three systems.

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
  (`mcp_enabled = true`, `mcp_port = 17845`).
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
| 1 | Settings (`mcp_enabled`, `mcp_port`) and UI; HTTP server, `TAIToolServer`, dispatcher; registration for ACP and Claude Code; tools: `open_file`, `get_active_editor`, `list_open_files`, `read_editor`, `project_info`, `list_project_units`, `compile_project`, `get_messages`; status line; tests. |
| 2 | `apply_edit` and `save_file`, ACP `fs/write_text_file` into editor buffers; CodeTools tools; Pi extension. |
| 3 | FastPlaz generators, Database Explorer tools, regex and JSON tools. |
| 4 | Debugger tools, `run_project`, ACP `terminal/*`; optional stdio bridge for runtimes without HTTP MCP. |

---

## 10. Open questions

1. Tool name prefix: plain names (`open_file`) or `lazarus_open_file` to avoid
   clashing with the runtime's own tools in ACP agents that do not namespace
   MCP tools?
2. Should `compile_project` save modified editor buffers first (the IDE does
   when you press F9), or compile only what is on disk?
3. Database tools: allow only connections already saved in the Database
   Explorer, or also ad-hoc connection strings?
4. Pi: extension in TypeScript inside the repo, or keep Pi without IDE tools
   until it has an MCP client?
5. Port in use: try the next ports (proposed, up to `+9`) or fail with a
   message so the user picks another port?

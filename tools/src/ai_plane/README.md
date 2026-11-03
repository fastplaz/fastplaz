# Agent Plane

**FastPlaz Agent Plane: coding agents inside the Lazarus IDE.**

Chat window inside the Lazarus IDE (menu *FastPlaz → Agent Plane*) that talks to
coding agents such as Claude Code, OpenCode and Pi. This folder holds the core,
which has no LCL or IDEIntf dependency, so the IDE package, a standalone tester
and the unit tests all share the same code.

Status: **phase 3** — core adapters, chat frame, LCL tester and the IDE
integration (menu *FastPlaz → Agent Plane*, dockable window, *AI Harness* tab in
*FastPlaz → Configuration*).

The tool was first called *AI Plane*; the internal names keep that: the
`aiplane_*` units, the `aiplane/...` settings in `config.json` (so existing
settings stay) and the `ai_plane` folder.

## Units

| Unit | Purpose |
| --- | --- |
| `aiplane_types.pas` | `TAIAdapter` base class, `TAIEvent`, permission/model records |
| `aiplane_process.pas` | `TAIProcessChannel`: child process with line based stdout, polled without threads; finds `.exe`/`.cmd` shims on `PATH` |
| `aiplane_acp.pas` | `TACPAdapter`: Agent Client Protocol (JSON-RPC 2.0 over stdio) |
| `aiplane_pi.pas` | `TPiRpcAdapter`: Pi's native RPC mode (`pi --mode rpc`): streamed text/thinking, tool execution, abort, new session, model list, `set_model`, `set_thinking_level` without restart, extension confirm/select dialogs as permission cards |
| `aiplane_claude.pas` | `TClaudeStreamAdapter`: Claude Code `--input-format/--output-format stream-json` |
| `aiplane_presets.pas` | Known runtimes (command, arguments, adapter class), `AICreateAdapter()` |
| `aiplane_markdown.pas` | Markdown parser (no LCL): headings, lists, quotes, fenced code, tables, rules; bold, italic, strikethrough, `code`, links (with titles), images `![alt](src "title")` and linked images `[![alt](src)](href)`. Half-streamed markup stays readable |
| `aiplane_codehighlight.pas` | Syntax colours for code blocks from the SynEdit highlighters (Pascal, LFM, SQL, JS/JSON, XML, HTML, Python, PHP, shell, batch, INI, CSS, C/C++, Java, diff, Perl), mapped to a theme-neutral palette |
| `aiplane_chatview.pas` | `TAIChatView` (LCL): owner drawn conversation; assistant and thinking text rendered as markdown, links open in the browser, local images (relative to the project) drawn scaled, remote images shown as a placeholder link (never downloaded), titles as tooltips, foldable tool/thinking/plan cards, permission buttons; text selection by dragging or double click, Ctrl+C / Ctrl+A, copy menu (wrapped lines are joined again, table cells tab separated) |
| `aiplane_settingsframe.pas/.lfm` | `TAIPlaneSettingsFrame` (LCL): default runtime, command, arguments, model (*Detect* reads the runtime's list), effort and *Run Test* (also checks the IDE tools when MCP is on) |
| `aiplane_ide.pas/.lfm` | IDEIntf: `AIPlaneWindow` (dockable, hosts the chat), menu handler, settings in `config.json` under `aiplane`, project directory as working directory, unsaved editor buffers served to the agent |
| `aiplane_chatframe.pas/.lfm` | `TAIChatFrame` (LCL): runtime/model/effort pickers, conversation, input (Enter sends, Shift+Enter new line, Esc stops), Send/Stop, protocol log, status line, permission cards for IDE tools (`ShowToolPermission`) |
| `aiplane_httpserver.pas` | `TAIHttpServer`: minimal HTTP/1.1 server on `127.0.0.1`, non-blocking and polled (no threads); keep-alive, pipelining, chunked bodies, answers that come later; Windows/macOS/Linux socket differences (SIGPIPE, exclusive port) |
| `aiplane_mcpserver.pas` | `TAIMcpServer`: MCP over Streamable HTTP (`POST /mcp`, JSON responses), Bearer token, `Origin` check, sessions, `tools/list`, `tools/call`, tools that ask the user first (once or for the session), result size limit, port fallback |
| `aiplane_tools_ide.pas` | IDEIntf: the IDE tools offered over MCP (`open_file`, `get_active_editor`, `list_open_files`, `read_editor`, `project_info`, `list_project_units`, `compile_project`, `get_messages`) |
| `aiplane_tools_fastplaz.pas` | IDEIntf: the FastPlaz generators offered over MCP (`create_simple_controller`, `create_api_controller`, `create_model`), the same code as the FastPlaz menu items |

## How the UI uses it

```pascal
Adapter := AICreateAdapter(AI_PRESET_OPENCODE);
Adapter.OnEvent := @AdapterEvent;          // text, thinking, tool calls, done...
Adapter.OnPermission := @AdapterPermission; // answer now or later with RespondPermission
Adapter.OnReadFile := @ReadEditorBuffer;    // serve unsaved IDE buffers
Adapter.Model := 'opencode/big-pickle';
Adapter.Prompt('Explain this unit');        // starts the runtime when needed
// in a TTimer (20-50 ms):
Adapter.Poll;
```

Every event is raised from `Poll`, in the caller's thread, so handlers can
update controls directly. Do not call `Prompt`/`Start` from inside an event
handler of the same adapter; post it (`Application.QueueAsyncCall`).

## In the IDE

* *FastPlaz → Agent Plane* opens the chat window; it docks like any IDE window.
* *FastPlaz → Configuration → AI Harness* sets the default runtime,
  command, arguments, model and effort (`config.json`, key `aiplane`).
  Runtime, model and effort picked in the chat window become the new
  defaults too.
* The model is remembered per runtime (`aiplane/models/<runtime>`).
  Switching runtime in the chat window brings back the model last used
  with that runtime (or the runtime default).
* The runtime starts as soon as it is chosen, so its model list fills the
  model box without a first prompt. *Send* stays disabled until the
  runtime answers, and while the chosen model is not one it offers. The
  reason shows in the status line. If the runtime fails to start, *New
  conversation* tries again.
* The runtime starts in the active project's directory and restarts when
  another project is opened (checked when the window is activated).

## IDE tools (MCP)

The Agent Plane window runs an MCP server inside the IDE, so the agent can use
the IDE itself: open a file at a line, read the active editor (unsaved
changes included), read the project settings, compile the project and get
the compiler messages with file, line and column. Design and later phases:
[docs/AI_PLANE_TOOL_RUNDOWN.md](../../../docs/AI_PLANE_TOOL_RUNDOWN.md).

* Transport: MCP Streamable HTTP, `POST http://127.0.0.1:<port>/mcp`, JSON
  responses, no extra binary. A new Bearer token at every server start.
* Settings (*Configuration → AI Harness*): **IDE tools (MCP)** on/off
  (`aiplane/mcp_enabled`, default on) and **Port** (`aiplane/mcp_port`,
  default `17845`; when taken the next free port up to `+9` is used). The
  status line shows `MCP` when the server runs, the error when it could
  not start, nothing when off.
* The IDE *Messages* window (view `FastPlaz`) gets a line when the runtime
  and model become active, when the MCP server starts (with its address)
  or fails, and when the agent reports an error. `get_messages` skips that
  view, so these lines never reach the agent as build messages.
* Registered per runtime:

  | Runtime | How |
  | --- | --- |
  | ACP (OpenCode, ...) | `mcpServers` entry `type: http` in `session/new`, only when the agent advertises `mcpCapabilities.http` |
  | Claude Code | `--mcp-config <temp file>` plus `--allowedTools mcp__fastplaz`; tools appear as `mcp__fastplaz__<tool>` |
  | Pi | a temporary extension loaded with `pi --mode rpc -e <file>` calls `pi.registerMcpServer` for the session (Pi with built-in MCP, 1.4.0); tools appear as `mcp__fastplaz__<tool>` |
  | GitHub Copilot CLI | ACP `mcpServers`; the preset adds `--allow-tool=fastplaz` so Copilot does not ask a second time |

* `compile_project` asks first (*Allow*, *Allow for this session*, *Deny*)
  in the chat; the other phase 1 tools only read and run at once.
  `read_editor` is limited to the project, its search paths and open files.
* Verified 2026-11-02 with `aiplane_cli --mcp`: OpenCode 2.2.5 (ACP) and
  Claude Code 2.1.316 call the tools; the permission card answers the IDE
  side and Claude Code does not ask a second time. Verified 2026-11-03:
  Pi 1.4.0 and GitHub Copilot CLI 1.0.103 call `secret_word` the same way
  (Copilot without its own prompt thanks to `--allow-tool=fastplaz`).

### Testing the IDE tools through the chat

Quickest check: *Configuration → AI Harness → Run Test*. With *IDE tools
(MCP)* on, the test serves a temporary MCP server with one tool,
`connection_check`, hands it to the runtime the way the chat does, and asks
the agent to call it and repeat the check word it returns:

| Result | Meaning |
| --- | --- |
| green `OK ...: runtime answered, IDE tools (MCP) reached` | the runtime works and the agent called the tool |
| orange `Runtime OK ..., IDE tools not reached` | the runtime answers, but the agent never called the tool; the log shows why (for example `IDE tools off: ...`) |
| red `Failed: ...` | the runtime did not start or answer |

The test server uses the configured port, or the next free one while the
IDE's own server holds it. With *IDE tools (MCP)* off, Run Test only sends
`Reply with exactly one word: OK`.

In the IDE:

1. Install `fastplaz_tools.lpk`, open a project, then *FastPlaz → Agent Plane*.
2. The status line shows `MCP`. The *Messages* window shows
   `Agent Plane: MCP server active on http://127.0.0.1:17845/mcp` and, once
   the runtime answers, `Agent Plane: runtime ... active, model ...`. No `MCP`
   in the status line: check *Configuration → AI Harness → IDE tools
   (MCP)*. An error line in *Messages* says why the server did not start.
3. Pick OpenCode, Claude Code, Pi or GitHub Copilot CLI.
4. Ask for a tool by name, so the agent does not answer from the files it
   reads itself:

   | Prompt | Expected |
   | --- | --- |
   | `Use the fastplaz project_info tool: what is this project's name and main file?` | the project name and `.lpi`/`.lpr` |
   | `Use the fastplaz list_open_files tool: which files are open?` | the editor tabs |
   | `Open unit1.pas at line 20 with the fastplaz open_file tool.` | the source editor jumps there |
   | `Compile the project with the fastplaz compile_project tool, then show the errors from get_messages.` | a permission card first; after *Allow for this session* the IDE compiles and the agent lists the errors |

5. The chat shows the tool call as `fastplaz_<tool>` (OpenCode) or
   `mcp__fastplaz__<tool>` (Claude Code). An agent error also lands in
   *Messages* as `Agent Plane: ...`.

When the agent does not see the tools:

* OpenCode: an older version may not take MCP servers over HTTP; the adapter
  then turns the IDE tools off (the protocol log says `IDE tools off: ...`).
* The port is taken by another IDE: the server moves to the next free port
  (see the address in *Messages*); the runtime is given the new address.

Without the IDE, `aiplane_cli --mcp` serves two demo tools, which checks
the runtime ↔ MCP path in seconds:

```
aiplane_cli --runtime opencode --mcp --prompt "Call the fastplaz secret_word tool and tell me the word"
aiplane_cli --runtime claude --model haiku --mcp --prompt "Call the fastplaz confirm_action tool" --yes
```

The CLI prints the secret word; the agent must answer with the same word.
`confirm_action` asks in the terminal (`--yes` allows it); `--log` shows
the protocol traffic. The tester app (`aiplane_tester`) has no MCP server.

## Runtimes

| Preset | Adapter | Command | Verified |
| --- | --- | --- | --- |
| `claude` | stream-json | `claude -p --input-format stream-json --output-format stream-json --verbose --include-partial-messages --permission-prompt-tool stdio` | yes (Claude Code 2.1.316), permission prompt not yet seen live |
| `opencode` | ACP | `opencode acp` | yes (OpenCode 2.1.4) |
| `pi` | Pi RPC | `pi --mode rpc` | yes (Pi 1.4.0): protocol and errors; answers need a provider login (`pi`, then `/login`) |
| `claude-acp` | ACP | `claude-agent-acp` (npm bridge, needs Node.js) | no |
| `copilot` | ACP | `copilot --acp --allow-tool=fastplaz` (sign in with `copilot login`) | yes (Copilot CLI 1.0.103), IDE tools included. Copilot offers no model list over ACP (only `mode` and `allow_all` options), so the chat shows the runtime default; add `--model <id>` to the arguments to pick one |

Spike findings (2026-11-02):

* OpenCode ACP: `initialize` → `session/new` returns `configOptions` with a
  `model` select (the model list, no extra command needed) and a `mode`
  select (`build`/`plan`). Streaming arrives as `session/update`
  (`agent_message_chunk`, `agent_thought_chunk`, `tool_call`,
  `tool_call_update`, `usage_update`). No `thought_level` option was offered
  for the free models, so effort is ignored there.
* Claude Code stream-json: `system/init` only follows the first user message.
  With `--include-partial-messages` text comes as `text_delta`; the full
  `assistant` message repeats it, so the adapter skips that text. Models are
  aliases (`opus`, `sonnet`, `haiku`); `--effort` accepts
  `low|medium|high|xhigh|max`. Changing model or effort restarts the process
  with `--resume <session id>`.
* Windows: npm installs `.cmd` shims, which `CreateProcess` cannot start;
  `TAIProcessChannel` runs them through `cmd.exe /c`. Killing a runtime only
  stops the direct child (a Node bridge may leave its child running).

## Testing without rebuilding Lazarus

* `tools/src/test/test_aiplane.pas` — FPCUnit, part of `tools_test`
  (`run_tests.bat` / `run_tests.sh`). The ACP tests run against
  `mock_acp_agent`, a fake runtime with scripted scenarios (`hello`, `read
  <path>`, `permission`, `slow`, `crash`, `fail`, ...).
* `tools/src/test/aiplane_tester/aiplane_tester.lpi` — LCL window hosting
  `TAIChatFrame`, the same chat as the IDE (`lazbuild aiplane_tester.lpi`).
  Settings live in `aiplane_tester.ini` in the user config folder; the
  *Chat* menu opens the same settings page as the IDE tab, the working
  directory or the mock agent. For scripted UI checks:

  ```
  aiplane_tester --runtime mock --prompt "hello|plan|permission" --screenshot ui.png
  aiplane_tester --runtime opencode --prompt "Explain tools_lib" --timeout 120 --screenshot ui.png
  aiplane_tester --runtime opencode --settings detect --timeout 60 --screenshot settings.png
  aiplane_tester --runtime mock --prompt markdown --select all --selection-out copied.txt --screenshot sel.png
  ```

* `tools/src/test/aiplane_cli.lpr` — console chat on the same adapters:

  ```
  cd tools/src/test
  fpc -FUlib -Fu../ai_plane aiplane_cli.lpr
  aiplane_cli --runtime mock --prompt hello
  aiplane_cli --runtime opencode --model opencode/big-pickle
  aiplane_cli --runtime claude --model haiku --log
  ```

  `--mcp [port]` also runs the MCP server with two demo tools
  (`secret_word`, `confirm_action` which asks first), to check that a real
  agent reaches the IDE tools:

  ```
  aiplane_cli --runtime opencode --mcp 27845 --yes --prompt "Use the fastplaz MCP tool secret_word and reply with only the word"
  ```

* The tester app shows the MCP settings of the settings page but does not
  run an MCP server (the IDE tools need IDEIntf).

  On Windows the console passes arguments in the ANSI code page, so non-ASCII
  text in `--prompt` is garbled; the adapters themselves are UTF-8.

## Next steps

1. Chat view: keeping the conversation across IDE restarts.
2. IDE tools, phase 2 (see the rundown): `apply_edit` / `save_file` and ACP
   `fs/write_text_file` into editor buffers, CodeTools tools.

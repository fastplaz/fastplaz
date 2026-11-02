# AI Plane

Chat window inside the Lazarus IDE (menu *FastPlaz → AI Plane*) that talks to
coding agents such as Claude Code, OpenCode and Pi. This folder holds the core,
which has no LCL or IDEIntf dependency, so the IDE package, a standalone tester
and the unit tests all share the same code.

Status: **phase 3** — core adapters, chat frame, LCL tester and the IDE
integration (menu *FastPlaz → AI Plane*, dockable window, *AI Plane* tab in
*Fast Code Configuration*).

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
| `aiplane_settingsframe.pas/.lfm` | `TAIPlaneSettingsFrame` (LCL): default runtime, command, arguments, model (*Detect* reads the runtime's list), effort and *Run Test* |
| `aiplane_ide.pas/.lfm` | IDEIntf: `AIPlaneWindow` (dockable, hosts the chat), menu handler, settings in `config.json` under `aiplane`, project directory as working directory, unsaved editor buffers served to the agent |
| `aiplane_chatframe.pas/.lfm` | `TAIChatFrame` (LCL): runtime/model/effort pickers, conversation, input (Enter sends, Shift+Enter new line, Esc stops), Send/Stop, protocol log, status line |

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

* *FastPlaz → AI Plane* opens the chat window; it docks like any IDE window.
* *FastPlaz → Fast Code Configuration → AI Plane* sets the default runtime,
  command, arguments, model and effort (`config.json`, key `aiplane`).
  Runtime, model and effort picked in the chat window become the new
  defaults too.
* The runtime starts in the active project's directory and restarts when
  another project is opened (checked when the window is activated).

## Runtimes

| Preset | Adapter | Command | Verified |
| --- | --- | --- | --- |
| `claude` | stream-json | `claude -p --input-format stream-json --output-format stream-json --verbose --include-partial-messages --permission-prompt-tool stdio` | yes (Claude Code 2.1.316), permission prompt not yet seen live |
| `opencode` | ACP | `opencode acp` | yes (OpenCode 2.1.4) |
| `pi` | Pi RPC | `pi --mode rpc` | yes (Pi 1.4.0): protocol and errors; answers need a provider login (`pi`, then `/login`) |
| `claude-acp` | ACP | `claude-agent-acp` (npm bridge, needs Node.js) | no |

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

  On Windows the console passes arguments in the ANSI code page, so non-ASCII
  text in `--prompt` is garbled; the adapters themselves are UTF-8.

## Next steps

1. Chat view: keeping the conversation across IDE restarts.
2. IDE tools for the agent: `fs/write_text_file` into editor buffers, an MCP
   server for "open file in the IDE" and similar actions.

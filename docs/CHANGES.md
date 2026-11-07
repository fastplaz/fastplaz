# Development Changes

## 20261107

New Feature/Library:
- tools: AI Agent Plane MCP server serves a FastPlaz reference for agents (controllers, routes, themes and template tags, `TSimpleModel`, `TJSONUtil`, `config.json`, pitfalls) as the resource `fastplaz://reference` and the tool `get_fastplaz_reference` (one section with `section`); the server instructions tell agents to read it instead of guessing the API. The text is `tools/templates/agent/fastplaz-reference.md`, editable without rebuilding the IDE
- tools: AI Agent Plane MCP tools `edit_text` (replace a text in the editor buffer, one undo step, optional save), `save_file`, `revert_file`, `search_project` (project files or a directory, unsaved changes included) and `find_unit` (where the compiler finds a unit, which packages contain it, whether the project requires them)
- tools: MCP tools carry annotations (`readOnlyHint`, `destructiveHint`, `idempotentHint`, `openWorldHint`); the server answers `resources/list`, `resources/read` and `resources/templates/list`
- docs: [MCP.md](MCP.md) describes the MCP server of AI Agent Plane and its 13 IDE tools; [AI_PLANE_TOOL_RUNDOWN.md](AI_PLANE_TOOL_RUNDOWN.md) and [AI_AGENT_by_FASTPLAZ.md](AI_AGENT_by_FASTPLAZ.md) brought in line with the code

## 20261106

New Feature/Library:
- tools: AI Agent Plane MCP tool `build_project` builds all of the active project, like Run > Build
- tools: AI Agent Plane MCP tool `clean_and_compile_project` works like Run > Clean up and Build: deletes compiled files (never source files), then builds; asks every time

## 20261105

Bug Fix (Linux/macOS, found by running the test suites on Linux):
- smtp_lib: writing to a socket the server already closed raised SIGPIPE and killed the process
- common: values set by `_SetEnv` (and so `LoadDotEnv`) were not visible to `GetEnvironmentVariable`
- datetime_helpers: `FromString('01/11/2026', 'dd/mm/yyyy')` returned `Now`, `/` and `:` in the pattern are now read literally
- fastplaz_app: `fpIsATTY` does not exist in `BaseUnix`, use `termio.IsATTY` (build failed on Linux)
- test: the library test program links `cthreads` on Unix; the test scripts pass `-B` to `instantfpc` so a changed unit is not run from a stale cached binary

## 20261103

New Feature/Library:
- standalone HTTP mode: one binary runs as CGI or as a long-lived HTTP server (`http://127.0.0.1:17845` by default). See [STANDALONE-roadmap.md](STANDALONE-roadmap.md) and [STANDALONE-deployment.md](STANDALONE-deployment.md)
  - mode from `--mode=cgi|http`, `GATEWAY_INTERFACE`, `systems/server/mode` in `config.json`, default HTTP
  - `fastplaz_app` replaces `fpcgi`; old `.lpr` files with `uses fpcgi` keep working as CGI
  - per-request state reset, `Redirect`/`Die` stop the request and not the process, static theme assets from an allow-list of extensions
  - graceful shutdown (Ctrl+C / SIGTERM), `max_requests`, database connections closed per request by default (`db_persistent`, `db_idle_timeout`)
  - startup banner `ShowStandaloneBanner`, replaceable through `OnStandaloneBanner`
  - wizards: Run Mode choice (HTTP / CGI) for Simple, API and Full Package Application; Cancel no longer creates a project
  - example: `examples/standalone`
- tools: AI Agent Plane, coding agents inside the Lazarus IDE (FastPlaz > Agent Plane)
  - runtimes: OpenCode, Claude Code, Pi, GitHub Copilot CLI and other ACP agents
  - dockable chat window: markdown answers, highlighted code blocks, images, permission cards, runtime/model/effort switch, model remembered per runtime
  - configuration tab "AI Harness" in FastPlaz > Configuration, with Run Test
  - Claude Code questions (`AskUserQuestion`) shown as a question card
  - open editors the agent changed are reloaded (`aiplane/auto_reload`, default on)
- tools: IDE tools for the agents over MCP (HTTP on 127.0.0.1, default port 17826): open a file, read the editor, project info, compile, compiler messages, and the FastPlaz generators `create_simple_controller`, `create_api_controller`, `create_model`. See [MCP.md](MCP.md)
- tools: scripts that rebuild the IDE, strip debug info and start it again (Windows `.bat`, Linux/macOS `.sh`)
- config_lib: `TMyConfig.GetString` / `SetString` (no "not inlined" notes from the Variant default property)

Bug Fix:
- standalone: a request was freed twice; the CORS object FPC 3.2 leaks per web module is freed
- tools: model generator gave `TUsergroupModel` for "user group"

## 20261102

New Feature/Library:
- model: `ExecScript`, `QueryExecScript`, `ExecSQLScript` run a multi-statement SQL script (SQLite and MySQL `ExecuteDirect` take one statement per call) in one transaction, rolled back on the first failure
- model: `SplitSQLStatements` splits a script on `;` outside quotes, comments, `$tag$` bodies and trigger `BEGIN..END` blocks
- tools: FastPlaz > Open Console/Terminal and editor popup Open Terminal from Here

Bug Fix:
- model: `Find(string)` quotes the key value
- regexpr: include file renamed `regexpr_compilers_lib.inc` (duplicate file warning with LazEdit when rebuilding the IDE)
- packages: `fastplaz_runtime_register.pas` moved to `tools/runtime` (unit path warning "belongs to package fastplaz_tools")
- project_lib: the generated `.lpr` no longer gives an implicit string conversion warning

## 20261101

Review and fix pass over `FastPlaz/src` and `tools/src`, with an FPCUnit suite for every library unit. Read [UPGRADE.md](UPGRADE.md) before upgrading: some changes alter how an application behaves.

Behaviour changes:
- session: timeout is in minutes (the 3 day default expired after 72 minutes); default directory `<TEMP>/sessions`
- model: a failed `Save`/`Delete`/`SoftDelete` returns False with `Message` instead of ending the request
- theme: `[flashmessages]` is HTML encoded, `[flashmessages html=1]` keeps markup
- common: `X-Forwarded-For`/`Client-IP` are only used with `systems/trust_proxy`
- recaptcha: v1 API removed, the widget is always v2
- integration: discontinued services marked deprecated (bmkg, clarifai, googleanalytics, kloudless, newrelic, office365, yandex, zomato)

New Feature/Library:
- model: `QuoteIdentifier`, `QuoteValue`, quoted table and alias names, `SoftDelete`
- theme: `escape` filter, rewritten `[if]`, nested `[foreach]`, per-session page cache
- session: `RegenerateID`, HttpOnly/SameSite/Secure cookie
- html: one CSRF token per form, constant time comparison
- mailer: built-in SMTP client (`smtp_lib`), Cc/Bcc, `HeloName`
- redis: IPv6, full RESP parsing, connect timeout
- tools: name sanitising in the wizards, Database Explorer port and prefix, Fast Code for OpenAI-compatible servers

Bug Fix:
- handler: code after `die()` no longer runs during initialisation, 405 on unsupported method, CORS with credentials, header line breaks stripped
- cms/app: compile again, exit after `Redirect`, SQL values quoted, CSRF checked
- integration: destructors run (30 classes were missing `override`), responses no longer shared between calls
- many leaks, double frees, XXE in rss_lib, endless loops in StripTags and the Nazief stemmer

## 20231222.1419


New Feature/Library:
- vocal/consonant determinator
- integration: dotid whois support
- helper: add and remove slash
- helper: quoted string
- theme: search include file from basepath

Bug Fix:
- session: change session lastused field to unixtime format
- fix markup converter
- parsing form data with regex
- datatojson: smallint & largeint support and unknown field type
- fix post header; fix wording function

## 20210608.1111

- handler: float to string format
- handler: whois handler for .id domain
- handler: OutputJson with json utils
- theme: foreach for json data
- filter: unix2date with format
- filter: number with format

## 20201218.1424

- Session with custom UniqueID 

## 20201206.0001

- Reveal File in Explorer/Finder

## 20201205.1405

- Tools: JSON Visualization (TreeView)
- FDE: Load custom DB Library
- Bug: release secondary db memory

## 20201203.0344

- Tools: Database Explorer
- Tools: Json Formatter & Validator
- Tools: Regex Tester
- config lib: set json data value

## 20201124.2145

- url routing with group name regex
- api test doc
- Add action file templates
- Add last_update files
- Action: updater
- Change version number

## 20201122.0923

- Add changes document
- Contact & login example
- Bug: ConditionalProcessor cannot executed in Foreach bug 
- Force rename theme "Simple"
- Fix error page; check db connection before saving
- Readme & environment
- Tambah informasi database-example yg lebih kompleks
- File upload example
- Form example & dashboard theme


## 20201121.0927

- DB: check db connection
- Fix error page
- Readme: add docs url
- Add [baseurl] to template
- Add global variabel: Environment
- Tambah informasi kompleks database-example 
- File upload example
- Dashboard theme template

## undocumented changes 

....

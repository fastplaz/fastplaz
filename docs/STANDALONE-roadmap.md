# Standalone HTTP Mode — Roadmap

Status: **phase 1 and 2 done** (2026-11-02).
Branch: `feature/standalone`. Example project: `examples/standalone`.

Goal: let a FastPlaz application run as a **long-lived standalone HTTP
server** in addition to the current CGI mode. The app can be opened directly
in a browser (e.g. `http://127.0.0.1:17845`) during development, and put behind
nginx/Apache as a reverse proxy in production:

```
development:  Browser --HTTP--> app (127.0.0.1:17845)
production:   Browser --HTTP(S)--> nginx/Apache --HTTP--> app (127.0.0.1:17845)
```

| Mode | FPC unit | nginx | Apache |
|---|---|---|---|
| CGI (current) | `custcgi` / `fpcgi` | `fastcgi` + fcgiwrap | `mod_cgi` |
| HTTP (new) | `custhttpapp` / `fphttpserver` | `proxy_pass http://127.0.0.1:17845` | `mod_proxy_http` |

FastCGI was considered and **dropped**: it cannot be opened directly in a
browser, and HTTP covers the reverse proxy use case equally well.

Both modes share `TCustomWebApplication`, so the protocol part is already in
FPC. The real work is removing FastPlaz's assumption that **one process = one
request**.

Must keep working on **Windows, macOS and Linux**. CGI mode must keep working
unchanged; consumer projects (Dashboard, Carik, SimpleAI, SimpleBOT) must not be
forced to migrate.

---

## 1. Current CGI coupling (`FastPlaz/src`)

| Issue | Count | Why it breaks in a long-lived process |
|---|---|---|
| `uses fpcgi` | 26 files | Mode is hard-wired; `fpcgi` creates its own `Application` in its initialization |
| `Application.Request` / `Application.Response` | ~90 | Only exist on `TCGIApplication`, not on `THTTPApplication` |
| `Application.Terminate` in `Redirect` / `Die` (`fastplaz_handler.pas`) | 4 | Would stop the whole server, not just the request |
| `GetEnvironmentVariable` (`HTTPS`, `SERVER_NAME`, `SCRIPT_NAME`, headers) | 29 | No CGI environment; values must come from `TRequest` |
| Per-request globals: `_GET`, `_POST`, `_SESSION`, `_REQUEST`, `_SERVER`, `AppData`, `ThemeUtil`, `SessionController`, `ModuleLoaded`, `FastPlasAppandler` | — | **Critical:** without a reset, state of user A leaks into user B |
| `InitializeFastPlaz` | — | Mixes once-per-process work (config, theme, DB) with per-request work (base URL, session) |
| Static files (theme CSS/JS/images) | — | Served by the web server in CGI mode; nobody serves them when the app is opened directly |
| Small memory leaks | ? | Harmless in CGI, accumulate in a process that runs for days |
| DB connection lifetime | — | Needs reconnect handling (e.g. MySQL "server has gone away") |

---

## 2. Mode selection (design)

One binary, mode chosen **at runtime** (no rebuild to switch between CGI and
HTTP).

Priority:

| # | Source | Example |
|---|---|---|
| 1 | Command line | `app --mode=http --port=17845 --address=127.0.0.1` |
| 2 | Auto-detect | `GATEWAY_INTERFACE` set → **CGI** (started by a web server) |
| 3 | `config.json` | `systems/server/*`, see below |
| 4 | Default | **HTTP** on `127.0.0.1:17845` (decided 2026-11-02) |

```json
"systems": {
  "server": {
    "mode": "http",
    "address": "127.0.0.1",
    "port": 17845,
    "static": true
  }
}
```

Defaults bind to `127.0.0.1`, not `0.0.0.0`, so a dev server is not exposed
to the network by accident.

### Wrapper unit `fastplaz_app`

`fpcgi` and `fphttpapp` both create `Application` in their initialization, so
the wrapper uses `custcgi` / `custhttpapp` and creates exactly one instance:

```pascal
function Application: TCustomWebApplication;  // lazy getter
begin
  if FApp = nil then
  begin
    if Assigned(CustomApplication) then        // old .lpr still `uses fpcgi`
      FApp := CustomApplication as TCustomWebApplication
    else
      case DetectMode of                       // args > GATEWAY_INTERFACE > config > default
        amCGI:  FApp := TCustomCGIApplication.Create(nil);
        amHTTP: FApp := TCustomHTTPApplication.Create(nil);  // + Port/Address
      end;
  end;
  Result := FApp;
end;
```

A function (not a variable) so unit initialization order does not matter and
existing `.lpr` files that still `uses fpcgi` keep working in CGI mode.
`Application.Title`, `.Initialize` and `.Run` keep the same syntax.

New project `.lpr`:

```pascal
uses fastplaz_app, fastplaz_handler, main;
begin
  Application.Title := 'HTTP Example';
  Application.Initialize;
  Application.Run;
end.
```

Optional define `FASTPLAZ_CGI_ONLY` leaves `custhttpapp` out for minimal CGI
binaries.

---

## 3. Phase 1 — Proof of concept (done 2026-11-02)

Single-threaded only (`Threaded := False`). As implemented:

1. **`fastplaz_app` wrapper unit** with mode selection (section 2); `uses
   fpcgi` replaced in the 26 units.
2. **Current request/response**: tracked by fastplaz_app's own handlers
   (`TFastPlazHTTPHandler`, and `TFastPlazCGIHandler` installed through
   `CGIWebHandlerClass`, so an old `.lpr` with `uses fpcgi` is covered too).
   A class helper gives `Application` the fpcgi style `Request`, `Response`
   and `EnvironmentVariable`, so the ~90 existing usages compile unchanged.
   `ServerVariable()` derives the CGI variables from the request.
3. **Redirect / Die**: `Application.Terminate` replaced by `StopRequest`
   (terminates in CGI only). No exception: `Die` still does not halt in
   either mode, so the behaviour is the same in both.
4. **Per-request lifecycle** (`OnBeginRequest` / `OnEndRequest` →
   `FastPlazBeginRequest` / `FastPlazEndRequest`): `AppData.initialized`
   is reset so `InitializeFastPlaz` runs per request, the session
   controller is recreated from the request cookie, ThemeUtil (with
   HTMLUtil and the tag callbacks) and the load-on-start modules are freed
   afterwards, controller modules are one-shot. `InitializeFastPlaz` was not
   split; that can still be done for speed in phase 2.
5. **Static files**: served by the HTTP handler before routing (not
   `fpwebfile`, whose module routing clashes with FastPlaz routes), asset
   extensions only, see `StaticFileForPath()`.
6. **Example project** in `examples/standalone` with README (nginx, Apache,
   CGI).

Verified: example at `http://127.0.0.1:17845` (theme assets, 304, redirect
keeps the server running, a second client gets its own session and no
flash message of the first), the same binary as CGI, Dashboard `test.bin`
under Apache unchanged, test suite (`test_fastplaz_app` added).

Findings for phase 2 (status in section 4):

- **memory grows about 1.5 KB per request** (1000 requests: 9.4 → 11.0 MB);
  the heaptrc audit is needed before production use — fixed (double free
  and FPC CORS leak, section 4)
- consumer units that `uses fpcgi` and call `Application.Request` directly
  get the idle fpcgi application in HTTP mode; they must switch to
  `fastplaz_app` (CGI keeps working either way)
- HTTPS is not detected behind a proxy yet — done with `trust_proxy`
- `.html` files under `public_html` are not served as static files (by design)

---

## 4. Phase 2 — Production ready (done 2026-11-02)

Deployment guide: [STANDALONE-deployment.md](STANDALONE-deployment.md).

| # | Item | Status |
|---|---|---|
| 1 | CGI variables from the request; HTTPS/host/port from `X-Forwarded-*` only with `trust_proxy` | done (`RequestServerVariable()`) |
| 2 | Memory audit with heaptrc | done: 0 unfreed blocks after 8 and 108 requests; two leaks fixed (see below) |
| 3 | Database: dropped connections, transactions across requests | done: closed after every request by default, `db_persistent` + `db_idle_timeout` |
| 4 | Config / theme caching | not needed: `config.json` is read once per process (restart to reload); templates are read per render as in CGI |
| 5 | Error isolation | done: unhandled exception → 500, next request served (example `/fail`) |
| 6 | Graceful shutdown | done: Ctrl+C / SIGTERM, `max_requests` |
| 7 | Unit tests | done for mode selection, request variables, proxy headers, static filter, DB end of request; state isolation between requests checked end to end with curl (no in-process server test yet) |
| 8 | IDE: generated projects | done: `.lpr`, module generators and the Simple template use `fastplaz_app`; no mode option needed, the binary picks CGI or HTTP at runtime |
| 9 | Deployment docs | done: nginx, Apache, systemd, launchd, NSSM |
| 10 | Consumer check | done in CGI mode: 46 projects (Dashboard dashboard/api, Carik telegram/carik) compile; Dashboard `test.bin` serves 200 under Apache |

Bugs found and fixed on the way:

- **request freed twice**: inside the handlers `EndRequest(...)` resolved to
  the inherited `TWebHandler.EndRequest`, which frees the request and the
  response; fpWeb then used and freed them again (CGI too), and
  fastplaz_app's own end-of-request code never ran (ThemeUtil leaked per
  request). Renamed to `RequestStarted` / `RequestFinished`.
- **FPC 3.2 leak**: `TCustomHTTPModule` creates a CORS object it never
  frees; `TMyCustomWebModule.Destroy` frees it (FPC < 3.3).
- `DataBaseInit` reset the working directory to the executable's directory
  and did not set `databaseActive` on an open connection.
- `http_lib` pulled the web application unit into the IDE package without
  using it.

Still open:

- consumer projects use `fpcgi` in 237 files (16 of them call
  `Application.Request/Response/EnvironmentVariable`); they run unchanged as
  CGI and need the `fpcgi` → `fastplaz_app` replacement before running in
  HTTP mode (one `sed`, see the deployment guide)
- an automated in-process test of state isolation between requests
- Dashboard in HTTP mode (after the replacement above)

---

## 5. Phase 3 — Scaling (optional)

**Recommended:** run several single-threaded processes on different ports and
balance them with an nginx `upstream` block. No framework change needed.

**Not recommended (for now):** multi-threaded mode. Every global listed in
section 1 would need to become a `threadvar` or move into a per-request
context object, which changes the public API used by consumer projects.
Estimated 3–6 extra weeks.

---

## 6. Risks

- **State leakage between users** if any global is missed: needs explicit
  tests, not just manual checks.
- **Third-party / consumer code** that calls `Application.Request`,
  `Application.Terminate` or `GetEnvironmentVariable` directly: keep
  compatible shims where possible and document the migration.
- **Static file serving** must not expose files outside the public directory
  (path traversal, `config.json`, `ztemp`).
- **Port already in use** / firewall prompts on Windows when binding.

---

## 7. Open decisions

- Default DB connection strategy in long-lived mode.
- Whether `Application.Request` shims are kept permanently or deprecated.

## 8. Estimate summary

| Phase | Effort |
|---|---|
| 1 — PoC | 3–5 days (done) |
| 2 — Production ready | 1–2 weeks (done) |
| 3 — Multi-process via nginx upstream | config only |
| 3 — Multi-threaded (not recommended) | 3–6 weeks |

# Unit Test

- [English](#english)
- [Bahasa Indonesia](#bahasa-indonesia)

---

## English

Unit tests use **FPCUnit** (part of the Free Pascal FCL) with the console test runner.
Each test project is a standalone console program, so it can be run without installing the FastPlaz package into the Lazarus IDE.

### Available test projects

| Project | Location | What is tested |
| --- | --- | --- |
| `tools_test` | `tools/src/test` | `regex_tester_lib` – logic behind the *Regex Tester* tool and the `REGEX_*` constants; `tools_lib` – name sanitising, template copy, `.htaccess`, model names, FastCode response helpers and the wizard run mode written to `config.json` (`systems/server/mode`), shared by the IDE wizards; `aiplane_*` (`tools/src/ai_plane`) – the Agent Plane core: `TACPAdapter` against the `mock_acp_agent` fake runtime (handshake, queued prompt, models from `configOptions`, file reads with line/limit and from the editor buffer, permission answered now/later/never, cancel, crash and restart, model/effort selection, UTF-8, new session), `AISplitCodeBlocks()` used by the chat view, the markdown parser `MDParse()`/`MDParseInline()` (headings, lists, quotes, fenced and unclosed code, tables, rules, bold/italic/strike/code/links with titles, images and linked images, nested brackets, snake_case and half-streamed markers kept as text), `TPiRpcAdapter` on Pi RPC lines (text/thinking deltas, tool execution, models and state, rejected prompt, agent_settled, errors, aborts, usage, confirm/select/input dialogs) and `TClaudeStreamAdapter` on captured Claude Code stream-json lines (text/thinking deltas, tool use and results, result, permission response, `AskUserQuestion` questions answered through `updatedInput.answers` or declined, command line arguments, `--mcp-config`), `AIParseQuestions()`/`AIQuestionsText()`, the IDE MCP server `TAIMcpServer` over real HTTP on a local port (token and `Origin` checks, sessions, `tools/list`, `tools/call`, unknown tool, tool exception, result truncation, permission allow once / for the session / deny / answered later, permission card text (tool summary, `name: value` arguments via `AIMcpArgsText()`), notifications, `GET` refused, parse error, chunked and pipelined requests, port fallback) and the `mcpServers` entry the ACP adapter sends |
| `fastplaz_test` | `FastPlaz/test` | `datetime_lib` – `DateTimeHuman()`, `DateTimeHumanAt()` and the date string parser `TryParseHumanDate()`<br>`redis_lib` – `TRedisLib`: configuration, RESP replies, SET/GET, timeout and reconnect (see [Redis tests](#redis-tests))<br>`json_lib` – `TJSONUtil`: loading, path access, typed values, arrays and items<br>`config_lib` – `TMyConfig`: `ValidateFile()`, `Value[]`, `GetObject()` and `SetDataValue()`<br>`common` – base64, `HTMLDecode()`, `StripHTML()`, `RandomString()`, `isIPAddress()` and string helpers<br>`math_lib` – `Fact()`, `Power()`, `Choose()` and `Stirling()` including invalid arguments<br>`http_lib` – multipart body (`HTTPMultipartEncode()`), `FormData[]`, `RequestBody` ownership, `Clear()` and `THTTPResponse`<br>`mailer_lib` – `FormatMailAddress()`, `SanitizeMailHeader()`, To/Cc/Bcc lists and `Send()` without mailer support<br>`logutil_lib` – `TLogUtil.Add()`: file creation, append, module name, skip, one line per entry and `Prefix`<br>`smtp_lib` – `TSMTPClient` against an in-process fake SMTP server (EHLO, AUTH PLAIN, RCPT, DATA, QUIT, error paths) and the message builders<br>`stemmingnazief_lib` – `TStemmingNazief`: dictionary loading, prefix/suffix/plural rules, numbers, non-standard words, termination on unknown words and JSON output<br>`html_lib` – `HTMLEncode()`, `StripTags()`, `MoreLess()`, the escaped form/link helpers and the `ReCaptcha()` v2 widget<br>`recaptcha_lib` – `TReCaptcha`: siteverify response parsing and the verification request (fields, token with `&`, errors) through an overridable transport<br>`rss_lib` – `TRSSReaderLib`: RSS 2.0, RSS 1.0 and Atom parsing, item filtering, image fallbacks, malformed input and failed fetches<br>`solr_lib` – `TSOLRModel`: URL building and encoding, result iteration, empty results, HTTP/Solr/JSON errors through an overridable request<br>`serialize_lib` – `serialize()`/`unserialize()`: PHP format, legacy FastPlaz format, every variant type, malformed and oversized input, round trips<br>`verbal_expressions_lib` – `TVerbalExpressions`: Digit/word/Tab/LineBreak, SomethingBut, escaping inside character classes, literal Replace, modifiers<br>`nlp_lib` – `TNLP.Similarity()`: bounded cosine score, empty input, stop words, list assignment<br>`image_lib` – `TImageLib`: PNG/JPEG loading, wrong content, resize (aspect, stretch, invalid sizes), saving, `AsStream` reuse<br>`language_lib` – `__()` and `LoadPOFile()`: exact matching, escapes, multi-line entries, fallback language, cache reload<br>`database_lib` – `TSimpleModel` against a temporary SQLite database: end of request in standalone mode (`DataBaseRequestFinished()` rolls back and closes, `DataBasePersistent`, idle timeout), field list, Save/Update/Delete/SoftDelete, Find (string key quoted), All, JSON output, query helpers, quoted table and alias names (`QuoteIdentifier()`), failed Save/Delete return False with `Message`, multi-statement scripts (`ExecScript()`, `QueryExecScript()`: one commit, rollback on the first failure) and `SplitSQLStatements()` (quotes, comments, MySQL hints, `$tag$` bodies, trigger `BEGIN..END`; runs without SQLite) (ignored when the SQLite library cannot be loaded, see `SQLITE3_LIBRARY`)<br>`session_controller` – `TSessionController`: cookie/session id, file storage round trips, value encoding, timeout in minutes, expiry wipe, end/restart, `RegenerateID()`, start again after a forced `EndSession()`; database storage against SQLite (parameterised SQL, untrusted `X-Forwarded-For`)<br>`theme_controller` – `TThemeUtil`: the `[if] [else] [/if]` processor (operators, nesting, multi-line HTML, quoted values, unknown variables, inside a JSON foreach), the escape/date filters, whitespace trimming, cache time, instance state and shared object ownership, `SafeTemplateName()`, nested `[foreach]` blocks and the dataset cursor reset, HTML-encoded `[flashmessages]`<br>`fastplaz_handler` – `SanitizeHeaderValue()`, `HeaderToEnvName()` and the request accessors created at startup<br>`fastplaz_app` – `CGINameToHeaderName()`, CGI variables and `Application` outside a request, the standalone static file filter `StaticFileForPath()` (asset extensions only, query string, percent encoding, `..`, backslash, drive/stream names, dot files, `ztemp/`, sibling directory with the same prefix), CGI variables from a request (`RequestServerVariable()`) and `X-Forwarded-Proto/-Host/-Port` honoured only with `trust_proxy`, mode selection order `SelectAppMode()` (command line, `GATEWAY_INTERFACE`, config, default HTTP), the startup banner `StandaloneBannerText()` (UTF-8 logo, URL, mode, colors only on request, all interfaces line)<br>`string_helpers`, `datetime_helpers`, `json_helpers`, `array_helpers`, `integer_helpers` – the type helpers (date parsing, diffs, unix time, JSON by name, delimited arrays)<br>`modvar_model`/`modvar_util` – `ModVar[]` against SQLite: set/get, update in place, quotes in names, NULL values, `QuoteValue()`<br>`telegram_integration`, `facebookmessenger_integration`, `line_integration` – update/webhook parsing (messages, callback queries, postbacks, groups), keyboard payloads built from a copy of the rows, per-instance payload handlers, `TLineTemplateMessage` JSON, calls without token/file return False<br>`whois_integration`, `graphql_integration`, `slack_integration`, `botframework_integration`, `elasticsearch_integration`, `openai_integration` – `ExtensionOf()`, escaped request bodies, `ServiceURL` slash, URL building, payload rebuilt per call; every integration class is created and freed (destructors run)<br>`security_util`, `grouppermission_model`, `user_model`/`user_util`, `group_model`/`group_util`, `permission_util` (src/cms) – salted hash round trip, password/e-mail rules, `TimingSafeEquals()`, `FixSecurityString()`, and against SQLite: add/lookup, quotes not injected, change password, activate/suspend/delete, groups and membership counter, permission levels incl. `ANY`, login/logout on the session; every cms and app controller is compiled |

### Prerequisites

* FPC / Free Pascal Compiler, version 3.2.2
* Lazarus (for `lazbuild`), optional when using `instantfpc`
* A Redis server for the `TRedisLibServerTest` suite, optional (see [Redis tests](#redis-tests))
* The SQLite client library (`sqlite3.dll` / `libsqlite3.so`) for the `TSimpleModelSQLiteTest` suite, optional: the run scripts look in `PATH` and a few known folders (HeidiSQL, `C:\sqlite`, the usual Linux/macOS library paths) and the suite is ignored when it is missing. Set `SQLITE3_LIBRARY` to the library file to use a specific one.

### Quick run with instantfpc

The fastest way: compile and run the tests in one command, without `lazbuild` and without the Lazarus IDE.
`instantfpc` is shipped with FPC (Windows: `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\instantfpc.exe`).

Windows (can be called from any directory):

```bat
tools\src\test\run_tests.bat
FastPlaz\test\run_tests.bat
```

Linux / macOS:

```bash
sh tools/src/test/run_tests.sh
sh FastPlaz/test/run_tests.sh
```

Without arguments all tests are run (`-a`). Runner options can be passed directly, for example:

```bat
tools\src\test\run_tests.bat --suite=TestEmailRegex
tools\src\test\run_tests.bat --format=xml --file=result.xml
```

Notes:

* The scripts set the unit search paths through the `INSTANTFPCOPTIONS` environment variable. `instantfpc` 1.3 does not handle `-Fu` with a path separator on its own command line (the option is passed to the test program and fails with `Invalid option ... "F"`).
* `-B` is always used, because the `instantfpc` cache only checks the main program (`.lpr`); without it changes in other units would not be recompiled.
* The compiled program is stored in the `instantfpc` cache (`instantfpc --get-cache`), not in the repository.
* On Windows, if `instantfpc` is not in `PATH`, `run_tests.bat` uses `C:\lazarus\fpc\3.2.2\bin\x86_64-win64`.
* `FastPlaz/test/run_tests.*` adds the unit paths of the `fastplaz_runtime` package, because `datetime_lib` depends on `language_lib` and `common`.
* `tools/src/test/run_tests.*` adds `FastPlaz/test` to the unit path for the shared `fastplaz_testrunner` unit.
* `tools/src/test/run_tests.*` first compiles `mock_acp_agent.lpr` with `fpc`, the fake ACP runtime used by the Agent Plane tests. When it is missing those tests are ignored; `AIPLANE_MOCK_AGENT` can point at another build.

### Redis tests

`test_redis_lib` has two suites:

* `TRedisLibOfflineTest` needs no server: defaults, port validation, `BulkPayload()`, master configuration, connection refused, connect timeout, IPv6 literal forms, unreachable IPv6 address, unresolvable host, deprecated `Synch`.
* `TRedisLibServerTest` talks to a real Redis server: SET/GET, special characters, code page tagged values, large values, INFO, numeric and multi bulk replies, error replies, inline commands, read timeout and reconnect, host name lookup, IPv6 loopback (`::1`, ignored when the server is IPv4 only), write to master.

The server defaults to `127.0.0.1:6379`. When it is not reachable the whole `TRedisLibServerTest` suite is reported as *ignored*, not as failed, so the run still passes. Another server can be set through environment variables:

| Variable | Default | Description |
| --- | --- | --- |
| `FASTPLAZ_TEST_REDIS_HOST` | `127.0.0.1` | IP address or host name |
| `FASTPLAZ_TEST_REDIS_PORT` | `6379` | port |
| `FASTPLAZ_TEST_REDIS_PASSWORD` | *(empty)* | sent with `AUTH` when set |

Every key written by the tests starts with `fastplaz:test:` and is deleted in `TearDown`; the database is never flushed. Run only the Redis tests with `--suite=TRedisLibServerTest`.

### Build

Windows:

```bat
cd tools\src\test
C:\lazarus\lazbuild.exe tools_test.lpi
```

Linux / macOS (`lazbuild` in `PATH`):

```bash
cd tools/src/test
lazbuild tools_test.lpi
```

The same applies to `FastPlaz/test/fastplaz_test.lpi`. That project requires the `fastplaz_runtime` package (`tools/fastplaz_runtime.lpk`), which has to be known to Lazarus / `lazbuild`; register it once with `lazbuild --add-package-link tools/fastplaz_runtime.lpk` if it is not.

Compiled units go to `lib/<cpu>-<os>/`. Build output is ignored by `.gitignore`.

### Run

Windows:

```bat
tools_test.exe -a
```

Linux / macOS:

```bash
./tools_test -a
```

Both projects use `TFastPlazTestRunner` (`FastPlaz/test/fastplaz_testrunner.pas`), a thin extension of the FPCUnit console runner that prints every suite and every test the moment it starts, followed by its result and duration. A long run therefore shows what is going on instead of staying silent until the end. Example output:

```
FastPlaz Tools - Regex Tester Test

TRegexTesterLibTest (16 tests)
  [ 1/16] TestEmptyExpression ............................ OK (0 ms)
  [ 2/16] TestEmptyText .................................. OK (0 ms)
  ...
  [16/16] TestIpRegexInvalid ............................. OK (0 ms)

------------------------------------------------------------------------
PASSED  tests: 16  passed: 16  failed: 0  errors: 0  ignored: 0  time: 0 ms
```

A failing test is marked `FAILED` (assertion) or `ERROR` (exception) with the message right below it, an `Ignore()` call shows up as `IGNORED`, and all problems are listed again in the summary. Results are colored when stdout is a terminal (set `NO_COLOR` or pass `--no-color` to turn that off); when the output is redirected to a file the same text is written without escape codes.

The program exits with code `0` when all tests pass and a non-zero code when a test fails, so it can be used in scripts or CI.

### Runner options

| Option | Description |
| --- | --- |
| `-a`, `--all` | run all tests |
| `-l`, `--list` | list registered tests |
| `--suite=NAME` | run a single test class or test method, e.g. `--suite=TestEmailRegex` |
| `--format=FMT` | output format: `plain` (default for this project), `plainnotiming`, `xml`, `latex` |
| `--sparse` | show only errors and failures (plain FPCUnit report only) |
| `--file=FILE` | write the result to a file, e.g. `--format=xml --file=result.xml`; the live progress still goes to the console |
| `--no-live` | plain FPCUnit output: nothing is printed until the run has finished |
| `--no-color` | no ANSI colors in the live output |
| `-h` | show all options |

The live output is switched off automatically when a machine readable format (`xml`, `latex`) is written to stdout, so `--format=xml > result.xml` stays valid XML.

### Running from the Lazarus IDE

1. Open `tools/src/test/tools_test.lpi` or `FastPlaz/test/fastplaz_test.lpi` (*Project → Open Project*).
2. Set the parameter in *Run → Run Parameters* → *Command line parameters*: `-a`.
3. Press **F9**.

### Adding a new test

1. Put the logic to be tested in a unit **without** dependencies on LCL forms or the IDE (`IDEIntf`, `PackageIntf`, …), for example `xxx_lib.pas`.
2. Create a test unit next to the matching test project (`tools/src/test` for the Lazarus tools, `FastPlaz/test` for the framework library), e.g. `test_xxx_lib.pas`:

   ```pascal
   unit test_xxx_lib;

   {$mode objfpc}{$H+}

   interface

   uses
     xxx_lib, Classes, SysUtils, fpcunit, testregistry;

   type
     TXxxLibTest = class(TTestCase)
     published
       procedure TestSomething;
     end;

   implementation

   procedure TXxxLibTest.TestSomething;
   begin
     AssertEquals(2, 1 + 1);
   end;

   initialization
     RegisterTest(TXxxLibTest);

   end.
   ```

3. Add the unit to the `uses` clause of the test program (`.lpr`) and to the project (`.lpi`).
4. Build and run again.

---

## Bahasa Indonesia

Unit test menggunakan **FPCUnit** (bagian dari FCL Free Pascal) dengan *console test runner*.
Setiap project test adalah program console mandiri, sehingga bisa dijalankan tanpa perlu meng-install package FastPlaz ke Lazarus IDE.

### Project test yang tersedia

| Project | Lokasi | Yang diuji |
| --- | --- | --- |
| `tools_test` | `tools/src/test` | `regex_tester_lib` – logika di balik tool *Regex Tester* dan konstanta `REGEX_*`; `tools_lib` – sanitasi nama, salin template, `.htaccess`, nama model, helper respons FastCode dan run mode wizard yang ditulis ke `config.json` (`systems/server/mode`), dipakai wizard IDE; `aiplane_*` (`tools/src/ai_plane`) – core Agent Plane: `TACPAdapter` terhadap runtime palsu `mock_acp_agent` (handshake, prompt yang diantrekan, daftar model dari `configOptions`, baca file dengan line/limit dan dari buffer editor, izin dijawab langsung/nanti/tidak dijawab, cancel, crash lalu restart, pilihan model/effort, UTF-8, sesi baru), `AISplitCodeBlocks()` yang dipakai tampilan chat, parser markdown `MDParse()`/`MDParseInline()` (heading, list, quote, kode berpagar dan yang belum ditutup, tabel, garis, bold/italic/strike/code/link dengan judul, gambar dan gambar ber-link, kurung bersarang, snake_case dan penanda yang belum lengkap saat streaming tetap jadi teks), `TPiRpcAdapter` dengan baris RPC Pi (delta teks/thinking, eksekusi tool, model dan state, prompt ditolak, agent_settled, error, abort, usage, dialog confirm/select/input) dan `TClaudeStreamAdapter` dengan baris stream-json Claude Code hasil rekaman (delta teks/thinking, tool use dan hasilnya, result, jawaban izin, argumen command line, `--mcp-config`), server MCP IDE `TAIMcpServer` lewat HTTP sungguhan di port lokal (cek token dan `Origin`, sesi, `tools/list`, `tools/call`, tool tidak dikenal, tool yang raise exception, pemotongan hasil, izin sekali / untuk sesi / ditolak / dijawab belakangan, notifikasi, `GET` ditolak, parse error, request chunked dan pipelined, pindah port) serta entri `mcpServers` yang dikirim adapter ACP |
| `fastplaz_test` | `FastPlaz/test` | `datetime_lib` – `DateTimeHuman()`, `DateTimeHumanAt()` dan parser string tanggal `TryParseHumanDate()`<br>`redis_lib` – `TRedisLib`: konfigurasi, balasan RESP, SET/GET, timeout dan reconnect (lihat [Test Redis](#test-redis))<br>`json_lib` – `TJSONUtil`: load, akses path, nilai bertipe, array dan item<br>`config_lib` – `TMyConfig`: `ValidateFile()`, `Value[]`, `GetObject()` dan `SetDataValue()`<br>`common` – base64, `HTMLDecode()`, `StripHTML()`, `RandomString()`, `isIPAddress()` dan helper string<br>`math_lib` – `Fact()`, `Power()`, `Choose()` dan `Stirling()` termasuk argumen tidak valid<br>`http_lib` – body multipart (`HTTPMultipartEncode()`), `FormData[]`, kepemilikan `RequestBody`, `Clear()` dan `THTTPResponse`<br>`mailer_lib` – `FormatMailAddress()`, `SanitizeMailHeader()`, daftar To/Cc/Bcc dan `Send()` tanpa dukungan mailer<br>`logutil_lib` – `TLogUtil.Add()`: pembuatan file, append, nama modul, skip, satu baris per entri dan `Prefix`<br>`smtp_lib` – `TSMTPClient` terhadap server SMTP palsu dalam proses (EHLO, AUTH PLAIN, RCPT, DATA, QUIT, jalur error) dan pembangun pesan<br>`stemmingnazief_lib` – `TStemmingNazief`: pemuatan kamus, aturan prefiks/sufiks/jamak, angka, kata tidak baku, terminasi pada kata tak dikenal dan keluaran JSON<br>`html_lib` – `HTMLEncode()`, `StripTags()`, `MoreLess()`, helper form/link yang di-escape dan widget `ReCaptcha()` v2<br>`recaptcha_lib` – `TReCaptcha`: parsing respons siteverify dan permintaan verifikasi (field, token berisi `&`, error) lewat transport yang bisa di-override<br>`rss_lib` – `TRSSReaderLib`: parsing RSS 2.0, RSS 1.0 dan Atom, penyaringan item, fallback gambar, input rusak dan fetch gagal<br>`solr_lib` – `TSOLRModel`: pembentukan dan encoding URL, iterasi hasil, hasil kosong, error HTTP/Solr/JSON lewat request yang bisa di-override<br>`serialize_lib` – `serialize()`/`unserialize()`: format PHP, format lama FastPlaz, semua tipe variant, input rusak dan terlalu besar, round trip<br>`verbal_expressions_lib` – `TVerbalExpressions`: Digit/word/Tab/LineBreak, SomethingBut, escaping di dalam character class, Replace literal, modifier<br>`nlp_lib` – `TNLP.Similarity()`: skor cosine terbatas 0..1, input kosong, stop word, assignment list<br>`image_lib` – `TImageLib`: pemuatan PNG/JPEG, isi salah, resize (aspek, stretch, ukuran tidak valid), penyimpanan, pemakaian ulang `AsStream`<br>`language_lib` – `__()` dan `LoadPOFile()`: pencocokan eksak, escape, entri multi-baris, bahasa fallback, reload cache<br>`database_lib` – `TSimpleModel` terhadap database SQLite sementara: akhir request di mode standalone (`DataBaseRequestFinished()` rollback dan tutup koneksi, `DataBasePersistent`, idle timeout), daftar field, Save/Update/Delete/SoftDelete, Find (key string dikutip), All, keluaran JSON, helper query, nama tabel dan alias dikutip (`QuoteIdentifier()`), Save/Delete yang gagal mengembalikan False dengan `Message`, skrip multi-statement (`ExecScript()`, `QueryExecScript()`: satu commit, rollback pada kegagalan pertama) dan `SplitSQLStatements()` (kutip, komentar, hint MySQL, body `$tag$`, `BEGIN..END` trigger; berjalan tanpa SQLite) (diabaikan bila library SQLite tidak bisa dimuat, lihat `SQLITE3_LIBRARY`)<br>`session_controller` – `TSessionController`: cookie/id sesi, simpan-baca di file, encoding nilai, timeout dalam menit, penghapusan saat kedaluwarsa, end/restart, `RegenerateID()`, start ulang setelah `EndSession()` paksa; storage database dengan SQLite (SQL berparameter, `X-Forwarded-For` tidak dipercaya)<br>`theme_controller` – `TThemeUtil`: prosesor `[if] [else] [/if]` (operator, nested, HTML multi-baris, nilai berkutip, variabel tak dikenal, di dalam foreach JSON), filter escape/date, pemangkasan spasi, cache time, state per instance dan kepemilikan objek bersama, `SafeTemplateName()`, blok `[foreach]` bersarang dan reset kursor dataset, `[flashmessages]` di-HTML-encode<br>`fastplaz_handler` – `SanitizeHeaderValue()`, `HeaderToEnvName()` dan accessor request yang dibuat saat startup<br>`fastplaz_app` – `CGINameToHeaderName()`, variabel CGI dan `Application` di luar request, filter file statis mode standalone `StaticFileForPath()` (hanya ekstensi aset, query string, percent encoding, `..`, backslash, nama drive/stream, file berawalan titik, `ztemp/`, direktori sebelah dengan prefix sama), variabel CGI dari request (`RequestServerVariable()`) dan `X-Forwarded-Proto/-Host/-Port` yang hanya dipakai bila `trust_proxy` aktif, urutan pemilihan mode `SelectAppMode()` (command line, `GATEWAY_INTERFACE`, config, default HTTP), banner startup `StandaloneBannerText()` (logo UTF-8, URL, mode, warna hanya bila diminta, baris semua interface)<br>`string_helpers`, `datetime_helpers`, `json_helpers`, `array_helpers`, `integer_helpers` – type helper (parsing tanggal, selisih, unix time, JSON per nama, array dari string)<br>`modvar_model`/`modvar_util` – `ModVar[]` dengan SQLite: set/get, update di tempat, kutip pada nama, nilai NULL, `QuoteValue()`<br>`telegram_integration`, `facebookmessenger_integration`, `line_integration` – parsing update/webhook (pesan, callback query, postback, grup), payload keyboard dibuat dari salinan baris, handler payload per instance, JSON `TLineTemplateMessage`, pemanggilan tanpa token/file mengembalikan False<br>`whois_integration`, `graphql_integration`, `slack_integration`, `botframework_integration`, `elasticsearch_integration`, `openai_integration` – `ExtensionOf()`, body request yang di-escape, slash `ServiceURL`, pembentukan URL, payload dibangun ulang tiap panggilan; semua kelas integrasi dibuat dan dibebaskan (destructor berjalan)<br>`security_util`, `grouppermission_model`, `user_model`/`user_util`, `group_model`/`group_util`, `permission_util` (src/cms) – round trip salted hash, aturan password/e-mail, `TimingSafeEquals()`, `FixSecurityString()`, dan dengan SQLite: tambah/cari, kutip tidak di-inject, ganti password, aktivasi/suspend/hapus, grup dan penghitung anggota, level izin termasuk `ANY`, login/logout pada sesi; semua controller cms dan app ikut dikompilasi |

### Prasyarat

* FPC / Free Pascal Compiler, versi 3.2.2
* Lazarus (untuk `lazbuild`), opsional jika memakai `instantfpc`
* Server Redis untuk suite `TRedisLibServerTest`, opsional (lihat [Test Redis](#test-redis))
* Library klien SQLite (`sqlite3.dll` / `libsqlite3.so`) untuk suite `TSimpleModelSQLiteTest`, opsional: skrip run mencarinya di `PATH` dan beberapa folder umum (HeidiSQL, `C:\sqlite`, path library Linux/macOS yang lazim) dan suite ini di-ignore bila tidak ada. Set `SQLITE3_LIBRARY` ke file library untuk memakai yang spesifik.

### Jalankan cepat dengan instantfpc

Cara tercepat: compile dan jalankan test dalam satu perintah, tanpa `lazbuild` dan tanpa Lazarus IDE.
`instantfpc` sudah disertakan bersama FPC (Windows: `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\instantfpc.exe`).

Windows (bisa dipanggil dari direktori mana saja):

```bat
tools\src\test\run_tests.bat
FastPlaz\test\run_tests.bat
```

Linux / macOS:

```bash
sh tools/src/test/run_tests.sh
sh FastPlaz/test/run_tests.sh
```

Tanpa argumen, semua test dijalankan (`-a`). Opsi runner bisa langsung diberikan, contoh:

```bat
tools\src\test\run_tests.bat --suite=TestEmailRegex
tools\src\test\run_tests.bat --format=xml --file=result.xml
```

Catatan:

* Script mengatur path pencarian unit lewat variabel lingkungan `INSTANTFPCOPTIONS`. `instantfpc` 1.3 tidak bisa menangani `-Fu` yang berisi pemisah path di command line-nya (opsi tersebut malah diteruskan ke program test dan gagal dengan `Invalid option ... "F"`).
* `-B` selalu dipakai, karena cache `instantfpc` hanya memeriksa program utama (`.lpr`); tanpa `-B` perubahan di unit lain tidak akan di-compile ulang.
* Program hasil compile disimpan di cache `instantfpc` (`instantfpc --get-cache`), bukan di repositori.
* Di Windows, jika `instantfpc` tidak ada di `PATH`, `run_tests.bat` memakai `C:\lazarus\fpc\3.2.2\bin\x86_64-win64`.
* `FastPlaz/test/run_tests.*` menambahkan path unit dari package `fastplaz_runtime`, karena `datetime_lib` bergantung pada `language_lib` dan `common`.
* `tools/src/test/run_tests.*` menambahkan `FastPlaz/test` ke path unit untuk unit bersama `fastplaz_testrunner`.
* `tools/src/test/run_tests.*` lebih dulu meng-compile `mock_acp_agent.lpr` dengan `fpc`, runtime ACP palsu untuk test Agent Plane. Bila tidak ada, test tersebut di-ignore; `AIPLANE_MOCK_AGENT` bisa menunjuk ke build lain.

### Test Redis

`test_redis_lib` berisi dua suite:

* `TRedisLibOfflineTest` tidak butuh server: nilai default, validasi port, `BulkPayload()`, konfigurasi master, koneksi ditolak, timeout connect, bentuk literal IPv6, alamat IPv6 tak terjangkau, host tidak dikenal, `Synch` yang sudah deprecated.
* `TRedisLibServerTest` memakai server Redis sungguhan: SET/GET, karakter khusus, nilai ber-codepage, nilai besar, INFO, balasan numerik dan multi bulk, balasan error, perintah inline, timeout baca dan reconnect, lookup nama host, loopback IPv6 (`::1`, di-ignore jika server hanya IPv4), tulis ke master.

Server default `127.0.0.1:6379`. Jika tidak bisa dihubungi, seluruh suite `TRedisLibServerTest` dilaporkan sebagai *ignored*, bukan gagal, sehingga run tetap lulus. Server lain bisa diatur lewat variabel lingkungan:

| Variabel | Default | Keterangan |
| --- | --- | --- |
| `FASTPLAZ_TEST_REDIS_HOST` | `127.0.0.1` | alamat IP atau nama host |
| `FASTPLAZ_TEST_REDIS_PORT` | `6379` | port |
| `FASTPLAZ_TEST_REDIS_PASSWORD` | *(kosong)* | dikirim dengan `AUTH` jika diisi |

Semua key yang ditulis test diawali `fastplaz:test:` dan dihapus di `TearDown`; database tidak pernah di-flush. Untuk menjalankan test Redis saja: `--suite=TRedisLibServerTest`.

### Build

Windows:

```bat
cd tools\src\test
C:\lazarus\lazbuild.exe tools_test.lpi
```

Linux / macOS (`lazbuild` ada di `PATH`):

```bash
cd tools/src/test
lazbuild tools_test.lpi
```

Hal yang sama berlaku untuk `FastPlaz/test/fastplaz_test.lpi`. Project itu membutuhkan package `fastplaz_runtime` (`tools/fastplaz_runtime.lpk`) yang harus sudah dikenal oleh Lazarus / `lazbuild`; jika belum, daftarkan sekali dengan `lazbuild --add-package-link tools/fastplaz_runtime.lpk`.

Hasil kompilasi unit disimpan di `lib/<cpu>-<os>/`. Hasil build sudah diabaikan oleh `.gitignore`.

### Menjalankan

Windows:

```bat
tools_test.exe -a
```

Linux / macOS:

```bash
./tools_test -a
```

Kedua project memakai `TFastPlazTestRunner` (`FastPlaz/test/fastplaz_testrunner.pas`), turunan tipis dari console runner FPCUnit yang mencetak setiap suite dan setiap test begitu mulai dijalankan, diikuti hasil dan durasinya. Jadi saat test berjalan lama, terlihat test mana yang sedang berjalan, bukan diam sampai selesai. Contoh output:

```
FastPlaz Tools - Regex Tester Test

TRegexTesterLibTest (16 tests)
  [ 1/16] TestEmptyExpression ............................ OK (0 ms)
  [ 2/16] TestEmptyText .................................. OK (0 ms)
  ...
  [16/16] TestIpRegexInvalid ............................. OK (0 ms)

------------------------------------------------------------------------
PASSED  tests: 16  passed: 16  failed: 0  errors: 0  ignored: 0  time: 0 ms
```

Test yang gagal ditandai `FAILED` (assertion) atau `ERROR` (exception) dengan pesannya tepat di bawahnya, pemanggilan `Ignore()` tampil sebagai `IGNORED`, dan semua masalah diulang lagi di ringkasan akhir. Hasil diberi warna bila stdout adalah terminal (set `NO_COLOR` atau beri `--no-color` untuk mematikannya); bila output dialihkan ke file, teks yang sama ditulis tanpa kode escape.

Program keluar dengan kode `0` jika semua test lulus, dan kode selain `0` jika ada test yang gagal, sehingga bisa dipakai di script atau CI.

### Opsi runner

| Opsi | Keterangan |
| --- | --- |
| `-a`, `--all` | jalankan semua test |
| `-l`, `--list` | tampilkan daftar test yang terdaftar |
| `--suite=NAMA` | jalankan satu class test atau satu method test saja, contoh `--suite=TestEmailRegex` |
| `--format=FMT` | format output: `plain` (default di project ini), `plainnotiming`, `xml`, `latex` |
| `--sparse` | hanya tampilkan error dan failure (hanya untuk laporan plain FPCUnit) |
| `--file=FILE` | simpan hasil ke file, contoh `--format=xml --file=result.xml`; progres live tetap tampil di console |
| `--no-live` | output plain FPCUnit: tidak ada yang dicetak sampai seluruh test selesai |
| `--no-color` | tanpa warna ANSI pada output live |
| `-h` | tampilkan semua opsi |

Output live otomatis dimatikan bila format yang bisa dibaca mesin (`xml`, `latex`) ditulis ke stdout, sehingga `--format=xml > result.xml` tetap menghasilkan XML yang valid.

### Menjalankan dari Lazarus IDE

1. Buka `tools/src/test/tools_test.lpi` atau `FastPlaz/test/fastplaz_test.lpi` (*Project → Open Project*).
2. Isi parameter di *Run → Run Parameters* → *Command line parameters*: `-a`.
3. Tekan **F9**.

### Menambah test baru

1. Letakkan logika yang akan diuji di unit yang **tidak** bergantung pada form LCL maupun IDE (`IDEIntf`, `PackageIntf`, …), misalnya `xxx_lib.pas`.
2. Buat unit test di samping project test yang sesuai (`tools/src/test` untuk tools Lazarus, `FastPlaz/test` untuk library framework), misalnya `test_xxx_lib.pas` (lihat contoh kode di bagian [English](#adding-a-new-test)).
3. Tambahkan unit tersebut ke `uses` program test (`.lpr`) dan ke project (`.lpi`).
4. Build dan jalankan ulang.

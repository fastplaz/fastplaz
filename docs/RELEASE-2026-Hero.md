# FastPlaz Rilis 2026 "Hero" — Brief Publikasi

Ringkasan semua fitur, perbaikan, dan peningkatan keamanan sejak branch `refactor` dimulai (30 Oktober 2026) sampai 7 November 2026.

Skala perubahan: sekitar 170 commit, 365 file, +51.000 / −11.900 baris. Rincian teknis ada di [CHANGES.md](CHANGES.md); hal yang perlu dicek sebelum upgrade ada di [UPGRADE.md](UPGRADE.md).

---

## Headline

1. **Satu binary, dua mode: Standalone HTTP dan CGI.** Aplikasi FastPlaz bisa jalan sebagai web server sendiri atau tetap sebagai CGI di Apache/Nginx, tanpa ganti kode.
2. **AI Agent Plane.** Coding agent (Claude Code, OpenCode, Pi, GitHub Copilot CLI, agent ACP lain) langsung di dalam Lazarus IDE, lengkap dengan MCP server yang bisa membuka file, compile, dan generate controller/model FastPlaz.
3. **Review dan perbaikan menyeluruh.** Semua unit di `FastPlaz/src` dan `tools/src` direview: memory leak, double free, celah keamanan, dan bug lama diperbaiki, dengan unit test untuk setiap library.
4. **Lebih aman secara default.** CSRF per form, cookie session HttpOnly/SameSite/Secure, CSPRNG, perlindungan XXE, SQL yang di-quote, header injection ditutup.
5. **Lintas platform yang benar-benar dites.** Test suite dijalankan di Windows dan Linux, bug khusus Linux/macOS ikut diperbaiki.

---

## 1. Fitur Baru

### Standalone HTTP mode (auto mode: HTTP / CGI)
- Unit baru `fastplaz_app` menggantikan `fpcgi`. Mode dipilih otomatis: `--mode=cgi|http`, variabel `GATEWAY_INTERFACE`, atau `systems/server/mode` di `config.json`. Default HTTP.
- Project lama dengan `uses fpcgi` tetap jalan sebagai CGI.
- Default `http://127.0.0.1:17845`, bisa diubah dengan `--port=` atau `systems/server/port`.
- State direset tiap request; `Redirect`/`Die` menghentikan request, bukan proses.
- Static asset tema dilayani langsung (allow-list ekstensi).
- Graceful shutdown (Ctrl+C / SIGTERM), `max_requests`, koneksi database per request (`db_persistent`, `db_idle_timeout`).
- Dukungan `X-Forwarded-*` di belakang reverse proxy terpercaya.
- Startup banner dengan logo FASTPLAZ dan URL lokal (bisa diganti lewat `OnStandaloneBanner`).
- Wizard project (Simple, API, Full Package) punya pilihan Run Mode HTTP/CGI.
- Contoh: `examples/standalone`. Panduan: [STANDALONE-deployment.md](STANDALONE-deployment.md).

### AI Agent Plane (FastPlaz > Agent Plane)
- Coding agent di dalam Lazarus IDE: OpenCode, Claude Code, Pi (mode RPC native), GitHub Copilot CLI, dan agent ACP lain.
- Jendela chat yang bisa di-dock: jawaban markdown, code block dengan syntax highlighting, gambar, teks bisa dipilih dan disalin.
- Permission card yang mudah dibaca; pertanyaan Claude Code (`AskUserQuestion`) tampil sebagai kartu pertanyaan.
- Ganti runtime, model, dan effort langsung dari chat; model diingat per runtime.
- Editor yang diubah agent di-reload otomatis (`aiplane/auto_reload`).
- Tab konfigurasi "AI Harness" dengan tombol Run Test (cek runtime dan MCP).
- **MCP server** (HTTP di 127.0.0.1, port 17826) dengan 19 tool dalam tiga kelompok:
  - IDE: buka file, baca editor, info project, compile, build, clean & build, pesan compiler.
  - Editor/pencarian/unit: `edit_text` (satu langkah undo), `save_file`, `revert_file`, `search_project`, `find_unit`.
  - FastPlaz: `create_simple_controller`, `create_api_controller`, `create_model`, `get_fastplaz_reference`.
- Referensi FastPlaz untuk agent (`fastplaz://reference`): controller, route, tema dan tag template, `TSimpleModel`, `TJSONUtil`, `config.json`, jebakan umum. Agent diarahkan membaca referensi ini, bukan menebak API.
- Tool MCP punya annotation (`readOnlyHint`, `destructiveHint`, dll.) dan server mendukung `resources/*`.
- Dokumentasi: [MCP.md](MCP.md), [AI_AGENT_by_FASTPLAZ.md](AI_AGENT_by_FASTPLAZ.md).

### Library
- **SMTP client bawaan** (`smtp_lib`): `TMailer` bisa kirim email tanpa Synapse; Cc/Bcc, `HeloName`.
- **Database**: `ExecScript` / `ExecSQLScript` menjalankan SQL multi-statement dalam satu transaksi (rollback saat gagal), `SplitSQLStatements`, `QuoteIdentifier`, `QuoteValue`, `SoftDelete` yang benar-benar berfungsi.
- **Tema**: filter `escape`, `[if]` ditulis ulang (operator perbandingan, nesting, HTML multi-baris), `[foreach]` bersarang, page cache per session.
- **Session**: `RegenerateID`, cookie HttpOnly/SameSite/Secure.
- **Redis**: IPv6, parsing RESP lengkap, connect timeout, I/O socket ditulis ulang.
- **Config**: `TMyConfig.GetString` / `SetString`.
- **Serialize**: parser ditulis ulang, bisa menulis format PHP.
- **Language**: file `.po` di-parse dengan benar dan di-cache.

### IDE Tools
- Fast Code mendukung server OpenAI-compatible (base URL, daftar model, tes koneksi async, API key tersamar).
- Database Explorer: default MySQL 8.0, toolbar ikon, lebar kolom otomatis, port dan prefix tabel tersimpan.
- FastPlaz > Open Console/Terminal dan "Open Terminal from Here" di popup editor.
- Shortcut keyboard untuk menu FastPlaz.
- Nama modul/model/project otomatis jadi identifier Pascal yang valid (`my-app` → `myapp`, `2fa` → `_2fa`).
- Terjemahan Bahasa Indonesia untuk IDE tools.
- Script rebuild IDE untuk Windows, Linux, dan macOS (`scripts/`).

---

## 2. Keamanan

- **CSRF**: satu token per form (dua form di satu halaman tidak saling membatalkan), perbandingan constant-time.
- **Session**: cookie `HttpOnly`, `SameSite=Lax`, `Secure` di HTTPS; `RegenerateID` untuk mencegah session fixation; query session pakai parameter.
- **SQL injection**: `Find(string)`, nama tabel/alias, dan query di modul CMS sekarang di-quote; `QuoteValue` untuk nilai di klausa `Where`.
- **XSS**: `[flashmessages]` di-encode HTML, filter `escape`, halaman error di-escape, nilai di helper form di-escape.
- **XXE**: `rss_lib` memblokir external entity di XML.
- **Header injection**: line break di nilai header dibuang.
- **Path traversal**: nama template dari request disaring (`SafeTemplateName`), path dokumentasi dijaga, path POST field disanitasi.
- **Spoofing IP**: `X-Forwarded-For`/`Client-IP` hanya dipakai jika `systems/trust_proxy` aktif.
- **Random**: `RandomString()` memakai CSPRNG.
- **CORS**: `*` tidak lagi digabung dengan credentials.
- **`/initialize`** menolak jalan jika `config.json` sudah ada.
- **HTTP 405** untuk method yang tidak ditangani controller.
- **reCAPTCHA**: API v1 (sudah dimatikan Google) dihapus; `siteverify` dikirim sebagai form POST ter-encode.
- Kode language divalidasi; CMS memeriksa CSRF dan berhenti setelah `Redirect`.

---

## 3. Perbaikan Bug

### Memory leak dan double free
- Leak di `database_lib`, `theme_controller`, `http_lib`, `image_lib`, `rss_lib`, `TNLP`, JSON tools, dialog IDE.
- Double free pada redirect di handler, request di mode standalone, `remotedataset`, JSON tools.
- Objek CORS yang di-leak FPC 3.2 per web module sekarang dibebaskan.
- 30 class integrasi tidak menjalankan destructor (lupa `override`), sekarang sudah.

### Stabilitas dan kebenaran
- `die()` saat inisialisasi tidak lagi melanjutkan eksekusi kode.
- Session timeout benar dalam menit (sebelumnya default 3 hari kedaluwarsa setelah 72 menit).
- Hang di `StripTags` dan rekursi tak berujung di stemmer Nazief.
- Parsing tanggal tidak lagi bergantung locale; offset epoch UTC benar; `FromString` dengan pola `dd/mm/yyyy` bekerja.
- `base64_encode`, `HTMLDecode`, `StripHTML`, `Choose(n, 0)` diperbaiki.
- `TJSONUtil` kembali menerima array sebagai root.
- Response integrasi tidak lagi dibagi antar pemanggilan; thread update Telegram bisa selesai di program console/CGI.
- Log file selalu ditutup dan berganti harian.
- Solr: request gagal dilaporkan, query di-encode.
- Model yang gagal `Save`/`Delete` mengembalikan `False` + `Message`, tidak lagi menghentikan request.
- Modul CMS bisa di-compile lagi.

### Linux / macOS
- `smtp_lib` tidak lagi mati karena SIGPIPE.
- `_SetEnv` / `LoadDotEnv` terlihat oleh `GetEnvironmentVariable`.
- Build `fastplaz_app` di Linux (pakai `termio.IsATTY`).

### IDE Tools
- Database Explorer: freeze saat koneksi gagal, access violation saat IDE ditutup, port tidak terpakai.
- Regex Tester: daftar group dan regex URL.
- Wizard: Cancel tidak lagi membuat file/project; *Create Theme Structure* menyalin tema default lagi; model generator memberi nama class yang benar (`TUserGroupModel`).
- Fast Code membaca konfigurasi di main thread; About dialog membuka link di semua platform.

---

## 4. Packaging dan Kualitas

- Package dirapikan: setiap package punya output dir sendiri, `fastplaz_runtime` dan `fastplaz_integration` jadi RunTimeOnly, `fastplaz_tools` bisa diinstal tanpa `fastplaz_integration`, `fastplaz_components` build dari checkout bersih.
- Project web baru tidak lagi bergantung pada LCL.
- Line ending dinormalisasi (`.gitattributes`).
- **Unit test**: suite FPCUnit untuk setiap library (sekitar 28 suite, termasuk fake SMTP server in-process dan tes SQLite), plus suite untuk IDE tools dan AI Agent Plane (termasuk mock agent ACP). Script `run_tests.bat` / `run_tests.sh`, dites di Windows dan Linux.

## 5. Dokumentasi

- README ditulis ulang dalam Bahasa Inggris, versi Indonesia di `README.id.md`.
- Dokumen baru: UPGRADE.md, STANDALONE-roadmap.md, STANDALONE-deployment.md, MCP.md, AI_AGENT_by_FASTPLAZ.md, AI_PLANE_TOOL_RUNDOWN.md.
- Dokumentasi CMS, SimpleModel, template, REST, integrasi, dan unit test diperbarui.

---

## Perubahan Perilaku (perlu dicek saat upgrade)

| Area | Perubahan |
|---|---|
| Redirect / die | Tidak keluar dari method; tambahkan `Exit` setelahnya |
| Session | Timeout dalam menit; direktori default `<TEMP>/sessions` |
| Model | `Save`/`Delete` gagal → `False` + `Message` |
| Tema | `[flashmessages]` di-encode; pakai `html=1` untuk markup tepercaya |
| Proxy | Aktifkan `systems/trust_proxy` di belakang reverse proxy |
| reCAPTCHA | Selalu widget v2 |
| Integrasi | bmkg, clarifai, googleanalytics, kloudless, newrelic, office365, yandex, zomato ditandai deprecated |
| Port | Standalone `17845` (dulu 8080), MCP Agent Plane `17826` |

Detail lengkap: [UPGRADE.md](UPGRADE.md).

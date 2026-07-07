# Circular Reference Analysis: `database_lib.pas`

> Hasil penelusuran dependensi unit `FastPlaz/src/library/database_lib.pas` dan
> unit-unit terkait di paket `fastplaz_runtime.lpk`. Dokumen ini menjelaskan
> mengapa perubahan pada `database_lib` (atau unit dependennya) kadang tidak
> dikenali oleh kode pengguna sehingga harus **clean up + recompile package**.

---

## Ringkasan

`database_lib.pas` **tidak** memiliki circular reference pada level
**interface ↔ interface** yang akan membuat FPC menolak mengompilasi. Yang ada
adalah circular reference pada level **implementation ↔ interface** antara
`database_lib`, `fastplaz_handler`, dan `theme_controller`. Skema ini legal
secara bahasa Pascal, tetapi membingungkan *smart linker / make logic* FPC
sehingga deteksi *stale unit* menjadi tidak andal. Akibatnya, kode pengguna
yang memakai `.ppu` lama dari `UnitOutputDirectory` tidak melihat perubahan
interface sampai folder output dibersihkan dan paket dikompilasi ulang.

---

## Peta Dependensi

Hanya dependensi antar unit project yang ditampilkan. Unit FCL/FPC
(`fpcgi`, `db`, `sqldb`, `fpjson`, `Classes`, `SysUtils`, dll.) diabaikan.

### `database_lib` (interface — leaf)

Tidak bergantung pada unit project mana pun. Hanya memakai unit standar FPC/FCL:

```pascal
uses
  fpcgi, fphttp, db, fpjson, jsonparser, fgl,
  sqldb, sqldblib, mysql50conn, ... , sqlite3conn, pqconnection, IBConnection,
  TypInfo, variants, Classes, SysUtils;
```

### `database_lib` (implementation — `database_lib.pas:167`)

```pascal
uses common, config_lib, fastplaz_handler, logutil_lib;
```

### Dependensi unit tetangga

| Unit | Interface `uses` | Implementation `uses` | Memakai `database_lib`? |
|---|---|---|---|
| `common` | `regexpr_lib`, `fastplaz_handler`, `config_lib`, `array_helpers` | `logutil_lib`, `language_lib` | tidak |
| `config_lib` | (standar FPC) | — | tidak |
| `fastplaz_handler` | `regexpr_lib`, `sqldb`, `session_controller`, `module_controller`, `config_lib` | `common`, `language_lib`, **`database_lib`**, `logutil_lib`, `theme_controller`, `html_lib` | **ya (impl)** |
| `logutil_lib` | `common` | `config_lib`, `fastplaz_handler` | tidak |
| `theme_controller` | `common`, `fastplaz_handler`, **`database_lib`**, `datetime_lib`, `modvar_util`, `json_lib` | `config_lib`, `logutil_lib`, `language_lib`, `versioninfo_lib`, `html_lib`, `session_controller`, `initialize_controller` | **ya (interface)** |
| `html_lib` | `common`, `sqldb` | — | tidak |
| `session_controller` | (standar FPC) | — | tidak |
| `module_controller` | (standar FPC) | — | tidak |
| `language_lib` | (standar FPC) | — | tidak |
| `datetime_lib` | `common`, `language_lib` | — | tidak |
| `modvar_util` | `common`, `modvar_model`, `serialize_lib` | — | tidak |
| `json_lib` | (standar FPC) | — | tidak |
| `array_helpers` | (standar FPC) | — | tidak |
| `regexpr_lib` | (standar FPC) | — | tidak |

---

## Siklus yang Ditemukan

### Siklus 1 — mutual implementation (`database_lib` ↔ `fastplaz_handler`)

```
database_lib (impl) ──► fastplaz_handler (interface)   [AppData, DisplayError, Redirect, _DebugInfo]
fastplaz_handler (impl) ──► database_lib (interface)   [DataBaseInit, QueryExec, QueryOpenToJson, ...]
```

### Siklus 2 — melalui `theme_controller`

```
database_lib (impl) ──► fastplaz_handler (interface)
fastplaz_handler (impl) ──► theme_controller (interface)
theme_controller (interface) ──► database_lib (interface)   ← theme_controller.pas:21
```

### Siklus 3 — melalui `common` & `logutil_lib`

```
database_lib (impl) ──► common (interface)          ──► fastplaz_handler (interface)
database_lib (impl) ──► logutil_lib (interface)     ──► common (interface) ──► fastplaz_handler (interface)
fastplaz_handler (impl) ──► database_lib (interface)
```

---

## Apakah Ini Fatal?

**Tidak.** Semua siklus bersifat **implementation ↔ interface**, bukan
**interface ↔ interface**. Interface `database_lib` adalah leaf (tidak
bergantung unit project manapun), sehingga seluruh interface unit dapat
dikompilasi lebih dulu tanpa siklus. **Kode tetap bisa di-compile.**

---

## Mengapa Perubahan Tidak Dikenali Consumer?

### 1. FPC make logic (`-M`) bingung dengan graf siklikal

Smart-linker menelusuri timestamp `.ppu` untuk menentukan unit yang perlu
di-rebuild. Pada graf siklikal `database_lib ↔ fastplaz_handler ↔ theme_controller`,
perubahan interface salah satu unit seharusnya memicu recompile unit lain.
Namun make logic kadang salah menyimpulkan *"semua sudah up-to-date"* karena
rantai dependensi kembali ke titik awal.

### 2. `UnitOutputDirectory` shared

Paket memakai satu folder output bersama:

```
lib/$(TargetCPU)-$(TargetOS)
```

`.ppu` stale menumpuk di folder ini. Consumer project memakai `.ppu` lama
dari folder tersebut, sehingga tidak melihat perubahan interface
`TSimpleModel` dan simbol lain di `database_lib`.

### 3. Kasus kritis

Saat Anda mengubah **interface** `database_lib` (mis. menambah method ke
`TSimpleModel`), `theme_controller` (yang memakai `database_lib` di
**interface**-nya, `theme_controller.pas:21`) wajib di-recompile. Tapi karena
`fastplaz_handler` (impl) → `theme_controller` (interface) →
`database_lib` (interface) membentuk siklus, make logic kadang melewatkan
recompile `theme_controller`, sehingga consumer melihat interface lama.

---

## Workaround (yang sudah berjalan)

**Clean up + recompile package `fastplaz_runtime.lpk`.**

```sh
# Linux/macOS
rm -rf tools/lib/$(TargetCPU)-$(TargetOS)
# atau lebih sederhana:
sh tools/clean.sh
```

```bat
:: Windows
tools\clean.bat
```

Kemudian recompile `fastplaz_runtime.lpk` dari Lazarus IDE sebelum
mengompilasi project consumer. Ini satu-satunya cara aman selama siklus
dependensi masih ada.

---

## Solusi Permanen (diterapkan: Opsi A — simbol di `common.pas`)

Pecah siklus dengan memindahkan simbol yang dipakai `database_lib` dari
`fastplaz_handler` ke `common.pas` (unit yang sudah dipakai semua konsumer,
sehingga tidak ada project consumer yang perlu diubah).

### Simbol yang dipindah

| Simbol | Lokasi lama | Lokasi baru |
|---|---|---|
| `TMainData` (record type) | `fastplaz_handler.pas` interface | `common.pas` interface |
| `AppData` (var) | `fastplaz_handler.pas` var | `common.pas` var |
| `Config` (var) | `fastplaz_handler.pas` var | `common.pas` var |
| `_DebugInfo` (var) | `fastplaz_handler.pas` var | `common.pas` var |
| `DisplayError` (procedure) | `fastplaz_handler.pas` | `common.pas` (dispatcher) |
| `Redirect` (procedure) | `fastplaz_handler.pas` | `common.pas` (dispatcher) |

`Die` dan `_GetTickCount` sudah ada di `common.pas`, tidak perlu dipindah.

### Dispatcher pattern untuk `DisplayError` / `Redirect`

Implementasi `DisplayError`/`Redirect` butuh `ThemeUtil` (dari `theme_controller`)
dan `Application`/`FastPlasAppandler` (dari `fastplaz_handler`) yang tidak
tersedia di `common`. Solusi: `common` mendefinisikan procedure variable
`DisplayErrorHandler`/`RedirectHandler` dan procedure wrapper `DisplayError`/
`Redirect` yang mendelegasikan. `fastplaz_handler` menempati handler asli
(`DoDisplayError`/`DoRedirect`) di `initialization`.

### `database_lib.pas` (impl uses)

```
- uses common, config_lib, fastplaz_handler, logutil_lib;
+ uses common, config_lib, logutil_lib;
```

`fastplaz_handler` dihapus sepenuhnya — `AppData`, `Config`, `DisplayError`,
`Redirect`, `_DebugInfo` kini didapat dari `common` (sudah ada di uses).

### Konsumer lain — TIDAK PERLU UBAH

Semua konsumer (`theme_controller`, `logutil_lib`, `mailer_lib`,
`initialize_controller`, `redis_controller`, 6 CMS controller, dan semua
project consumer) sudah memakai `common` di uses clause, sehingga otomatis
mendapatkan simbol yang dipindah. **Tidak ada project consumer yang perlu
ditambah `uses` baru.**

---

## Peta Dependensi Setelah Perbaikan

```
common (interface)
  ├── config_lib
  └── fastplaz_handler (interface)   [untuk StartTime, _SERVER, dll.]

database_lib (impl) ──► common        (AppData, Config, DisplayError, Redirect, _DebugInfo)
database_lib (impl) ──► config_lib
database_lib (impl) ──► logutil_lib
  └── TIDAK LAGI ──► fastplaz_handler

fastplaz_handler (impl) ──► common        (AppData, Config, handler vars)
fastplaz_handler (impl) ──► database_lib  (DataBaseInit, QueryExec)
fastplaz_handler (init) ──► DisplayErrorHandler := @DoDisplayError
                           RedirectHandler := @DoRedirect
```

**Siklus 1 (database_lib ↔ fastplaz_handler): PECAH**
`database_lib` tidak lagi langsung bergantung pada `fastplaz_handler`.
Hanya `fastplaz_handler (impl) → database_lib (intf)` yang tersisa (satu arah).

**Siklus 2 (via theme_controller): PECAH**
`database_lib` tidak lagi langsung → `fastplaz_handler`.

**Siklus 3 (via common): TERATASI**
`database_lib → common` (sudah ada sebelumnya), tetapi `common` tidak
membentuk siklus kembali ke `database_lib`.

---

## Status

- **Dianalisis:** Jul 2026
- **Siklus aktif:** tidak (setelah Opsi A diterapkan)
- **Fatal compile:** tidak
- **Workaround:** clean + recompile package (tidak lagi wajib, tetapi disarankan setelah upgrade major)
- **Solusi permanen:** Opsi A — simbol dipindah ke `common.pas` (diterapkan Jul 2026)

---

## Catatan: Opsi B (unit terpisah) pernah dicoba

Sebelumnya diterapkan Opsi B (unit terpisah `appstate_util.pas`), tetapi
hal itu mengharuskan setiap project consumer menambah `appstate_util` ke
uses clause. Opsi A (memasukkan ke `common.pas`) lebih baik karena
`common` sudah dipakai oleh semua konsumer, sehingga **tidak ada project
consumer yang perlu diubah**.

---

## Referensi File

| File | Baris relevan |
|---|---|
| `FastPlaz/src/library/database_lib.pas` | 167 (impl uses) |
| `FastPlaz/src/systems/fastplaz_handler.pas` | 343 (`AppData`), 337 (`DisplayError`), 335 (`Redirect`), 357 (`_DebugInfo`), 363 (impl uses `database_lib`) |
| `FastPlaz/src/systems/theme_controller.pas` | 21 (interface uses `database_lib`) |
| `FastPlaz/src/library/common.pas` | 18 (interface uses `fastplaz_handler`), 132 (`_GetTickCount`), 183-185 (`Die`) |
| `FastPlaz/src/library/logutil_lib.pas` | 9 (interface uses `common`), 39 (impl uses `fastplaz_handler`) |
| `tools/fastplaz_runtime.lpk` | `UnitOutputDirectory` |

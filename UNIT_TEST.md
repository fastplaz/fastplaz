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
| `regex_tester_test` | `tools/src/test` | `regex_tester_lib` – logic behind the *Regex Tester* tool and the `REGEX_*` constants |

### Prerequisites

* FPC / Free Pascal Compiler, version 3.2.2
* Lazarus (for `lazbuild`), optional when using `instantfpc`

### Quick run with instantfpc

The fastest way: compile and run the tests in one command, without `lazbuild` and without the Lazarus IDE.
`instantfpc` is shipped with FPC (Windows: `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\instantfpc.exe`).

Windows (can be called from any directory):

```bat
tools\src\test\run_tests.bat
```

Linux / macOS:

```bash
sh tools/src/test/run_tests.sh
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

### Build

Windows:

```bat
cd tools\src\test
C:\lazarus\lazbuild.exe regex_tester_test.lpi
```

Linux / macOS (`lazbuild` in `PATH`):

```bash
cd tools/src/test
lazbuild regex_tester_test.lpi
```

Compiled units go to `lib/<cpu>-<os>/`. Build output is ignored by `.gitignore`.

### Run

Windows:

```bat
regex_tester_test.exe -a
```

Linux / macOS:

```bash
./regex_tester_test -a
```

Example output:

```
Number of run tests: 16
Number of errors:    0
Number of failures:  0
```

The program exits with code `0` when all tests pass and a non-zero code when a test fails, so it can be used in scripts or CI.

### Runner options

| Option | Description |
| --- | --- |
| `-a`, `--all` | run all tests |
| `-l`, `--list` | list registered tests |
| `--suite=NAME` | run a single test class or test method, e.g. `--suite=TestEmailRegex` |
| `--format=FMT` | output format: `plain` (default for this project), `plainnotiming`, `xml`, `latex` |
| `--sparse` | show only errors and failures |
| `--file=FILE` | write the result to a file, e.g. `--format=xml --file=result.xml` |
| `-h` | show all options |

### Running from the Lazarus IDE

1. Open `tools/src/test/regex_tester_test.lpi` (*Project → Open Project*).
2. Set the parameter in *Run → Run Parameters* → *Command line parameters*: `-a`.
3. Press **F9**.

### Adding a new test

1. Put the logic to be tested in a unit **without** dependencies on LCL forms or the IDE (`IDEIntf`, `PackageIntf`, …), for example `xxx_lib.pas`.
2. Create a test unit in `tools/src/test`, e.g. `test_xxx_lib.pas`:

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
| `regex_tester_test` | `tools/src/test` | `regex_tester_lib` – logika di balik tool *Regex Tester* dan konstanta `REGEX_*` |

### Prasyarat

* FPC / Free Pascal Compiler, versi 3.2.2
* Lazarus (untuk `lazbuild`), opsional jika memakai `instantfpc`

### Jalankan cepat dengan instantfpc

Cara tercepat: compile dan jalankan test dalam satu perintah, tanpa `lazbuild` dan tanpa Lazarus IDE.
`instantfpc` sudah disertakan bersama FPC (Windows: `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\instantfpc.exe`).

Windows (bisa dipanggil dari direktori mana saja):

```bat
tools\src\test\run_tests.bat
```

Linux / macOS:

```bash
sh tools/src/test/run_tests.sh
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

### Build

Windows:

```bat
cd tools\src\test
C:\lazarus\lazbuild.exe regex_tester_test.lpi
```

Linux / macOS (`lazbuild` ada di `PATH`):

```bash
cd tools/src/test
lazbuild regex_tester_test.lpi
```

Hasil kompilasi unit disimpan di `lib/<cpu>-<os>/`. Hasil build sudah diabaikan oleh `.gitignore`.

### Menjalankan

Windows:

```bat
regex_tester_test.exe -a
```

Linux / macOS:

```bash
./regex_tester_test -a
```

Contoh output:

```
Number of run tests: 16
Number of errors:    0
Number of failures:  0
```

Program keluar dengan kode `0` jika semua test lulus, dan kode selain `0` jika ada test yang gagal, sehingga bisa dipakai di script atau CI.

### Opsi runner

| Opsi | Keterangan |
| --- | --- |
| `-a`, `--all` | jalankan semua test |
| `-l`, `--list` | tampilkan daftar test yang terdaftar |
| `--suite=NAMA` | jalankan satu class test atau satu method test saja, contoh `--suite=TestEmailRegex` |
| `--format=FMT` | format output: `plain` (default di project ini), `plainnotiming`, `xml`, `latex` |
| `--sparse` | hanya tampilkan error dan failure |
| `--file=FILE` | simpan hasil ke file, contoh `--format=xml --file=result.xml` |
| `-h` | tampilkan semua opsi |

### Menjalankan dari Lazarus IDE

1. Buka `tools/src/test/regex_tester_test.lpi` (*Project → Open Project*).
2. Isi parameter di *Run → Run Parameters* → *Command line parameters*: `-a`.
3. Tekan **F9**.

### Menambah test baru

1. Letakkan logika yang akan diuji di unit yang **tidak** bergantung pada form LCL maupun IDE (`IDEIntf`, `PackageIntf`, …), misalnya `xxx_lib.pas`.
2. Buat unit test di `tools/src/test`, misalnya `test_xxx_lib.pas` (lihat contoh kode di bagian [English](#adding-a-new-test)).
3. Tambahkan unit tersebut ke `uses` program test (`.lpr`) dan ke project (`.lpi`).
4. Build dan jalankan ulang.

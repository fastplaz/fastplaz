Lazarus Menu Tool: AI Agent by FastPlaz

AI Plane untuk berkomunikasi dengan ai agent (spt: Claude Code, OpenCode, pi).

nama menu: AI Plane
lokasi menu: sebelum Database Explorer

rekomendasi penempatan source di: tools/src/ai_plane
branch: feature/ai-plane

## Konfigurasi:

terdapat form konfigurai yang mengatur:
- default adapter harness/runtime yang digunakan: Claude Code/OpenCode/Pi
  adapter akan mungkin bertambah di masa depan
- defaul model yang digunakan (auto detect daftar model ketika memilih harness)
- default thinking effort (auto/off/minimal/low/medium/high/xhigh)

terdapat tombol untuk "Run Test"

konfigurasi bisa dijadikan satu dengan form "Fast Code Configuration", gunakan TPageControl atau TTabControl

## Use Case

ketika user pilih menu "FastPlaz/AI Agent", akan muncul window chat (dockable).
ui/ux-nya seperti pada chat Anti Grafity.
terdapat:
- window conversation scrollable 
- input chat
- info runtime adapter yang digunakan
- pilihan model yang bisa digunakan.

user bisa chat yang akan berinteraksi dengan runtime yang aktif saat itu.

## Tools

Persiapkan untuk dukungan penggunaan internal/custom/additional tool di phase development berikutnya, seperti untuk interaksi dengan Lazarus IDE.
misal: membuka file

## App Tester

rekomendasikan app tester yang bisa dibuat.
ekpektasi saya: ada app tester yang perilakunya bisa sama dengan window chat yg di buat.

tujuan: agar tidak perlu recompile lazarus yang seringkali membutuhkan waktu lama

## Komunikasi dengan adapter/runtime/harness

diharapkan komunikasi bisa mode stream. sehingga user bisa langsung melihat apa yang terjadi di window chat.



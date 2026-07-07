# AGENTS.md

> Panduan singkat untuk agent AI (termasuk opencode) yang bekerja pada repository ini.

**Spesifikasi lengkap dan sumber tunggal kebenaran ada di [`BRIEF.md`](./BRIEF.md).**
Wajib membaca seluruh isi `BRIEF.md` sebelum melakukan perubahan apa pun pada project ini.

---

## Aturan Wajib untuk Agent

1. **Baca `BRIEF.md` terlebih dahulu.** Dokumen ini adalah satu-satunya acuan resmi.
   Sebelum menulis kode atau mengedit file, pastikan perubahan selaras dengan
   struktur direktori, konvensi, palet warna, dan format data yang dijelaskan di sana.
2. **Jangan hardcode URL.** Gunakan file config.
3. **Hindari komentar** dalam kode kecuali diminta eksplisit.
4. **Ikuti konvensi**:
   - Pascal: free pascal.
   - HTML: semantic tags, `lang="id"`.
   - JavaScript: ES6+, modular, hindari global vars kecuali `window.__APP_CONFIG__`.
   - CSS: gunakan CSS variables `:root` (`--bg`, `--text`, `--text-highlight`, `--accent`, `--hover`).
   - Nama file: lowercase, dash-separated.
5. **Jangan commit** file yang berisi secret/data log (mis. `repo/*.txt`).
6. **Tidak ada build step** — asset di-serve apa adanya. Jangan tambahkan tooling build.

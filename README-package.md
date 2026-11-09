
FastPlaz
===
**FastPlaz** merupakan .....

package fastplaz membantu memudahkan anda dalam membuat aplikasi web-based berbasis fastplaz.
Menu-menu tambahan baru akan otomatis terpasang di menu 'File/New ...', section 'FastPlaz'


Requirement
---
* fpc / lazarus
* fcl-web
* package `jcfbase` & `jcfidelazarus` (Jedi Code Format) — dipasang via Lazarus Online Package Manager; wajib sebelum compile `fastplaz_tools.lpk`


Instalasi
---
1. open file 'tools/fastplaz_runtime.lpk', then compile
2. open file 'tools/fastplaz_tools.lpk', then compile and install

Package Opsional
---
* `tools/fastplaz_extras.lpk` — library hash (DCPcrypt: SHA1, SHA256, SHA512, Base64) dan unit `hash_tools`. Dibutuhkan oleh modul CMS (`FastPlaz/src/cms`). Compile saja bila diperlukan, tidak perlu di-install ke IDE.
* `tools/fastplaz_components.lpk` — komponen `TRemoteDataset` (dataset dari REST endpoint FastPlaz). Butuh `fastplaz_runtime`; compile lalu *install* agar komponennya muncul di palette `FastPlaz`.

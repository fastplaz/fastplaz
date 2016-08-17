
FastPlaz - API Project Wizard

ini adalah API pascal webbased yang dibangun dengan menggunakan FastPlaz.
contoh project ini memiliki ketergantungan dengan Lazarus,
sehingga tidak bisa dicompile di mesin yang tidak terinstall lazarus di dalamnya.

Disarankan untuk mengubah lokasi target file name ke webroot directory.
Bisa dikonfigurasikan di menu "Project | Project Options | Compiler Options | Path | Target file name"

Test CURL example:
curl -X POST -H "Authorization: Basic dW5hbWU6cGFzc3dvcmQ=" "http://yourtargeturl/"

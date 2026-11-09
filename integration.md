
## INTEGRATION

note: sebagian fitur integrasi sudah _deprecated_. Unit untuk layanan yang sudah berhenti (bmkg, clarifai, googleanalytics, kloudlesscalendar, newrelic, office365, yandextranslate, zomato) masih bisa dikompilasi, tetapi ditandai `deprecated` dan akan dihapus. Lihat [docs/UPGRADE.md](docs/UPGRADE.md).

- BMKG - Badan Meteorologi Klimatologi Dan Geofisika (discontinued)
- Terjemahan Al Quran
- Pajak Kendaraan
- Office 365 (discontinued)
- Mata Uang
- Resi Paket/Kurir

Networking / Infrastruktur

- WHOIS Domain
- Newrelic (discontinued)
- Image Cloudinary

Entertainment & Kuliner

- IMDB Movie Info
- Sprout Video
- Zomato (discontinued)

Bahasa

- International Language
- Kamus Besar Bahasa Indonesia
- Bahasa Lokal (sunda)

Geo Lokasi & Cuaca

- Google GEO Location
- OpenWeatherMap
- Apixu

Messenger

- Telegram
- Facebook
- Line
- Slack



Email Features
---

`TMailer` (`mailer_lib`) sends email out of the box through `smtp_lib`, the
built-in SMTP client (FPC `ssockets` + `opensslsockets`). No third-party
component is needed.

* `SSL := True` : implicit TLS (usually port 465)
* `TLS := True` : STARTTLS (usually port 587)
* neither       : plain SMTP (port 25)
* STARTTLS / SSL need the OpenSSL shared library at runtime:
  Windows `libssl-1_1-x64.dll` + `libcrypto-1_1-x64.dll` (or `ssleay32`/`libeay32`),
  Linux `libssl.so` / `libcrypto.so`. Plain connections work without it.

```pascal
with TMailer.Create do
begin
  MailServer := 'smtp.example.com';
  Port := '587';
  UserName := 'user';
  Password := 'secret';
  TLS := True;
  Sender := FormatMailAddress('noreply@example.com', 'My App');
  AddTo('someone@example.com', 'Someone');
  Subject := 'Hello';
  Message.Add('<p>Hi</p>');
  if not Send then
    WriteLn(ErrorMessage, LineEnding, Logs);
  Free;
end;
```

`TSMTPClient` in `smtp_lib` can also be used directly (see the unit header).

### Optional: xmailer / Synapse

* using: xmailer [https://github.com/silvioprog/xmailer](https://github.com/silvioprog/xmailer)
* requirement : synapse component

To send through xmailer instead of the built-in client:

- open file define_fastplaz.inc
- add code :

```
{$define synapse}
{$define xmailer}
```

- pick the Synapse OpenSSL plugin that matches the installed library
  (without one of these, the OpenSSL 1.0.x plugin `ssl_openssl` is used):

```
{$define XMAILER_OPENSSL11}   // OpenSSL 1.1.x -> ssl_openssl11
{$define XMAILER_OPENSSL3}    // OpenSSL 3.x   -> ssl_openssl3
```


# FastPlaz

**[FastPlaz](http://www.fastplaz.com)** adalah satu satu web framework dengan menggunakan bahasa  free pascal  . Dibuat sedemikian rupa sehingga requirement server yang diperlukan pun sederhana dan minimal, dan bahkan bisa di  compile  langsung dari console/terminal tanpa memerlukan ide/editor  Lazarus  . Cukup dengan menggunakan  apache  biasa seperti di shared hosting, fastplaz sudah bisa langsung digunakan.

## INTEGRATION

note: sebagian fitur integrasi sudah _deprecated_.

- BMKG - Badan Meteorologi Klimatologi Dan Geofisika
- Terjemahan Al Quran
- Pajak Kendaraan
- Office 365
- Mata Uang
- Resi Paket/Kurir

Networking / Infrastruktur

- WHOIS Domain
- Newrelic
- Image Cloudinary

Entertainment & Kuliner

- IMDB Movie Info
- Sprout Video
- Zomato

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



## Requirement

* fpc / free pascal compiler, version 3.0.0
* fcl-web
* lazarus (optional)


Install Package
---


Compile App Example from Console
---
```
fpc fastplaz.lpr @extra.cfg
```


Email Features
---
* using: xmailer [https://github.com/silvioprog/xmailer](https://github.com/silvioprog/xmailer)
* requirement : synapse component

how to enable email support

- open file define_fastplaz.inc
- add code :

```
{$define synapse}
{$define xmailer}
```

=======


# FastPlaz


===
**[FastPlaz](http://www.fastplaz.com)** adalah satu satu web framework dengan menggunakan bahasa  free pascal  . Dibuat sedemikian rupa sehingga requirement server yang diperlukan pun sederhana dan minimal, dan bahkan bisa di  compile  langsung dari console/terminal tanpa memerlukan ide/editor  Lazarus  . Cukup dengan menggunakan  apache  biasa seperti di shared hosting, fastplaz sudah bisa langsung digunakan.


Requirement
---
* fpc / free pascal compiler, version 2.6.4
* fcl-web
* lazarus (optional)


Compile from Console
---
```
fpc fastplaz.lpr @extra.cfg
```


Email Features
---
* using: xmailer [https://github.com/silvioprog/xmailer](https://github.com/silvioprog/xmailer)
* requirement : synapse component

how to enable email support

- open file define.inc
- add code :

```
{$define synapse}
{$define xmailer}
```

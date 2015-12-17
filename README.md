
FastPlaz
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


# DateTime Helper

*Helper* ini akan memudahkan dalam melakukan operasi terhadap variabel bertipe *TDateTime*.

**Prerequisites :**

```delphi
uses
  ...., datetime_helpers;
```

Persiapkan variabel berikut:

```delphi
  var
    d : TDateTime;
    s : String;
```

Kita dapat mengisi variabel bertipe tanggal langsung dari string, dan juga sekaligus melakukan konversi langsung ke string.

```delphi
d.FromString( '17-08-1945 09:59:00');
.
.
.
s := d.AsString;

// with formatted string datetime
s := d.Format( 'yyyy-mm-dd HH:nn:ss');

```

Juga memudahkan dalam melakukan pengkondisian :

```delphi
if d.IsToday then 
begin 
  if d.IsAM then
  begin
  .
  .
  .
  end;
end;

```

variasi lainnya :

```delphi
if Tomorrow.IsSaturday then 
begin
  // Sleeping time ...
end;
```

Operasi penambahan

```delphi
d := d.IncYear;
d := d.IncMinute(10);
```

perbandingan selisih waktu

```delphi
if d.YearsDiff( Now) > 40 then 
begin 
  // you very old !
end;
```

Dan .... *'Human readable time'* :

```delphi
s := d.HumanReadable;

// will result:
// 'more than 73 years ago'

d := d.IncHour(-1).IncMinute(10); // multiple operation
s := d.HumanReadable;
// result: 49 minutes ago
```

Single-line operation

```delphi
s := d.IncHour(-1).IncMinute(-20).Format( 'yyyy-mm-dd HH:nn:ss');

// result:
// '1945-08-17 08:39:00'
```


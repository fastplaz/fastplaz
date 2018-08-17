# String Helper

*Helper* ini akan memudahkan dalam melakukan operasi terhadap variabel bertipe *String*.

**Prerequisites :**

```delphi
uses
  ...., string_helpers;
```

prepare this definition:

```delphi
var
  d : TDateTime;
  s : String;
```

```delphi
  s := 'one two three four five six';
  s := s.ToUpper; // legacy function, convert to uppercae
  s := s.ToLower; // legacy function, convert to lowercase
  s := s.UcWords;
  
  //result:
  //  'One Two Three Four Five Six'

```

Comparison

```delphi
  if s.IsEqualTo('One Two Three Four Five Six') then
  begin
    // same string
  end;
  
  .
  .
  .
    
  if ((not s.IsEmpty) and (not s.IsNumeric)) then
  begin
    if s.IsJson then
    begin
      // json string

    end;
  end;
  
  .
  .
  .
  
  if s.Has('Three Four') then
  begin
    // has string 'Three Four'
  end;  
  
```

Single-line operation

```delphi
  s := s.Cut( 'Two', 'Five').Trim;
  
  // result:
  //  'Three Four'
```

Encode

```delphi
  s := s.UrlEncode;
  s := s.Encode64;

```



Conversion:

```delphi
  // convert string to datetime
  s := '17-08-1945 09:59:00';
  d := s.AsDateTime;

```


and others ....
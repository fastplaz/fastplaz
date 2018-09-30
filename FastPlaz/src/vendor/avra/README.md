# BitHelpers #



BitHelpers enable additional bit manipulation for qword, longword, word, byte and boolean FreePascal types which will make your life much easier if you need such a feature.



### History

FreePascal type helpers [TBooleanHelper](https://www.freepascal.org/docs-html/rtl/sysutils/tbooleanhelper.html), [TByteHelper](https://www.freepascal.org/docs-html/rtl/sysutils/tbytehelper.html), [TWordHelper](https://www.freepascal.org/docs-html/rtl/sysutils/twordhelper.html), [TCardinalHelper](https://www.freepascal.org/docs-html/rtl/sysutils/twordhelper.html) and [TQWordHelper](https://www.freepascal.org/docs-html/rtl/sysutils/tqwordhelper.html) do not offer much when bit manipulation and presentation are needed. That's where BitHelpers package jumps in, nicely extending mentioned type helpers.



### Installation

While you can simply copy **bithelpers** unit to your project directory and start using it, the recommended way would be to open **bithelpers_pkg.lpk** package and compile it. That would add BitHelpers source directory to Lazarus and make it available to all your projects.



### Usage ###

* **TBooleanBitHelper** example code and it's output:

```pascal
  uses
    bithelpers;
  ...
  procedure TForm1.BooleanBitTestBtnClick(Sender: TObject);
  var
    MyBool: boolean;
  begin
    MyBool := true;
    Memo1.Append(MyBool.ToOneZeroString);
    Memo1.Append(MyBool.ToOnOffString); // default is scfUnchangedCase and can be ommited
    Memo1.Append(MyBool.ToOnOffString(scfLowerCase));
    Memo1.Append(MyBool.ToTrueFalseString(scfUpperCase));
    Memo1.Append(MyBool.ToString('OnState', 'OffState')); // true/false custom strings
    Memo1.Append(MyBool.ToString('Укључено', 'Искључено', scfUpperCase)); // when case and unicode matter
  end; 
```
```
  1
  On
  on
  TRUE
  OnState
  УКЉУЧЕНО
```
* **TByteBitHelper** example code and it's output:

```pascal
  procedure TForm1.ByteBitTestBtnClick(Sender: TObject);
  var
    MyByte: byte;
  begin
    MyByte.Clear;                                  // %00000000 MyByte equals 0
    MyByte.Bit[0] := true;                         // %00000001 MyByte equals 1
    MyByte.Bit[2] := true;                         // %00000101 MyByte equals 5
    Memo1.Append(MyByte.ToString);
    Memo1.Append('$' + MyByte.ToHexString);
    Memo1.Append(MyByte.ToBooleanString(lzHideLeadingZeros));   // hide leading zeros
    Memo1.Append(MyByte.ToBooleanString);                       // show leading zeros   
  end;
```

```
  5
  $05
  101
  00000101
```

- **TWordBitHelper** example code and it's output:

```pascal
  procedure TForm1.WordBitTestBtnClick(Sender: TObject);
  var
    MyWord: word;
  begin
    MyWord.Clear;                  // %0000000000000000 MyWord equals 0
    MyWord.Byte[0] := 2;           // %0000000000000010 MyWord equals 2
    MyWord.Byte[1] := 1;           // %0000000100000010 MyWord equals 258 (2 + 256)
    MyWord.Byte[1].Bit[7] := true; // %0000000100000010 MyWord equals 258 (Beware!!! This DOES NOT set a bit in MyWord !!!)
    MyWord.Bit[10] := true;        // %0000010100000010 MyWord equals 1282 (258 + 2^10)
    Memo1.Append(MyWord.ToString);
    Memo1.Append('$' + MyWord.ToHexString);
    Memo1.Append(MyWord.ToBooleanString(lzHideLeadingZeros)); // hide leading zeros
    Memo1.Append(MyWord.ToBooleanString);                     // show leading zeros
  end; 
```

```
  1282
  $0502
  10100000010
  0000010100000010
```

- **TLongwordBitHelper** example code and it's output:

```pascal
  procedure TForm1.LongwordBitTestBtnClick(Sender: TObject);
  var
    MyLongword: longword;
  begin
    MyLongword.Clear;                  // %00000000000000000000000000000000 MyLongword equals 0
    MyLongword.Word[0] := 250;         // %00000000000000000000000011111010 MyLongword equals 250
    MyLongword.Word[1].Byte[0] := 100; // %00000000000000000000000011111010 MyLongword equals 250 (Beware!!! This DOES NOT set a byte in MyLongword !!!)
    MyLongword.Byte[1] := 4;           // %00000000000000000000010011111010 MyLongword equals 1274 (250 + 2^(8 + 2), 2^2 = 4)
    MyLongword.Bit[26] := true;        // %00000100000000000000010011111010 MyLongword equals 67110138 (1274 + 2^26)
    Memo1.Append(MyLongword.ToString);
    Memo1.Append('$' + MyLongword.ToHexString);
    Memo1.Append(MyLongword.ToBooleanString(lzHideLeadingZeros)); // hide leading zeros
    Memo1.Append(MyLongword.ToBooleanString);                     // show leading zeros
    Memo1.Append('');
  end; 
```

```
  67110138
  $040004FA
  100000000000000010011111010
  00000100000000000000010011111010
```

- **TQuadwordBitHelper** example code and it's output:

```pascal
  procedure TForm1.QuadwordBitTestBtnClick(Sender: TObject);
  var
    MyQuadword: qword;
  begin
    MyQuadword.Clear;                      // %0000000000000000000000000000000000000000000000000000000000000000 MyQuadword equals 0
    MyQuadword.Longword[0] := 12345;       // %0000000000000000000000000000000000000000000000000011000000111001 MyQuadword equals 12345
    MyQuadword.Longword[1].Word[0] := 100; // %0000000000000000000000000000000000000000000000000011000000111001 MyQuadword equals 12345 (Beware!!! This DOES NOT set a word in MyQuadword !!!)
    MyQuadword.Byte[3] := 2;               // %0000000000000000000000000000000000000010000000000011000000111001 MyQuadword equals 33566777 (12345 + 2^(8 + 8 + 8 + 2), 2^1 = 2)
    MyQuadword.Bit[50] := true;            // %0000000000000100000000000000000000000010000000000011000000111001 MyQuadword equals 1125899940409401 (33566777 + 2^50)
    Memo1.Append(MyQuadword.ToString);
    Memo1.Append('$' + MyQuadword.ToHexString);
    Memo1.Append(MyQuadword.ToBooleanString(lzHideLeadingZeros)); // hide leading zeros
    Memo1.Append(MyQuadword.ToBooleanString);                     // show leading zeros
  end;
```

```
  1125899940409401
  $0004000002003039
  100000000000000000000000010000000000011000000111001
  0000000000000100000000000000000000000010000000000011000000111001
```

- **TQuadwordOverlay**, **TLongwordOverlay**, **TWordOverlay** and **TByteOverlay** variant records are also provided for qword, longword, word and byte. Sometimes they are more convenient to use then type helpers, and nothing stops you to mix them when needed. Here is an example code and it's output:

```pascal
  procedure TForm1.OverlaysTestBtnClick(Sender: TObject);
  var
    MyQuadOverlay: TQuadwordOverlay;
  begin
    MyQuadOverlay.AsQuadword.Clear;
    MyQuadOverlay.AsByte[0] := 100;
    Memo1.Append(MyQuadOverlay.AsQuadword.ToBooleanString);
    MyQuadOverlay.AsLongword[1] := 1;
    Memo1.Append(MyQuadOverlay.AsQuadword.ToBooleanString);
    MyQuadOverlay.AsQuadword.Bit[32] := false;
    Memo1.Append(MyQuadOverlay.AsQuadword.ToBooleanString);
    MyQuadOverlay.AsWordOverlay[3].AsByte[1] := $FF; // recursive overlays are allowed
    Memo1.Append(MyQuadOverlay.AsQuadword.ToBooleanString);
    MyQuadOverlay.AsWord[3].Byte[1].Bit[5] := false; // NO CHANGE !!! Bit is changed in a result byte, not in a byte that belongs to MyQuadOverlay
    Memo1.Append(MyQuadOverlay.AsQuadword.ToBooleanString);
    MyQuadOverlay.AsBit[63] := false;
    Memo1.Append(MyQuadOverlay.AsQuadword.ToBooleanString);
  end; 
```

```
  0000000000000000000000000000000000000000000000000000000001100100
  0000000000000000000000000000000100000000000000000000000001100100
  0000000000000000000000000000000000000000000000000000000001100100
  1111111100000000000000000000000000000000000000000000000001100100
  1111111100000000000000000000000000000000000000000000000001100100
  0111111100000000000000000000000000000000000000000000000001100100
```



### Download ###

If for some reason you do not handle git, then full repository can be downloaded manually from [here](https://bitbucket.org/avra/bithelpers/downloads).



### License ###

BitHelpers package is released under triple license:

1. [LGPLv3](https://www.gnu.org/licenses/lgpl-3.0.en.html). Chosen for compatibility with [Pasettimino](https://bitbucket.org/avra/pasettimino). License [explained in plain english](https://tldrlegal.com/license/gnu-lesser-general-public-license-v3-(lgpl-3)).

2. [FPC modified LGPL](http://wiki.freepascal.org/FPC_modified_LGPL). Chosen for compatibility with FreePascal and Lazarus.

3. [BSD3](https://opensource.org/licenses/BSD-3-Clause). Chosen for compatibility with everything else. License [explained in plain english](https://tldrlegal.com/license/bsd-3-clause-license-(revised)).

   

### Author ###

Made by Zeljko Avramovic (user Avra in Lazarus forum).



### Versions ###

* 1.0.0.0 First public version.
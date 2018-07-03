////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                            //
//  BITHELPERS - additional bit manipulation for qword, longword, word, byte and boolean types                //
//                                                                                                            //
//  Made by Zeljko Avramovic (user avra in Lazarus forum)                                                     //
//                                                                                                            //
//  This very basic unit is released under 3 licenses: 1) LGPL3, 2) FPC modified LGPL, 3) BSD3. That should   //
//  cover most cases that I can think of. First was chosen to be compatible with original Settimino library.  //
//  Second was chosen for full compatibility with FreePascal/Lazarus, and the third for every other case.     //
//  Yes, in short this means that commercial usage is allowed and ecouraged.                                  //
//                                                                                                            //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

unit bithelpers;

{$mode objfpc}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils;

const
  BIT_ON_TXT    = 'On';
  BIT_OFF_TXT   = 'Off';

  BIT_TRUE_TXT  = 'True';
  BIT_FALSE_TXT = 'False';

  BIT_ONE_TXT   = '1';
  BIT_ZERO_TXT  = '0';

  lzShowLeadingZeros = true;
  lzHideLeadingZeros = false;

type

  ///////////////////////
  // TBooleanBitHelper //
  ///////////////////////

  TStringCaseFormat = (scfUnchangedCase, scfLowerCase, scfUpperCase);

  TBooleanBitHelper = type helper(TBooleanHelper) for Boolean
  public
    function ToString(const aBit: boolean; const aTrueStr, aFalseStr: string; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload;
    function ToString(const aTrueStr, aFalseStr: string; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload;
    function ToOneZeroString: string;
    function ToOnOffString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string;
    function ToTrueFalseString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string;
  end;

  ////////////////////
  // TByteBitHelper //
  ////////////////////

  TByteBits = 0..7;

  TByteOverlay = bitpacked record case integer of     // for fast extraction of bits
    0: (AsBit:  bitpacked array[TByteBits] of boolean);
    1: (AsByte: byte);
  end;

  TByteBitHelper = type helper(TByteHelper) for Byte
  const
    MinBit = Low(TByteBits);
    MaxBit = High(TByteBits);
  public
    procedure Clear;
    function  ToBooleanString(const aShowLeadingZeros: boolean = true): string;
    function  GetBit(const aIndex: TByteBits): boolean;
    procedure SetBit(const aIndex: TByteBits; const NewValue: boolean);
    property  Bit[aIndex: TByteBits]: boolean read GetBit write SetBit;
  end;

  ////////////////////
  // TWordBitHelper //
  ////////////////////

  TWordBits  = 0..15;
  TWordBytes = 0..1;

  TWordOverlay = bitpacked record case integer of     // for fast extraction of bytes and bits
    0: (AsBit:         bitpacked array[TWordBits] of boolean);
    1: (AsByte:        array[TWordBytes] of byte);
    2: (AsWord:        word);
    // recursive overlays:
    3: (AsByteOverlay: array[TWordBytes] of TByteOverlay);
  end;

  TWordBitHelper = type helper(TWordHelper) for Word
  const
    MinBit  = Low(TWordBits);
    MaxBit  = High(TWordBits);
    MinByte = Low(TWordBytes);
    MaxByte = High(TWordBytes);
  public
    procedure Clear;
    function  ToBooleanString(const aShowLeadingZeros: boolean = true): string;
    function  GetBit(const aIndex: TWordBits): boolean;
    procedure SetBit(const aIndex: TWordBits; const NewValue: boolean);
    property  Bit[aIndex: TWordBits]: boolean read GetBit write SetBit;
    function  GetByte(const aIndex: TWordBytes): byte;
    procedure SetByte(const aIndex: TWordBytes; const NewValue: byte);
    property  Byte[aIndex: TWordBytes]: byte read GetByte write SetByte;
  end;

  ////////////////////////
  // TLongwordBitHelper //
  ////////////////////////

  TLongwordBits  = 0..31;
  TLongwordBytes = 0..3;
  TLongwordWords = 0..1;

  TLongwordOverlay = bitpacked record case integer of     // for fast extraction of words, bytes and bits
    0: (AsBit:         bitpacked array[TLongwordBits] of boolean);
    1: (AsByte:        array[TLongwordBytes] of byte);
    2: (AsWord:        array[TLongwordWords] of word);
    3: (AsLongword:    longword);
    // recursive overlays:
    4: (AsByteOverlay: array[TLongwordBytes] of TByteOverlay);
    5: (AsWordOverlay: array[TLongwordWords] of TWordOverlay);
  end;

  TLongwordBitHelper = type helper(TCardinalHelper) for cardinal
  const
    MinBit  = Low(TLongwordBits);
    MaxBit  = High(TLongwordBits);
    MinByte = Low(TLongwordBytes);
    MaxByte = High(TLongwordBytes);
    MinWord = Low(TLongwordWords);
    MaxWord = High(TLongwordWords);
  public
    procedure Clear;
    function  ToBooleanString(const aShowLeadingZeros: boolean = true): string;
    function  GetBit(const aIndex: TLongwordBits): boolean;
    procedure SetBit(const aIndex: TLongwordBits; const NewValue: boolean);
    property  Bit[aIndex: TWordBits]: boolean read GetBit write SetBit;
    function  GetByte(const aIndex: TLongwordBytes): byte;
    procedure SetByte(const aIndex: TLongwordBytes; const NewValue: byte);
    property  Byte[aIndex: TLongwordBytes]: byte read GetByte write SetByte;
    function  GetWord(const aIndex: TLongwordWords): word;
    procedure SetWord(const aIndex: TLongwordWords; const NewValue: word);
    property  Word[aIndex: TLongwordWords]: word read GetWord write SetWord;
  end;

  ////////////////////////
  // TQuadwordBitHelper //
  ////////////////////////

  quadword = qword; // alias

  TQuadwordBits      = 0..63;
  TQuadwordBytes     = 0..7;
  TQuadwordWords     = 0..3;
  TQuadwordLongwords = 0..1;

  TQuadwordOverlay = bitpacked record case integer of     // for fast extraction of longwords, words, bytes and bits
    0: (AsBit:             bitpacked array[TQuadwordBits] of boolean);
    1: (AsByte:            array[TQuadwordBytes] of byte);
    2: (AsWord:            array[TQuadwordWords] of word);
    3: (AsLongword:        array[TQuadwordLongwords] of longword);
    4: (AsQuadword:        qword);
    // recursive overlays:
    5: (AsByteOverlay:     array[TQuadwordBytes] of TByteOverlay);
    6: (AsWordOverlay:     array[TQuadwordWords] of TWordOverlay);
    7: (AsLongwordOverlay: array[TQuadwordLongwords] of TLongwordOverlay);
  end;

  TQuadwordBitHelper = type helper(TQWordHelper) for qword
  const
    MinBit      = Low(TQuadwordBits);
    MaxBit      = High(TQuadwordBits);
    MinByte     = Low(TQuadwordBytes);
    MaxByte     = High(TQuadwordBytes);
    MinWord     = Low(TQuadwordWords);
    MaxWord     = High(TQuadwordWords);
    MinLongword = Low(TQuadwordLongwords);
    MaxLongword = High(TQuadwordLongwords);
  public
    procedure Clear;
    function  ToBooleanString(const aShowLeadingZeros: boolean = true): string;
    function  GetBit(const aIndex: TQuadwordBits): boolean;
    procedure SetBit(const aIndex: TQuadwordBits; const NewValue: boolean);
    property  Bit[aIndex: TWordBits]: boolean read GetBit write SetBit;
    function  GetByte(const aIndex: TQuadwordBytes): byte;
    procedure SetByte(const aIndex: TQuadwordBytes; const NewValue: byte);
    property  Byte[aIndex: TQuadwordBytes]: byte read GetByte write SetByte;
    function  GetWord(const aIndex: TQuadwordWords): word;
    procedure SetWord(const aIndex: TQuadwordWords; const NewValue: word);
    property  Word[aIndex: TQuadwordWords]: word read GetWord write SetWord;
    function  GetLongword(const aIndex: TQuadwordLongwords): longword;
    procedure SetLongword(const aIndex: TQuadwordLongwords; const NewValue: longword);
    property  Longword[aIndex: TQuadwordLongwords]: longword read GetLongword write SetLongword;
  end;


implementation

///////////////////////
// TBooleanBitHelper //
///////////////////////

function TBooleanBitHelper.ToString(const aBit: boolean; const aTrueStr, aFalseStr: string; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload;
begin
  if aBit then
    case aCharsCase of
      scfLowerCase:        Result := AnsiLowerCase(aTrueStr);
      scfUpperCase:        Result := AnsiUpperCase(aTrueStr);
    else
      {scfUnchangedCase:}  Result := aTrueStr;
    end
  else
    case aCharsCase of
      scfLowerCase:        Result := AnsiLowerCase(aFalseStr);
      scfUpperCase:        Result := AnsiUpperCase(aFalseStr);
    else
      {scfUnchangedCase:}  Result := aFalseStr;
    end;
end;

function TBooleanBitHelper.ToString(const aTrueStr, aFalseStr: string; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload;
begin
  Result := ToString(Self, aTrueStr, aFalseStr, aCharsCase);
end;

function TBooleanBitHelper.ToOneZeroString: string;
begin
  Result := ToString(BIT_ONE_TXT, BIT_ZERO_TXT);
end;

function TBooleanBitHelper.ToOnOffString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string;
begin
  Result := ToString(BIT_ON_TXT, BIT_OFF_TXT, aCharsCase);
end;

function TBooleanBitHelper.ToTrueFalseString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string;
begin
  Result := ToString(BIT_TRUE_TXT, BIT_FALSE_TXT, aCharsCase);
end;

////////////////////
// TByteBitHelper //
////////////////////

procedure TByteBitHelper.Clear;
begin
  Self := 0;
end;

function TByteBitHelper.ToBooleanString(const aShowLeadingZeros: boolean = true): string;
var
  i: TByteBits;
  LeadingZeros: boolean;
begin
  Result := '';
  LeadingZeros := true;
  for i := MaxBit downto MinBit do
  begin
    LeadingZeros := LeadingZeros and not Self.Bit[i];
    if aShowLeadingZeros or (not LeadingZeros) or (i = MinBit) then
      Result := Result + Self.Bit[i].ToOneZeroString
  end;
end;

function TByteBitHelper.GetBit(const aIndex: TByteBits): boolean;
begin
  if aIndex in [MinBit..MaxBit] then
    Result := ((Self shr aIndex) and Byte(1)) = Byte(1)
  else
    Result := false;
end;

procedure TByteBitHelper.SetBit(const aIndex: TByteBits; const NewValue: boolean);
begin
  if aIndex in [MinBit..MaxBit] then
    Self := (Self or (Byte(1) shl aIndex)) xor (Byte(not NewValue) shl aIndex);
end;

////////////////////
// TWordBitHelper //
////////////////////

procedure TWordBitHelper.Clear;
begin
  Self := 0;
end;

function TWordBitHelper.ToBooleanString(const aShowLeadingZeros: boolean = true): string;
var
  i: TWordBits;
  LeadingZeros: boolean;
begin
  Result := '';
  LeadingZeros := true;
  for i := MaxBit downto MinBit do
  begin
    LeadingZeros := LeadingZeros and not Self.Bit[i];
    if aShowLeadingZeros or (not LeadingZeros) or (i = MinBit) then
      Result := Result + Self.Bit[i].ToOneZeroString
  end;
end;

function TWordBitHelper.GetBit(const aIndex: TWordBits): boolean;
begin
  if aIndex in [MinBit..MaxBit] then
    Result := Self.Byte[aIndex div 8].Bit[aIndex mod 8]
  else
    Result := false;
end;

procedure TWordBitHelper.SetBit(const aIndex: TWordBits; const NewValue: boolean);
begin
  if aIndex in [MinBit..MaxBit] then
    Self := (Self or (Word(1) shl aIndex)) xor (Word(not NewValue) shl aIndex);
end;

function TWordBitHelper.GetByte(const aIndex: TWordBytes): byte;
begin
  if aIndex in [MinByte..MaxByte] then
    Result := TWordOverlay(Self).AsByte[aIndex]
  else
    Result := 0;
end;

procedure TWordBitHelper.SetByte(const aIndex: TWordBytes; const NewValue: byte);
begin
  if aIndex in [MinByte..MaxByte] then
    TWordOverlay(Self).AsByte[aIndex] := NewValue;
end;

////////////////////////
// TLongwordBitHelper //
////////////////////////

procedure TLongwordBitHelper.Clear;
begin
  Self := 0;
end;

function TLongwordBitHelper.ToBooleanString(const aShowLeadingZeros: boolean = true): string;
var
  i: TLongwordBits;
  LeadingZeros: boolean;
begin
  Result := '';
  LeadingZeros := true;
  for i := MaxBit downto MinBit do
  begin
    LeadingZeros := LeadingZeros and not Self.Bit[i];
    if aShowLeadingZeros or (not LeadingZeros) or (i = MinBit) then
      Result := Result + Self.Bit[i].ToOneZeroString
  end;
end;

function TLongwordBitHelper.GetBit(const aIndex: TLongwordBits): boolean;
begin
  if aIndex in [MinBit..MaxBit] then
    Result := Self.Byte[aIndex div 8].Bit[aIndex mod 8]
  else
    Result := false;
end;

procedure TLongwordBitHelper.SetBit(const aIndex: TLongwordBits; const NewValue: boolean);
begin
  if aIndex in [MinBit..MaxBit] then
    Self := (Self or (Longword(1) shl aIndex)) xor (Longword(not NewValue) shl aIndex);
end;

function TLongwordBitHelper.GetByte(const aIndex: TLongwordBytes): byte;
begin
  if aIndex in [MinByte..MaxByte] then
    Result := TLongwordOverlay(Self).AsByte[aIndex]
  else
    Result := 0;
end;

procedure TLongwordBitHelper.SetByte(const aIndex: TLongwordBytes; const NewValue: byte);
begin
  if aIndex in [MinByte..MaxByte] then
    TLongwordOverlay(Self).AsByte[aIndex] := NewValue;
end;

function TLongwordBitHelper.GetWord(const aIndex: TLongwordWords): word;
begin
  if aIndex in [MinWord..MaxWord] then
    Result := TLongwordOverlay(Self).AsWord[aIndex]
  else
    Result := 0;
end;

procedure TLongwordBitHelper.SetWord(const aIndex: TLongwordWords; const NewValue: word);
begin
  if aIndex in [MinWord..MaxWord] then
    TLongwordOverlay(Self).AsWord[aIndex] := NewValue;
end;

////////////////////////
// TQuadwordBitHelper //
////////////////////////

procedure TQuadwordBitHelper.Clear;
begin
  Self := 0;
end;

function TQuadwordBitHelper.ToBooleanString(const aShowLeadingZeros: boolean = true): string;
var
  i: TQuadwordBits;
  LeadingZeros: boolean;
begin
  Result := '';
  LeadingZeros := true;
  for i := MaxBit downto MinBit do
  begin
    LeadingZeros := LeadingZeros and not Self.Bit[i];
    if aShowLeadingZeros or (not LeadingZeros) or (i = MinBit) then
      Result := Result + Self.Bit[i].ToOneZeroString
  end;
end;

function TQuadwordBitHelper.GetBit(const aIndex: TQuadwordBits): boolean;
begin
  if aIndex in [MinBit..MaxBit] then
    Result := Self.Byte[aIndex div 8].Bit[aIndex mod 8]
  else
    Result := false;
end;

procedure TQuadwordBitHelper.SetBit(const aIndex: TQuadwordBits; const NewValue: boolean);
begin
  if aIndex in [MinBit..MaxBit] then
    Self := (Self or (qword(1) shl aIndex)) xor (qword(not NewValue) shl aIndex);
end;

function TQuadwordBitHelper.GetByte(const aIndex: TQuadwordBytes): byte;
begin
  if aIndex in [MinByte..MaxByte] then
    Result := TQuadwordOverlay(Self).AsByte[aIndex]
  else
    Result := 0;
end;

procedure TQuadwordBitHelper.SetByte(const aIndex: TQuadwordBytes; const NewValue: byte);
begin
  if aIndex in [MinByte..MaxByte] then
    TQuadwordOverlay(Self).AsByte[aIndex] := NewValue;
end;

function TQuadwordBitHelper.GetWord(const aIndex: TQuadwordWords): word;
begin
  if aIndex in [MinWord..MaxWord] then
    Result := TQuadwordOverlay(Self).AsWord[aIndex]
  else
    Result := 0;
end;

procedure TQuadwordBitHelper.SetWord(const aIndex: TQuadwordWords; const NewValue: word);
begin
  if aIndex in [MinWord..MaxWord] then
    TQuadwordOverlay(Self).AsWord[aIndex] := NewValue;
end;


function TQuadwordBitHelper.GetLongword(const aIndex: TQuadwordLongwords): longword;
begin
  if aIndex in [MinLongword..MaxLongword] then
    Result := TQuadwordOverlay(Self).AsLongword[aIndex]
  else
    Result := 0;
end;

procedure TQuadwordBitHelper.SetLongword(const aIndex: TQuadwordLongwords; const NewValue: longword);
begin
  if aIndex in [MinLongword..MaxLongword] then
    TQuadwordOverlay(Self).AsLongword[aIndex] := NewValue;
end;

end.


unit verbal_expressions_lib;

{
  // USAGE:

  VE := TVerbalExpressions.Create;
  VE.StartOfLine()
    .Has('http')
    .Maybe('s')
    .Has('://')
    .Maybe('www.')
    .AnythingBut(' ')
    .EndOfLine(False);
  if VE.IsMatch('https://fastplaz.com') then
  begin
    // your code
  end;

  // Replace String
  varString := 'Replace bird with a duck';
  VE.Find('bird');
  varString := VE.Replace(varString, 'duck');
  //result: 'Replace duck with a duck'

  // or
  varString := VE.Find('red').Replace('We have a red house', 'blue');
  //result: 'We have a blue house'

  Values passed to Has/Find/Maybe/AnyOf/... are escaped, so user input can
  be used there. AndThen/Has/Have/Find with ASanitize = False and
  Range(array of string) insert raw regex syntax: never pass user input to
  those. Replace treats the replacement as literal text.

  inspiration from:
    http://verbalexpressions.github.io/
}

{$mode objfpc}{$H+}

interface

uses
  common, fgl, RegExpr,
  Classes, SysUtils;

type

  generic TStringHashMap<T> = class(specialize TFPGMap<string, T>)
  end;

  TVEValueMap = specialize TStringHashMap<variant>;

  { TVerbalExpressions }

  TVerbalExpressions = class
  private
    FLastAdded: string;
    FModifiers: string;
    FPrefixes: string;
    FSource: string;
    FSuffixes: string;
    function getExpression: string;
    function CreateRegExpr: TRegExpr;
  public
    constructor Create;
    destructor Destroy; override;

    function IsMatch(const AText: string): boolean;
    function Replace(const ASourceString, AValue: string): string;

    procedure Clean;
    function Sanitize(AValue: string): string;

    function Add(AValue: string): TVerbalExpressions;
    function StartOfLine(AEnable: boolean = True): TVerbalExpressions;
    function EndOfLine(AEnable: boolean = True): TVerbalExpressions;
    function AndThen(AValue: string; ASanitize: boolean = True): TVerbalExpressions;
    function Has(AValue: string; ASanitize: boolean = True): TVerbalExpressions;
    function Have(AValue: string; ASanitize: boolean = True): TVerbalExpressions;
    function Find(AValue: string; ASanitize: boolean = True): TVerbalExpressions;
    function Maybe(AValue: string): TVerbalExpressions;
    function AnythingBut(AValue: string): TVerbalExpressions;
    function SomethingBut(AValue: string): TVerbalExpressions;
    function AnyOf(AValue: string): TVerbalExpressions;
    function Any(AValue: string): TVerbalExpressions;
    function Range(AValue: string): TVerbalExpressions;
    function Range(AValue: array of string): TVerbalExpressions;
    function Something: TVerbalExpressions;
    function Anything: TVerbalExpressions;
    function Digit: TVerbalExpressions;
    function word: TVerbalExpressions;
    function Tab: TVerbalExpressions;
    function LineBreak: TVerbalExpressions;
    function BR: TVerbalExpressions;

  published
    property Expression: string read getExpression;
    property Source: string read FSource;
    property Prefixes: string read FPrefixes write FPrefixes;
    property Suffixes: string read FSuffixes write FSuffixes;
    // 'm': ^ and $ match at line breaks; 'i': ignore case; 's': . matches
    // line breaks. Default 'm'.
    property Modifiers: string read FModifiers write FModifiers;
    property LastAdded: string read FLastAdded;
  end;


implementation

const
  REGEX_GROUP_LABEL = ''; //'?:';

{ TVerbalExpressions }

function TVerbalExpressions.getExpression: string;
begin
  Result := FPrefixes + FSource + FSuffixes;
end;

function TVerbalExpressions.CreateRegExpr: TRegExpr;
begin
  Result := TRegExpr.Create;
  Result.ModifierM := Pos('m', FModifiers) > 0;
  Result.ModifierI := Pos('i', FModifiers) > 0;
  Result.ModifierS := Pos('s', FModifiers) > 0;
  Result.Expression := getExpression;
end;

constructor TVerbalExpressions.Create;
begin
  inherited Create;
  Clean;
end;

destructor TVerbalExpressions.Destroy;
begin
  inherited Destroy;
end;

function TVerbalExpressions.IsMatch(const AText: string): boolean;
var
  re: TRegExpr;
begin
  Result := False;
  re := CreateRegExpr;
  try
    try
      Result := re.Exec(AText);
    except
      Result := False;
    end;
  finally
    re.Free;
  end;
end;

// Replaces every match with AValue taken literally ($1, \n and the like
// in AValue are not interpreted).
function TVerbalExpressions.Replace(const ASourceString, AValue: string): string;
var
  re: TRegExpr;
begin
  Result := ASourceString;
  re := CreateRegExpr;
  try
    try
      Result := re.Replace(ASourceString, AValue, False);
    except
      Result := ASourceString;
    end;
  finally
    re.Free;
  end;
end;

// Deletes the current regex for a fresh start
procedure TVerbalExpressions.Clean;
begin
  FSource := '';
  FModifiers := 'm';
  FPrefixes := '';
  FSuffixes := '';
  FLastAdded := '';
end;

// Escapes every regex meta character, including '-' and ']' so the result
// is also safe inside a character class.
function TVerbalExpressions.Sanitize(AValue: string): string;
begin
  Result := QuoteRegExprMetaChars(AValue);
  Result := StringReplace(Result, '-', '\-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '\/', [rfReplaceAll]);
end;

function TVerbalExpressions.Add(AValue: string): TVerbalExpressions;
begin
  FLastAdded := Avalue;
  FSource := FSource + AValue;
  Result := Self;
end;

// Mark the expression to start at the beginning of the line.
function TVerbalExpressions.StartOfLine(AEnable: boolean): TVerbalExpressions;
begin
  if AEnable then
    FPrefixes := '^'
  else
    FPrefixes := '';
  Result := Self;
end;

// Mark the expression to end at the last character of the line.
function TVerbalExpressions.EndOfLine(AEnable: boolean): TVerbalExpressions;
begin
  if AEnable then
    FSuffixes := '$'
  else
    FSuffixes := '';
  Result := Self;
end;

// Add a string to the expression
function TVerbalExpressions.AndThen(AValue: string;
  ASanitize: boolean): TVerbalExpressions;
begin
  if ASanitize then
    Add('(' + REGEX_GROUP_LABEL + Sanitize(AValue) + ')')
  else
    Add('(' + REGEX_GROUP_LABEL + AValue + ')');
  Result := Self;
end;

function TVerbalExpressions.Has(AValue: string; ASanitize: boolean): TVerbalExpressions;
begin
  Result := AndThen(AValue, ASanitize);
end;

function TVerbalExpressions.Have(AValue: string;
  ASanitize: boolean): TVerbalExpressions;
begin
  Result := AndThen(AValue, ASanitize);
end;

function TVerbalExpressions.Find(AValue: string;
  ASanitize: boolean): TVerbalExpressions;
begin
  Result := AndThen(AValue, ASanitize);
end;

// Add a string to the expression that might appear once (or not).
function TVerbalExpressions.Maybe(AValue: string): TVerbalExpressions;
begin
  Add('(' + REGEX_GROUP_LABEL + Sanitize(AValue) + ')?');
  Result := Self;
end;

// Anything but this chars
function TVerbalExpressions.AnythingBut(AValue: string): TVerbalExpressions;
begin
  Add('(' + REGEX_GROUP_LABEL + '[^' + Sanitize(AValue) + ']*)');
  Result := Self;
end;

// Anything non-empty except for these chars
function TVerbalExpressions.SomethingBut(AValue: string): TVerbalExpressions;
begin
  Add('(' + REGEX_GROUP_LABEL + '[^' + Sanitize(AValue) + ']+)');
  Result := Self;
end;

// Any of the listed chars
function TVerbalExpressions.AnyOf(AValue: string): TVerbalExpressions;
begin
  Add('[' + Sanitize(AValue) + ']');
  Result := Self;
end;

function TVerbalExpressions.Any(AValue: string): TVerbalExpressions;
begin
  Result := AnyOf(AValue);
end;

// Adds a set of literal characters (same as AnyOf); use the array form
// for real ranges such as 'a-z'.
function TVerbalExpressions.Range(AValue: string): TVerbalExpressions;
begin
  Add('[' + Sanitize(AValue) + ']');
  Result := Self;
end;

// Adds a range to our expression
// example:
//   VE.Range(['a-z','0-9']);
function TVerbalExpressions.Range(AValue: array of string): TVerbalExpressions;
var
  i: integer;
  s: string;
begin
  s := '';
  for i := low(AValue) to high(AValue) do
  begin
    s += AValue[i];
  end;
  Add('[' + s + ']');
  Result := Self;
end;

// Accept any non-empty string
function TVerbalExpressions.Something: TVerbalExpressions;
begin
  Add('(' + REGEX_GROUP_LABEL + '.+)');
  Result := Self;
end;

function TVerbalExpressions.Anything: TVerbalExpressions;
begin
  Add('(' + REGEX_GROUP_LABEL + '.*)');
  Result := Self;
end;

// Match any digit
function TVerbalExpressions.Digit: TVerbalExpressions;
begin
  Add('\d');
  Result := Self;
end;

// Match any alpha numeric
function TVerbalExpressions.word: TVerbalExpressions;
begin
  Add('\w');
  Result := Self;
end;

// Match tabs
function TVerbalExpressions.Tab: TVerbalExpressions;
begin
  Add('\t');
  Result := Self;
end;

function TVerbalExpressions.LineBreak: TVerbalExpressions;
begin
  Add('(' + REGEX_GROUP_LABEL + '\n|(\r\n))');
  Result := Self;
end;

function TVerbalExpressions.BR: TVerbalExpressions;
begin
  Result := LineBreak;
end;

end.

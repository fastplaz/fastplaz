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


  inspiration from:
    https://github.com/VerbalExpressions/
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
    FExpressions: TStringList;
    FLastAdded: string;
    FModifiers: string;
    FPrefixes: string;
    FSource: string;
    FSuffixes: string;
    function getExpression: string;
  public
    constructor Create;
    destructor Destroy; override;

    function IsMatch(const AText: string): boolean;

    procedure Clean;
    function Sanitize(AValue: string): string;

    function AddValue(AValue: string): TVerbalExpressions;
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
    property Modifiers: string read FModifiers write FModifiers;
    property LastAdded: string read FLastAdded;
  end;


implementation

const
  REGEX_GROUP_LABEL = ''; //'?:';

{ TVerbalExpressions }

function TVerbalExpressions.getExpression: string;
begin
  //Result := '/' + FPrefixes + FSource + FSuffixes + '/' + FModifiers;
  Result := FPrefixes + FSource + FSuffixes;
end;

constructor TVerbalExpressions.Create;
begin
  FExpressions := TStringList.Create;
  FSource := '';
  FModifiers := 'm';
  FPrefixes := '';
  FSuffixes := '';
  FLastAdded := '';
end;

destructor TVerbalExpressions.Destroy;
begin
  FExpressions.Free;
  inherited Destroy;
end;

function TVerbalExpressions.IsMatch(const AText: string): boolean;
begin
  Result := preg_match(getExpression, AText);
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

function TVerbalExpressions.Sanitize(AValue: string): string;
begin
  //TODO: check with preg_quote
  Result := QuoteRegExprMetaChars(AValue);
  Result := StringReplace(Result, '/', '\/', [rfReplaceAll]);
end;

function TVerbalExpressions.AddValue(AValue: string): TVerbalExpressions;
begin
  FLastAdded := Avalue;
  FSource := FSource + AValue;
  FExpressions.Text := Trim(FExpressions.Text) + AValue;
  Result := Self;
end;

// Mark the expression to start at the beginning of the line.
function TVerbalExpressions.StartOfLine(AEnable: boolean): TVerbalExpressions;
begin
  if AEnable then
    FPrefixes := '^';
  Result := Self;
end;

// Mark the expression to end at the last character of the line.
function TVerbalExpressions.EndOfLine(AEnable: boolean): TVerbalExpressions;
begin
  if AEnable then
    FSuffixes := '$';
  Result := Self;
end;

// Add a string to the expression
function TVerbalExpressions.AndThen(AValue: string; ASanitize: boolean
  ): TVerbalExpressions;
begin
  if ASanitize then
    AddValue('(' + REGEX_GROUP_LABEL + Sanitize(AValue) + ')')
  else
    AddValue('(' + REGEX_GROUP_LABEL + AValue + ')');
  Result := Self;
end;

function TVerbalExpressions.Has(AValue: string; ASanitize: boolean
  ): TVerbalExpressions;
begin
  Result := AndThen(AValue, ASanitize);
end;

function TVerbalExpressions.Have(AValue: string; ASanitize: boolean
  ): TVerbalExpressions;
begin
  Result := AndThen(AValue, ASanitize);
end;

function TVerbalExpressions.Find(AValue: string; ASanitize: boolean
  ): TVerbalExpressions;
begin
  Result := AndThen(AValue, ASanitize);
end;

// Add a string to the expression that might appear once (or not).
function TVerbalExpressions.Maybe(AValue: string): TVerbalExpressions;
begin
  AddValue('(' + REGEX_GROUP_LABEL + Sanitize(AValue) + ')?');
  Result := Self;
end;

// Anything but this chars
function TVerbalExpressions.AnythingBut(AValue: string): TVerbalExpressions;
begin
  AddValue('(' + REGEX_GROUP_LABEL + '[^' + Sanitize(AValue) + ']*)');
  Result := Self;
end;

// Anything non-empty except for these chars
function TVerbalExpressions.SomethingBut(AValue: string): TVerbalExpressions;
begin
  AddValue('(' + REGEX_GROUP_LABEL + '[^' + Sanitize(AValue) + ']+b)');
  Result := Self;
end;

// Any of the listed chars
function TVerbalExpressions.AnyOf(AValue: string): TVerbalExpressions;
begin
  AddValue('[' + Sanitize(AValue) + ']');
  Result := Self;
end;

function TVerbalExpressions.Any(AValue: string): TVerbalExpressions;
begin
  Result := AnyOf(AValue);
end;

// Adds a range to our expression
function TVerbalExpressions.Range(AValue: string): TVerbalExpressions;
begin
  AddValue('[' + Sanitize(AValue) + ']');
  Result := Self;
end;

// Accept any non-empty string
function TVerbalExpressions.Something: TVerbalExpressions;
begin
  AddValue('(' + REGEX_GROUP_LABEL + '.+)');
  Result := Self;
end;

function TVerbalExpressions.Anything: TVerbalExpressions;
begin
  AddValue('(' + REGEX_GROUP_LABEL + '.*)');
  Result := Self;
end;

// Match any digit
function TVerbalExpressions.Digit: TVerbalExpressions;
begin
  AddValue('\\d');
  Result := Self;
end;

// Match any alpha numeric
function TVerbalExpressions.word: TVerbalExpressions;
begin
  AddValue('\\w');
  Result := Self;
end;

// Match tabs
function TVerbalExpressions.Tab: TVerbalExpressions;
begin
  AddValue('\\t');
  Result := Self;
end;

function TVerbalExpressions.LineBreak: TVerbalExpressions;
begin
  AddValue('(' + REGEX_GROUP_LABEL + '\\n|(\\r\\n))');
  Result := Self;
end;

function TVerbalExpressions.BR: TVerbalExpressions;
begin
  Result := LineBreak;
end;

end.

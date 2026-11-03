unit security_util;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_app, common, hash_tools,
  Math, Classes, SysUtils;

const
  SALT_DELIM = '$';
  SALT_PREFIX = '8';
  PASSWORD_LENGTH_MIN = 5;

  CSRFTOKEN_LENGTH = 12;
  CSRFTOKEN_KEY = '_csrf_token_key';

type

  { TSecurityUtil }

  TSecurityUtil = class
  private
  public
    function CheckSaltedHash(const UnhasedData: string;
      const SaltedHash: string): boolean;
    function GeneratePassword: string;
    function GenerateSaltedHash(const UnhasedData: string): string;
    function GenerateSaltedHash(const UnhasedData: string; SaltString: string;
      SaltDelimiter: string = SALT_DELIM): string;

    function GenerateCSRF(const FormID: string = ''): string;

    // at least PASSWORD_LENGTH_MIN characters
    class function IsPasswordAcceptable(const APassword: string): boolean;
    // a plausible e-mail address (no spaces, one @, a dot in the domain)
    class function IsValidEmail(const AEmail: string): boolean;
  end;

// constant time comparison for hashes and tokens
function TimingSafeEquals(const A, B: string): boolean;

implementation

function TimingSafeEquals(const A, B: string): boolean;
var
  i, diff: integer;
begin
  diff := Length(A) xor Length(B);
  for i := 1 to Length(A) do
    if i <= Length(B) then
      diff := diff or (Ord(A[i]) xor Ord(B[i]))
    else
      diff := diff or Ord(A[i]);
  Result := diff = 0;
end;

{ TSecurityUtil }

function TSecurityUtil.CheckSaltedHash(const UnhasedData: string;
  const SaltedHash: string): boolean;
var
  lst: TStrings;
  dataHash: string;
begin
  Result := False;
  if (UnhasedData = '') or (SaltedHash = '') then
    Exit;
  if Pos(SALT_DELIM, SaltedHash) = 0 then
    Exit;
  lst := Explode(SaltedHash, SALT_DELIM);
  try
    if lst.Count < 3 then
      Exit;
    dataHash := GenerateSaltedHash(UnhasedData, lst[1]);
    Result := TimingSafeEquals(dataHash, SaltedHash);
  finally
    lst.Free;
  end;
end;

class function TSecurityUtil.IsPasswordAcceptable(const APassword: string): boolean;
begin
  Result := Length(APassword) >= PASSWORD_LENGTH_MIN;
end;

class function TSecurityUtil.IsValidEmail(const AEmail: string): boolean;
begin
  Result := (Length(AEmail) <= 254) and (Pos(' ', AEmail) = 0) and isEmail(AEmail);
end;

function TSecurityUtil.GeneratePassword: string;
var
  min, max: integer;
begin
  min := RandomRange(PASSWORD_LENGTH_MIN, 15);
  max := RandomRange(PASSWORD_LENGTH_MIN + 3, 15);
  Result := RandomString(min, max, False, True, True, False, True, False, False, '');
end;

function TSecurityUtil.GenerateSaltedHash(const UnhasedData: string): string;
var
  saltString: string;
begin
  saltString := RandomString(5, 5, False, False, True, False, True, False, False, '');
  Result := GenerateSaltedHash(UnhasedData, saltString, SALT_DELIM);
end;

function TSecurityUtil.GenerateSaltedHash(const UnhasedData: string;
  SaltString: string; SaltDelimiter: string): string;
begin
  Result := SALT_PREFIX + SALT_DELIM + SaltString + SALT_DELIM +
    LowerCase(SHA256(saltString + UnhasedData));
end;

function TSecurityUtil.GenerateCSRF(const FormID: string): string;
begin
  Result := RandomString(CSRFTOKEN_LENGTH, FormID);
end;

end.

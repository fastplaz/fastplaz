unit security_util;

{$mode objfpc}{$H+}

interface

uses
  fpcgi, common, hash_tools,
  math, Classes, SysUtils;

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
  end;

implementation

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
  if lst.Count < 3 then
  begin
    lst.Free;
    Exit;
  end;

  dataHash := GenerateSaltedHash(UnhasedData, lst[1]);
  if dataHash = SaltedHash then
    Result := True;

  lst.Free;
end;

function TSecurityUtil.GeneratePassword: string;
var
  min, max: integer;
begin
  Randomize;
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

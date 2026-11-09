unit hash_tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// SHA-256 digest of str as an uppercase hex string (64 characters)
function SHA256(const str: string): ansistring;

implementation

uses DCPsha256;

function SHA256(const str: string): ansistring;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;
  i: integer;
begin
  Result := '';
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(str);
    Hash.Final(Digest);
    for i := Low(Digest) to High(Digest) do
      Result := Result + IntToHex(Digest[i], 2);
  finally
    Hash.Free;
  end;
end;

end.

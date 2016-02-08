unit hash_tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function SHA256(const str: string): ansistring;

implementation

uses DCPsha256;

function SHA256(const str: string): ansistring;
var
  Hash: TDCP_sha256;
  Digest: array[0..31] of byte;
  i: integer;
  s: ansistring;
begin
  s := '';
  Hash := TDCP_sha256.Create(nil);
  if hash <> nil then
  begin
    try
      Hash.Init;
      Hash.UpdateStr(str);
      Hash.Final(Digest);
      for i := 0 to length(Digest) - 1 do
        s := s + IntToHex(Digest[i], 2);
    except
    end;
    hash.Free;
  end;
  Result := s;
end;

end.


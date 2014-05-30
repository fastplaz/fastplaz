unit language_lib;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef LCL}
  Translations,
  {$endif}
  fastplaz_handler, fphttp, HTTPDefs,
  Classes, SysUtils;



function __( str:string):string;

var
  LANG, FallbackLANG : string;

implementation

uses common;

function __(str: string): string;
var
//  strl : TStringList;
  po_file : string;
  i : integer;
begin
  Result := str;
  po_file:= 'locale\fastplaz.'+LANG+'.po';
  if not FileExists( po_file) then Exit;

  {$ifdef LCL}
  // if using lcl lazarus
  try
    with TPOFile.Create( po_file) do begin
      Result := Translate( str, str);
      Free;
    end;
  except
    on e: Exception do begin
      die( e.Message);
    end;
  end;
  // if using lcl lazarus - end
  {$else}
  with TStringList.Create do
  begin
      LoadFromFile( po_file);
      i := IndexOf( 'msgid "'+str+'"');
      if i <> -1 then
      begin
        Result := Copy(ValueFromIndex[i+1], 9, length(ValueFromIndex[i+1])-9);
        if Result = '' then
          Result := str;
      end;
      Free;
  end;
  {$endif}

end;



initialization

end.


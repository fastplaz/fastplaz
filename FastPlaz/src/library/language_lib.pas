unit language_lib;

{
  Minimal gettext style lookup: __('text') returns the msgstr of the
  matching msgid in locale/fastplaz.<LANG>.po (relative to the working
  directory), falling back to locale/fastplaz.<FallbackLANG>.po, and to
  the text itself when nothing matches.

  The .po file is parsed once per process and reloaded when it changes
  on disk; it used to be read and scanned on every call. msgid/msgstr are
  matched exactly (case sensitive), may span several quoted lines and
  understand the escapes \" \\ \n \t.
}

{$mode objfpc}{$H+}

interface

uses
  {$ifdef LCL}
  Translations,
  {$endif}
  Classes, SysUtils;

function __(str: string; AutoCreate: boolean = False): string;

// Parses a .po file into key = msgid, value = msgstr pairs (public for
// tests and tooling). Returns False when the file does not exist.
function LoadPOFile(const AFileName: string; AMsgIds, AMsgStrs: TStrings): boolean;
// Forgets the cached translations so the next call reads the file again.
procedure ClearLanguageCache;

var
  LANG, FallbackLANG: string;

implementation

uses IniFiles;

var
  CacheLock: TRTLCriticalSection;
  CacheFile: string = '';
  CacheAge: longint = -1;
  CacheIds: THashedStringList = nil;
  CacheStrs: TStringList = nil;

// "..." with gettext escapes -> raw text; non-string lines return False
function UnquotePO(const ALine: string; out AValue: string): boolean;
var
  s: string;
  i: integer;
begin
  Result := False;
  AValue := '';
  s := Trim(ALine);
  if (Length(s) < 2) or (s[1] <> '"') or (s[Length(s)] <> '"') then
    Exit;
  i := 2;
  while i < Length(s) do
  begin
    if (s[i] = '\') and (i + 1 < Length(s)) then
    begin
      Inc(i);
      case s[i] of
        'n': AValue := AValue + #10;
        't': AValue := AValue + #9;
        'r': AValue := AValue + #13;
        else
          AValue := AValue + s[i];
      end;
    end
    else
      AValue := AValue + s[i];
    Inc(i);
  end;
  Result := True;
end;

function LoadPOFile(const AFileName: string; AMsgIds, AMsgStrs: TStrings): boolean;
var
  lines: TStringList;
  i: integer;
  line, part, msgid, msgstr: string;
  state: (psNone, psId, psStr);

  procedure Flush;
  begin
    if (state = psStr) and (msgid <> '') then
    begin
      AMsgIds.Add(msgid);
      AMsgStrs.Add(msgstr);
    end;
    msgid := '';
    msgstr := '';
    state := psNone;
  end;

begin
  Result := False;
  AMsgIds.Clear;
  AMsgStrs.Clear;
  if not FileExists(AFileName) then
    Exit;
  lines := TStringList.Create;
  try
    lines.LoadFromFile(AFileName);
    state := psNone;
    msgid := '';
    msgstr := '';
    for i := 0 to lines.Count - 1 do
    begin
      line := Trim(lines[i]);
      if (line = '') or (line[1] = '#') then
        Continue;
      if Copy(line, 1, 6) = 'msgid ' then
      begin
        Flush;
        if UnquotePO(Copy(line, 7, MaxInt), part) then
        begin
          msgid := part;
          state := psId;
        end;
      end
      else if Copy(line, 1, 7) = 'msgstr ' then
      begin
        if (state = psId) and UnquotePO(Copy(line, 8, MaxInt), part) then
        begin
          msgstr := part;
          state := psStr;
        end
        else
          state := psNone;
      end
      else if UnquotePO(line, part) then
      begin
        // continuation line of the previous msgid/msgstr
        case state of
          psId: msgid := msgid + part;
          psStr: msgstr := msgstr + part;
        end;
      end
      else
        state := psNone; // msgctxt, msgid_plural, ...: not supported
    end;
    Flush;
    Result := True;
  finally
    lines.Free;
  end;
end;

procedure ClearLanguageCache;
begin
  EnterCriticalSection(CacheLock);
  try
    CacheFile := '';
    CacheAge := -1;
    if Assigned(CacheIds) then
      CacheIds.Clear;
    if Assigned(CacheStrs) then
      CacheStrs.Clear;
  finally
    LeaveCriticalSection(CacheLock);
  end;
end;

function POFileFor(const ALang: string): string;
begin
  Result := '';
  if Trim(ALang) = '' then
    Exit;
  Result := 'locale' + DirectorySeparator + 'fastplaz.' + ALang + '.po';
end;

// (re)loads the cache for AFile when the file is new or changed
procedure EnsureCache(const AFile: string);
var
  age: longint;
begin
  age := FileAge(AFile);
  if (AFile = CacheFile) and (age = CacheAge) then
    Exit;
  if not Assigned(CacheIds) then
  begin
    CacheIds := THashedStringList.Create;
    CacheIds.CaseSensitive := True;
    CacheStrs := TStringList.Create;
  end;
  LoadPOFile(AFile, CacheIds, CacheStrs);
  CacheFile := AFile;
  CacheAge := age;
end;

function __(str: string; AutoCreate: boolean): string;
var
  po_file: string;
  i: integer;
begin
  Result := str;
  po_file := POFileFor(LANG);
  if (po_file = '') or not FileExists(po_file) then
  begin
    po_file := POFileFor(FallbackLANG);
    if (po_file = '') or not FileExists(po_file) then
      Exit;
  end;

  {$ifdef LCL}
  // if using lcl lazarus
  try
    with TPOFile.Create(po_file) do
    try
      Result := Translate(str, str);
    finally
      Free;
    end;
  except
    Result := str;
  end;
  {$else}
  EnterCriticalSection(CacheLock);
  try
    try
      EnsureCache(po_file);
      i := CacheIds.IndexOf(str);
      if (i <> -1) and (CacheStrs[i] <> '') then
        Result := CacheStrs[i];
    except
      // an unreadable file must not break the request
      Result := str;
    end;
  finally
    LeaveCriticalSection(CacheLock);
  end;
  {$endif}

  if AutoCreate then
  begin
    // prepare for autocreate language
  end;
end;

initialization
  InitCriticalSection(CacheLock);

finalization
  CacheIds.Free;
  CacheStrs.Free;
  DoneCriticalSection(CacheLock);

end.

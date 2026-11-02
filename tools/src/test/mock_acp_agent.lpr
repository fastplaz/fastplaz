program mock_acp_agent;

{
  A fake ACP agent for AI Plane tests and the tester app. It speaks the same
  JSON-RPC lines as a real runtime but needs no network, account or tokens.

  The prompt text picks the scenario:
    hello        three text chunks
    think        a thought chunk, then text
    plan         a plan update
    read <path>  tool call that asks the client for fs/read_text_file
    permission   asks the client for permission, reports the chosen option
    slow         20 chunks, 100 ms apart, stops early on session/cancel
    crash        sends one chunk, then exits with code 3
    fail         answers the prompt with a JSON-RPC error
    model?       reports the current model and effort
    unicode      replies with non-ASCII text
    markdown     a markdown answer (headings, lists, code, table, ...),
                 streamed line by line
    anything     "echo: <text>"

  Options: --banner prints a non-JSON line on stdout first.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, syncobjs, fpjson, jsonparser;

type

  { TStdinReader }

  TStdinReader = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  Lock: TCriticalSection;
  Inbox: TStringList;
  InputClosed: boolean = False;
  NextId: integer = 1000;
  SessionCounter: integer = 0;
  CurrentModel: string = 'mock/fast';
  CurrentEffort: string = 'medium';
  CancelRequested: boolean = False;

procedure TStdinReader.Execute;
var
  stream: THandleStream;
  buf: array[0..4095] of char;
  pending, line, chunkText: string;
  n, p: integer;
begin
  stream := THandleStream.Create(StdInputHandle);
  try
    pending := '';
    repeat
      n := stream.Read(buf, SizeOf(buf));
      if n <= 0 then
        Break;
      SetString(chunkText, PChar(@buf[0]), n);
      pending := pending + chunkText;
      repeat
        p := Pos(#10, pending);
        if p = 0 then
          Break;
        line := Trim(Copy(pending, 1, p - 1));
        Delete(pending, 1, p);
        if line <> '' then
        begin
          Lock.Enter;
          try
            Inbox.Add(line);
          finally
            Lock.Leave;
          end;
        end;
      until False;
    until Terminated;
  finally
    stream.Free;
    InputClosed := True;
  end;
end;

procedure Send(AMessage: TJSONObject);
begin
  AMessage.Add('jsonrpc', '2.0');
  WriteLn(AMessage.AsJSON);
  Flush(Output);
  AMessage.Free;
end;

function TakeLine(out ALine: string): boolean;
begin
  Lock.Enter;
  try
    Result := Inbox.Count > 0;
    if Result then
    begin
      ALine := Inbox[0];
      Inbox.Delete(0);
    end;
  finally
    Lock.Leave;
  end;
end;

procedure Respond(AId: TJSONData; AResult: TJSONData);
var
  msg: TJSONObject;
begin
  msg := TJSONObject.Create;
  msg.Add('id', AId.Clone);
  msg.Add('result', AResult);
  Send(msg);
end;

procedure RespondError(AId: TJSONData; ACode: integer; const AMessage: string);
var
  msg: TJSONObject;
begin
  msg := TJSONObject.Create;
  msg.Add('id', AId.Clone);
  msg.Add('error', TJSONObject.Create(['code', ACode, 'message', AMessage]));
  Send(msg);
end;

procedure Update(const ASessionId: string; AUpdate: TJSONObject);
var
  msg: TJSONObject;
begin
  msg := TJSONObject.Create;
  msg.Add('method', 'session/update');
  msg.Add('params', TJSONObject.Create(['sessionId', ASessionId, 'update', AUpdate]));
  Send(msg);
end;

procedure Chunk(const ASessionId, AKind, AText: string);
begin
  Update(ASessionId, TJSONObject.Create(['sessionUpdate', AKind,
    'content', TJSONObject.Create(['type', 'text', 'text', AText])]));
end;

function ConfigOptions: TJSONArray;
begin
  Result := TJSONArray.Create([
    TJSONObject.Create(['id', 'model', 'name', 'Model', 'category', 'model',
      'type', 'select', 'currentValue', CurrentModel, 'options', TJSONArray.Create([
        TJSONObject.Create(['value', 'mock/fast', 'name', 'Mock Fast']),
        TJSONObject.Create(['group', 'smart', 'name', 'Smart', 'options', TJSONArray.Create([
          TJSONObject.Create(['value', 'mock/smart', 'name', 'Mock Smart'])])])])]),
    TJSONObject.Create(['id', 'effort', 'name', 'Effort', 'category', 'thought_level',
      'type', 'select', 'currentValue', CurrentEffort, 'options', TJSONArray.Create([
        TJSONObject.Create(['value', 'low', 'name', 'Low']),
        TJSONObject.Create(['value', 'medium', 'name', 'Medium']),
        TJSONObject.Create(['value', 'high', 'name', 'High'])])])]);
end;

procedure HandleMessage(AMessage: TJSONObject); forward;

{ Sends a request to the client and waits for its answer. Other messages that
  arrive meanwhile (session/cancel) are handled as usual. }
function Call(const AMethod: string; AParams: TJSONObject): TJSONObject;
var
  msg: TJSONObject;
  id: integer;
  line: string;
  data: TJSONData;
begin
  Result := nil;
  Inc(NextId);
  id := NextId;
  msg := TJSONObject.Create;
  msg.Add('id', id);
  msg.Add('method', AMethod);
  msg.Add('params', AParams);
  Send(msg);
  repeat
    if not TakeLine(line) then
    begin
      if InputClosed then
        Exit;
      Sleep(5);
      Continue;
    end;
    data := GetJSON(line);
    if (data is TJSONObject) and (TJSONObject(data).Find('method') = nil) and
      (TJSONObject(data).Get('id', -1) = id) then
      Exit(TJSONObject(data));
    if data is TJSONObject then
      HandleMessage(TJSONObject(data));
    data.Free;
  until False;
end;

{ Lets queued notifications (session/cancel) through during a long turn. }
procedure DrainInbox;
var
  line: string;
  data: TJSONData;
begin
  while TakeLine(line) do
  begin
    data := GetJSON(line);
    try
      if data is TJSONObject then
        HandleMessage(TJSONObject(data));
    finally
      data.Free;
    end;
  end;
end;

procedure RunPrompt(AId: TJSONData; AParams: TJSONObject);
var
  sid, text, path, stopReason, answer: string;
  i: integer;
  reply, outcome: TJSONObject;
  prompt: TJSONArray;
begin
  sid := AParams.Get('sessionId', '');
  prompt := AParams.Get('prompt', TJSONArray(nil));
  text := '';
  if Assigned(prompt) and (prompt.Count > 0) then
    text := TJSONObject(prompt[0]).Get('text', '');
  stopReason := 'end_turn';
  CancelRequested := False;

  if text = 'hello' then
  begin
    Chunk(sid, 'agent_message_chunk', 'Hello');
    Chunk(sid, 'agent_message_chunk', ' from');
    Chunk(sid, 'agent_message_chunk', ' mock');
  end
  else if text = 'think' then
  begin
    Chunk(sid, 'agent_thought_chunk', 'Let me think.');
    Chunk(sid, 'agent_message_chunk', 'Done thinking.');
  end
  else if text = 'plan' then
    Update(sid, TJSONObject.Create(['sessionUpdate', 'plan', 'entries', TJSONArray.Create([
      TJSONObject.Create(['content', 'Read the unit', 'status', 'completed', 'priority', 'high']),
      TJSONObject.Create(['content', 'Write the test', 'status', 'pending', 'priority', 'medium'])])]))
  else if Copy(text, 1, 5) = 'read ' then
  begin
    path := Copy(text, 6, MaxInt);
    Update(sid, TJSONObject.Create(['sessionUpdate', 'tool_call', 'toolCallId', 'call_1',
      'title', 'Read ' + ExtractFileName(path), 'kind', 'read', 'status', 'pending',
      'locations', TJSONArray.Create([TJSONObject.Create(['path', path])])]));
    reply := Call('fs/read_text_file', TJSONObject.Create(['sessionId', sid, 'path', path,
      'line', 2, 'limit', 1]));
    try
      if Assigned(reply) and Assigned(reply.Find('result')) then
      begin
        answer := reply.Objects['result'].Get('content', '');
        Update(sid, TJSONObject.Create(['sessionUpdate', 'tool_call_update',
          'toolCallId', 'call_1', 'status', 'completed', 'content', TJSONArray.Create([
          TJSONObject.Create(['type', 'content', 'content',
            TJSONObject.Create(['type', 'text', 'text', answer])])])]));
        Chunk(sid, 'agent_message_chunk', 'line 2: ' + Trim(answer));
      end
      else
      begin
        Update(sid, TJSONObject.Create(['sessionUpdate', 'tool_call_update',
          'toolCallId', 'call_1', 'status', 'failed']));
        Chunk(sid, 'agent_message_chunk', 'read failed');
      end;
    finally
      reply.Free;
    end;
  end
  else if text = 'permission' then
  begin
    reply := Call('session/request_permission', TJSONObject.Create(['sessionId', sid,
      'toolCall', TJSONObject.Create(['toolCallId', 'call_2', 'title', 'Delete temp.txt',
        'kind', 'delete', 'rawInput', TJSONObject.Create(['path', 'temp.txt'])]),
      'options', TJSONArray.Create([
        TJSONObject.Create(['optionId', 'yes', 'name', 'Allow', 'kind', 'allow_once']),
        TJSONObject.Create(['optionId', 'no', 'name', 'Reject', 'kind', 'reject_once'])])]));
    try
      answer := 'none';
      if Assigned(reply) then
      begin
        outcome := TJSONObject(reply.FindPath('result.outcome'));
        if Assigned(outcome) then
        begin
          answer := outcome.Get('outcome', '');
          if answer = 'selected' then
            answer := outcome.Get('optionId', '');
        end;
      end;
      // A client cancels by answering "cancelled" and then sending
      // session/cancel; give the notification a moment to arrive.
      Sleep(50);
      DrainInbox;
      if CancelRequested then
        stopReason := 'cancelled';
      Chunk(sid, 'agent_message_chunk', 'permission: ' + answer);
    finally
      reply.Free;
    end;
  end
  else if text = 'slow' then
  begin
    for i := 1 to 20 do
    begin
      DrainInbox;
      if CancelRequested then
      begin
        stopReason := 'cancelled';
        Break;
      end;
      Chunk(sid, 'agent_message_chunk', IntToStr(i) + ' ');
      Sleep(100);
    end;
  end
  else if text = 'crash' then
  begin
    Chunk(sid, 'agent_message_chunk', 'about to crash');
    Halt(3);
  end
  else if text = 'fail' then
  begin
    RespondError(AId, -32000, 'mock failure');
    Exit;
  end
  else if text = 'model?' then
    Chunk(sid, 'agent_message_chunk', 'model=' + CurrentModel + ' effort=' + CurrentEffort)
  else if text = 'unicode' then
    Chunk(sid, 'agent_message_chunk', 'Halo dunia ' + #$E2#$9C#$93 + ' caf' + #$C3#$A9)
  else if text = 'markdown' then
  begin
    Chunk(sid, 'agent_message_chunk', '## Ringkasan perubahan' + #10);
    Chunk(sid, 'agent_message_chunk', 'Unit `tools_lib` sekarang **lebih aman** dan *lebih cepat*. Detail ada di [dokumentasi](https://fastplaz.com/docs) atau https://github.com/fastplaz/fastplaz.' + #10);
    Chunk(sid, 'agent_message_chunk', '' + #10);
    Chunk(sid, 'agent_message_chunk', '### Langkah' + #10);
    Chunk(sid, 'agent_message_chunk', '1. Baca `SanitizeIdentifier()` di **tools_lib**' + #10);
    Chunk(sid, 'agent_message_chunk', '2. Jalankan test:' + #10);
    Chunk(sid, 'agent_message_chunk', '   `run_tests.bat -a`' + #10);
    Chunk(sid, 'agent_message_chunk', '3. Commit' + #10);
    Chunk(sid, 'agent_message_chunk', '' + #10);
    Chunk(sid, 'agent_message_chunk', '- Perubahan utama' + #10);
    Chunk(sid, 'agent_message_chunk', '  - nama modul dibersihkan' + #10);
    Chunk(sid, 'agent_message_chunk', '  - ~~fungsi lama~~ dihapus' + #10);
    Chunk(sid, 'agent_message_chunk', '- Catatan kecil' + #10);
    Chunk(sid, 'agent_message_chunk', '' + #10);
    Chunk(sid, 'agent_message_chunk', '> Catatan: jalankan `lazbuild -B` bila hanya file .lfm yang berubah.' + #10);
    Chunk(sid, 'agent_message_chunk', '' + #10);
    Chunk(sid, 'agent_message_chunk', '```pascal' + #10);
    Chunk(sid, 'agent_message_chunk', 'function SanitizeIdentifier(const AName: string): string;' + #10);
    Chunk(sid, 'agent_message_chunk', 'begin' + #10);
    Chunk(sid, 'agent_message_chunk', '  Result := Trim(AName);  // komentar panjang supaya baris ini lebih lebar dari kotak kode dan harus dipotong' + #10);
    Chunk(sid, 'agent_message_chunk', 'end;' + #10);
    Chunk(sid, 'agent_message_chunk', '```' + #10);
    Chunk(sid, 'agent_message_chunk', '' + #10);
    Chunk(sid, 'agent_message_chunk', '| Unit | Test | Status |' + #10);
    Chunk(sid, 'agent_message_chunk', '|---|---|:-:|' + #10);
    Chunk(sid, 'agent_message_chunk', '| tools_lib | 12 | **lulus** |' + #10);
    Chunk(sid, 'agent_message_chunk', '| regex_tester_lib | 16 | lulus |' + #10);
    Chunk(sid, 'agent_message_chunk', '| aiplane_markdown | 33 | baru |' + #10);
    Chunk(sid, 'agent_message_chunk', '' + #10);
    Chunk(sid, 'agent_message_chunk', '---' + #10);
    Chunk(sid, 'agent_message_chunk', 'Selesai. Variabel seperti my_var_name dan 2*3*4 tetap apa adanya.');
  end
  else
    Chunk(sid, 'agent_message_chunk', 'echo: ' + text);

  Update(sid, TJSONObject.Create(['sessionUpdate', 'usage_update', 'used', 42, 'size', 1000]));
  Respond(AId, TJSONObject.Create(['stopReason', stopReason]));
end;

procedure HandleMessage(AMessage: TJSONObject);
var
  method, configId: string;
  id: TJSONData;
  params: TJSONObject;
begin
  method := AMessage.Get('method', '');
  id := AMessage.Find('id');
  params := AMessage.Get('params', TJSONObject(nil));
  if method = '' then
    Exit; // a response nobody waits for
  if method = 'initialize' then
    Respond(id, TJSONObject.Create(['protocolVersion', 1,
      'agentCapabilities', TJSONObject.Create(['loadSession', False]),
      'agentInfo', TJSONObject.Create(['name', 'MockAgent', 'version', '1.0']),
      'authMethods', TJSONArray.Create]))
  else if method = 'session/new' then
  begin
    Inc(SessionCounter);
    Respond(id, TJSONObject.Create(['sessionId', 'mock_session_' + IntToStr(SessionCounter),
      'configOptions', ConfigOptions]));
  end
  else if method = 'session/set_config_option' then
  begin
    configId := params.Get('configId', '');
    if configId = 'model' then
      CurrentModel := params.Get('value', '')
    else if configId = 'effort' then
      CurrentEffort := params.Get('value', '');
    Respond(id, TJSONObject.Create(['configOptions', ConfigOptions]));
  end
  else if method = 'session/prompt' then
    RunPrompt(id, params)
  else if method = 'session/cancel' then
    CancelRequested := True
  else if Assigned(id) then
    RespondError(id, -32601, 'Method not found: ' + method);
end;

var
  reader: TStdinReader;
  line: string;
  data: TJSONData;
begin
  DefaultSystemCodePage := CP_UTF8;
  SetTextCodePage(Output, CP_UTF8);
  Lock := TCriticalSection.Create;
  Inbox := TStringList.Create;
  if (ParamCount > 0) and (ParamStr(1) = '--banner') then
  begin
    WriteLn('Mock agent banner, not JSON');
    Flush(Output);
  end;
  WriteLn(StdErr, 'mock agent started');
  reader := TStdinReader.Create(False);
  repeat
    if TakeLine(line) then
    begin
      try
        data := GetJSON(line);
      except
        Continue;
      end;
      try
        if data is TJSONObject then
          HandleMessage(TJSONObject(data));
      finally
        data.Free;
      end;
    end
    else if InputClosed then
      Break
    else
      Sleep(5);
  until False;
  // The reader thread is blocked on stdin; the process is ending anyway.
  reader.FreeOnTerminate := False;
  Halt(0);
end.

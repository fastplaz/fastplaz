program tools_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF} // the MCP tests poll the server from a thread
  Classes, consoletestrunner, fastplaz_testrunner, test_regex_tester_lib, test_tools_lib,
  test_aiplane, test_aiplane_markdown, test_aiplane_mcp, test_aiplane_textedit;

type

  { TMyTestRunner }

  TMyTestRunner = class(TFastPlazTestRunner)
  protected
  end;

var
  App: TMyTestRunner;

begin
  // As in the IDE (LazUTF8): agent output is UTF-8.
  DefaultSystemCodePage := CP_UTF8;
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  App := TMyTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FastPlaz Tools Test';
  App.Run;
  App.Free;
end.

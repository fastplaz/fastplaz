program regex_tester_test;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, test_regex_tester_lib;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  end;

var
  App: TMyTestRunner;

begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  App := TMyTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FastPlaz Tools - Regex Tester Test';
  App.Run;
  App.Free;
end.

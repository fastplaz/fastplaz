program fastplaz_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF} // the SMTP and HTTP tests run a server thread
  Classes, consoletestrunner, fastplaz_testrunner, test_datetime_lib, test_redis_lib,
  test_json_lib, test_config_lib, test_common, test_math_lib,
  test_http_lib, test_mailer_lib, test_logutil_lib, test_smtp_lib,
  test_stemmingnazief_lib, test_html_lib, test_recaptcha_lib, test_rss_lib,
  test_solr_lib, test_serialize_lib, test_verbal_expressions_lib, test_nlp_lib,
  test_image_lib, test_language_lib, test_database_lib, test_session_controller,
  test_theme_controller, test_fastplaz_handler, test_fastplaz_app, test_helpers,
  test_integration, test_cms;

type

  { TMyTestRunner }

  TMyTestRunner = class(TFastPlazTestRunner)
  protected
  end;

var
  App: TMyTestRunner;

begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  App := TMyTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FastPlaz - Library Test';
  App.Run;
  App.Free;
end.

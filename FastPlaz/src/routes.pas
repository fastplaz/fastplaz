unit routes;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler, Classes, SysUtils;

implementation

uses about_controller, example_controller, docs_controller, info_controller;

initialization
  Route.Add('example', '^/example(/.*)?$', TExampleWebModule, ALL, False);
  Route.Add('docs', '^/docs(/.*)?$', TDocsModule, GET);
  Route.Add('info', '^/info(/.*)?$', TInfoModule);

end.



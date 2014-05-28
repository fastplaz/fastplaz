unit routes;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler, Classes, SysUtils;


implementation

uses about_controller, example_controller, docs_controller, info_controller;

initialization
  AddRoute('about', TAboutModule, false);
  AddRoute('example', TExampleWebModule, false);
  AddRoute('docs', TDocsModule);
  AddRoute('info', TInfoModule);

end.


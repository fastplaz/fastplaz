unit routes;

{$mode objfpc}{$H+}

interface

uses
  custom_handler, Classes, SysUtils;


implementation

uses about_controller, example_controller, docs_controller;

initialization
  AddRoute('about', TAboutModule, false);
  AddRoute('example', TExampleWebModule, false);
  AddRoute('docs', TDocsModule);

end.


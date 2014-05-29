unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

implementation

uses info_controller, main;

initialization
  AddRoute('main', TMainModule);
  //AddRoute('info', TInfoModule);

end.


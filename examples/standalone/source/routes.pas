unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

implementation

uses main_controller, serverinfo_controller;

initialization
  Route['/info'] := TServerInfoController;
  Route['/go'] := TGoController;
  Route['/'] := TMainController; // main controller, keep it last

end.

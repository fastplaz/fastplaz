unit app_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses info_controller, app_controller;

initialization
  Route[ '/'] := TFastplazController; // Main Controller

end.


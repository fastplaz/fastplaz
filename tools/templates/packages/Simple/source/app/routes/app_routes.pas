unit app_routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fastplaz_handler;

implementation

uses info_controller, app_controller, example_controller,
  database_controller, example_form_controller, example_foreach_controller;

initialization
  Route[ '/example/foreach'] := TForeachController;
  Route[ '/example/form'] := TFormController;
  Route[ '/example'] := TExampleController;
  Route[ '/database'] := TDatabaseController;
  Route[ '/'] := TFastplazController; // Main Controller

end.


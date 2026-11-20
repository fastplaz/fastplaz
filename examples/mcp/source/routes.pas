unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler, mcp_handler;

implementation

initialization
  Route['/mcp'] := TMcpController;

end.

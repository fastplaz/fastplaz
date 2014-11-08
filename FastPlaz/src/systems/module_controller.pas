unit module_controller;

// PREPARE FOR MODULE CONTROLLER

{$mode objfpc}{$H+}

interface

uses
  fpcgi, fpTemplate, fphttp, fpWeb, HTTPDefs, dateutils,
  dynlibs,
  Classes, SysUtils;

resourcestring
  __err_module_call_failed = 'Failed call module "%s": %s';

type

  // defined manually
  THandle = DWord;
  HWND = THandle;

  TModuleCallResult = record
    err: integer;
    message: WideString;
    ptr: Pointer;
  end;

  TModuleCall = function(AppHandle: HWND;
    var ModuleResult: TModuleCallResult): Pointer; cdecl;

  { TModUtil }

  TModUtil = class
  private
    FDirectory: string;
    procedure SetDirectory(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Directory: string read FDirectory write SetDirectory;
    function Load(ModuleName: string): TLibHandle;
    function Call(const ModuleName, FunctionName: string;
      Parameter: array of string): TModuleCallResult;
  end;

implementation

uses language_lib;

{ TModUtil }

procedure TModUtil.SetDirectory(AValue: string);
begin
  if FDirectory = AValue then
    Exit;
  FDirectory := IncludeTrailingPathDelimiter(AValue);
end;

constructor TModUtil.Create;
begin
  FDirectory := 'modules';
end;

destructor TModUtil.Destroy;
begin
  inherited Destroy;
end;

// example
//   ModUtl.Load('modulename');
function TModUtil.Load(ModuleName: string): TLibHandle;
var
  lib_handle: TLibHandle;
begin
  Result := 0;
  {$ifdef windows}
  ModuleName := ModuleName + '.dll';
  {$else}
  ModuleName := ModuleName + '.so';
  {$endif}
  if not FileExists(FDirectory + ModuleName) then
    Exit;
  lib_handle := LoadLibrary(PChar(FDirectory + ModuleName));
  Result := lib_handle;
end;

// example
//   Call( 'modulename', 'about', ['a=b','c=d']);
function TModUtil.Call(const ModuleName, FunctionName: string;
  Parameter: array of string): TModuleCallResult;
var
  lib_handle: TLibHandle;
  function_pointer: pointer;
  module_result: TModuleCallResult;
begin
  Result.err := -1;

  lib_handle := Load(ModuleName);
  if lib_handle = 0 then
    Exit;
  try
    function_pointer := GetProcAddress(lib_handle, FunctionName);
    if function_pointer = nil then
    begin
      FreeLibrary(lib_handle);
      Exit;
    end;
    TModuleCall(function_pointer)(0, module_result);
    Result := module_result;
  except
    on e: Exception do
    begin
      Result.message := format(__(__err_module_call_failed), [ModuleName, e.Message]);
    end;
  end;
  UnloadLibrary(lib_handle);
  FreeLibrary(lib_handle);
end;


end.

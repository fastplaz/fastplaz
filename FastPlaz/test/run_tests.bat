@echo off
rem Compile and run the FastPlaz library unit tests with instantfpc.
rem Usage: run_tests.bat [runner options]   (default: -a)

setlocal
cd /d "%~dp0"

where instantfpc >nul 2>nul
if errorlevel 1 set "PATH=C:\lazarus\fpc\3.2.2\bin\x86_64-win64;%PATH%"

rem SQLite client library for the database_lib suite (skipped when missing).
rem SQLITE3_LIBRARY wins; otherwise look in a few known folders, then PATH.
if not defined SQLITE3_LIBRARY call :find_sqlite "%ProgramFiles%\HeidiSQL"
if not defined SQLITE3_LIBRARY call :find_sqlite "%ProgramFiles(x86)%\HeidiSQL"
if not defined SQLITE3_LIBRARY call :find_sqlite "%LOCALAPPDATA%\Programs\HeidiSQL"
if not defined SQLITE3_LIBRARY call :find_sqlite "C:\app\dev\HeidiSQL"
if not defined SQLITE3_LIBRARY call :find_sqlite "%ProgramFiles%\SQLite"
if not defined SQLITE3_LIBRARY call :find_sqlite "C:\sqlite"
if not defined SQLITE3_LIBRARY for %%f in (sqlite3.dll) do if not "%%~$PATH:f"=="" set "SQLITE3_LIBRARY=%%~$PATH:f"
if defined SQLITE3_LIBRARY echo SQLite library: %SQLITE3_LIBRARY%

rem instantfpc does not accept -Fu paths on its command line, pass them here.
rem -B: always rebuild, the cache only tracks the main program source.
rem The unit paths mirror tools\fastplaz_runtime.lpk.
set "INSTANTFPCOPTIONS=-Fi.. -Fu..\..\tools\src -Fu..\src\systems -Fu..\src\library -Fu..\src\vendor -Fu..\src\app\about -Fu..\src\model -Fu..\src\util -Fu..\src\helper -Fu..\src\integration -Fu..\src\vendor\dcrypt -Fu..\src\cms\util -Fu..\src\cms\model -Fu..\src\cms\controller -Fu..\src\app\example -Fu..\src\app\example\models"

echo Compiling (instantfpc)...
if "%~1"=="" (
  instantfpc -B fastplaz_test.lpr -a
) else (
  instantfpc -B fastplaz_test.lpr %*
)
exit /b %errorlevel%

:find_sqlite
if exist "%~1\sqlite3.dll" set "SQLITE3_LIBRARY=%~1\sqlite3.dll"
exit /b 0

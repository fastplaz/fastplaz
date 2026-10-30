@echo off
rem Compile and run the unit tests with instantfpc.
rem Usage: run_tests.bat [runner options]   (default: -a)

setlocal
cd /d "%~dp0"

where instantfpc >nul 2>nul
if errorlevel 1 set "PATH=C:\lazarus\fpc\3.2.2\bin\x86_64-win64;%PATH%"

rem instantfpc does not accept -Fu paths on its command line, pass them here.
rem -B: always rebuild, the cache only tracks the main program source.
set "INSTANTFPCOPTIONS=-B -Fu.. -Fu..\..\..\FastPlaz\src\library"

if "%~1"=="" (
  instantfpc regex_tester_test.lpr -a
) else (
  instantfpc regex_tester_test.lpr %*
)
exit /b %errorlevel%

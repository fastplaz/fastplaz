@echo off
rem Compile and run the unit tests with instantfpc.
rem Usage: run_tests.bat [runner options]   (default: -a)

setlocal
cd /d "%~dp0"

where instantfpc >nul 2>nul
if errorlevel 1 set "PATH=C:\lazarus\fpc\3.2.2\bin\x86_64-win64;%PATH%"

rem instantfpc does not accept -Fu paths on its command line, pass them here.
rem -B: always rebuild, the cache only tracks the main program source.
set "INSTANTFPCOPTIONS=-B -Fu.. -Fu..\ai_plane -Fu..\..\..\FastPlaz\src\library -Fu..\..\..\FastPlaz\test"

rem The AI Plane tests talk to this fake agent.
echo Compiling mock_acp_agent...
if not exist lib mkdir lib
fpc -O1 -FUlib mock_acp_agent.lpr > lib\mock_acp_agent.txt 2>&1
if errorlevel 1 (
  type lib\mock_acp_agent.txt
  exit /b 1
)

echo Compiling (instantfpc)...
if "%~1"=="" (
  instantfpc tools_test.lpr -a
) else (
  instantfpc tools_test.lpr %*
)
exit /b %errorlevel%

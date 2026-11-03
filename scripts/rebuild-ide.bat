@echo off
setlocal

set LAZDIR=C:\lazarus
set STRIP=%LAZDIR%\fpc\3.2.2\bin\x86_64-win64\strip.exe

tasklist /FI "IMAGENAME eq lazarus.exe" 2>nul | find /I "lazarus.exe" >nul
if not errorlevel 1 (
  echo Lazarus masih berjalan, tutup dulu IDE-nya.
  pause
  exit /b 1
)

echo === Build Lazarus IDE ===
"%LAZDIR%\lazbuild.exe" --build-ide=
if errorlevel 1 (
  echo.
  echo Build GAGAL.
  pause
  exit /b 1
)

echo.
echo === Strip debug info ===
if exist "%STRIP%" (
  "%STRIP%" "%LAZDIR%\lazarus.exe"
) else (
  echo strip.exe tidak ditemukan, dilewati.
)

echo.
echo === Start Lazarus ===
rem same as the taskbar shortcut: no parameter, working dir = lazarus dir
start "" /D "%LAZDIR%" "%LAZDIR%\lazarus.exe"

endlocal

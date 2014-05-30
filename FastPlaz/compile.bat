@echo off

cd C:\lazarus-1.2.2\fpc\2.6.4\bin\i386-win32
mkdir lib
c:fpc fastplaz.lpr @extra.cfg

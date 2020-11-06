@echo off
del *~
rem del *.rst
rem del *.lrt
rem del *.lps
del /s *.or
del /s *.bak
del /s *.exe
del /s *.ppu
del /s *.o
del *.compiled
del /s /q lib\*
rmdir /s /q lib\
rmdir /s /q source\app\backup
rmdir /s /q source\app\lib\

timeout /t 3

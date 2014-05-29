@echo off
del *~
del *.rst
rem del *.lrt
rem del *.lps
del /s *.or
del /s *.bak
del /s *.exe
del /s *.ppu
del /s *.o
del *.compiled
rmdir /s /q lib\
rmdir /s /q backup\
rmdir /s /q src\backup
timeout /t 3
rmdir /s /q lib\

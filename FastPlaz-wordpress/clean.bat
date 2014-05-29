@echo off
del bin\fastplaz.bin
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
rmdir /s /q backup\
rmdir /s /q src\backup
rmdir /s /q src\library\backup
rmdir /s /q src\systems\backup
rmdir /s /q src\app\example\backup
rmdir /s /q src\app\example\models\backup
rmdir /s /q src\app\example\module\backup

rmdir /s /q src\app\wordpress\backup
rmdir /s /q src\app\wordpress\model\backup
rmdir /s /q src\app\wordpress\plugins\backup

rmdir /s /q src\lib\

timeout /t 3

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
rmdir /s /q backup\
rmdir /s /q src\backup
rmdir /s /q src\library\backup
rmdir /s /q src\systems\backup
rmdir /s /q src\vendor\backup
rmdir /s /q src\app\example\backup
rmdir /s /q src\app\example\models\backup
rmdir /s /q src\app\example\module\backup
rmdir /s /q src\integration\backup
rmdir /s /q src\cms\controller\backup
rmdir /s /q src\cms\model\backup
rmdir /s /q src\cms\util\backup

rmdir /s /q src\lib\

timeout /t 3

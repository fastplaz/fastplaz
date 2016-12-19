rm -rf .DS_Store
rm -rf *.or
rm -rf *.bak
rm -rf *.exe
rm -rf *.ppu
rm -rf *.o
rm -rf *.compiled
rm -rf lib/*
rm -rf backup/
rm -rf src/backup/
rm -rf src/integration/lib
rm -rf src/library/backup
rm -rf src/library/lib
rm -rf src/systems/backup/

find ./ -type f -name *.~ -delete
find ./ -type f -name *.o -delete
find ./ -type f -name *.oe -delete
find ./ -type f -name *.rst -delete
find ./ -type f -name *.bak -delete
find ./ -type f -name *.ppu -delete
find ./ -type f -name *.compiled -delete
find ./ -type d -name backup -delete

rm -rf bin/fastplaz.bin
rm -rf lib
mkdir lib
touch lib/empty


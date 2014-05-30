rm -rf bin/fastplaz.bin
rm -rf lib
mkdir lib

find ./ -type f -name *.~ -delete
find ./ -type f -name *.o -delete
find ./ -type f -name *.oe -delete
find ./ -type f -name *.rst -delete
find ./ -type f -name *.bak -delete
find ./ -type f -name *.ppu -delete
find ./ -type f -name *.compiled -delete

find ./ -type d -name backup -delete



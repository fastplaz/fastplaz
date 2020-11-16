rm -rf .DS_Store
rm -rf *.or
rm -rf *.bak
rm -rf *.exe
rm -rf *.ppu
rm -rf *.o
rm -rf *.compiled

find . -type f -name *.~ -delete
find . -type f -name *.o -delete
find . -type f -name *.oe -delete
find . -type f -name *.bin -delete
find . -type f -name *.rst -delete
find . -type f -name *.rsj -delete
find . -type f -name *.bak -delete
find . -type f -name *.ppu -delete
find . -type f -name *.log -delete
find . -type f -name *.ses -delete
find . -type f -name *.compiled -delete
find . -type f -name .DS_Store -delete

#cache image
#find . -type f -name *.jpg -delete

find . -type d -name 'backup' -exec rm -rf "{}" +
find . -type d -name 'lib' -exec rm -rf "{}" +



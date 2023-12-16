#!/bin/bash.exe

echo "preparing the Windows distribution"


cd /c/Users/jacek/Programming/Lisp/clos-sweeper/

echo "removing previous distribution artifacts"
rm -rvf ./distribution/


mkdir -p "./distribution/bin"

echo "copying lisp executable"
cp -v ./src/clos-sweeper.exe ./distribution/bin/
echo "copying ALL DLLs"
cp -v /mingw64/bin/*.dll ./distribution/bin/

echo "preparing share folders"
mkdir -p "./distribution/share/glib-2.0"
mkdir -p "./distribution/share/icons"


echo "copying share folders"
cp -rv /mingw64/share/glib-2.0/schemas ./distribution/share/glib-2.0/
cp -rv /mingw64/share/icons/Adwaita    ./distribution/share/icons/ 
cp -rv /mingw64/share/icons/hicolor    ./distribution/share/icons/

#!/bin/bash.exe

# use MSYS2 MINGW64 terminal to run this script

echo "preparing the Windows distribution"


cd /c/Users/jacek/Programming/Lisp/clos-sweeper/

echo "removing previous distribution artifacts"
rm -rvf ./distribution/


mkdir -p "./distribution/bin"

echo "copying lisp executable"
cp -v ./src/clos-sweeper.exe ./distribution/bin/

echo "copying ALL DLLs"
# cp -v /mingw64/bin/* ./distribution/bin
# we may need to add

cp -v /mingw64/bin/libglib-2.0-0.dll ./distribution/bin
cp -v /mingw64/bin/libgio-2.0-0.dll ./distribution/bin

cp -v /mingw64/bin/libgtk-4-1.dll ./distribution/bin
cp -v /mingw64/bin/libcairo-2.dll ./distribution/bin
cp -v /mingw64/bin/libgirepository-1.0-1.dll ./distribution/bin
cp -v /mingw64/bin/libgobject-2.0-0.dll ./distribution/bin
cp -v /mingw64/bin/libwinpthread-1.dll ./distribution/bin

echo "preparing share folders"
mkdir -p "./distribution/share/glib-2.0"
mkdir -p "./distribution/share/icons"


echo "copying share folders"
cp -rv /mingw64/share/glib-2.0/schemas ./distribution/share/glib-2.0/
cp -rv /mingw64/share/icons/Adwaita    ./distribution/share/icons/
cp -rv /mingw64/share/icons/hicolor    ./distribution/share/icons/

echo "copying all the rest"
cp -rv /mingw64/etc ./distribution/
# cp -rv /mingw64/include ./distribution/
# cp -rv /mingw64/lib ./distribution/
# cp -rv /mingw64/libexec ./distribution/
# cp -rv /mingw64/share ./distribution/
# cp -rv /mingw64/var ./distribution/

echo "==============done==================="

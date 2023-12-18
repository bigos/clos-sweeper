rem msys2 launcher

rem https://devblogs.microsoft.com/powershell-community/determine-if-a-folder-exists/

set BINDIR1=C:\msys64\mingw64\bin
if exist %BINDIR1% (
echo "Expected folder 1 exists"
) else (
echo "expected folder 1 does not exist"
)


set BINDIR2=C:\msys64\usr\bin
if exist %BINDIR2% (
echo "Expected folder 2 exists"
) else (
echo "expected folder 2 does not exist"
)

set PATH=c:/msys64/mingw64/bin;c:/msys64/usr/bin;%PATH%

src\clos-sweeper.exe

rem wait for the keypress giving the chance to read the output
pause

rem msys2 launcher

rem https://devblogs.microsoft.com/powershell-community/determine-if-a-folder-exists/

set BINDIR1=C:\msys64\mingw64\bin
if exist %BINDIR1% (
echo "Expected bin exists"
) else (
echo "expected bin folder does not exist"
)

set PATH=c:/msys64/mingw64/bin;%PATH%

clos-sweeper.exe

rem wait for the keypress giving the chance to read the output
pause

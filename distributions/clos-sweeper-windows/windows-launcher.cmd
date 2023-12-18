rem Lanucher for clos-sweeper

set CLOS_SWEEPER=%~dp0

set PATH=%CLOS_SWEEPER%bin;%PATH%

echo %PATH%

%CLOS_SWEEPER%clos-sweeper.exe

pause

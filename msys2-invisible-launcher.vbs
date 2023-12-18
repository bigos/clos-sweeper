rem


Set WshShell = CreateObject("WScript.Shell") 
WshShell.Run chr(34) & "msys2-launcher.cmd" & Chr(34), 0
Set WshShell = Nothing

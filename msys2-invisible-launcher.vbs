rem https://serverfault.com/questions/9038/run-a-bat-file-in-a-scheduled-task-without-a-window


Set WshShell = CreateObject("WScript.Shell") 
WshShell.Run chr(34) & "msys2-launcher.cmd" & Chr(34), 0
Set WshShell = Nothing

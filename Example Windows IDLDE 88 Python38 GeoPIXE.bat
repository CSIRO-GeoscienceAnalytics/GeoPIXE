rem
rem Launch IDLDE for GeoPIXE-source workspace under Windows
rem

rem "Use IDL 8.8 and python 3"

call c:\Anaconda3\condabin\conda.bat activate base

PATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Anaconda3;C:\Anaconda3\Scripts;%PATH%

rem Also add any local python directories here ...
SET PYTHONPATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Program Files\Harris\IDL88\lib\bridges;C:\Anaconda3;%PYTHONPATH%

"C:\Program Files\Harris\IDL88\bin\bin.x86_64\idlde.exe" -data "C:\Software\IDL\GeoPIXE-open-source\Workspace" -pref="C:\Software\IDL\GeoPIXE-open-source\Workspace\idl88-py38.pref"


rem
rem Launch IDLDE for GeoPIXE-source workspace under Windows
rem

rem "Use IDL 9.1 and python 3.11"

call c:\Anaconda3\condabin\conda.bat activate base

PATH=C:\Program Files\nv5\IDL91\bin\bin.x86_64;C:\Anaconda3;C:\Anaconda3\Scripts;%PATH%
SET PYTHONPATH=C:\Program Files\nv5\IDL91\bin\bin.x86_64;C:\Program Files\nv5\IDL91\lib\bridges;C:\Anaconda3;C:\software\python\SVN-Maia-Mapper\src\main\python;C:\software\python\SVN-Maia-Mapper\src\test\python;%PYTHONPATH%

"C:\Program Files\nv5\IDL91\bin\bin.x86_64\idlde.exe" -data "C:\Software\IDL\GeoPIXE-open-source\Workspace" -pref="C:\Software\IDL\GeoPIXE-open-source\Workspace\idl91-py311.pref"

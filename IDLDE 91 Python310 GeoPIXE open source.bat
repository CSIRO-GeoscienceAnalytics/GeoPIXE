rem
rem Launch IDLDE for GeoPIXE-source workspace under Windows
rem	Swithed from Anaconda to Mini-forge
rem

rem "Use IDL 9.1 and python 3.10"

path

call C:\Users\rya113\AppData\Local\miniforge3\condabin\conda.bat deactivate
rem call c:\Anaconda3\condabin\conda.bat activate base
call C:\Users\rya113\AppData\Local\miniforge3\condabin\conda.bat activate py310
pause

rem PATH=C:\Program Files\nv5\IDL91\bin\bin.x86_64;C:\Anaconda3;C:\Anaconda3\Scripts;%PATH%
PATH=C:\Program Files\nv5\IDL91\bin\bin.x86_64;%PATH%

rem SET PYTHONPATH=C:\Program Files\nv5\IDL91\bin\bin.x86_64;C:\Program Files\nv5\IDL91\lib\bridges;C:\Anaconda3;C:\software\python\SVN-Maia-Mapper\src\main\python;C:\software\python\SVN-Maia-Mapper\src\test\python;%PYTHONPATH%
SET PYTHONPATH=C:\Program Files\nv5\IDL91\bin\bin.x86_64;C:\Program Files\nv5\IDL91\lib\bridges;C:\software\python\SVN-Maia-Mapper\src\main\python;C:\software\python\SVN-Maia-Mapper\src\test\python;%PYTHONPATH%

"C:\Program Files\nv5\IDL91\bin\bin.x86_64\idlde.exe" -data "C:\Software\IDL\GeoPIXE-open-source\Workspace" -pref="C:\Software\IDL\GeoPIXE-open-source\Workspace\idl91-py310.pref"


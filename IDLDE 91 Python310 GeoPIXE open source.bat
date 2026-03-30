rem
rem Launch IDLDE for GeoPIXE-source workspace under Windows
rem	Switched from Anaconda to Mini-forge
rem	(using a local envirnment with python 3.10)

rem "Use IDL 9.1 and python 3.10"

path

call C:\Users\rya113\AppData\Local\miniforge3\condabin\conda.bat deactivate

rem For some extensions, assumes there is a python enviromnment "py310" for python 3.10

call C:\Users\rya113\AppData\Local\miniforge3\condabin\conda.bat activate py310
rem pause

PATH=C:\Program Files\nv5\IDL91\bin\bin.x86_64;%PATH%

SET PYTHONPATH=C:\Program Files\nv5\IDL91\bin\bin.x86_64;C:\Program Files\nv5\IDL91\lib\bridges;C:\software\python\SVN-Maia-Mapper\src\main\python;C:\software\python\SVN-Maia-Mapper\src\test\python;%PYTHONPATH%

"C:\Program Files\nv5\IDL91\bin\bin.x86_64\idlde.exe" -data "C:\Software\IDL\GeoPIXE-open-source\Workspace" -pref="C:\Software\IDL\GeoPIXE-open-source\Workspace\idl91-py310.pref"


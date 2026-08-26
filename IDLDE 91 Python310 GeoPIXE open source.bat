rem
rem Launch IDLDE for GeoPIXE-source workspace under Windows
rem	Switched from Anaconda to Mini-forge
rem	(using a local envirnment with python 3.10)

rem "Use IDL 9.1 and python 3.10"

path

rem call C:\Users\chris\miniforge3\condabin\conda.bat deactivate

rem For some extensions, assumes there is a python enviromnment "py310" for python 3.10
rem Remember to VPN to UMelb for license before continuing ...

call C:\Users\chris\miniforge3\condabin\conda.bat activate py310

pause

PATH=C:\Program Files\NV5\IDL91\bin\bin.x86_64;%PATH%

SET PYTHONPATH=C:\Program Files\NV5\IDL91\bin\bin.x86_64;C:\Program Files\NV5\IDL91\lib\bridges;Z:\software\python\SVN-Maia-Mapper\src\main\python;Z:\software\python\SVN-Maia-Mapper\src\test\python;%PYTHONPATH%

"C:\Program Files\NV5\IDL91\bin\bin.x86_64\idlde.exe" -data "Z:\Software\IDL\GeoPIXE\Workspace" -pref="Z:\Software\IDL\GeoPIXE\Workspace\idl91-py310.pref"

pause


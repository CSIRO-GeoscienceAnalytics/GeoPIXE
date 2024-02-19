
rem  Note: need to match IDL with correct python version ...

call c:\Anaconda3\condabin\conda.bat activate base

PATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Anaconda3;%PATH%
SET PYTHONPATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Program Files\Harris\IDL88\lib\bridges;C:\Anaconda3;C:\software\python\SVN-Maia-Mapper\src\main\python;C:\software\python\SVN-Maia-Mapper\src\test\python;%PYTHONPATH%

cd "C:\Software\IDL\GeoPIXE-open-source\workspace\geopixe"
"C:\Program Files\Harris\IDL88\bin\bin.x86_64\idlrt.exe" "maia_control.sav" 

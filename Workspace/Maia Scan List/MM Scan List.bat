rem
rem Launch mm_scan_list under Windows
rem

call c:\Anaconda3\condabin\conda.bat activate base

PATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Anaconda3:\Anaconda3\Scripts;%PATH%

SET PYTHONPATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Program Files\Harris\IDL88\lib\bridges;C:\Anaconda3;C:\software\python\SVN-Maia-Mapper\src\main\python;C:\software\python\SVN-Maia-Mapper\src\test\python;C:\Users\rya113\Documents\Python Scripts;%PYTHONPATH%

cd "C:\Software\IDL\GeoPIXE-Release\GeoPIXE3"

"C:\Program Files\Harris\IDL88\bin\bin.x86_64\idlrt.exe" "mm_scan_list.sav" -args "MM.Mel.SL.config"

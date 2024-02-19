rem
rem Launch MM_Scan_List under Windows
rem

rem "Use IDL 8.5"
PATH=C:\Program Files\Exelis\IDL85\bin\bin.x86_64;C:\Anaconda2;%PATH%
rem PATH=C:\Program Files\Exelis\IDL85\bin\bin.x86_64;C:\Anaconda2

rem PATH=C:\Program Files\Exelis\IDL85\bin\bin.x86_64;C:\Winpython\WinPython-64bit-2.7.10.3\python-2.7.10.amd64;%PATH%
rem PATH=C:\Program Files\Exelis\IDL85\bin\bin.x86_64;C:\Winpython\WinPython-64bit-2.7.10.3\python-2.7.10.amd64

rem SET PYTHONPATH=C:\Program Files\Exelis\IDL85\bin\bin.x86_64;C:\Program Files\Exelis\IDL85\lib\bridges;C:\Anaconda2;C:\Anaconda2\lib;C:\software\IDL\GeoPIXE-release\GeoPIXE2\python;C:\SVN-Maia-Mapper\src\main\python;C:\SVN-Maia-Mapper\src\test\python;%PYTHONPATH%

SET PYTHONPATH=C:\Program Files\Exelis\IDL85\bin\bin.x86_64;C:\Program Files\Exelis\IDL85\lib\bridges;C:\software\IDL\GeoPIXE-release\GeoPIXE2\python;C:\SVN-Maia-Mapper\src\main\python;C:\SVN-Maia-Mapper\src\test\python;%PYTHONPATH%

"C:\Program Files\Exelis\IDL85\bin\bin.x86_64\idlrt.exe" -rt="C:\software\IDL\GeoPIXE-release\GeoPIXE2\mm_scan_list.sav" -pref="C:\software\IDL\GeoPIXE-release\GeoPIXE2\idlrt.pref"

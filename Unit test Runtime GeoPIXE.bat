
rem  Note: need to match IDL with correct python version ...

rem "Remove Anaconda references if python is not available. This will not effect normal"
rem "GeoPIXE operation. "

rem "GeoPIXE can then be run simply by double clicking on 'GeoPIXE.sav' in the"
rem "folder 'geopixe' in 'workspace'."

call c:\Anaconda3\condabin\conda.bat activate base

PATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Anaconda3;%PATH%
SET PYTHONPATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Program Files\Harris\IDL88\lib\bridges;C:\Anaconda3;%PYTHONPATH%

cd "C:\Software\IDL\GeoPIXE-open-source\workspace\geopixe"
rem "C:\Program Files\Harris\IDL88\bin\bin.x86_64\idlrt.exe" "GeoPIXE.sav" 

rem "Use this variant to run GeoPIXE for Unit Tests ..."
rem "It assumes you have downloaded the Demo data to folder 'Demo/'."

"C:\Program Files\Harris\IDL89\bin\bin.x86_64\idlrt.exe" "GeoPIXE.sav" -args "compare_yields" "['C:\Software\Demo\Maia\XANES\analysis\pyrite-30um-7-35.yield','C:\Software\Demo\Maia\XANES\analysis\pyrite-30um-7-35-test.yield']" "C:\Software\Demo\Maia\XANES\analysis\pyrite-30um-7-35-test.txt"

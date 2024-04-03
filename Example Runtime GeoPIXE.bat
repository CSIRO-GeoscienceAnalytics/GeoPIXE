
rem  Note: need to match IDL with correct python version ...

rem "Remove Anaconda references if python is not available. This will not effect normal"
rem "GeoPIXE operation. "

rem "GeoPIXE can then be run simply by double clicking on 'GeoPIXE.sav' in the"
rem "folder 'geopixe' in 'workspace'."

call c:\Anaconda3\condabin\conda.bat activate base

PATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Anaconda3;%PATH%
SET PYTHONPATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Program Files\Harris\IDL88\lib\bridges;C:\Anaconda3;%PYTHONPATH%

cd "C:\Software\IDL\GeoPIXE-open-source\workspace\geopixe"
"C:\Program Files\Harris\IDL88\bin\bin.x86_64\idlrt.exe" "GeoPIXE.sav" 

rem "Use this variant to run GeoPIXE as a module in a workflow ..."
rem "It assumes you have downloaded the Demo data to folder 'Demo/'."

rem "C:\Program Files\Harris\IDL88\bin\bin.x86_64\idlrt.exe" "GeoPIXE.sav" -args "C:\Software\Demo\MM\analysis\925\image-history.gcf" "C:\Software\Demo\MM\analysis\925\925-Bi-x.dai" "C:\Software\Demo\MM\analysis\925\925-Bi-x-metadata3.txt"

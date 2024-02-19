
rem  Note: need to match IDL with correct python version ...

call c:\Anaconda3\condabin\conda.bat activate base

PATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Anaconda3;%PATH%
SET PYTHONPATH=C:\Program Files\Harris\IDL88\bin\bin.x86_64;C:\Program Files\Harris\IDL88\lib\bridges;C:\Anaconda3;%PYTHONPATH%

cd "C:\Software\IDL\GeoPIXE-open-source\workspace\geopixe"
rem "C:\Program Files\Harris\IDL88\bin\bin.x86_64\idlrt.exe" "GeoPIXE.sav" -args "C:\Software\Demo\MM\analysis\925\image-history.gcf" "C:\Software\Demo\MM\analysis\925\925-Bi-x.dai" "C:\Software\Demo\MM\analysis\925\925-Bi-x-metadata3.txt"
"C:\Program Files\Harris\IDL88\bin\bin.x86_64\idlrt.exe" "GeoPIXE.sav" 

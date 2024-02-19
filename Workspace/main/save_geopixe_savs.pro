pro save_geopixe_savs

print,''
print,'*** resolve Python class ...'
resolve_all, class=['Python'], /CONTINUE_ON_ERROR

print,''
print,'*** Save "GeoPIXE.sav" to release\GeoPIXE2 dir ...'
save, /routine, file='../../../GeoPIXE-Release/GeoPIXE2/GeoPIXE.sav'

print,''
return
end

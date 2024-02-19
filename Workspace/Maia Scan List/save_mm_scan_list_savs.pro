pro save_mm_scan_list_savs

prep_python_for_compile

print,''
print,'*** Save "mm_scan_list.sav" to release\GeoPIXE3 dir ...'
save, /routine, file='../../../GeoPIXE-Release/GeoPIXE3/mm_scan_list.sav'

print,''
return
end

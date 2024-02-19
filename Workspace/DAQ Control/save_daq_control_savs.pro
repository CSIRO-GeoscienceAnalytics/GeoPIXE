pro save_daq_control_savs

print,''
print,'*** Save "daq_control.sav" to release\GeoPIXE3 dir ...'
save, /routine, file='../../../GeoPIXE-Release/GeoPIXE3/daq_control.sav'

print,''
return
end

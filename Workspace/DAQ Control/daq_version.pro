function daq_version

COMPILE_OPT STRICTARR

; Format of version MUST be:
;	simple version n.m (numeric)
;	which also gets used for update dir name
;	followed by a SINGLE alphabet letter if needed.

daq_ver='8.7o'

return, daq_ver
end

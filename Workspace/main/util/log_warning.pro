pro log_warning, routine, message, error=error, info=info

COMPILE_OPT STRICTARR
common c_debug_warnings, enable_warning_popup
if n_elements(enable_warning_popup) lt 1 then enable_warning_popup=1
common c_debug_linux_bug, first						;@8-18
if n_elements(first) lt 1 then first=3				;@8-18

if n_elements(routine) lt 1 then routine='Unknown'
if n_elements(error) lt 1 then error=0
if n_elements(info) lt 1 then info=0

common c_logging_comms, logging_comms
if n_elements(logging_comms) lt 1 then logging_comms=0

	itype = 'WARNING'
	if error then itype = 'ERROR'
	if info then itype = 'INFO'
	log_message, logging_comms, type=itype, routine+': '+strjoin( message, ', ')
	
	return
end

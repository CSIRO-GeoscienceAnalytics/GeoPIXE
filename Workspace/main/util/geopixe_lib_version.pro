
function geopixe_lib_version

;	Return the external library version used for GeoPIXE on this platform

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_geopixe_library_1, geopixe_library_file, geopixe_library_suffix
common c_geopixe_library_2, geopixe_library_version

	ErrorNo = 0
	version = 0
	common c_errors_1, catch_errors_on
	if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'geopixe_lib_version',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, ''
		endif
	endif

	if (n_elements(geopixe_root) lt 1) then startupp
	if n_elements(geopixe_library_version) lt 1 then begin
		geopixe_library_file = ''
		geopixe_library_suffix = ''
		geopixe_library_version = 0
		lib = geopixe_library( version=version, /force)
	endif
	
	return, geopixe_library_version
end

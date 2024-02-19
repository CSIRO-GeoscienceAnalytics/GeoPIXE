function init_unidaq_32

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on = 1

if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'init_unidaq_32',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, bad
	endif
endif

err = call_external( geopixe_library(), geolib_name( 'init_unidaq_32'), cdecl=geolib_cdecl())

return, err

bad:
	print,' init_unidaq_32: Error - bad type for argument.'
	err = 1L
	return, err
end

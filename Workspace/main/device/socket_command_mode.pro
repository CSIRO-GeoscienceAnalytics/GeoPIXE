pro socket_command_mode, ddm_down=ddm_dead

; set flags for socket I/O
;
;	/ddm_down	indicates no DDM connected, so ignore subsequent errors of name
;				"DDM communication error (token)"

	COMPILE_OPT STRICTARR
	error = 1
	common c_errors_1, catch_errors_on
	if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'socket_command_mode',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif
	
	common c_socket_io, ddm_down, ddm_error_reported
	if n_elements(ddm_down) lt 1 then ddm_down=0
	if n_elements(ddm_error_reported) lt 1 then ddm_error_reported=0
	
	if n_elements(ddm_dead) gt 0 then begin
		ddm_down = ddm_dead
	endif
	
	return
end

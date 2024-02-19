pro socket_retry, ps, error=error

; Retry opening socket on read/write error
; Keep count of retries.

	error = 1
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
			warning,'socket_retry',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if (*ps).open then close_file, (*ps).unit
	(*ps).open = 0

start:
	if ((*ps).retries gt 0) and ((*ps).attempts gt (*ps).retries) then begin
		warning,'socket_retry',['Maximum retries exceeded; abort socket reopen:', $
						'Client = '+(*ps).client, 'IP = '+(*ps).ip ]
		return
	endif

	(*ps).attempts++
;	print, 'Retry ('+str_tidy((*ps).attempts)+') Maia socket open, unit=',(*ps).unit
	log_warning,'socket_retry','Retry ('+str_tidy((*ps).attempts)+'), client='+(*ps).client+', IP='+(*ps).ip

	new = open_socket( ip=(*ps).ip, port=(*ps).port, token=(*ps).token, error=error, $
			connect_timeout=(*ps).connect_timeout, write_timeout=(*ps).write_timeout, read_timeout=(*ps).read_timeout, $
			client=(*ps).client, retries=(*ps).retries)
	if error ne 0 then goto, problem

	new.attempts = (*ps).attempts
	new.version = (*ps).version
	new.n_detectors = (*ps).n_detectors
	ptr_free, (*ps).ps
	*ps = new
;	print,'Socket re-opened with n_detectors =', (*ps).n_detectors
	log_warning,'socket_retry','Retry successful, client='+(*ps).client
	error = 0
	return

problem:
	warning, timeout=10.,'socket_retry',['Bad socket retry to Kandinski socket.', 'Client = '+(*ps).client, $
			'Wait and try again ...'], cancel=cancel
	ptr_free, new.ps
	if cancel then return
	goto, start
end

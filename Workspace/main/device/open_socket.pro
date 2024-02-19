function open_socket, ip=ip, port=port, token=token, error=error, enable=enable, $
		read_timeout=read_timeout, connect_timeout=connect_timeout, write_timeout=write_timeout, $
		retries=retries, client=client

sock = define(/maia_port)
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
		warning,'open_socket',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		close_file, unit
		return, sock
	endif
endif

	if n_elements(enable) lt 1 then enable=1
	if n_elements(ip) lt 1 then ip='138.194.25.203'
	if n_elements(port) lt 1 then port=9001
	if n_elements(token) lt 1 then token='0'
	if n_elements(read_timeout) lt 1 then read_timeout=10
	if n_elements(connect_timeout) lt 1 then connect_timeout=10
	if n_elements(write_timeout) lt 1 then write_timeout=10
	if n_elements(retries) lt 1 then retries=5
	if n_elements(client) lt 1 then client='?'
	sock.port = port
	sock.ip = ip
	sock.token = token
	sock.connect_timeout = connect_timeout
	sock.write_timeout = write_timeout
	sock.read_timeout = read_timeout
	sock.retries = retries
	sock.client = client

;	return, sock					; return for debugging

	if enable eq 0 then begin
		error = 0
		return, sock
	endif
	
	on_ioerror, bad_open
	socket, unit, ip, port, error=error, /get_lun, connect_timeout=connect_timeout, $
						write_timeout=write_timeout, read_timeout=read_timeout
	on_ioerror, null
	if error ne 0 then goto, bad_open
	
	sock.unit = unit
	sock.open = 1
	return, sock

bad_open:
	warning, timeout=10, 'open_socket',['Error opening socket',!ERROR_STATE.MSG,'Error number '+str_tidy(error),'', $
			'IP address = '+ip,'Port number = '+str_tidy(port),'Connect timeout = '+str_tidy(connect_timeout), $
			'Client = '+client]
	close_file, unit
	error = 1
	return, sock
end

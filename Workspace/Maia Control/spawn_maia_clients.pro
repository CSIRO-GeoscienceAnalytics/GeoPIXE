pro spawn_maia_clients, n_detectors, obj=obj, prefix=prefix, conf=conf, server=serveri, error=error

; Launch maia clients ...

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
    Catch, ErrorNo
    if (ErrorNo ne 0) then begin
       Catch, /cancel
       on_error, 1
       help, calls = s
       n = n_elements(s)
       c = 'Call stack: '
       if n gt 2 then c = [c, s[1:n-2]]
       warning,'spawn_maia_clients',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
if n_elements(prefix) eq 0 then prefix=''
if n_elements(conf) eq 0 then conf=''
if n_elements(serveri) eq 0 then serveri=''
server = serveri
error = 1

	print,'  Spawn 2 Kandinski background processes ...'
	print,'        prefix = ',prefix
	print,'          conf = ',conf
	print,'    log server = ',server
	obj = objarr(2)

	path = geopixe_root + 'maia' + slash()

	debug = -1		; (0:off, 1:on, -1:default) Enable here to override default in .Maia.conf file.

;	Pass the 'server' to the child processes so they can log error messages to rsyslog
;	via the MMlibs Python library. This works fine for Linux, with system Python available.
;	Problems with Anaconda on Windows means that the subprocess does not get proper environment.
;	Hence, DO NOT pass on 'server' under Windows for now.

	case !version.os_family of
		'Windows': begin
			server = ''							; server does not work, due to Python issues on Windows
			end
		'unix': begin
			end
		else: begin
			warning,'spawn_maia_clients',['Spawn not supported on this platform.','Could not spawn blog clients.']
			end
	endcase

	args = { prefix:prefix, conf:conf, server:server, n_detectors:n_detectors, debug:debug}

;	0 	   			1		
;	Paramweters		Params slow

	obj[0] = spawn_bridge_object( path, 'maia_client_parameters', args=args, error=err)
	if err then return
	obj[1] = spawn_bridge_object( path, 'maia_client_parameters_slow', args=args, error=err)
	if err then return

	error = 0
	return
end

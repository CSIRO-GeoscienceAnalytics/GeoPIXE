pro spawn_blog_clients, obj=obj, prefix=prefix, conf=conf, server=serveri, $
					enable_groups=enable_groups, error=error

; Launch blog clients ...

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
       warning,'spawn_blog_clients',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif
if n_elements(prefix) eq 0 then prefix=''
if n_elements(conf) eq 0 then conf=''
if n_elements(serveri) eq 0 then serveri=''
if n_elements(enable_groups) eq 0 then enable_groups=1
server = serveri
error = 1

	print,'    Spawn 5 blog background processes ...'
	print,'        prefix = ',prefix
	print,'          conf = ',conf
	print,'    log server = ',server
	obj = objarr(5)

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
			warning,'spawn_blog_clients',['Spawn not supported on this platform.','Could not spawn blog clients.']
			end
	endcase

	args = { prefix:prefix, conf:conf, server:server, debug:debug}

;	0 	   		1		2		3		4		
;	activity	ET  	DA  	Epics	Group 	

	obj[0] = spawn_bridge_object( path, 'blog_client_activity', args=args, error=err)
	if err then return
	obj[1] = spawn_bridge_object( path, 'blog_client_et2_spectra', args=args, error=err)
	if err then return
	obj[2] = spawn_bridge_object( path, 'blog_client_da2', args=args, error=err)
	if err then return
	obj[3] = spawn_bridge_object( path, 'blog_client_epics', args=args, error=err)
	if err then return
	if enable_groups then begin
		obj[4] = spawn_bridge_object( path, 'blog_client_group_spectra', args=args, error=err)
		if err then return
	endif

	error = 0
	return
end

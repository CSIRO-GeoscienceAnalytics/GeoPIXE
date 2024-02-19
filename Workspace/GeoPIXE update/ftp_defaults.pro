function ftp_defaults, error=error

; Read GeoPIXE ftp_update defaults from 'geopixe-update.conf' file.
; If none found, set some defaults (at the bottom).

	COMPILE_OPT STRICTARR
	error = 1
	maia = {	host:	'pftp.csiro.au', $				; default ftp server host
				user:	'', $							; default username
				pass:	'', $							; default password
				path:	'/update/7.5', $				; default remote FTP server path
				dir:	'', $							; default local update dir
				proxy:	'', $							; default proxy server IP (or blank)
				port:	'' }							; default proxy server port (or blank)
						
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'ftp_defaults',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, maia
	endif

	root_dir = geopixe_environment()					; user .geopixe dir
	geopixe_update_conf = root_dir + 'geopixe-update.conf'
	if file_test(geopixe_update_conf, /read) eq 0 then goto, done

	on_ioerror, bad_open
	openr, unit, geopixe_update_conf, /get_lun
	line = ''
	
	on_ioerror, bad_read
	while NOT EOF(unit) do begin
		readf, unit, line
		i = locate('#', line)
		if i eq -1 then i=strlen(line)
		if i gt 0 then begin
			s1 = str_break( line, all=s2)
			s = strlowcase(s1)
			ns = n_elements(s)
			if ns ge 2 then begin
				case s[0] of
					'host': begin
						maia.host = s1[1]
						end
					'user': begin
						maia.user = s1[1]
						end
					'pass': begin
						maia.pass = s2[1]
						end
					'path': begin
						maia.path = s1[1]
						end
					'dir': begin
						maia.dir = s1[1]
						end
					'proxy': begin
						maia.proxy = s1[1]
						end
					'port': begin
						maia.port = s1[1]
						end
					else:
				endcase
			endif
		endif
	endwhile
	
done:
	close_file, unit
	error = 0
	return, maia
	
bad_find:	
	warning,'ftp_defaults',['Failed to find update config file: "' + geopixe_update_conf + '",', $
							'from dir "' + now + '"', $
							'Will use default settings.']
	goto, bad_cont
	
bad_open:	
	warning,'ftp_defaults',['Failed to open update config file: "' + geopixe_update_conf + '",', $
							'from dir "' + now + '"', $
							'Will use default settings.']			
bad_cont:
	close_file, unit
	return, maia
	
bad_read:	
	warning,'ftp_defaults','bad read from update config file: "' + geopixe_update_conf + '".'
	close_file, unit
	return, maia
end

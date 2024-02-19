function geopixe_environment, temp=temp_dir, geopixe=geop, force=force

; Return your HOME directory used for user GeoPIXE files on this platform
; Also return the TEMP path for temporary files and geopixe_root.
; See "startupp" for more info on geopixe_root.

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_geopixe_env_1, user_home
common c_geopixe_env_2, user_temp

ErrorNo = 0
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
		warning,'geopixe_environment',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif
if n_elements(force) eq 0 then force=0

	if n_elements(geopixe_root) lt 1 then startupp
	geop = geopixe_root
	if (force eq 0) and (n_elements(user_home) gt 0) then begin
		temp_dir = user_temp
		return, user_home
	endif
	
	case !version.os_family of
		'Windows': begin
			user_home = getenv('HOME')
			if user_home eq '' then user_home = getenv('USERPROFILE')
			if user_home eq '' then user_home = getenv('HOMEPATH')
			
			temp_dir = getenv('TEMP')
			if temp_dir eq '' then temp_dir = getenv('TMP')
			if temp_dir eq '' then temp_dir = '\Temp'
			end
		'unix': begin
			user_home = getenv('HOME')
			if user_home eq '' then user_home = '~'

			temp_dir = getenv('TEMP')
			if temp_dir eq '' then temp_dir = getenv('TMP')
			if temp_dir eq '' then temp_dir = '/tmp'
			end
		'MacOS': begin
			user_home = getenv('HOME')
			
			temp_dir = getenv('TEMP')
			if temp_dir eq '' then temp_dir = getenv('TMP')
			if temp_dir eq '' then temp_dir = '/tmp'
			end
			
		else: warning,'geopixe_environment','un-supported operating system ('+ !version.os_family +')'
	endcase
	
	ps = path_sep()
	user_home = fix_path( user_home)
	temp_dir = fix_path( temp_dir)
	
	user_root = user_home[0]
	user_home = user_root + '.geopixe' + ps

;	/tmp/geopixe ends up being owned by someone, so does not work always.
;	Will use ~/temp instead as temp dir.

	user_temp = user_root + 'temp' + ps
	
	if file_test(user_home,/dir) eq 0 then begin
		safe_file_mkdir, user_home, error=error
		if error then begin
			warning,'geopixe_environment',['Failed to create home directory: "'+user_home+'"']
		endif
	endif
	if file_test(user_home,/write) eq 0 then begin
		warning,'geopixe_environment',["Can't write to home directory: '"+user_home+"'",'Check home .geopixe dir permissions.']
	endif

	if file_test(user_temp,/dir) eq 0 then begin
		safe_file_mkdir, user_temp, error=error
		if error then begin
			warning,'geopixe_environment',['Failed to create temp directory: "'+user_temp+'"']
			user_temp = user_home
		endif
	endif
	if file_test(user_temp,/write) eq 0 then begin
		warning,'geopixe_environment',["Can't write to temp directory: '"+user_temp+"'",'Use home dir instead.']
		user_temp = user_home
	endif
	
	temp_dir = user_temp			
	return, user_home
end


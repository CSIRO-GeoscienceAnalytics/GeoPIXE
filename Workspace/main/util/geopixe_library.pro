function geopixe_library, version=version, force=force

;	Return the external library used for GeoPIXE on this platform

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root
common c_geopixe_library_1, geopixe_library_file, geopixe_library_suffix
common c_library_1, postfix, cdecl
common c_library_2, prefix

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
		warning,'geopixe_library',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif
if n_elements(force) lt 1 then force=0

if (n_elements(geopixe_root) lt 1) then startupp
if n_elements(postfix) lt 1 then init_library
option = 0
suffix = ''
find_lib = 0
if n_elements(geopixe_library_file) lt 1 then begin
	geopixe_library_file = ''
	geopixe_library_suffix = ''
	find_lib = 1
endif
if geopixe_library_file eq '' then find_lib = 1

	if find_lib or force then begin
		geopixe_library_file = ''
try:
		case option of
			0: begin
				path = geopixe_root
				end
			1: begin
				path = fix_path( file_dirname(file_expand_path('.')))
				end
		endcase
		case !version.os_family of
			'Windows': begin
;				f = file_search( path+'image_dll.dll')				; debug code only
;				if f ne '' then begin
;					geopixe_library_file = path+'image_dll.dll'
;					geopixe_library_suffix = 'wdeb'
;					break
;				endif
				
				case !version.MEMORY_BITS of
					32: begin
						f = file_search( path+'image_lib.32bit.dll')
						if f ne '' then begin
							geopixe_library_file = path+'image_lib.32bit.dll'
							geopixe_library_suffix = 'w32'
						endif
						end
					64: begin
						f = file_search( path+'image_lib.64bit.dll')
						if f ne '' then begin
							geopixe_library_file = path+'image_lib.64bit.dll'
							geopixe_library_suffix = 'w64'
						endif
						end
				endcase
				end
				
			'unix': begin
				case !version.os of
					'linux': begin
						case !version.MEMORY_BITS of
							32: begin
								f = file_search( path+'image_lib.32bit.so')
								if f ne '' then begin
									geopixe_library_file = path+'image_lib.32bit.so'
									geopixe_library_suffix = 'L32'
								endif
								end
							64: begin
								f = file_search( path+'image_lib.64bit.so')
								if f ne '' then begin
									geopixe_library_file = path+'image_lib.64bit.so'
									geopixe_library_suffix = 'L64'
								endif
								end
						endcase
						end
					'darwin': begin
						case !version.MEMORY_BITS of
							32: begin
								f = file_search( path+'image_lib.32bit.dylib')
								if f ne '' then begin
									geopixe_library_file = path+'image_lib.32bit.dylib'
									geopixe_library_suffix = 'm32'
								endif
								end
							64: begin
								f = file_search( path+'image_lib.64bit.dylib')
								if f ne '' then begin
									geopixe_library_file = path+'image_lib.64bit.dylib'
									geopixe_library_suffix = 'm64'
								endif
								end
						endcase
						end
					else: warning,'geopixe_library','un-supported unix operating system ('+ !version.os +')'
				endcase
				end
				
			else: warning,'geopixe_library','un-supported operating system ('+ !version.os_family +')'
		endcase
		
		if geopixe_library_file eq '' then begin
			option = option + 1
			if option le 1 then goto, try
		endif
	endif

if geopixe_library_file eq '' then begin
	warning,'geopixe_library','No GeoPIXE library file found.'
	return, ''
endif

version = 0L
value = bytarr(1)
value[*] = 0					; pass all by reference
err = 0L

ErrorNo = 0
Catch, ErrorNo
if (ErrorNo ne 0) then begin
	Catch, /cancel
	on_error, 1
	
	help, calls = s
	n = n_elements(s)
	c = 'Call stack: '
	if n gt 2 then c = [c, s[1:n-2]]
	warning,'geopixe_library',['IDL run-time error caught.', '', $
		'Error:  '+strtrim(!error_state.name,2), $
		!Error_state.msg,'',c], /error
	
	
	MESSAGE, /RESET
	version = -2L
	return, geopixe_library_file
endif

err = call_external( geopixe_library_file, geolib_name( 'geopixe_lib_version'), cdecl=geolib_cdecl(), $
		version, value=value )

if err then version=-1L
version = strtrim(string(version),2) + geopixe_library_suffix

return, geopixe_library_file
end


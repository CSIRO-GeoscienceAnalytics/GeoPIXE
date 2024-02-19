pro init_library

COMPILE_OPT STRICTARR
common c_library_1, postfix, cdecl
common c_library_2, prefix

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
		warning,'call_library',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

	case !version.os_family of
		'Windows': begin
			prefix = ''
			postfix = '_'
			cdecl = 0
			end
		'unix': begin
			case !version.os of
				'linux': begin
					prefix = ''
					postfix = '__'
					cdecl = 1
					end
				'darwin': begin
					prefix = ''
					postfix = '_'
					cdecl = 1
					end
				else: warning,'init_library','un-supported unix operating system ('+ !version.os +')'
			endcase
			end
		else: warning,'init_library','un-supported operating system ('+ !version.os_family +')'
	endcase
	return
end


function geopixe_case_sensitive

;	Return file-name are case sensitive on this platform

COMPILE_OPT STRICTARR
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
		warning,'geopixe_case_sensitive',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif

	case !version.os_family of
		'Windows': begin
			case_sensitive = 0
			end
			
		'unix': begin
			case_sensitive = 1
			end
			
		else: begin
			warning,'geopixe_case_sensitive','un-supported operating system ('+ !version.os_family +')'
			case_sensitive = 1
			end
	endcase

	return, case_sensitive
end


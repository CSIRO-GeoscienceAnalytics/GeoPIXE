function os_type, arch=arch

; return a string for O/S type:
;	Win, Linux, Mac, Mac-old
;
;	/arch	subdivide by architecture:
;	Win32, Win64, Linux32, Linux64, Mac-x86, Mac-arm, Mac-old

COMPILE_OPT STRICTARR
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
		warning,'os_type',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ''
	endif
endif
if n_elements(arch) eq 0 then arch=0

	case !version.os_family of
		'Windows': begin
			os1 = "Win"
			case !version.MEMORY_BITS of
				32: begin
					os2 = "Win32"
					end
				64: begin
					os2 = "Win64"
					end
				else: warning,'os_type','un-supported bits ('+!version.MEMORY_BITS+') for operating system ('+ !version.os_family +')'
			endcase
			end
			
		'MacOS': begin
			os1 = "Mac-old"
			os2 = "Mac-old"
			end
			
		'unix': begin
			case !version.os of
				'linux': begin
					os1 = "Linux"
					case !version.MEMORY_BITS of
						32: begin
							os2 = "Linux32"
							end
						64: begin
							os2 = "Linux64"
							end
						else: warning,'os_type','un-supported bits ('+!version.MEMORY_BITS+') for operating system ('+ !version.os +')'
					endcase
					end
				'darwin': begin
					os1 = "Mac"
					case !version.arch of
						'arm64': begin
							os2 = "Mac-arm"
							end
						'x86_64': begin
							os2 = "Mac-x86"
							end
						else: warning,'os_type','un-supported architecture ('+!version.arch+') for unix operating system ('+ !version.os +')'
					endcase
					end
				else: warning,'os_type','un-supported unix operating system ('+ !version.os +')'
			endcase
			end
			
		else: warning,'os_type','un-supported operating system ('+ !version.os_family +')'
	endcase

	if arch then return, os2
	return, os1
end

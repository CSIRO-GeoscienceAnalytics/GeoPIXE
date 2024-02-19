function idl_version, revision=revision

; Return the version of IDL
;
;	revision	returns revision (e.g. "8.5" for IDL 8.5.1 or "8.8" for IDL 8.8.0)

COMPILE_OPT STRICTARR
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
		warning,'IDL_version',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return,'?'
	endif
endif

	version = !version.release

	s1 = strsplit( version, '.', /extract, count=n)
	revision = s1[0]
	if n gt 1 then revision = revision+'.'+s1[1]

	return, version
end
function python_version, revision=revision

; Return the version of python across IDL-python bridge
;
;	revision	returns revision (e.g. "2.7" for python2.7 or "3.8" for python3.8)

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
;		warning,'python_version',['IDL run-time error caught.', '', $
;				'Error:  '+strtrim(!error_state.name,2), $
;				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return,'?'
	endif
endif

	revision = '?'
	version = '?'

	sys = python.import('sys')
	s = strsplit( sys.version, ' ', /extract)
	version = s[0]

	s1 = strsplit( version, '.', /extract, count=n)
	revision = s1[0]
	if n gt 1 then revision = revision+'.'+s1[1]

	return, version
end

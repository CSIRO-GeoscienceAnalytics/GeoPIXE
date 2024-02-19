function trim_user, user

; Trim out crazy characters from a username, back to alphanumeric only

ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'trim_user',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0L
	endif
endif

	if n_elements(user) eq 0 then return,''
	
	t = user
	b = byte(user)
	ba = byte('@')
	q = where( b eq ba[0], nq)
	if nq eq 0 then return, t
	
	t = string( b[0:q[0]-1])
	return, t
end

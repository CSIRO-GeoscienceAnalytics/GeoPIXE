function wizard_window_count, open=windows_open, needed=windows_needed, name=name

;	Return count of windows with name 'name' 

COMPILE_OPT STRICTARR
count = 0
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
		warning,'wizard_window_count',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif

	q = where( name eq windows_needed, nq)				; index to needed windows
	if nq eq 0 then return, 0

	current = *windows_open[q[0]]						; current open IDs
	count = n_elements(current)							; current unique open count
	return, count
end

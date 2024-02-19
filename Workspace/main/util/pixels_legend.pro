function pixels_legend, ni

; Return 'n' as a string of pixel number, with suffix

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
		warning,'pixels_legend',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.
	endif
endif
	n = float(ni)

	ranges = ['K','M','G','T','P']
	rtimes = [1000.,1000.,1000.,1000.]
	it = 0
	label = 'pixels'
	while (n gt 1024.) and (it lt 4) do begin
		n = n/rtimes[it]
		label = ranges[it] + ' pixels'
		it += 1
	endwhile
	sn = str_tidy( n, places=1) + ' ' + label
	return, sn
end
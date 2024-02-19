function time_legend, time_toti

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
		warning,'time_legend',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.
	endif
endif
	time_tot = time_toti

	times = ['s','m','h','d','m','y']
	rtimes = [60.,60.,24.,30.,12.]
	it = 0
	while (time_tot gt 100.) and (it lt 5) do begin
		time_tot = time_tot/rtimes[it]
		it += 1
	endwhile
	time_label = times[it]
	stime = str_tidy( time_tot, places=1) + ' ' + time_label
	return, stime
end
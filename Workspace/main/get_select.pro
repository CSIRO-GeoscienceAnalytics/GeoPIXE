function get_select, file, invert=invert, n_detectors=n_detectors, error=err

	COMPILE_OPT STRICTARR
	err = 1
	n = 0
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
			warning,'get_select',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0
		endif
	endif
	if n_elements(invert) eq 0 then invert=0

	on_ioerror, finish
	openr, lun, file, /get_lun
	k = 0L
	on_ioerror, bad_get
	n = intarr(10000)
	line = ''
	while NOT EOF(lun) do begin
		readf, lun, line
		if strmid( line, 0,1) eq '#' then continue
		n[k] = long(line)
		k = k+1
	endwhile
bad_get:
	close_file, lun
	err = 0
	if k eq 0 then return, -1
	n = n[0:k-1]

	if n_elements(n_detectors) eq 0 then n_detectors=max(n)+1
	if invert and (n_detectors gt 0) then begin
		q = replicate( 1, n_detectors)
		q[n] = 0
		n = where( q eq 1)
	endif
	
finish:
	return, n
end

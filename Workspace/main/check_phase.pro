function check_phase, phase

; Check the normalization (per pixel) of this phase map
; 
;	phase	phase per pixel vector, all phases

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
		warning,'check_phase',['IDL run-time error caught.', '', $
			'Error:  '+strtrim(!error_state.name,2), $
			!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, phase
	endif
endif

	pixeln = n_elements(phase[*,*,0])
	nphase = n_elements(phase[0,0,*])

	sum = phase[*,*,0]
	if nphase gt 1 then begin
		for j=1,nphase-1 do begin
			sum = sum + phase[*,*,j]
		endfor
	endif
	
	q = where( sum gt 1.e-6, nq)
	if nq gt 0 then begin
		q2 = where( (sum[q] gt 1.1) or (sum[q] lt 0.9), nq2)
		if nq2 gt 0 then begin
;			warning,'check_phase','Corrected non-normalized phase map'
		endif

		for j=0,nphase-1 do begin
			phase[q + pixeln*j] = phase[q + pixeln*j] / sum[q]
		endfor
	endif else begin
		warning,'check_phase','Zero phase map?'
	endelse
		
	return, phase
end

function centroid, x, weight=w, variance=var, error=err

; Weighted mean/centroid of 'x' with weights 'w'.
; Return variance in 'variance'.

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
		warning,'centroid',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.0
	endif
endif else on_error,0

	err = 1
	n = n_elements(x)
	var = 0.0
	if n lt 1 then return, 0.0

	if n_elements(w) eq 0 then w=replicate(1.0,n)
	weight = total(w)
	if weight le 0. then return, 0.0

	ave = total( x*w) / total(w)
	var = total( w*(x-ave)^2) / total(w)

	err = 0
	return, ave
end


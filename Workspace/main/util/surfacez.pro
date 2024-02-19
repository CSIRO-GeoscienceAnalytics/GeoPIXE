function surfaceZ, x,y, zsurface=kz

;	return Z value on bilinear surface defined by cooeficients 'kz'
;
;	x,y		position
;	kz		bilinear coefficients (k,y,x,xy terms)

COMPILE_OPT STRICTARR
ErrorNo = 0
error = 1
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
		warning,'surfaceZ',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.0
	endif
endif
if n_elements(kz) eq 0 then return, 0.0
if n_elements(kz) lt 4 then return, kz[0]

	z = kz[0] + kz[1]*y + kz[2]*x + kz[3]*x*y
	return, z
end

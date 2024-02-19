pro free_spectra, pp

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
		warning,'Free_spectra',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif

if n_elements(pp) lt 1 then return
if ptr_good(pp) eq 0 then return
if ptr_good(pp[0],/struct) then begin
	p = pp
endif else begin
	p = *pp
endelse
if n_elements(p) lt 1 then return
if ptr_good(p[0],/struct) eq 0 then return

for i=0L,n_elements(p)-1 do begin
	if ptr_valid(p[i]) then free_spectrum, p[i]
endfor

ptr_free, p
ptr_free, pp
return
end

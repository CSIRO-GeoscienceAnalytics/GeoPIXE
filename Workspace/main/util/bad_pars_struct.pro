function bad_pars_struct, pars, make_pars=make_p

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
		warning,'bad_pars_struct',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, ptr_new()
	endif
endif

make_p = 0
if n_elements(pars) lt 1 then begin
	make_p = 1
	p = ptr_new(/allocate_heap)
endif else begin
	p = pars
	if ptr_valid(p) eq 0 then begin
		make_p = 1
	endif else begin
		if size(*p, /tname) eq 'UNDEFINED' then make_p=1
	endelse
endelse

return, p
end

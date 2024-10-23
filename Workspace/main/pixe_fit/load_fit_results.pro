pro load_fit_results, pstate, F

	; Read the results from 'F'

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
			warning,'load_fit_results',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
			MESSAGE, /RESET
			return
		endif
	endif

	if n_params() lt 2 then begin
		print,'load_fit_results: missing args.'
		return
	endif
	if lenchr(F) lt 1 then return

	no_results = 0
	if n_elements(pstate) eq 0 then return
	if ptr_valid(pstate) eq 0 then return
	if size(*pstate,/tname) ne 'STRUCT' then return
	presults = (*pstate).presults							; pointer to array of pointers to results
	if ptr_valid(presults) eq 0 then goto, bad_ptr
	if size(*presults,/tname) ne 'POINTER' then begin
		no_results = 1
	endif else begin
		if ptr_valid( (*presults)[0] ) eq 0 then no_results=1
		if no_results eq 0 then if size(*(*presults)[0],/tname) ne 'STRUCT' then no_results=1
	endelse

	results = read_fit_results( F, error=error)				; returns an array of pointers to results
	if error then return

	if no_results then begin
		*presults = results
		no_results = 0
	endif else begin
		*presults = [ *presults, results]
	endelse

	finish:
	return

	bad_ptr:
	warning,'load_fit_results','Bad initial results pointer',/error
	return
end

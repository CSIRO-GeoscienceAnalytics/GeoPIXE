pro free_DA, p

; Free DA structure and pointers

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
		warning,'free_DA',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return
	endif
endif
if n_elements(p) eq 0 then return

; Simple structure ...

	if size(p,/tname) eq 'STRUCT' then begin
		if ptr_good( p.pmore) then begin
			n = n_elements( *p.pmore)
			if n ge 1 then begin
				for i=0,n-1 do begin
					free_da, (*p.pmore)[i]
				endfor
			endif
		endif
		return
	endif

; Pointer to structure (or pointer array to struct)...

	if ptr_good(p,/struct) then begin
		for j=0,n_elements(p)-1 do begin
			if ptr_good( (*p[j]).pmore) then begin
				n = n_elements( *(*p[j]).pmore)
				if n ge 1 then begin
					for i=0,n-1 do begin
						free_da, (*(*p[j]).pmore)[i]
					endfor
				endif
			endif
			ptr_free, p[j]
		endfor
		return
	endif
	
	return
end

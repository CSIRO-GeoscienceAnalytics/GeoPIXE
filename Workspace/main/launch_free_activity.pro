function launch_free_activity, d

; Search activity arrays for a free slot, not used.

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
		warning,'launch_free_activity',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, -1
	endif
endif

n = n_elements(d)
if n lt 1 then return, -1
for i=0L,n-1 do begin
	if widget_info( d[i].TLB, /valid) eq 0 then return, i
endfor
return, -1
end
 
;--------------------------------------------------------------------------


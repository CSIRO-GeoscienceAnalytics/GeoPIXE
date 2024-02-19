function launch_total_activity, d

; Search activity arrays and total % activity.

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
		warning,'launch_total_activity',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0.0
	endif
endif

n = n_elements(d)
tot = 0.0
if n lt 1 then return, 0.0
for i=0L,n-1 do begin
	if widget_info( d[i].TLB, /valid) then begin
		tot = tot + *d[i].ppercent
	endif
endfor
return, tot
end
 

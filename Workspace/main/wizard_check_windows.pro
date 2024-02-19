pro wizard_check_windows, pstate, error=error

; Check if needed windows are open. If not pop-up warning as guidance.

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
       warning,'wizard_check_windows',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif

	name = (*pstate).windows_needed
	for i=0,n_elements(name)-1 do begin
		now = wizard_window_count( needed=(*pstate).windows_needed, open=(*pstate).windows_open, name=name[i])
		if (now eq 0) then begin
			warning,'wizard_check_windows',['No "'+name[i]+'" window is open.', '', $
					'Open the "'+name[i]+'" window.', 'Then re-try this step.']
			return
		endif
	endfor

	error = 0
	return
end

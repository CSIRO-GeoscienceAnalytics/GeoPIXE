pro wizard_test_windows, name, pstate, error=error

; Send the 'open-test' command to all windows needed by wizard 'name'.
; Send them independently, not chained, using separate pointers

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
       warning,'wizard_test_windows',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
      return
    endif
endif
	if n_elements(name) eq 0 then name='?'
	if ptr_good( pstate) eq 0 then return

	error = 1
	if widget_info( (*pstate).tlb, /valid) eq 0 then return

	for i=0, n_elements((*pstate).windows_needed)-1 do begin
		wz = define(/wizard_notify)
		wz.wizard = name
		wz.window = (*pstate).windows_needed[i]
		wz.command = 'open-test'
		clear_wizard, (*pstate).parray[i]
		*(*pstate).parray[i] = wz
		notify, 'wizard-action', (*pstate).parray[i]
	endfor	
	error = 0
	return
end

pro free_wizard, p

;	Free Wizard struct pointer heap memory.
;		Free its pdata memeory, if 'local',
;		but do not free any pointers that are contained
;		in Wizard supplied pdata.
;
;   Similar to, but not the same as 'clear_wizard'

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
       warning,'free_wizard',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

;	print,'##### free_wizard: p ...
;	help, p
;	help,/traceback

	if ptr_good(p) then begin
		if ptr_good( (*p).pnext) then begin
			free_wizard, (*p).pnext
		endif
		
		if ptr_good( (*p).pdata) and (*p).local then begin
			ptr_free, (*p).pdata
		endif
	endif
	
	if ptr_valid( p) then ptr_free, p
	return
end

pro log_client, unit=lun, plog=plog

; Try to read from blog clients stdout pipe ...
; a FAILED EXPERIMENT - NOW USING IDL BRIDGE

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

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
       warning,'log_client',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !error_state.msg,'',c], /error
       MESSAGE, /RESET
       return
    endif
endif

	if typename(plog) ne 'POINTER' then return
	return
	
;	the readf or readu below hang Windows IDL 8.5.1 ...

	on_ioerror, bad_read
	line = ''
	readf, lun, line

;	b = bytarr(10)
;	readu, lun, b
;	readu, lun, b

	if (*plog).used ge (*plog).max then begin
		n = (*plog).max / 2
		t = (*plog).log[0]
		(*plog).log[0:n-1] = (*plog).log[n:(*plog).max-1] 
		(*plog).log[n:(*plog).max-1] = ''
		(*plog).log[0] = t
		(*plog).used = n
	endif
	(*plog).log[(*plog).used] = line
	(*plog).used++
	return

bad_read:
	print,'*** log_client: Bad read from LUN for ' + (*plog).log[0]
	return
end

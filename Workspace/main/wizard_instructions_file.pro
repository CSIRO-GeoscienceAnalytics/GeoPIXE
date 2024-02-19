function wizard_instructions_file, file, error=err
	
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
	       warning,'wizard_instructions_file',['IDL run-time error caught.', '', $
	          'Error:  '+strtrim(!error_state.name,2), $
	          !error_state.msg,'',c], /error
	       MESSAGE, /RESET
	      return, ''
	    endif
	endif
	
	err = 1
	if n_elements(file) eq 0 then return, ''
	
	on_ioerror, bad_file
	openr, lun, file, /get_lun
	line = ''
	while eof(lun) eq 0 do begin
		readf, lun, line
		b = byte(line)
		q = where(b eq 9, nq)
		if nq gt 0 then b[q]=32B
		line = string(b)
		if n_elements(list) eq 0 then begin
			list = line
		endif else begin
			list = [list,line]
		endelse
	endwhile
	close_file, lun
	err = 0
	return, (n_elements(list) eq 0) ? '' : list
	
bad_file:
	warning,'wizard_instructions_file','Failed to open instructions file '+file
	return, ''
end

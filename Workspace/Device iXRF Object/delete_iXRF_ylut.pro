pro delete_iXRF_ylut, file, output=output, error=error

; Delete a Y table ...

COMPILE_OPT STRICTARR
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
       warning,'delete_iXRF_ylut',['IDL run-time error caught.', '', $
          'Error:  '+strtrim(!error_state.name,2), $
          !Error_state.msg,'',c], /error
       MESSAGE, /RESET
       free_record, pp
       error = 1
       return
    endif
endif

error = 1
if (n_elements(output) lt 1) then begin
	if (n_elements(file) lt 1) then return
	if (strlen(file) lt 1) then return
endif

if n_elements(output) gt 0 then begin
	if n_elements(file) gt 0 then begin
		f0 = extract_path(output) + strip_file_ext( strip_path(file[0])) + '.ylut'
	endif else begin
		f0 = strip_file_ext( output, double=strip)+'.ylut'
	endelse
endif else begin
	f0 = strip_file_ext( file[0])+'.ylut'
endelse

on_ioerror, bad_del
file_delete, f0, /quiet, /verbose
error = 0
return

bad_del:
	warning,'delete_iXRF_ylut','error deleting Y lookup table.',/error
	return
end

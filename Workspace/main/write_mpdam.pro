pro write_mpdam, ofile, phase, correct, error=err

;	Write out the MPDAM file.

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
		warning,'write_mpdam',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		goto, finish
	endif
endif

	if n_elements( ofile) eq 0 then goto, finish
	if n_elements( phase) eq 0 then goto, finish
	if n_elements( correct) eq 0 then goto, finish

	on_ioerror, bad_mpdam 
	openw, lun, ofile, /get_lun, /xdr
	writeu, lun, -1
	writeu, lun, phase
	writeu, lun, correct

	close_file, lun
	err = 0
	return

bad_mpdam:
	warning,'write_mpdam','Error writing the MPDAM file: '+ofile,/error
	close_file, lun

finish:
	err = 1
	return
end


				
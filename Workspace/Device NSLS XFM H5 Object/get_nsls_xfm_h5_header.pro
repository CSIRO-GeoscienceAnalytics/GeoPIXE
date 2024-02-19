function get_nsls_xfm_h5_header, file, error=error, silent=silent

COMPILE_OPT STRICTARR
if n_elements(silent) eq 0 then silent=0

	error = 1
	on_ioerror, bad
	openr, lun, file, /get_lun

;	head = read_nsls_xfm_h5_header( lun, silent=silent, error=error)
	head = read_nsls_xfm_h5_header( lun, error=error)
	if error then goto, bad

	close_file, lun
	error = 0
	return, head

bad:
	close_file, lun
	return, 0
end

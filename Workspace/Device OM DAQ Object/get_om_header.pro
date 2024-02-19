function get_om_header, file, error=error

COMPILE_OPT STRICTARR

	error = 1
	on_ioerror, bad
	openr, lun, file, /get_lun

	head = read_om_header( 4, lun, error=error)	; device'4' is now ignored
	if error then goto, bad

	close_file, lun
	error = 0
	return, head

bad:
	close_file, lun
	return, 0
end

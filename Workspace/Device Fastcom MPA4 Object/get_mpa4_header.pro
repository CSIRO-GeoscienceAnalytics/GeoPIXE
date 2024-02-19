function get_mpa4_header, file, error=error

COMPILE_OPT STRICTARR

	error = 1
	on_ioerror, bad
	openr, lun, file, /get_lun

	head = read_mpa4_header( lun, error=error)
	if error then goto, bad

	close_file, lun
	error = 0
	return, head

bad:
	close_file, lun
	return, 0
end

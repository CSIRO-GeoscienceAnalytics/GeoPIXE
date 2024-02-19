function get_maia_32_header, filei, error=error, silent=silent

COMPILE_OPT STRICTARR
if n_elements(silent) eq 0 then silent=0

	error = 1
	file = filei
	t = strip_file_ext(file)						; look for segment ".0" file
	f = t + '.0'
	gprint,level=2,'get_maia_32_header: f = '+f+' OK='+str_tidy(file_test(f))
	if file_test(f) then file=f

	on_ioerror, bad
	openr, lun, file, /get_lun

;	head = read_maia_32_header( lun, no_scan=silent, error=error)
	head = read_maia_32_header( lun, error=error)
	if error then goto, bad

	close_file, lun
	error = 0
	return, head

bad:
	close_file, lun
	return, 0
end

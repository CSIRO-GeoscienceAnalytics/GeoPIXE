function write_test, dir

COMPILE_OPT STRICTARR
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		goto, bad_open
	endif
	
	on_ioerror, bad_open
	newdir = file_search( fix_path(dir), /expand_tilde)
	file = fix_path(newdir) + 'test_write.dummy'
	openw, unit, file, /get_lun
	close_file, unit
	file_delete, file, /quiet
	status = 1
	
finish:
	close_file, unit
	on_ioerror, null
	return, status

bad_open:
	status = 0
	goto, finish
end
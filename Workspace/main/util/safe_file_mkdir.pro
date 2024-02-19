pro safe_file_mkdir, file, error=error, verbose=verbose, _extra=extras

; Test 'file' for existence, and make it if it does not exist already.
; Test that it can be created, parent is writeable.
; /verbose will pop-up error messages on error.

COMPILE_OPT STRICTARR
	error = 1
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		error = 1
		return
	endif
	if n_elements(verbose) eq 0 then verbose=0
	
	ft_file = file_test( file)
	ft_file_dir = file_test( file, /dir)
	ft_file_dir_write = file_test( file, /dir, /write)
	ft_file_dir_write_tested = ft_file_dir ? write_test( file) : 0
	
	if ft_file and (ft_file_dir eq 0) then begin
		if verbose then warning,'safe_file_mkdir',['File "'+file+'" exists','but is not a directory.']
		return
	endif
	if ft_file_dir and (ft_file_dir_write eq 0) then begin
		print, 'safe_file_mkdir: Dir "'+file+'" exists, but appears to be not writeable. Try write test ...'
		if ft_file_dir_write_tested eq 0 then begin
			if verbose then warning,'safe_file_mkdir',['Dir "'+file+'" exists','but is not writeable.']
			goto, show
		endif
	endif

	error = 0
	if ft_file_dir_write_tested then return

;	Else, just try to create the directory ...
;	If this fails, the error Catch will catch it.

	print,'safe_file_mkdir: create dir: '+file
	file_mkdir, file, _extra=extras
	return

show:
	print,'safe_file_mkdir: "file_test" is inconsistent with actual dir write ability.'
	print,'safe_file_mkdir: /dir='+str_tidy(file_test( dir_up(file), /dir))+' /write='+str_tidy(file_test( dir_up(file), /dir, /write))+' for: '+dir_up(file)
	print,'safe_file_mkdir: /dir='+str_tidy(ft_file_dir)+' /write='+str_tidy(ft_file_dir_write)+' for: '+file
	print,'safe_file_mkdir: /dir='+str_tidy(ft_file_dir)+' "write test"='+str_tidy(ft_file_dir_write_tested)+' for: '+file
	return
end

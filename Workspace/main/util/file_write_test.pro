function file_write_test, F, path=d, dir=dir

;	Test that a scalar file 'F' can be written.
;	Create the dir if needed; return path as 'd'.
;	/dir	only test 'F' as a dir.
;	Blank path is interpreted as current working dir.
;	Blank file name is not allowed.

COMPILE_OPT STRICTARR

	if n_elements(dir) eq 0 then dir=0
	d = ''
	if F eq '' then goto, bad												; null file name
	if lenchr(F) eq 0 then goto, bad										; blank file name
	
	d = extract_path(F)
	if d eq '' then begin													; blank path (use current)
		if file_test('.',/write,/dir) eq 0 then begin	

			on_ioerror, bad_dir1											; test file write to current dir
			dummy = 'dummy.dummy'
			openw, lun, dummy, /get_lun
			printf, lun, 'test'
			close_file, lun
			file_delete, dummy, /quiet
			goto, cont	
bad_dir1:
			warning,'file_write_test',['Output directory is blank; use working dir.', $
								'Working directory is not writeable.']
			goto, bad
		endif
	endif else begin
		if file_test(d,/dir) eq 0 then begin								; dir does not exist (yet)
			safe_file_mkdir, d, error=error
			if error then begin
				warning,'file_write_test',['Directory not found.','Failed to make directory:',d]
				goto, bad
			endif
		endif
		
		if file_test(d,/write,/dir) eq 0 then begin							; dir is not writeable apparently
			on_ioerror, bad_dir												; so test it directly ...
			dummy = d + 'dummy.dummy'
			openw, lun, dummy, /get_lun
			printf, lun, 'test'
			close_file, lun
			file_delete, dummy, /quiet
			goto, cont		
bad_dir:
			warning,'file_write_test',['Cannot write test file to Output directory:',d]
			goto, bad
		endif
	endelse
	
cont:
	if dir eq 0 then begin													; only for normal files
		name = strip_path(F)
		if name eq '' then begin											; blank file name
			goto, bad							
		endif else begin
			if file_test(F) then begin										; file exists already
				if file_test(F, /write) eq 0 then begin						; but is not writeable apparently
					on_ioerror, bad_file									; so test write it ...
					openw, lun, F, /get_lun
					printf, lun, 'test'
					close_file, lun
					goto, cont2		
bad_file:
					warning,'file_write_test',['Error opening output file: ','      '+F, $
										'for direct write test.','', $
										'File exists, but is not writable.']
					goto, bad

;					wait, 0.5
;					if file_test(F, /write) eq 0 then begin					; try again
;						warning, 'file_write_test', ['Error opening output file: ','      '+F,'', $
;										'Failed delayed retry.','File exists, but is not writable.']
;						goto, bad
;					endif
				endif
			endif
		endelse
	endif
cont2:
	return, 1
	
bad:
	return, 0
end
	
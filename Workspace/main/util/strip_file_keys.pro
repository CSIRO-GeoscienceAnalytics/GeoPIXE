function strip_file_keys, file, remove=remove, add=add, file_m=file_m, element=nelement

;	Remove any of 'remove' keys and 'element' names from a file-name along with
;	the standard '-m' tags and optionaslly 'add' some back.
;	With element choose the max number of element names to remove 'nelement'

COMPILE_OPT STRICTARR

	ext = extract_extension( file)	
	if strmid(file,strlen(file)-1,1) eq '.' then begin
		ext = '.'
	endif else if ext eq '' then begin
		ext = ''
	endif else begin
		ext = '.' + ext
	endelse

	file = strip_file_ext(file)
	if n_elements(remove) gt 0 then begin
		file = str_remove( remove, file)
	endif
	if n_elements(nelement) gt 0 then begin
		if nelement ge 1 then begin
			for i=1,nelement do begin
			    file = strip_file_m( file, /element)
			endfor
		endif
	endif
	if n_elements(file_m) gt 0 then begin
		if file_m ge 1 then begin
			mext = ['-m','-g','-r','-e','-s','-c','-d','-x','-q1','-q2','-q3','-q4']
			file = str_remove( mext, file)
		endif
	endif
	if n_elements(add) gt 0 then begin
		for i=0,n_elements(add)-1 do begin	
			file = file + add[i]
		endfor
	endif
    file = file + ext
	return, file
end


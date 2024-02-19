pro free_midas_record, p

;	Free the 'read_midas' and 'read_midas_segment' records

COMPILE_OPT STRICTARR

	if n_elements(p) eq 0 then return
	if ptr_good(p[0]) eq 0 then return
	n = n_elements(p)
	for i=0L,n-1 do begin
		if size( *p[i], /tname) eq 'POINTER' then begin
			free_midas_record, *p[i]
		endif else begin
			if tag_present('B',*p[i]) then begin
				if ptr_valid( (*p[i]).b) then ptr_free, (*p[i]).b
			endif
		endelse
		ptr_free, p[i]
	endfor
	return
end

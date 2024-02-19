function extract_extension, file

; Extract the extension after last "."
; But not if last path separator comes later.

	nf = n_elements(file)
	if nf lt 1 then return, ''
	f = strarr(nf)
	q = where( strlen(file) gt 0)
	if q[0] eq -1 then return, f

	for i=0L,nf-1 do begin 
		n = lenchr(file[i])
		m = locate_last(';',file[i]) < (n-1)
		if m lt 0 then m=n

		ks = locate_last(slash(),file[i]) < (n-1)

		k = locate_last('.',file[i]) < (n-1)
		if (k lt 0) or (k lt ks) then begin
			f[i]=''
		endif else begin
			if m-1 ge k+1 then begin
				f[i] = strmid( file[i], k+1, m-(k+1))
			endif
		endelse
	endfor
	
	if n_elements(f) eq 1 then f=f[0]
	return, f
end

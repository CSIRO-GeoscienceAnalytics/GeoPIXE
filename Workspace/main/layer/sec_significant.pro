function sec_significant, slice, e, z, e_bind, sec_found=sec_found

;	Check to see if secondary fluorescence due to X-rays of
;	energy 'e' from elements in 'slice' (at. fraction) will be significant on
;	destination element of atomic number 'z', and edge energy 'e_bind'.
;	N.B. Assumes that source and destination element arrays have same Z order.
;
;	sec_found returns true if any SOURCE element line can fluoresce others.

	n_els = n_elements(e[0,*])
	n_lines = n_elements(e[*,0])
	sig = bytarr(n_lines*n_els, n_els)
	sec_found = 0
	if (n_elements(z) ne n_els) then begin
		warning,'sec_significant','bad input array dimensions'
		goto, done
	endif
	good = bytarr(n_lines,n_els)
	q = where(z gt 0)							; need to reject Compton, etc.
	good[*,q] = 1
	klm = [1,1,4,9]

	f = fltarr(n_els)
	for i=0L,slice.N-1 do begin
		q = where( z eq slice.Z[i])
		if q[0] ne -1 then f[q] = slice.F[i]
	endfor

	c = replicate(1,n_lines) # f			; at. fraction matrix of same size as [lines,els]

	for i2=0,n_els-1 do begin
		q = where( (e gt e_bind[i2]) and (c gt 0.03) and (good eq 1))
		if q[0] ne -1 then begin
			sig[q,i2] = alog10(100.*c[q]) gt 1.8*( (e[q]-e_bind[i2])/e_bind[i2] )
		endif
	endfor

	q = where( sig eq 1)
	if q[0] ne -1 then sec_found=1

done:
	sig = reform( sig, n_lines, n_els, n_els, /overwrite)
	return, sig
	end

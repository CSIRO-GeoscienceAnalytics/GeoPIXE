function select_pige_x, peaks, e_low, e_high, a, n_channels, pcuts

	eow = e_low												; FWHM energy origin
	eoc = 0.7D0 * e_low + 0.3D0 * e_high					; Cal energy origin

	x_low = (fix( a[2] + a[3] * (e_low-eoc) + 0.5) > 0) < (n_channels-1)
	x_high = (fix( a[2] + a[3] * (e_high-eoc) + 0.5) > 0) < (n_channels-1)
	if x_low ge x_high then return, -1
	x_low = clip(x_low,0,n_channels-1)
	x_high = clip(x_high,0,n_channels-1)

	ok = intarr(n_channels)
	n_els = (*peaks).n_els
	for i=0L,n_els-1 do begin
		e = (*peaks).e[0,i]
		w = sqrt( a[0]*a[0] + a[1]*a[1] * (e-eow))
		c = a[2] + a[3] * (e-eoc)

		m1 = ( round(3.*w) < 1000) > 5						; Gaussian range (in line_gamma too)
		m2 = round(2.5*m1)									; step range (>m1)
;		m2 = round(1.5*m1)									; step range (>m1)
		i1 = clip(c-m2,x_low,x_high)
		i2 = clip(c+m2,x_low,x_high)
		ok[i1:i2] = 1
	endfor

	if ptr_valid( pcuts) then begin
		for i=0L,n_elements(*pcuts)-1 do begin
			if (*pcuts)[i].e[2] lt (*pcuts)[i].e[3] then begin
				xl = (fix( ( a[2] + a[3] * ((*pcuts)[i].e[2]-eoc)) + 0.5) > 0) < (n_channels-1)
				xh = (fix( ( a[2] + a[3] * ((*pcuts)[i].e[3]-eoc)) + 0.5) > 0) < (n_channels-1)
				ok[xl:xh] = 0
			endif
		endfor
	endif

	x = where(ok eq 1)
	return, x
	end

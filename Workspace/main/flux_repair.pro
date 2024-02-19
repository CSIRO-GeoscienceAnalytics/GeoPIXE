function flux_repair, fluxi, time, x=x, median=median_on

;	Repair flux array for glitches and missing values, spikes
;	and random noise.
;	X=0		normal time order of increasing X for Y=0
;	X=1		reverse - increasing X for Y=1

	siz = size(fluxi)
	if siz[0] ne 2 then return, fluxi
	flux = fluxi
	if n_elements(x) lt 1 then x=0
	if n_elements(median_on) lt 1 then median_on=0
	sx = siz[1]
	sy = siz[2]
	
;	Find time ordered index order ...

	indices = lindgen(sx,sy)
	if X eq 0 then begin
		j1 = 1L
		jn = sy-1
	endif else begin
		j1 = 0L
		jn = sy-2
	endelse
	for j=j1,jn,2 do begin					; build pixel indices in beam/time order
		indices[*,j] = reverse(indices[*,j])
	endfor
	indices = reform( indices, sx*sy)

	ymx = max(flux)
	ker = replicate( 1, (sx/10)>3)
	t = reform(flux[indices],sx*sy)
	
;	Determine min and max curves using dilate ...
;	Actually, with rounding these seem reversed.

	tmax = dilate( clip((255./ymx)*t,0,255), ker, /gray)
	t3 = 255 - clip((255./ymx)*t,0,255)
	q = where( t3 eq 255, nq)
	if nq gt 0 then t3[q] = 0
	tmin = 255 - dilate( t3, ker, /gray)
	t2 = (ymx/255.) * (tmin + tmax)*0.5
	dflux = t2[indices]

;	Fix missing flux values. Replace with ave. of min/max curves ...

	f = reform( flux, sx*sy)
	q = where( f lt ymx*0.03, nq)
	if nq gt 0 then f[q] = dflux[q]
	
;	Use Median filter to remove glitches ...

	if median_on then begin
		f2 = median( f, (sx/10)>3)
	endif else f2=f
	
	flux2 = reform( f2, sx,sy)
	return, flux2
end

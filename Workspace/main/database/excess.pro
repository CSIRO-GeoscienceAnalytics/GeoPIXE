	function excess, Zi, Ai, amu=amu

;	Return mass excess (MeV) for Z,A
;	return in AMU if /amu is set

	common c_excess, ze, n_e, ae, ex, excess_OK
	if n_elements(amu) lt 1 then amu=0

	z = zi
	a = round(ai)

	nz = n_elements(z)
	na = n_elements(a)
	nt = nz

	if (nz lt 1) or (na lt 1) then return, 0.0
	if nz eq 1 then begin
		nt = na
		z = replicate(Z,nt)
	endif else if na eq 1 then begin
		nt = nz
		a = replicate(a,nt)
	endif else if nz ne na then begin
		print,'Excess: Z,N can only both be vectors if they have same length.'
		exces = fltarr(nt)
		goto, done
	endif

	exces = fltarr(nt)

	if( n_elements(excess_OK) eq 0) then excess_OK = 0
	if( excess_OK ne 1) then init_excess
	if( excess_OK ne 1) then return, exces

	for i=0L,nt-1 do begin
		q = where( (z[i] eq ze) and (a[i] eq ae))
		if q[0] ne -1 then exces[i] = ex[q[0]]
	endfor

done:
	exces = reform(exces)
	if n_elements(exces) eq 1 then exces = exces[0]
	if amu then exces = exces/931.481
	return, exces
	end

	function jump_ratio_xrf, Zi, Ni

;	Returns the absorption sec fluor yield jump ratio correction
;	for each shell, using the absorption jump-ratio of subshells
;	from the tabulations of Elam,
;	W.T. Elam et al, Radiation Physics and Chemistry 63 (2002) 121-128.
;
;	Note: This is the sec fluor yield factor, not the absorption
;		jump ratio itself. It also assumes that we are above all edges
;		for a shell, no split subshells.
;
;    shell = 1  K
;            2  L
;			 3  M

common c_xrf, xenergy, xrelint, old_xrf_e, xrf_OK
common c_ebel, ebel, xrf_ebel_OK
common c_elam, elam

	if( n_elements(xrf_OK) eq 0) then xrf_OK = 0
	if( xrf_OK ne 1) then init_xrf_lines
	if( xrf_OK ne 1) then goto, done

	z = zi
	n = ni

	nz = n_elements(z)
	nn = n_elements(n)
	nt = nz

	if (nz lt 1) or (nn lt 1) then return, 0.0
	if nz eq 1 then begin
		nt = nn
		z = replicate(z,nt)
	endif else if nn eq 1 then begin
		nt = nz
		n = replicate(n,nt)
	endif else if nz ne nn then begin
		print,'jump_ratio_xrf: Z,N can only both be vectors if they have same length.'
		yield = fltarr(nt)
		goto, done
	endif
	yield = fltarr(nt)

	for i=0L,nt-1 do begin
		if n[i] eq 2 then begin
			jump3 = elam[z[i]].jump[4]
			jump2 = elam[z[i]].jump[3]
			jump1 = elam[z[i]].jump[2]

			fy3 = (1.-1./jump3)/(jump1*jump2)
			fy2 = (1.-1./jump2)/(jump1)
			fy1 = (1.-1./jump1)

			yield[i] = fy3 + fy2 + fy1
		endif else begin
			yield[i] = 1. - 1./ebel[z].jump[n[i]]
		endelse
	endfor

done:
	q = where(yield le 1.0e-10, count)
	if count ne 0 then yield[q] = jump_ratio(z[q],n[q],photo=0)

	yield = reform(yield)
	if n_elements(yield) eq 1 then yield = yield[0]
	return, yield
	END

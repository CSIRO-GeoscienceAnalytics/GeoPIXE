	function fluor_yield_xrf, Zi, Ni

;	Returns the fluorescent yield of subshells from the tabulations
;	of Elam,
;	W.T. Elam et al, Radiation Physics and Chemistry 63 (2002) 121-128.
;
;        N = 1  K       5  Mi      9  Mv
;            2  Li      6  Mii
;            3  Lii     7  Miii
;            4  Liii    8  Miv

common c_xrf, xenergy, xrelint, old_xrf_e, xrf_OK
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
	endif else if nn eq 1 then begin
		nt = nz
	endif else if nz ne nn then begin
		print,'fluor_yield_xrf: Z,N can only both be vectors if they have same length.'
		yield = fltarr(nt)
		goto, done
	endif
	yield = fltarr(nt)

	if (nz gt 1) and (nn gt 1) then begin
		for i=0L,nt-1 do begin
			yield[i] = elam[z[i]].fluor[n[i]]
		endfor
	endif else begin
		yield[*] = elam[z].fluor[n]
	endelse

done:
	yield = reform(yield)
	if n_elements(yield) eq 1 then yield = yield[0]
	return, yield
	END

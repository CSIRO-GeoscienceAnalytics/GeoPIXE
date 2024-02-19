	function coster_kronig, zi, ni
;
;	Return Coster Kronig transition probabilities 'n' for element 'z'
;
;	Index Transition
;	0		f12
;	1		F13		total F13 = f13 + f12*f23 + f13'
;	2		f23

	common c_xrf, xenergy, xrelint, old_xrf_e, xrf_OK
	common c_elam, elam

	z = zi
	n = ni

	nz = n_elements(z)
	nn = n_elements(n)
	nt = nz

	if (nz lt 1) or (nn lt 1) then return, 0.0
	if nz eq 1 then begin
		nt = nn
		z = replicate(Z,nt)
	endif else if nn eq 1 then begin
		nt = nz
		n = replicate(n,nt)
	endif else if nz ne nn then begin
		print,'coster_kronig: Z,N can only both be vectors if they have same length.'
		e = fltarr(nt)
		goto, done
	endif
	e = fltarr(nt)

	if( n_elements(xrf_OK) eq 0) then xrf_OK = 0
	if( xrf_OK ne 1) then init_xrf_lines
	if( xrf_OK ne 1) then goto, done

	for i=0L,2 do begin
		q = where( ((z ge 1) and (z le 98)) and (n eq i), count )
		if (count ne 0) then begin
			case i of
				0: e[q] = elam[z[q]].ck.l.f12
				1: e[q] = elam[z[q]].ck.l.F13
				2: e[q] = elam[z[q]].ck.l.f23
			endcase
		endif
	endfor

done:
	e = reform(e)
	if n_elements(e) eq 1 then e = e[0]
	return, e
	end

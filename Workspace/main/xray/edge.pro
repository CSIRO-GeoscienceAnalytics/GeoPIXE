function edge, zi, ni

;	1  K		6  Mii		11 Nii		16 Nvii		21 Ov
;	2  Li		7  Miii		12 Niii		17 Oi		22 Pi
;	3  Lii		8  Miv		13 Niv		18 Oii		23 Pii
;	4  Liii		9  Mv		14 Nv		19 Oiii		24 Piii
;	5  Mi		10 Ni		15 Nvi		20 Oiv

; Return edge energy (keV) for all indices 'n' for each 'z'

COMPILE_OPT STRICTARR
common c_edge, edge, edge_OK

	z = zi
	n = ni
	if n_elements(n) eq 1 then begin
		if n eq 0 then n = 1 + indgen(24)
	endif
	
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
		print,'edge: Z,N can only both be vectors if they have same length.'
		e = fltarr(nt)
		goto, done
	endif
	e = fltarr(nt)

	if( n_elements(edge_OK) eq 0) then edge_OK = 0
	if( edge_OK ne 1) then init_edge
	if( edge_OK ne 1) then goto, done

	q = where( ((z ge 1) and (z lt n_elements(edge[*,0]))) and $
				((n ge 1) and (n le 24)) )
	if (q[0] eq -1) then goto, done

	e[q] = edge[z[q],n[q]-1]
done:
	e = reform(e)
	if n_elements(e) eq 1 then e = e[0]
	return, e
end

;-------------------------------------------------------------------------

;	if nz eq nn then begin
;		e[qz,qn] = edge[z[qz],n[qn]-1]
;	endif else begin
;		if (nz eq 1) or (nn eq 1) then begin
;			e[qz,qn] = edge[z[qz],n[qn]-1]
;		endif else begin
;			e[qz,qn] = edge[z[qz],n[qn]-1]
;			print,'edge: warning - argument mixed array lengths'
;		endelse
;	endelse

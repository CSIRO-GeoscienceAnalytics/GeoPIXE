function photo_subshell, zi, ni, ei

; Return sub-shell absorption cross-section (cm2)
; for energy 'ei' and element 'zi', subshell 'ni'
; Only 'e' can be a vector.
; 'ni' zero means total absorption cross-section using Ebel 'global data'.

; for sub-shell definitions ...
;	1  K		6  Mii		11 Nii		16 Nvii		21 Ov
;	2  Li		7  Miii		12 Niii		17 Oi		22 Pi
;	3  Lii		8  Miv		13 Niv		18 Oii		23 Pii
;	4  Liii		9  Mv		14 Nv		19 Oiii		24 Piii
;	5  Mi		10 Ni		15 Nvi		20 Oiv

COMPILE_OPT STRICTARR
	common c_xrf, xenergy, xrelint, old_xrf_e, xrf_OK
;	common c_ebel, tau, gabs, eedge, ejump, emass, xrf_ebel_OK
	common c_ebel, ebel, xrf_ebel_OK
	NA = 6.02252e23

	nen = n_elements(ei)
	nz = n_elements(zi)
	nn = n_elements(ni)
	if (nz lt 1) or (nen lt 1) or (nn lt 1) then return, 0.0

	e = ei
	z = zi[0]
	n = ni[0]

	r = fltarr(nen)
	if (n lt 0) or (n gt 24) or (z lt 1) or (z gt 94) then return, r

	if( n_elements(xrf_OK) eq 0) then xrf_OK = 0
	if( xrf_OK ne 1) then init_xrf_lines, max(e)
	if( xrf_OK ne 1) then goto, done

	if n eq 0 then begin					; total Abs
	  elast = 100.0
	  edge_index = [1,2,3,4,5,6,7,8,9,10]
	  global_index = [0,1,1,1,2,2,2,2,2,3]
	  multiplier = [1.,1.,1./ebel[z].jump[2],1./(ebel[z].jump[2]*ebel[z].jump[3]),1., $
	  		1./ebel[z].jump[5],1./(ebel[z].jump[5]*ebel[z].jump[6]),1./(ebel[z].jump[5]*ebel[z].jump[6]*ebel[z].jump[7]), $
	  		1./(ebel[z].jump[5]*ebel[z].jump[6]*ebel[z].jump[7]*ebel[z].jump[8]), $
	  		1./(ebel[z].jump[5]*ebel[z].jump[6]*ebel[z].jump[7]*ebel[z].jump[8]*ebel[z].jump[9]),1.]

	  for i=0L,8 do begin
		q = where( ((e gt ebel[z].edge[edge_index[i]]) and (e ge 1.0) and (e le elast)) )
		if (q[0] ne -1) then begin
			p = reform(ebel[z].abs[*,global_index[i]])

			lx = alog(e[q])
			m = n_elements(p[*])
			sum = replicate( p[m-1], nen)
			for j=m-2,0,-1 do begin
				sum = sum*lx + p[j]
			endfor
			r[q] = multiplier[i] * exp(sum)
		endif
		elast = ebel[z].edge[edge_index[i]]
	  endfor
	endif else begin						; sub-shell abs

		q = where( (e gt ebel[z].edge[n]) and ((e ge 0.1) and (e le 100.0)) )
		if (q[0] ne -1) then begin
			p = reform(ebel[z].tau[*,n])

			lx = alog(e[q])
			m = n_elements(p[*])
			sum = replicate( p[m-1], nen)
			for i=m-2,0,-1 do begin
				sum = sum*lx + p[i]
			endfor
			r[q] = exp(sum)
		endif
	endelse
done:
	r = ebel[z].mass * reform(r) / NA			; cm2 cross-section units
	if n_elements(r) eq 1 then r = r[0]
	return, r
	end

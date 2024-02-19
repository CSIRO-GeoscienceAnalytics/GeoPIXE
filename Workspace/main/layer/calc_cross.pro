	function calc_cross, z1,a1, z2,shelli, E, gamma=gamma, photo=photo, beam=beam

;	Calculate PIXE (or PIGE) cross-sections for beam z1,a1, target
;	z2,shell and energies E.
;	For XRF cross-sections, will assume that all E[] are the same,
;	unless in continuum mode, then its a vector across the source spectrum.

	COMPILE_OPT STRICTARR
	if n_elements(gamma) lt 1 then gamma=0
	if n_elements(photo) lt 1 then photo=0
	if n_elements(beam) lt 1 then begin					; beam spectrum flags
		continuum = 0
	endif else begin
		continuum = beam[0].continuum					; for a continuum source
	endelse
	if photo eq 0 then continuum = 0

	nels = n_elements(z2)
	nen = n_elements(E)
	shell = shelli
	if n_elements(shell) lt nels then shell=replicate(shell,nels)

	xsect = fltarr(	nels, nen)
	klm = [0,1,4,9,16]

;	We use the dominant sub-shell here as the relative intensities, calculated
;	in 'xrf_calc_int' assume that we'll use L3, M5 cross-sections only.
;	See s/w log 14, p. 108

	if photo then begin
		if continuum then begin
			for i=0L,nels-1 do begin
				if shell[i] eq 2 then begin
;					c1 = photo_subshell(z2[i],2,E)
;					c2 = photo_subshell(z2[i],3,E)
					c3 = photo_subshell(z2[i],4,E)

					xsect[i,*] = c3
				endif else if shell[i] eq 3 then begin
;					c1 = photo_subshell(z2[i],5,E)
;					c2 = photo_subshell(z2[i],6,E)
;					c3 = photo_subshell(z2[i],7,E)
;					c4 = photo_subshell(z2[i],8,E)
					c5 = photo_subshell(z2[i],9,E)

					xsect[i,*] = c5
				endif else begin
					xsect[i,*] = photo_subshell( z2[i], klm[shell[i]], E)
				endelse
			endfor
		endif else begin
			for i=0L,nels-1 do begin
				if shell[i] eq 2 then begin
;					c1 = photo_subshell(z2[i],2,E[0])
;					c2 = photo_subshell(z2[i],3,E[0])
					c3 = photo_subshell(z2[i],4,E[0])
	
					xsect[i,0] = c3
					xsect[i,*] = xsect[i,0]
				endif else if shell[i] eq 3 then begin
;					c1 = photo_subshell(z2[i],5,E[0])
;					c2 = photo_subshell(z2[i],6,E[0])
;					c3 = photo_subshell(z2[i],7,E[0])
;					c4 = photo_subshell(z2[i],8,E[0])
					c5 = photo_subshell(z2[i],9,E[0])
	
					xsect[i,0] = c5
					xsect[i,*] = xsect[i,0]
				endif else begin
					xsect[i,0] = photo_subshell( z2[i], klm[shell[i]], E[0])
					xsect[i,*] = xsect[i,0]
				endelse
			endfor
		endelse
	endif else begin
		for i=0L,nels-1 do begin
			xsect[i,*] = ion_xsect( z1,a1, z2[i], shell[i], E, gamma=gamma)
		endfor
	endelse

	return, xsect
	end

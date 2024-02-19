	FUNCTION AUGER_REL_INTENSITY, Zi, Ni


;	Relative radiative Auger line intensity     N = 1  KLL rad. Auger
;													2  KMM

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
		print,'Auger_Rel_Intensity: Z,N can only both be vectors if they have same length.'
		Auger_Rel_Intensity = fltarr(nt)
		goto, done
	endif

	Auger_Rel_Intensity = fltarr(nt)
;;	F = RELATIVE_INTENSITY(Z,1)

	q = where( n eq 1)
	if q[0] ne -1 then begin
		Auger_Rel_Intensity[q] = 2.53598E-2 * EXP(-0.66114*E_LINE(Z[q],2))  $
								* RELATIVE_INTENSITY(Z[q],9)
;;								* RELATIVE_INTENSITY(Z[q],9) / F[q]
	endif

	q = where( n eq 2)
	if q[0] ne -1 then begin
		Auger_Rel_Intensity[q] = 1.60459E-1 * EXP(-0.324192*E_LINE(Z[q],4)) $
								* RELATIVE_INTENSITY(Z[q],10)
;;								* RELATIVE_INTENSITY(Z[q],10) / F[q]
	endif

done:
	Auger_Rel_Intensity = reform(Auger_Rel_Intensity)
	if n_elements(Auger_Rel_Intensity) eq 1 then Auger_Rel_Intensity = Auger_Rel_Intensity[0]
	return, Auger_Rel_Intensity
	end

function Auger_Energy, zi, ni

;	energy of radiative Auger transition (keV)     N = 1  KLL Rad. Auger
;                                                      2  KMM

	AEE_KLL = fltarr(100)
	aee_kll[12:30] = [1.182,1.389,1.607,1.845,2.106,2.382,2.669,2.973, $
			           3.292,3.638,4.002,4.381,4.778,5.187,5.622,6.075, $
			           6.542,7.030,7.526]
    aee_kll[35] = [10.244]


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
		print,'Auger_energy: Z,N can only both be vectors if they have same length.'
		auger_energy = fltarr(nt)
		goto, done
	endif

	auger_energy = fltarr(nt)
	q = where( n eq 1)
	if q[0] ne -1 then begin
		AUGER_ENERGY[q] = AEE_KLL[Z[q]]
	endif

	q = where( n eq 2)
	if q[0] ne -1 then begin
		AUGER_ENERGY[q] = E_LINE(Z[q],2) + edge(Z[q],3) - edge(Z[q],5) $
					- 0.5*( edge(Z[q],6) + edge(Z[q],7) )
	endif

done:
	Auger_energy = reform(Auger_energy)
	if n_elements(Auger_energy) eq 1 then Auger_energy = Auger_energy[0]
	return, Auger_energy
	end

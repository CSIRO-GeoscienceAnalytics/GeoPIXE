	FUNCTION FLUOR_YIELD, Zi, Ni, photo=photo
;
;	Returns the fluorescent yield of the K, Li, Lii, Liii and Miv
;	subshells for Z= 15 - 92 from the parameterization of
;	M.O. Krause J. Phys. Chem. Ref Data 8 (1979) pp307
;
;        N = 1  K       5  Mi      9  Mv
;            2  Li      6  Mii
;            3  Lii     7  Miii
;            4  Liii    8  Miv

if n_elements(photo) lt 1 then photo=0
if photo then return, fluor_yield_xrf( Zi, Ni)

	C = [[8.51051E-2, 2.63414E-2, 1.63531E-4, -1.85999E-6], $	; K
     	 [1.70027E-1, 2.98746E-3, 8.70636E-5, -2.19916E-7], $	; Li
     	 [2.72447E-1,-9.47505E-4, 1.37650E-4, -4.64780E-7], $	; Lii
     	 [1.77650E-1, 2.98937E-3, 8.91297E-5, -2.67184E-7], $	; Liii
     	[-3.25820E-1, 9.01791E-3, 3.80983E-5, -4.62897E-7]]		; Miv

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
		print,'fluor_yield: Z,N can only both be vectors if they have same length.'
		yield = fltarr(nt)
		goto, done
	endif
	yield = fltarr(nt)


	ZZ = FLOAT(Z)
	k = intarr(nt)

	q = where( (n ge 1) and (n le 4))
	if q[0] ne -1 then k[q] = n[q]

	q = where( (n ge 5) and (n le 9))
	if q[0] ne -1 then k[q] = 5			; Returns Miv for M-shell

	q = where( (k ge 1) and (k le 5))
	if q[0] ne -1 then begin
		yield[q] = (C[0,k[q]-1] + zz[q]*( C[1,k[q]-1] + zz[q]*( C[2,k[q]-1] + zz[q]*C[3,k[q]-1])))^4
		yield[q] = yield[q] / (1. + yield[q])
	endif

done:
	yield = reform(yield)
	if n_elements(yield) eq 1 then yield = yield[0]
	return, yield
	END

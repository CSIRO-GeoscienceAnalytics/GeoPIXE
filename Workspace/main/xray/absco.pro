	function absco_hubbell, Zi, Ei, photo=photo, compton=compton, pair=pair

; returns the mass absorption coefficient for the element of atomic number Z
; at energy E keV, based on the tables of Hubbell and Seltzer.

; Z and E can be vectors. If they both are vectors, then they must be equal length.
; Works much better for Z scalar and E vector.

COMPILE_OPT STRICTARR
	common c_hubbell_1, hubbell_ok, hubbell

	if n_elements(photo) lt 1 then photo=0
	if n_elements(compton) lt 1 then compton=0
	if n_elements(pair) lt 1 then pair=0
	if( n_elements(hubbell_OK) eq 0) then hubbell_OK = 0
	if( hubbell_OK ne 1) then init_hubbell
	if( hubbell_OK ne 1) then return, 0.0

	z = zi
	e = ei

	nen = n_elements(E)
	nz = n_elements(Z)
	nn = nz

	if (nen lt 1) or (nz lt 1) then return, 0.0
	if nen eq 1 then begin
		nn = nz
		absco_hubbell = fltarr(nn)

		q = where((z ge 1) and (z le 92))
		if q[0] eq -1 then goto, done

		for i=0L,n_elements(q)-1 do begin
			n = hubbell[z[q[i]]-1].n
			if z[q[i]] ne hubbell[z[q[i]]-1].z then print,'absco_hubbell: Z out of sync.'

			if photo then begin
				absco_hubbell[i] = exp( interpol( alog( hubbell[z[q[i]]-1].photo[0:n-1]), $
									alog( hubbell[z[q[i]]-1].e[0:n-1]), alog(e)))
			endif else if compton then begin
				absco_hubbell[i] = exp( interpol( alog( hubbell[z[q[i]]-1].compton[0:n-1]), $
									alog( hubbell[z[q[i]]-1].e[0:n-1]), alog(e)))
			endif else if pair then begin
				absco_hubbell[i] = exp( interpol( alog( hubbell[z[q[i]]-1].pair[0:n-1]), $
									alog( hubbell[z[q[i]]-1].e[0:n-1]), alog(e)))
			endif else begin
				absco_hubbell[i] = exp( interpol( alog( hubbell[z[q[i]]-1].atten[0:n-1]), $
									alog( hubbell[z[q[i]]-1].e[0:n-1]), alog(e)))
			endelse
		endfor
	endif else if nz eq 1 then begin
		nn = nen
		absco_hubbell = fltarr(nn)

		if (z lt 1) or (z gt 92) then goto, done
		n = hubbell[z-1].n
		if z ne hubbell[z-1].z then print,'absco_hubbell: Z out of sync.'

		if photo then begin
			absco_hubbell = exp( interpol( alog( hubbell[z-1].photo[0:n-1]), $
							alog( hubbell[z-1].e[0:n-1]), alog(e)))
		endif else if compton then begin
			absco_hubbell = exp( interpol( alog( hubbell[z-1].compton[0:n-1]), $
							alog( hubbell[z-1].e[0:n-1]), alog(e)))
		endif else if pair then begin
			absco_hubbell = exp( interpol( alog( hubbell[z-1].pair[0:n-1]), $
							alog( hubbell[z-1].e[0:n-1]), alog(e)))
		endif else begin
			absco_hubbell = exp( interpol( alog( hubbell[z-1].atten[0:n-1]), $
							alog( hubbell[z-1].e[0:n-1]), alog(e)))
		endelse
	endif else if nz ne nn then begin
		print,'absco_hubbell: Z,E can only both be vectors if they have same length.'
		absco_hubbell = fltarr(nn)
		goto, done
	endif

done:
	return, absco_hubbell
end

;--------------------------------------------------------------------------------

	function absco_poly, Zi, Ei

; returns the mass absorption coefficient for the element of atomic number Z
; at energy E keV.

; Z and E can be vectors. If they both are vector, then they must be equal length.

COMPILE_OPT STRICTARR
	z = zi
	e = ei

	COEFF = fltarr(7,10)
	coeff[*,*] = [  $
		-.6396,.4649,-.1357,2.404E-2,-9.382E-4,1.937E-5,-1.552E-7, $
		-.2887,.1264,-2.218E-2,1.985E-3,-3.189E-5,3.272E-7,-1.41E-9, $
		-9.885,1.749,-.1142,4.301E-3,-6.378E-5,5.325E-7,-1.884E-9, $
		-10.531,1.833,-.119,4.277E-3,-6.621E-5,5.606E-7,-1.974E-9, $
		56.0595,-8.3133,.48688,-1.436E-2,2.3126E-4,-1.8852E-6,6.1809E-9, $
		93.4532,-12.0904,.63557,-1.7339E-2,2.626E-4,-2.0483E-6,6.4866E-9, $
		79.245,-10.2893,.54254,-1.4837E-2,2.2494E-4,-1.7553E-6,5.557E-9, $
		68.993,-9.0531,.48183,-1.3277E-2,2.0225E-4,-1.5824E-6,5.0123E-9, $
		2.676E-2,-1.955E-3,3.973E-6,5.973E-5,1.19E-7,-6.135E-10, $
		1.532E-12, $
		-385.7058,37.148,-1.4501,2.9444E-2,-3.2486E-4,1.8472E-6, $
		-4.2113E-9 ]

	nen = n_elements(E)
	nz = n_elements(Z)
	nn = nz

	if (nen lt 1) or (nz lt 1) then return, 0.0
	if nen eq 1 then begin
		nn = nz
		e = replicate(E,nz)
	endif else if nz eq 1 then begin
		nn = nen
		z = replicate(Z,nen)
	endif else if nz ne nn then begin
		print,'absco_poly: Z,E can only both be vectors if they have same length.'
		absco_poly = fltarr(nn)
		goto, done
	endif
	absco_poly = fltarr(nn)

	W = ANGSTROMS(E)

	bad = where( ((Z gt 92) OR (Z LE 0)) or (E lt 0.1) )		; reject these
	more = replicate(1,nn)						; continue calculating
	if bad[0] ne -1 then more[bad] = 0
	test = intarr(nn)

	for n=1L,10 do begin
		IEX = N
		ED = EDGE(Z,IEX)
		q = where( ((ED gt 0.0) and (E gt ED)) and more)
		if q[0] ne -1 then begin
			test[*] = 0
			test[q] = 1
			more[q] = 0

			SUM = fltarr(nn)
			for I = 7,2,-1 do begin
				SUM[q] = (SUM[q] + COEFF[I-1,IEX-1]) * FLOAT(Z[q])
			ENDfor
			SUM[q] = SUM[q] + COEFF[0,IEX-1]

			q = where( SUM le 0.0)
			if q[0] ne -1 then begin
				sum[q] = 0.0
				test[q] = 0
			endif
			q = where( test)
			if q[0] ne -1 then begin
				K = 1+fix(IEX/2)-fix(IEX/4)+fix(IEX/5)-fix(IEX/6)+fix(IEX/9)-fix(IEX/10)
				EX = fltarr(nn)
				case K of
					1: begin
						EX[q] = 2.9308262+FLOAT(Z[q])*(-.020468343+FLOAT(Z[q])* $
							(.0010586311+FLOAT(Z[q])*(-3.741926E-5+FLOAT(Z[q])* $
							(7.204498E-7-5.7357248E-9*FLOAT(Z[q])))))
						GOTO, more19
						end
					2: begin
						EX[q] = 2.7376804+FLOAT(Z[q])*(-.0020643454+FLOAT(Z[q])* $
							(1.4651705E-4+FLOAT(Z[q])*(-3.5898183E-6+FLOAT(Z[q])* $
							(2.8732263E-8-8.1864238E-11*FLOAT(Z[q])))))
						GOTO, more19
						end
					3: begin
						EX[q] = 2.60
						GOTO, more21
						end
					4: begin
						EX[q] = 2.33
						GOTO, more21
						end
					5: begin
						EX[q] = 2.20
						GOTO, more21
						end
				endcase

 more19:
				IEX = fix(100.*EX+0.5)
				EX = float(IEX)
				EX = EX/100.

 more21:
				absco_poly[q] = SUM[q]*W[q]^EX[q]
			endif
		endif
	endfor

 done:
	absco_poly = reform(absco_poly)
	if n_elements(absco_poly) eq 1 then absco_poly = absco_poly[0]
	RETURN, absco_poly
	END

;----------------------------------------------------------------------------

function absco, Zi, Ei, hubbell=hubbell, mayer=mayer, poly=poly, gamma=gamma, $
					photo=photo, compton=compton, pair=pair

; returns the mass absorption coefficient (cm^2/g) for the element of atomic number Z
; at energy E (keV).
;
; Z and E can be vectors. If they both are vector, then they must be equal length.
;
; Database set:
;	0	Mayer and Rimini tables (default) (or if /Mayer set).
;	1	Berger and Hubbell (or if /Hubbell set, or /Gamma set)
;	2	polynomial approximation
;
; Mayer dataset:
;	Returns the mass absortion coefficient in cm**2 per g
;	for element Z at energy E (keV) for a given element using the
;	parameterisation of the coefficient table used in "Ion Beam
;	Handbook for Materials analysis", Mayer & Rimini, Academic Press,1977.
;
;	Where this parameterisation does not give a value a call
;	is made to a routine ABSCO_POLY which returns a value for applications
;	where continuous ranges of energy are involved. See this
;	program for documentation on the parametrisation used. The
;	Mayer & Rimini coefficients are stored in a lookup table
;	in the data statments below. Each line contains the coefficients
;	for a given Z range
;
; /Photo	Use ratio of Photo electric cross-section to total to return
;		mass attenuation coefficient for just photo electric term.
; /compton	Same for Compton component.
; /Pair		Same for pair production.

COMPILE_OPT STRICTARR
	common c_absco_1, absco_database_set
	if n_elements(absco_database_set) lt 1 then absco_database_set=0

	if n_elements(mayer) lt 1 then mayer=0
	if n_elements(hubbell) lt 1 then hubbell=0
	if n_elements(poly) lt 1 then poly=0
	if n_elements(gamma) lt 1 then gamma=0
	if n_elements(photo) lt 1 then photo=0
	if n_elements(compton) lt 1 then compton=0
	if n_elements(pair) lt 1 then pair=0

	if mayer or (photo and (absco_database_set eq 0)) then goto, mayer
	if hubbell or photo or gamma or compton or pair then begin
		return, absco_hubbell( Zi,Ei, photo=photo, compton=compton, pair=pair)
	endif

	case absco_database_set of
		0: goto, mayer
		1: return, absco_hubbell( Zi,Ei)
		2: return, absco_poly( Zi,Ei)
		else: goto, mayer
	endcase

mayer:
	TABLE = fltarr(3,5,7)

	table[*,0,0] = [5.40E-3,2.92,3.07]

	table[*,0:1,1] = [1.38E-2,2.79,2.73, 5.33E-4,2.74,3.03]

	table[*,0:1,2] = [3.12E-2,2.66,2.47, 9.59E-4,2.70,2.90]
	table[*,4,2] = [2.73E-5,2.44,3.47]

	table[*,0:1,3] = [5.23E-2,2.6,2.28, 1.03E-3,2.70,2.88]

	table[*,0:1,4] = [1.58E-2,2.47,2.53, 1.24E-3,2.70,2.83]
	table[*,3:4,4] = [8.03E-4,2.62,2.82, 1.58E-4,2.50,2.98]

	table[*,0:1,5] = [1.58E-2,2.47,2.53, 1.03E-4,2.50,3.38]
	table[*,4,5] = [9.39E-5,2.55,3.09]

	table[*,0:1,6] = [1.58E-2,2.47,2.53, 1.03E-4,2.50,3.38]
	table[*,4,6] = [5.76E-7,2.63,4.26]

	z = zi
	e = ei

	absco = 0.0
	nen = n_elements(E)
	nz = n_elements(Z)
	nn = nz

	if (nen lt 1) or (nz lt 1) then return, 0.0
	if nen eq 1 then begin
		nn = nz
		e = replicate(E,nz)
	endif else if nz eq 1 then begin
		nn = nen
		z = replicate(Z,nen)
	endif else if nen ne nz then begin
		print,'absco: Z,E can only both be vectors if they have same length.'
		absco = fltarr(nn)
		goto, done
	endif
	absco = fltarr(nn)

	W = ANGSTROMS(E)										;convert to wavelength

	Z_INDEX = replicate(0, nn)
	t = [10,18,36,54,71,86]
	for i=0L,5 do begin
		q = where(Z gt t[i])
		if q[0] ne -1 then z_index[q] = i+1
	endfor

	bad = where( ((Z gt 92) OR (Z LE 0)) or (E lt 0.2) )	; reject these
	more = replicate(1,nn)									; continue calculating
	if bad[0] ne -1 then more[bad] = 0
	poly = intarr(nn)										; use absco_poly
	test = intarr(nn)

	for K = 1,4 do begin
		q = where( (E gt EDGE(Z,K)) and more)
		if q[0] ne -1 then begin
			test[*] = 0
			test[q] = 1
			more[q] = 0

			q = where( (TABLE[0,K-1,Z_INDEX] LT 1.E-10) and test)
			if q[0] ne -1 then begin
				poly[q] = 1
				more[q] = 0
				test[q] = 0
			endif
			q = where( test)
			if q[0] ne -1 then begin
				absco[q] = TABLE[0,K-1,Z_INDEX[q]] * W[q]^TABLE[1,K-1,Z_INDEX[q]] $
					* Z[q]^TABLE[2,K-1,Z_INDEX[q]]
			endif
		endif
	ENDfor

	q = where( (E gt EDGE(Z,8)) and more)
	if q[0] ne -1 then begin
		test[*] = 0
		test[q] = 1
		more[q] = 0

		q = where( (TABLE[0,4,Z_INDEX] LT 1.E-10) and test)
		if q[0] ne -1 then begin
			poly[q] = 1
			more[q] = 0
			test[q] = 0
		endif
		q = where( test)
		if q[0] ne -1 then begin
			absco[q] = TABLE[0,4,Z_INDEX[q]] * W[q]^TABLE[1,4,Z_INDEX[q]] $
					* Z[q]^TABLE[2,4,Z_INDEX[q]]
		endif
	endif
	q = where( more)
	if q[0] ne -1 then poly[q] = 1
	q = where( (absco lt 1.0E-10) and (E gt 0.6))
	if q[0] ne -1 then poly[q] = 1

	q = where( poly)
  	if q[0] ne -1 then begin
		absco[q] = absco_poly( Z[q], E[q])
	endif
done:
	if bad[0] ne -1 then begin
		q = where( z[bad] eq 100)
		if q[0] ne -1 then absco[bad[q]] = 10000.
	endif
	absco = reform(absco)
	if n_elements(absco) eq 1 then absco = absco[0]
	RETURN, absco
END

function tube_spectrum2, E0, phi, eps, Zi, bin=bin, Energy=E, detector=deti, peak=peak, error=error, $
				galinstan=galinstan, proportion=proportion, char=char, formula=formula, weight=weight_formula, $
				lines=lines, transmission=transmission

; X-ray tube spectrum, according to Ebel's recommended: see also 'tube_spectrum.pro' for simpler version
; (H. Ebel , "X-ray tube spectra", XRS 28 (1999) 255-266).
; 
; Electron beam incident and X-rays exiting from same side usually, unless a "transmission" thickness
; is specified, then a thin transmission target is assumed.
;
; input:
;	Z		target Z (use Z only if no 'galinstan' index or 'formula' used)
;	E0		electron energy/tube voltage (keV)
;	phi		incident angle (to surface)
;	eps		takeoff angle (to surface)
;	bin		energy spectrum channel width (keV)
;	det		detector struct (else return spectrum per sr)
;	galinstan=n	selects one of the alloys:
;			0	off								use formula or Z
;			1	Ga = 95%, In = 5% 						(Alloy G1)
;			2	Ga = 68.5%, In = 21.5%, Sn = 10% 		(galinstan, Alloy I1)
;			3	Ga = 47%, In = 37%, Sn = 16% 			(Alloy I2)
;			4	Bi = 49%, Pb = 18%, Sn = 12%, In = 21%	(Ostalloy 136)
;			5	Bi = 32.5%, In = 51%, Sn = 16.5%		(Fields metal)
;	formula	use a chemical formula for the anode
;	/weight	formula uses weight% on elements (outside any brackets if present)
;	transmission	thickness of thin transmission target (mg/cm2)
;
; return:
;	dN		spectrum per each dE bin (ph / mA / keV / sr / s)
;	E		E values for spectrum
;	peak	peak line intensity
;	proportion	proportion in each channel due to continuum
;	char	characteristic line spectrum
;	lines	details of all characteristic lines
;	error	error=1, OK=0

	COMPILE_OPT STRICTARR
	common c_errors_1, catch_errors_on
	if n_elements(catch_errors_on) lt 1 then catch_errors_on=1
	if catch_errors_on then begin
		Catch, ErrorNo
		if (ErrorNo ne 0) then begin
			Catch, /cancel
			on_error, 1
			help, calls = s
			n = n_elements(s)
			c = 'Call stack: '
			if n gt 2 then c = [c, s[1:n-2]]
			warning,'tube_spectrum2',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0.
		endif
	endif
	error = 1

	if n_elements(Zi) lt 1 then Zi=45
	if n_elements(E0) lt 1 then E0=40.
	if n_elements(bin) lt 1 then bin=E0/1000.
	if n_elements(phi) lt 1 then phi=90.
	if n_elements(eps) lt 1 then eps=5.
	if n_elements(galinstan) lt 1 then galinstan=0
	if n_elements(formula) gt 0 then galinstan = 0
	if n_elements(weight_formula) lt 1 then weight_formula=0
	use_lay = 0
	if (n_elements(formula) gt 0) or (galinstan ne 0) then use_lay=1
	if n_elements(transmission) lt 1 then begin
		trans_on = 0
	endif else begin
		trans_on = 1
		trans_thick = transmission
		if trans_thick eq 0. then trans_on = 0
	endelse
	if E0 lt 1. then return, 0.
	if (Zi eq 0) and (use_lay eq 0) then return, 0.0
	
	startupp, /database
	Z = float(Zi)
	dE = bin
	
;	Tube continuum bremsstrahlung spectrum
;	get detector details (eff, solid-angle, FWHM parameters), if selected
;	else return data for 1 sr, 100% efficient 'unit' detector.

	n = 1.05*E0 / dE
	E = 1.05*E0 * (1+findgen(n))/float(n)
	if (n_elements(deti) ge 1) then begin
		det = (size( deti, /tname) eq 'POINTER') ? (*deti) : deti
		eff = det_eff( det, E)
		solid = 1.e-3 * solid_angle( det.distance, det.diameter, tilt=det.tilt, array=0, shape=det.shape)
		ge_det = (det.crystal.n eq 1) and (det.crystal.z[0] eq 32)
		if ge_det then begin
			w1 = 0.002089
			w0 = (det.resolution*det.resolution - w1 * 5.985) > 0.001
		endif else begin
			w1 = 0.0033
			w0 = (det.resolution*det.resolution - w1 * 5.985) > 0.001
		endelse
		fwhm1 = (sqrt( w0 + w1*E0/3) / bin) > 2
	endif else begin
		eff = 1.
		solid = 1.
		w1 = 0.0033
		w0 = (0.165*0.165 - w1 * 5.985) > 0.001
		fwhm1 = (sqrt( w0 + w1*E0/3) / bin) > 2
	endelse
	
;	If galinstan, just base rz electron mean depth on Ga only, as it varies weakly with Z,
;	but calculate absorption for a full layer 't' for the alloy.
;	Alternatively, we could do a weighted sum of rz, with mass weighting by Bragg rule (later).
;	Actually, Bragg rule weights dE/dX, so we should weight inverse range (1/rz) really.

	case galinstan of
		1: begin
			lay = make_layer( '(Ga)95(In)5', 100., name='AlloyG1', /weight_formula, error=err)
			if err then goto, bad_layer
			t = 1000. * atten( lay, E, /hubbell)
			ZZ = lay.Z
			FF = lay.F
			AA = mass( ZZ)
			NN = lay.n
			end
		2: begin
			lay = make_layer( '(Ga)68.5(In)21.5(Sn)10', 100., name='AlloyI1', /weight_formula, error=err)
			if err then goto, bad_layer
			t = 1000. * atten( lay, E, /hubbell)
			ZZ = lay.Z
			FF = lay.F
			AA = mass( ZZ)
			NN = lay.n
			end
		3: begin
			lay = make_layer( '(Ga)47(In)37(Sn)16', 100., name='AlloyI2', /weight_formula, error=err)
			if err then goto, bad_layer
			t = 1000. * atten( lay, E, /hubbell)
			ZZ = lay.Z
			FF = lay.F
			AA = mass( ZZ)
			NN = lay.n
			end
		4: begin
			lay = make_layer( '(Bi)49(Pb)18(Sn)12(In)21', 100., name='Ostalloy136', /weight_formula, error=err)
			if err then goto, bad_layer
			t = 1000. * atten( lay, E, /hubbell)
			ZZ = lay.Z
			FF = lay.F
			AA = mass( ZZ)
			NN = lay.n
			end
		5: begin
			lay = make_layer( '(Bi)32.5(In)51(Sn)16.5', 100., name='Fields', /weight_formula, error=err)
			if err then goto, bad_layer
			t = 1000. * atten( lay, E, /hubbell)
			ZZ = lay.Z
			FF = lay.F
			AA = mass( ZZ)
			NN = lay.n
			end
		else: begin
			if n_elements(formula) gt 0 then begin
				lay = make_layer( formula, 100., name=formula, weight=weight_formula, error=err)
				if err then goto, bad_layer
				t = 1000. * atten( lay, E, /hubbell)
				ZZ = lay.Z
				FF = lay.F
				AA = mass( ZZ)
				NN = lay.n
			endif else begin
				t = absco( Z, E, /hubbell)
				ZZ = Z
				AA = mass( ZZ)
				FF = 1.
				NN = 1
			endelse
			end
	endcase
	if NN eq 0 then return, 0.0

	irz = E
	irz[*] = 0.
	wirz = 0.
	for i=0,NN-1 do begin
		Z = ZZ[i]
		A = AA[i]
		weight = FF[i] * A
		
		U0 = E0/E
		J = 0.0135*Z
		m = 0.1382 - (0.9211/sqrt(Z))
		g = (E0^m)*(0.1904 - 0.2236*alog(Z) + 0.1292*(alog(Z))^2 - 0.0149*(alog(Z))^3)
		rzm = (A/Z)*(0.787e-5 * sqrt(J)*(E0^1.5) + 0.735e-6*(E0^2) )
		wirz = wirz + weight
		irz = irz + weight/(rzm * alog(U0) * ((0.49269 - 1.0987*g + 0.78557*g*g) / (0.70256 - 1.09865*g + 1.0046*g*g + alog(U0))))
	endfor
	rz = (wirz / irz) > 0.0
	
	const = 1.35e+9
	y = t*2.*rz*sin(phi*!dtor)/sin(eps*!dtor)	
	
	if trans_on then begin
		trt = (trans_thick/1000.)											; limit integral over 'rz*sin(phi)' 
		y = t*2.* (rz*sin(phi*!dtor) < trt) / sin(eps*!dtor)				; to less than thickness 'trt'
		
		f = exp( -t*trt/sin(eps*!dtor)) * (((exp(+y) - 1) / y) < 1000.) 	
	endif else begin
		y = t*2.*rz*sin(phi*!dtor)/sin(eps*!dtor)	
		f = (1 - exp(-y)) / y	
	endelse
	
;	print,'rz = ',rz[n_elements(rz)/3],' f = ',f[n_elements(f)/3]
	
;	Form atomic weighted sum for alloy ...

	dN = E
	dN[*] = 0.
	sW = 0.
	for i=0,NN-1 do begin
		Z = ZZ[i]
		A = AA[i]
		weight = FF[i]

		x = 1.109 - 0.00435*Z + 0.00175*E0	
		dN = dN + weight * const * eff*solid * Z * ( (E0/(E < E0)) - 1)^x * f * dE
		sW = sW + weight
	endfor
	dN = dN /sW
	
	q = where( finite(dN) eq 0, nq)
	if nq gt 0 then dN[q] = 0.
	
	char = dN							; same length as dN continuum spectrum
	char[*] = 0.

;	Characteristic lines (assume only K shell for now)
;	Prelim loop to get mean backscatter factor R, based on atomic weights

	R = 0.
	wR = 0.
	for i=0,NN-1 do begin
		Z = ZZ[i]
		A = AA[i]
		weight = FF[i]

		Ejk = edge( Z, 1)				; assume K shell
		U0 = E0/Ejk
	
;		Backscattering factor assumes current includes backscattered electron contribution ...
		
		R = R + weight * (1. - 0.0081517*Z + 3.613e-5*Z*Z + 0.009583*Z*exp(-U0) + 0.001141*E0)
		wR = wR + weight
	endfor
	R = R / wR
;	print,'Backscatter factor = ',R

;	Characteristic lines 
;	Use PIXE relative intensities for electrons.
;	Just add up contributions for all elements (atomic weighting),
;	which ignores matrix effects in 'rz' and stopping power terms for now.

	fwhm2 = 0.
	klm = [0,1,4,9,16]
	linmax = 10
	e_lines = fltarr(linmax,2*NN)							; for characteristic line return
	rels = fltarr(linmax,2*NN)
	arels = fltarr(linmax,2*NN)
	lines = intarr(linmax,2*NN)
	n_lines = intarr(2*NN)
	shells = intarr(2*NN)
	z2 = intarr(2*NN)
	nz = 0
	mxl = 0
	
	for i=0,NN-1 do begin
		Z = ZZ[i]
		A = AA[i]
		weight = FF[i]

		for shell=1,2 do begin
			Ejk = edge( Z, shell)
			if E0 lt Ejk then continue

			case shell of
				1: begin
					zk = 2.								; for K shell
					bk = 0.35
					end
				2: begin
					zk = 8.								; for L shell
					bk = 0.35
					end
			endcase
			list = list_line_index( Z, shell)
			pjkl = relative_intensity( Z, list) 		;/ total(relative_intensity( Z, list))
			Ejkl = e_line( Z, list)
			wjk = fluor_yield( Z, klm[shell])
			if use_lay then begin
				t = 1000. * atten( lay, Ejkl, /hubbell)
			endif else begin
				t = absco( Z, Ejkl, /hubbell)
			endelse			
		
			if (n_elements(deti) ge 1) then begin
				det = (size( deti, /tname) eq 'POINTER') ? (*deti) : deti
				eff = det_eff( det, Ejkl)
				solid = 1.e-3 * solid_angle( det.distance, det.diameter, tilt=det.tilt, array=0, shape=det.shape)
				ge_det = (det.crystal.n eq 1) and (det.crystal.z[0] eq 32)
				if ge_det then begin
					w1 = 0.002089
					w0 = (det.resolution*det.resolution - w1 * 5.985) > 0.001
				endif else begin
					w1 = 0.0033
					w0 = (det.resolution*det.resolution - w1 * 5.985) > 0.001
				endelse
				fwhm2 = fwhm2 > ((sqrt( w0 + w1*Ejkl[0]) / bin) > 2)
			endif else begin
				eff = 1.
				solid = 1.
				w1 = 0.0033
				w0 = (0.165*0.165 - w1 * 5.985) > 0.001
				fwhm2 = fwhm2 > ((sqrt( w0 + w1*Ejkl[0]) / bin) > 2)
			endelse
	
			constkl = 6.e+13 * weight
	
;			ro-z mean electron depth term, for characteristic radiation ...
			U0 = E0/Ejk
			J = 0.0135*Z
			m = 0.1382 - (0.9211/sqrt(Z))
			g = (E0^m)*(0.1904 - 0.2236*alog(Z) + 0.1292*(alog(Z))^2 - 0.0149*(alog(Z))^3)
			rzm = (A/Z)*(0.787e-5 * sqrt(J)*(E0^1.5) + 0.735e-6*(E0^2) )
			rz = rzm * alog(U0) * ((0.49269 - 1.0987*g + 0.78557*g*g) / (0.70256 - 1.09865*g + 1.0046*g*g + alog(U0)))
			
			if trans_on then begin
				trt = (trans_thick/1000.)
				y = t*2.* (rz*sin(phi*!dtor) < trt)/sin(eps*!dtor)	
				fjkl = exp( -t*trt/sin(eps*!dtor)) * (((exp(+y) - 1) / y) < 1000.)
			endif else begin
				y = t*2.*rz*sin(phi*!dtor)/sin(eps*!dtor)	
				fjkl = (1 - exp(-y)) / y	
			endelse
;			print,'f(jkl) (Z=',Z,') = ',fjkl

;			Stopping power term ...
			SB = 1. + 16.05 * sqrt(J/Ejk) * ((sqrt(U0)*alog(U0) + 2.*(1.-sqrt(U0)))/(U0*alog(U0) + 1. - U0))
			Sjk = 1./(zk*bk*(U0*alog(U0) + 1. - U0) * SB/Z)
;			print,'Sjk (Z=',Z,') = ', Sjk

			Njkl = constkl * eff*solid * R * wjk * pjkl * fjkl / Sjk
	
;			Need to place these intensities in the right spectrum bin
;			Do we need to scale by bin energy width? No; only for the continuum spectrum above.
			scale = 1.
		
			E1 = E + dE/2.										; right of energy bins
			for k=0,n_elements(list)-1 do begin
				q = where( E1 gt Ejkl[k], nq)
				if nq gt 0 then begin
					char[q[0]] += scale * Njkl[k]
					if k eq 0 then peak = scale * Njkl[k]		; peak major line intensity
				endif											; messed up for /galinstan?
			endfor
;		print,'Peak intensity (Z=',Z,') = ',peak

			nlist = n_elements(list) < linmax
			mxl = mxl > nlist
			n_lines[nz] = nlist
			lines[0:nlist-1,nz] = list[0:nlist-1]
			e_lines[0:nlist-1,nz] = Ejkl[0:nlist-1]
			arels[0:nlist-1,nz] = Njkl[0:nlist-1]
			rels[0:nlist-1,nz] = Njkl[0:nlist-1] / Njkl[0]
			z2[nz] = Z
			shells[nz] = shell
			nz = nz+1
		endfor
	endfor
	
	if nz gt 0 then begin
		amax = max(arels)
		q = where( arels[0,0:nz-1] gt 1.0e-3*amax, nq)			; veto
		if nq gt 0 then begin
			lines = { n_lines:	n_lines[q], $					; number of lines per element/shell
					Z:			z2[q], $						; Z
					shell:		shells[q], $					; shells of elements
					line:		lines[0:mxl-1,q], $				; line index
					e:			e_lines[0:mxl-1,q], $			; line energy
					rel:		rels[0:mxl-1,q], $				; line relative intensity
					arel:		arels[0:mxl-1,q]}				; absolute line intensity
		endif
	endif
			
	q = where( finite(dN) eq 0, nq)
	if nq gt 0 then dN[q] = 0.
	q = where( finite(char) eq 0, nq)
	if nq gt 0 then char[q] = 0.
	
	char = gaussian_smooth( char, 0, n_elements(char)-1, fwhm2)
	
	dN = gaussian_smooth( dN, 0, n_elements(dN)-1, fwhm1)

;	print,'Total: brem:',total(dN),', lines:',total(char)
	tot = (dN + char)

	proportion = dN / tot
	q = where( finite(proportion) eq 0, nq)
	if nq gt 0 then proportion[q] = 0.
	error = 0
	return, tot
	
bad_layer:
	warning,'tube_spectrum2','Bad layer spec for compound anode'
	return, 0.
end

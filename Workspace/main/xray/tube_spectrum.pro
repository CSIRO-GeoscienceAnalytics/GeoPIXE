function tube_spectrum, E0, phi, eps, Zi, bin=bin, E=E, detector=deti, peak=peak

; X-ray tube spectrum, according to Ebel's recommended:
; (H. Ebel , "X-ray tube spectra", XRS 28 (1999) 255-266)
;
; input:
;	Z		target Z
;	E0		electron energy/tube voltage (keV)
;	phi		incident angle (to surface)
;	eps		takeoff angle (to surface)
;	bin		energy spectrum channel width (keV)
;	det		detector struct (else return spectrum per sr)
;
; return:
;	dN		spectrum per each dE bin (ph / mA / keV / sr / s)
;	E		E values for spectrum
;	peak	peak line intensity

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
			warning,'tube_spectrum',['IDL run-time error caught.', '', $
					'Error:  '+strtrim(!error_state.name,2), $
					!Error_state.msg,'',c], /error
			MESSAGE, /RESET
			return, 0.
		endif
	endif

	if n_elements(Zi) lt 1 then Zi=45
	if n_elements(E0) lt 1 then E0=40.
	if n_elements(bin) lt 1 then bin=E0/1000.
	if n_elements(phi) lt 1 then phi=90.
	if n_elements(eps) lt 1 then eps=5.
	startupp, /database
	Z = float(Zi)
	dE = bin
	
;	Tube continuum bremsstrahlung spectrum

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
	
	t = absco( Zi, E, /hubbell)
	A = mass( Zi)
	
	U0 = E0/E
	J = 0.0135*Z
	m = 0.1382 - (0.9211/sqrt(Z))
	g = (E0^m)*(0.1904 - 0.2236*alog(Z) + 0.1292*(alog(Z))^2 - 0.0149*(alog(Z))^3)
	rzm = (A/Z)*(0.787e-5 * sqrt(J)*(E0^1.5) + 0.735e-6*(E0^2) )
	rz = rzm * alog(U0) * ((0.49269 - 1.0987*g + 0.78557*g*g) / (0.70256 - 1.09865*g + 1.0046*g*g + alog(U0)))
	
	const = 1.35e+9
	x = 1.109 - 0.00435*Z + 0.00175*E0
	
	y = t*2.*rz*sin(phi*!dtor)/sin(eps*!dtor)	
	f = (1 - exp(-y)) / y
	
	print,'rz = ',rz[n_elements(rz)/3],' f = ',f[n_elements(f)/3]
	
	dN = const * eff*solid * Z * ( (E0/(E < E0)) - 1)^x * f * dE
	
	q = where( finite(dN) eq 0, nq)
	if nq gt 0 then dN[q] = 0.
	
;	Characteristic lines (assume only K shell for now)
;	Use PIXE relative intensities for electrons.

	shell = 1						; assume K shell only for now
	zk = 2.							;	(zl = 8.	for L shell	)
	bk = 0.35						;	(bl = 0.25				)
	
	list = list_line_index( Zi, shell)
	pjkl = relative_intensity( Zi, list) 		;/ total(relative_intensity( Zi, list))
	Ejkl = e_line( Zi, list)
	Ejk = edge( Zi, 1)				; assume K shell
	wjk = fluor_yield( Zi, 1)
	t = absco( Zi, Ejkl, /hubbell)
	
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
		fwhm2 = (sqrt( w0 + w1*Ejkl[0]) / bin) > 2
	endif else begin
		eff = 1.
		solid = 1.
		w1 = 0.0033
		w0 = (0.165*0.165 - w1 * 5.985) > 0.001
		fwhm2 = (sqrt( w0 + w1*Ejkl[0]) / bin) > 2
	endelse
	
	constkl = 6.e+13
		
;	ro-z mean electron depth term, for characteristic radiation ...
	U0 = E0/Ejk
	J = 0.0135*Z
	m = 0.1382 - (0.9211/sqrt(Z))
	g = (E0^m)*(0.1904 - 0.2236*alog(Z) + 0.1292*(alog(Z))^2 - 0.0149*(alog(Z))^3)
	rzm = (A/Z)*(0.787e-5 * sqrt(J)*(E0^1.5) + 0.735e-6*(E0^2) )
	rz = rzm * alog(U0) * ((0.49269 - 1.0987*g + 0.78557*g*g) / (0.70256 - 1.09865*g + 1.0046*g*g + alog(U0)))
	y = t*2.*rz*sin(phi*!dtor)/sin(eps*!dtor)	
	fjkl = (1 - exp(-y)) / y

;	Inverse of stopping power term ...
	SB = 1. + 16.05 * sqrt(J/Ejk) * ((sqrt(U0)*alog(U0) + 2.*(1.-sqrt(U0)))/(U0*alog(U0) + 1. - U0))
	iSjk = zk*bk*(U0*alog(U0) + 1. - U0) * SB/Z
	
;	Backscattering factor assumes current includes backscattered electron contribution ...
	R = 1. - 0.0081517*Z + 3.613e-5*Z*Z + 0.009583*Z*exp(-U0) + 0.001141*E0

	Njkl = constkl * eff*solid * R * wjk * pjkl * fjkl * iSjk
	
;	Need to place these intensities in the right spectrum bin
;	Do we need to scale by bin energy width? No; only for the continuum spectrum above.
	scale = 1.
	
	char = dN									; same length as dN continuum spectrum
	char[*] = 0.

	E1 = E + dE/2.								; right of energy bins
	for i=0,n_elements(list)-1 do begin
		q = where( E1 gt Ejkl[i], nq)
		if nq gt 0 then begin
			char[q[0]] += scale * Njkl[i]
			peak = scale * Njkl[i]				; peak major line intensity
		endif
	endfor
	
	char = gaussian_smooth( char, 0, n_elements(char)-1, fwhm2)
	
	dN = gaussian_smooth( dN, 0, n_elements(dN)-1, fwhm1)

	print,'Total: brem:',total(dN),', lines:',total(char)
	return, dN + char
end

pro line, k, ei, beta, xc, a, f, df, detector, tail_amp, tail_len, $
		mask=mask, do_tail=do_tail, e_low=e_low, e_high=e_high, $
		no_df=no_df, auger=auger, tweek=tweek, compton=comp, $
		e_beam=e_beam, comp_tail_amp=Camp, comp_tail_len=Clen, $
		comp_shift=comp_shift, comp_spread=comp_spread, beta_tail=beta_tail

;	Calculate spectrum vector increment for X-ray line at energy 'e'
;	for element 'k', with relative intensity 'beta'.
;
;	'xc' is the vector of all channels to fit (less cuts, etc.).
;
;	'a' is the vector of ALL parameters.
;	'mask' enables the varying of selected parameters.
;	'detector' is a pointer to a detector struct.
;
;	Increment the function vector 'f', and the partial differivative 'df'.
;	Both of these run over ALL spectrum channels.
;
;	Note that the 'df' matrix is also over ALL parameters, even those
;	that are 'mask'ed off.
;
;	'do_tail'	enables a tail for this line.
;	'beta_tail'	scaling factor for tails, larger for beta
;	'tail_amp'	tail amplitude
;	'tail_len'	tail length
;	'e_low, e_high' are nominally the fitting range.
;
;	'no_df'		veto calculation of DF terms.
;	'tweek'		select tweek scaling parameter for this line.
;	'auger=1'	for a radiative Auger line.
;
;	'comp=1'	this flags a Compton scattering line
;	'e_beam'	the photon energy when using Compton
;	'Camp'		Compton tail amplitude
;	'Clen'		Compton tail length
;	'comp_shift'	adjust Compton peak position due to mean electron momentum
;	'comp_spread'	adjust Compton peak spread due to mean electron momentum distribution
;
;	Remember that arrays 'f' and 'df' are over the full spectrum range.
;	However, the calculation here is limited to the range of channels
;	given by 'xc'.
;
;	See Method/software log 8, page 260.
;
;	Parameters:	0 Noise		2 cal B		4 pileup	5 tail amp		7  backgnd 1	8 Compton tail amp
;				1 Fano		3 cal A					6 tail length	10 backgnd 2	9 Compton tail length
;
;-------------------------------------------------------------------------

COMPILE_OPT STRICTARR

	if n_elements(mask) lt 1 then mask = replicate(1, n_elements(a))
	if n_elements(do_tail) lt 1 then do_tail = 0
	if n_elements(e_low) lt 1 then e_low = 4.0
	if n_elements(e_high) lt 1 then e_high = 36.0
	if n_elements(no_df) lt 1 then no_df=0
	if n_elements(tweek) lt 1 then tweek=-1
	if n_elements(comp) lt 1 then comp=0
	if n_elements(e_beam) lt 1 then e_beam=0.0
	if n_elements(tail_amp) lt 1 then tail_amp=0.01
	if n_elements(tail_len) lt 1 then tail_len=0.5
	if n_elements(Camp) lt 1 then Camp = 1.0
	if n_elements(Clen) lt 1 then Clen = 1.0
	if n_elements(comp_shift) lt 1 then comp_shift=-0.006
	if n_elements(comp_spread) lt 1 then comp_spread=1.0

	if no_df and (abs(a[k]) lt 1.0e-10) then return
	do_df = (no_df eq 0)
	nxc = n_elements(xc)
	e = ei

	kw = 5.5451D0
	kw05 = sqrt(kw)
	kf = 0.39894D0
	eow = e_low												; FWHM energy origin
	eoc = 0.7D0 * e_low + 0.3D0 * e_high					; Cal energy origin

	alpha_zero = -0.05D0									; tail amplitude origin
	gamma_zero = 0.1D0										; tail length minimum
	rho_zero = 0.2D0										; Compton tail amplitude minimum
	eta_zero = 0.1D0										; Compton tail length minimum
	do_high_tail = 0
	Rel_amp2 = 0.0D0		; 1.0		; 2.				; Compton high tail amplitude rel to low
	Rel_len2 = 1.5D0		; 1.5		; 0.5				; Compton high tail length rel to low

;	FWHM and centroid, all in channels

	w2 = double( a[0]*a[0] + a[1]*a[1] * (e-eow))
	w = sqrt(w2)											; FWHM (channels)

	if auger then begin										; For Auger lines, the energy
		e = e - (0.5*w/a[3])								; is the end-point energy
		w = 2.5 * w											; of a broader distribution.
		w2 = w*w
	endif
	if comp ge 1 then begin
;		w = sqrt(2.) * w									; cgr, try this bee
;		de = 0.025 * e_beam*a[3]							; good for lumps spectra
;		de = 0.022 * e_beam*a[3]							; need this for Bragg on pyrite @ 12.1
;		de = 0.03 * e_beam*a[3]								; good for pottery spectra
;		de = 0.018 * e_beam*a[3]							; Rob's PNC, donuts @ 16.1 keV
;		de = 0.007 * e_beam*a[3]							; Cathy\30

		de = e_beam - e										; care here, this may be Compton escape
		e = e - comp_shift * e_beam
		w2 = 2.0 * w2										; moved 'comp_spread' down to Compton section
		if comp eq 2 then begin
			w2=2.*w2										; wider for Compton+Compton pile-up
		endif
		w = sqrt(w2)
	endif

	c = double(a[2] + a[3] * (e-eoc))						; centroid channel
	if (c lt xc[0]) or (c gt xc[nxc-1]) then return

;-----------------------------------------------------------------------------
;	Determine channels 'x' to use for this line
;	Changed low side scale from 3 to 4 (26 July 06)

	if do_tail then begin
		m1 = ( fix(25.*w) < 1000) > 50						; For long tails
		m2 = ( fix(4.*w) < 1000) > 5
	endif else begin
		m1 = ( fix(4.*w) < 1000) > 5						; Gaussian range
		m2 = m1
	endelse
	q = where( (xc ge (c-m2)) and (xc le (c+m2)) )
	if q[0] eq -1 then return
	x = double(xc[q])

;-----------------------------------------------------------------------------
;	The Gaussian

	if comp ge 1 then begin
		w2 = w2 * comp_spread
		w = sqrt(w2)
	endif

	kxc = kw * (x-c) / w2
	kfbw = kf * kw05 * beta / w
	kxc2 = kxc * (x-c)
	g = exp( -kxc2 / 2.0D0)

	z = kfbw * g
	if do_df then begin
		if tweek ge 0 then begin
			df[x,k] = df[x,k] + z * a[tweek]
		endif else begin
			df[x,k] = df[x,k] + z
		endelse
	endif
	incf = a[k] * z

	if tweek ge 0 then begin
		if do_df and mask[tweek] then begin
			df[x,tweek] = df[x,tweek] + incf
		endif
		incf = incf * a[tweek]
	endif

	if do_df and (mask[0] or mask[1]) then begin
		z = incf * a[0] * ( kxc2 - 1.0D0) / w2
		df[x,0] = df[x,0] + z
		if mask[1] then df[x,1] = df[x,1] + z * a[1] * (e-eow) / a[0]
	endif

	if do_df and (mask[2] or mask[3]) then begin
		z = kxc * incf
		df[x,2] = df[x,2] + z
		if mask[3] then df[x,3] = df[x,3] + z * (e-eoc)
	endif

	f[x] = f[x] + incf

	if comp ge 1 then begin
		w2 = w2 / comp_spread
		w = sqrt(w2)
	endif

;------------------------------------------------------------------------------
;	The tail function

	if do_tail eq 0 then goto, ctail

	q = where( (xc ge (c-m1)) and (xc lt c) )
	if q[0] eq -1 then return
	x = double(xc[q])
	kxc = kw * (x-c) / w2
	kxc2 = kxc * (x-c)
	g = exp( -kxc2 / 2.0D0)

	alpha = tail_amp * ( alpha_zero + a[5]*a[5])
	gamma = tail_len * ( gamma_zero + a[6]*a[6])

	xcw = (x-c) / (gamma*w)
	aket = alpha * beta_tail * beta * exp(xcw) / w
	if tweek ge 0 then aket = aket * a[tweek]

	t = aket * (1.0D0 - g)
	if do_df then begin
		if tweek ge 0 then begin
			df[x,k] = df[x,k] + t * a[tweek]
		endif else begin
			df[x,k] = df[x,k] + t
		endelse
	endif

	t = a[k] * t

	if tweek ge 0 then begin
		if do_df and mask[tweek] then begin
			df[x,tweek] = df[x,tweek] + t
		endif
		t = t * a[tweek]
	endif

	f[x] = f[x] + t

	if no_df then goto, ctail

	aket = a[k] * aket

	if do_df and (mask[0] or mask[1]) then begin
		dtdw = -aket * ( (1.0 + xcw)*(1.0 - g) + kxc2*g) / w
		df[x,0] = df[x,0] + dtdw * a[0] / w
		df[x,1] = df[x,1] + dtdw * a[1] * (e-eow) / w
	endif

	if do_df and (mask[2] or mask[3]) then begin
		dtdc = -aket * ( (1.0 - g)/(w*gamma) + kxc*g)
		df[x,2] = df[x,2] + dtdc
		df[x,3] = df[x,3] + dtdc * (e-eoc)
	endif

	if do_df and (mask[5] or mask[6]) then begin
		dtda = t / alpha
		df[x,5] = df[x,5] + dtda * tail_amp * 2.0 * a[5]

		dtdg = -t * xcw / gamma
		df[x,6] = df[x,6] + dtdg * tail_len * 2.0 * a[6]
	endif

;------------------------------------------------------------------------------
;	The Compton tail function

ctail:
	if comp eq 0 then return
	q = where( (xc ge (c-m1)) and (xc lt c+w/2.) )
	if q[0] eq -1 then return
	x = double(xc[q])
	kxc = kw * (x-c-w/2.) / w2
	kxc2 = kxc * (x-c-w/2.)
	g = exp( -kxc2 / 2.0D0)

	rho = Camp * ( rho_zero + a[8]*a[8])
	eta = Clen * ( eta_zero + a[9]*a[9])

	xcw = (x-c) / (eta*w)
	v = rho * beta * exp(xcw) / w
	if tweek ge 0 then v = v * a[tweek]

	s = v * (1.0D0 - g)
	if do_df then df[x,k] = df[x,k] + s

	s = a[k] * s

	f[x] = f[x] + s

	if no_df then goto, done

	v = a[k] * v

	if do_df and (mask[0] or mask[1]) then begin
		dtdw = -v * ( (1.0 + xcw)*(1.0 - g) + kxc2*g) / w
		df[x,0] = df[x,0] + dtdw * a[0] / w
		df[x,1] = df[x,1] + dtdw * a[1] * (e-eow) / w
	endif

	if do_df and (mask[2] or mask[3]) then begin
		dtdc = -v * ( (1.0 - g)/(w*eta) + kxc*g)
		df[x,2] = df[x,2] + dtdc
		df[x,3] = df[x,3] + dtdc * (e-eoc)
	endif

	if do_df and (mask[8] or mask[9]) then begin
		dtda = s / rho
		df[x,8] = df[x,8] + dtda * Camp * 2.0 * a[8]

		dtdg = -s * xcw / eta
		df[x,9] = df[x,9] + dtdg * Clen * 2.0 * a[9]
	endif

;	High side tail ...

	if do_high_tail eq 0 then goto, done
	cbeam = a[2] + a[3] * (e_beam - eoc)
	q = where( (xc le (c+m1)) and (xc gt c) and (xc le cbeam) )
	if q[0] eq -1 then return
	x = double(xc[q])
	kxc = -kw * (x-c) / w2
	kxc2 = -kxc * (x-c)
	g = exp( -kxc2 / 2.0D0)

	Camp2 = Rel_amp2 * Camp
	Clen2 = Rel_len2 * Clen

	rho = Camp2 * ( rho_zero + a[8]*a[8])
	eta = Clen2 * ( eta_zero + a[9]*a[9])

	xcw = -(x-c) / (eta*w)
	v = beta * exp(xcw) / w
	if tweek ge 0 then v = v * a[tweek]

	s = v * (1.0D0 - g)
	if do_df then df[x,k] = df[x,k] + s * rho

	s = a[k] * s
	f[x] = f[x] + s * rho

	if no_df then goto, done

	v = a[k] * v * rho

	if do_df and (mask[0] or mask[1]) then begin
		dtdw = -v * ( (1.0 + xcw)*(1.0 - g) + kxc2*g) / w
		df[x,0] = df[x,0] + dtdw * a[0] / w
		df[x,1] = df[x,1] + dtdw * a[1] * (e-eow) / w
	endif

	if do_df and (mask[2] or mask[3]) then begin
		dtdc = -v * ( (1.0 - g)/(w*eta) + kxc*g)
		df[x,2] = df[x,2] + dtdc
		df[x,3] = df[x,3] + dtdc * (e-eoc)
	endif

	if do_df and (mask[8] or mask[9]) then begin
		dtda = s
		df[x,8] = df[x,8] + dtda * Camp2 * 2.0 * a[8]

		dtdg = -s * rho * xcw / eta
		df[x,9] = df[x,9] + dtdg * Clen2 * 2.0 * a[9]
	endif

done:
	return
	end


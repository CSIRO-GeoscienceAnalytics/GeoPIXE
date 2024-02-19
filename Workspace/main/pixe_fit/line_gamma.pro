pro line_gamma, k, kb, ei, beta, xc, a, f, df, detector, $
		mask=mask,  do_step=do_step, e_low=e_low, e_high=e_high, $
		no_df=no_df

;	Calculate spectrum vector increment for gamma-ray line at energy 'ei'
;	for element 'k', and relative intensity 'beta'. Note that 'ei' is not a vector.
;	;kb' is parameter index for flat background term.
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
;	'do_step' enables a step for this line.
;	'e_low, e_high' are nominally the fitting range.
;
;	'no_df'		veto calculation of DF terms.
;
;	Remember that arrays 'f' and 'df' are over the full spectrum range.
;	However, the calculation here is limited to the range of channels
;	given by 'xc'.
;
;	See Method/software log 13, page 98.
;
;	Parameters:	0 Noise		2 cal offset	4 free		5 step amp		7 free
;				1 Fano		3 cal gain					6 step slope	8,9 free
;
;-------------------------------------------------------------------------

COMPILE_OPT STRICTARR

	if n_elements(mask) lt 1 then mask = replicate(1, n_elements(a))
	if n_elements(do_step) lt 1 then do_step = 0
	if n_elements(e_low) lt 1 then e_low = 4.0
	if n_elements(e_high) lt 1 then e_high = 36.0
	if n_elements(no_df) lt 1 then no_df=0
	if no_df and (abs(a[k]) lt 1.0e-10) then return
	do_df = (no_df eq 0)
	nxc = n_elements(xc)
	e = ei

	kw = 5.5451D0
	kw2 = sqrt(kw)
	kf = 0.39894D0
;	kg = 2.1972D0
	kg = 1.5D0
	eow = e_low												; FWHM energy origin
	eoc = 0.7D0 * e_low + 0.3D0 * e_high					; Cal energy origin

;	FWHM and centroid, all in channels

	w2 = double( a[0]*a[0] + a[1]*a[1] * (e-eow))
	w = sqrt(w2)											; FWHM

	c = double(a[2] + a[3] * (e-eoc))						; centroid channel
	if (c lt xc[0]) or (c gt xc[nxc-1]) then return

;-----------------------------------------------------------------------------
;	Determine channels 'x' to use for this line

	m1 = ( round(3.*w) < 1000) > 5							; Gaussian range (in select_pige_x too)
	m2 = round(2.5*m1)										; step range (>m1)
;	m2 = round(1.5*m1)											; step range (>m1)

;-----------------------------------------------------------------------------
;	The Gaussian

	q = where( (xc ge (c-m1)) and (xc le (c+m1)) )
	if q[0] eq -1 then return
	x = double(xc[q])

	z = (x-c) / w
	kxc = kw * z / w
	kfw = kf * kw2 / w
	kxc2 = kxc * (x-c)
	exp1 = exp( -kxc2 / 2.0D0)
	g = kfw * exp1

	incf = a[k]*beta * g

	if do_df then begin
		df[x,k] = df[x,k] + g*beta

		if (mask[0] or mask[1]) then begin
			t = incf * a[0] * ( kxc2 - 1.0D0) / w2
			df[x,0] = df[x,0] + t
			if mask[1] then df[x,1] = df[x,1] + t * a[1] * (e-eow) / a[0]
		endif

		if (mask[2] or mask[3]) then begin
			t = kxc * incf
			df[x,2] = df[x,2] + t
			if mask[3] then df[x,3] = df[x,3] + t * (e-eoc)
		endif
	endif

	f[x] = f[x] + incf

;------------------------------------------------------------------------------
;	The step function

	q = where( (xc ge (c-m2)) and (xc le (c+m2)) )
	x = double(xc[q])

	f[x] = f[x] + a[kb]

;	Flat background

	if do_df then df[x,kb] = 1.0D0

	if do_step eq 0 then return

;	Step function

	z = (x-c) / w
	exp2 = exp( kg * z)
	rat = 1.D0/(1.D0 + exp2)
	step = a[5]*(1.D0 + a[6]*e) / a[3]
	srat = step * rat

	f[x] = f[x] + a[k]*beta * srat

	if do_df then begin
		df[x,k] = df[x,k] + srat*beta

		dtb = a[k] * beta * rat
		dtf = dtb * srat * exp2 * kg

		if mask[0] or mask[1] then begin
			dtdw = dtf * z/w
			df[x,0] = df[x,0] + dtdw * a[0] / w
			df[x,1] = df[x,1] + dtdw * a[1] * (e-eow) / w
		endif

		if mask[2] or mask[3] then begin
			dtdw = dtf / w
			df[x,2] = df[x,2] + dtdw
			df[x,3] = df[x,3] + dtdw * (e-eoc) - dtb * step/a[3]
		endif

		if mask[5] or mask[6] then begin
			dtdw = dtb / a[3]
			df[x,5] = df[x,5] + dtdw * (1.+a[6]*e)
			df[x,6] = df[x,6] + dtdw * a[5]*e
		endif
	endif

	return
	end


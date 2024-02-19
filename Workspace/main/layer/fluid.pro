;--------------------------------------------------------------------

function fluid_y_limits, x

;	Integration limits in Y, perpendicular to detector,
;	and parallel to surface

common c_fluid_limits, a,b,c, xm,ym,zm

	w = 1. - x*x/(a*a)

	if w le 0.0 then w = 0.01

	yp = b*sqrt(w) < ym

	return, [0.0, yp]
end

;--------------------------------------------------------------------

function fluid_z_limits, x,y

;	Integration limits in Z, parallel to surface normal

common c_fluid_limits, a,b,c, xm,ym,zm

	w = 1. - x*x/(a*a) - y*y/(b*b)

	if w le 0.0 then w = 0.01

	zp = c*sqrt(w) < zm

	return, [-zp,zp]
end

;--------------------------------------------------------------------

function si_path, x,y,z

;	Absorption path through Si (or whatever matrix is).
;	(Need to veto bubble volume in here later.)

common c_fluid_limits, a,b,c, xm,ym,zm
common c_fluid_pars1, Rx,Ry, Bflat, Bellipse, infinite, mu
common c_fluid_pars2, a_si,b_si,c_si

	if infinite then return, 1.0

	ca = 1. + ((c_si*c_si) / (a_si*a_si))
	xz2 = (x - z) * (x - z)
	By = 1. - ((y*y) / (b_si*b_si)) > 0.0

	x1 =  xz2 - ca*( xz2 - (c_si*c_si*By)) > 0.0
	x1 = ( (x-z) + sqrt(x1) ) / ca

	z1 = c_si * sqrt( By - ((x1*x1) / (a_si*a_si)) > 0.0 )

	f = (x - x1) + (c_si - z)
	return, sqrt( f*f + (c_si - z1)*(c_si - z1) )
end

;--------------------------------------------------------------------

function fluid_function, x,y,z

;	Integration in X, towards detector, Y, perpendicular to detector,
;	and parallel to surface, and Z, parallel to surface normal.

common c_fluid_limits, a,b,c, xm,ym,zm
common c_fluid_pars1, Rx,Ry, Bflat, Bellipse, infinite, mu

; Returns the transmission of X-ray through extra host path outside the ellipsoid,
; less the same path through the fluid, scaled by beam fall-off for a gaussian beam (BFlat=0)..
; If /infinite, then assume no absorption, which returns 1.0 if within inclusion,
; and within beam envelope.

;common c_test, beams
;if n_elements(beams) lt 1 then beams=1.0

	r = x*x/(a*a) + y*y/(b*b) + z*z/(c*c)
	n = n_elements(r)
	f = fltarr(n)
	q = where(r le 1.0)
	if q[0] eq -1 then return, f

	if Bflat then begin
	    beam = 1.0
	    if Bellipse then begin
			w = 1. - x*x/(Rx*Rx) - y*y/(Ry*Ry)
			if w lt 0.0 then beam = 0.0
	    endif else begin
			if (abs(x) gt Rx) or (abs(y) gt Ry) then beam = 0.0
	    endelse
	endif else begin
	    beam = exp( -2.3* (x*x/(Rx*Rx) + y*y/(Ry*Ry)) )
	endelse

;	beams = [beams,beam]

    si_transmission = replicate(1.0,n)
	if infinite eq 0 then si_transmission[q] = exp( -mu * si_path(x,y,z[q]) )

	f[q] = beam * si_transmission[q]
	return, f
end

;--------------------------------------------------------------------

pro setup_fluid_integral, Bfi, Rxi,Ryi, ai,bi,ci, toli,E, $
					range_in, matrix, d_matrix, set_cavity, ellipse, weight

; Uses the difference between host mu*d and that for the fluid, because the
; fluid one has already been assumed in the original planar modelling. This
; calculates the difference, for the correction.

common c_fluid_limits, a,b,c, xm,ym,zm
common c_fluid_pars1, Rx,Ry, Bflat, Bellipse, infinite, mu
common c_fluid_pars2, a_si,b_si,c_si

	d_h20 = 1.0									; g/cm**3
	c_Na = 0.079
	c_Fe = 0.084
	c_K = 0.031
	c_Cl = 0.256								; for 45 wt% tot
	salt = 20.0									; wt% salt

	a = ai										; dimensions of the inclusion
	b = bi
	c = ci

	if set_cavity  then begin					; dimensions of the cavity space
	    a_si = ai
	    b_si = bi
	    c_si = ci
	endif

	layer = make_layer( matrix, 1.0, weight=weight, error=error)

	Bflat = Bfi									; flat beam profile
	Bellipse = ellipse							; elliptical beam scan/extraction
	if Bflat then begin
	    Rx = Rxi								; Scanned beam dimensions
	    Ry = Ryi
	    xm = a < Rx
	    ym = b < Ry
	endif else begin
	    Rx = Rxi * 1.82							; FWTM for Gaussian beam
	    Ry = Ryi * 1.82
	    xm = a < 2.0*Rx
	    ym =  b < 2.0*Ry
	endelse

	zm = c
	tol = toli
	infinite = range_in

	c_H = 0.111 * (100. - salt)/100.
	c_O = 0.889 * (100. - salt)/100.

	mu_matrix = 1000.0 * atten(layer,E)

	mu_h2o = c_H * absco(1,E) + c_O * absco(8,E) + (salt/45.) *  $
 				(c_Na * absco(11,E) + c_Fe * absco(26,E) +  $
				 c_K * absco(19,E) + c_Cl * absco(17,E) )

	mu = (mu_matrix * d_matrix - mu_h2o * d_h20) * 0.0001		; for distance in microns

	return
end

;--------------------------------------------------------------------

function path_integral, Bfi, Rxi,Ryi, ai,bi,ci, toli, E, matrix, d_matrix, ellipse, $
			host_weight=weight, infinite_range=range_in, set_cavity=set_cavity

; Integrate absorption along path through host, scale by mu host-fluid
; to correct for new host absorption path less the original fluid absorption for this path segment.
; For high energies E, asymptotes to half of volume.

common c_fluid_limits, a,b,c, xm,ym,zm
common c_fluid_pars1, Rx,Ry, Bflat, Bellipse, infinite, mu
common c_fluid_pars2, a_si,b_si,c_si

	if n_elements(range_in) lt 1 then range_in = 0
	if n_elements(set_cavity) lt 1 then set_cavity = 0
	if n_elements(weight) lt 1 then weight = 0

	setup_fluid_integral, Bfi,Rxi,Ryi, ai,bi,ci, toli,e, range_in, $
				matrix,d_matrix, set_cavity, ellipse, weight

	f = int_3D( 'fluid_function', [-xm,xm], 'fluid_y_limits', 'fluid_z_limits', 10)

	return, f
end

;--------------------------------------------------------------------

function fluid, E, Rx,Ry, a,b,c, fluid_density, model_density, host_formula, host_density, $
			bubble_in, bubble_mode=bubble_mode, $
			beam_mode=beam_mode, host_weight=host_weight, error=error

;	Calculate the geometry normalization correction for the approx.
;	ellipsoidal shape of a fluid inclusion in mineral 'host_formula'.
;
;	E				energy of X-ray (keV)
;
;	beam_mode		selects the beam mode:
;						0	XY flat scanned beam
;						1	Elliptical flat area
;						2	Gaussian (defocussed beam), enter FWHM

;	Rx				X FWHM of Gaussian beam, or width of scanned beam
;	Ry				Y FWHM of Gaussian beam, or height of scanned beam
;
;	a				length of inclusion in plane of detector (microns)
;	b				width of inclusion in sample plane (microns)
;	c				thickness of inclusion (microns), already corrected for
;					refractive index.
;
;	fluid_density	fluid density (g/cm3)
;	model_density	fluid density as used in layer_setup/geo_yield model
;	bubble			vapor bubble diameter (microns)
;
;	host_formula	composition formula for host surrounding inclusion
;	host_density	host density
;	host_weight		flags that formula is in wt% (default to at. fraction)
;
;	This routine is called in one of three ways, using bubble_mode:
;
;	0	Use homogenous fluid density and set bubble to zero.
;		This is for inclusions with small bubbles, randomly positioned.
;
;	1	Force the calculation of a bubble size, assuming
;		the density represents the homogeneous density assuming the
;		surrounding fluid has a density 1.0 g/cm3 at room temperature.
;
;	2	Use the room temperature fluid density of fluid shell, and input the
;		diameter of the bubble at room temperature. In this case the
;		bubble is assumed to be (near)spherical and centered on the beam.
;
;	Fluid			returns the scaling factor to correct the ppm numbers for
;					this element.
;
;-----------------------------------------------------------------------------

	if n_elements(host_weight) lt 1 then host_weight=0
	if n_elements(beam_mode) lt 1 then beam_mode=0
	Bflat = (beam_mode ne 2)
	ellipse = (beam_mode eq 1)

	scale = 10000.0					; scale for infinite width
	tol = 0.1						; 10% tolerance on integrals
	small = 1.0E-15
	f = 0.0
	error = 0

	dense = fluid_density
	bubble = bubble_in
	if bubble_mode eq 0 then bubble = 0.0

	bubble = replicate( bubble, 3)
	if bubble_mode eq 1  then begin
		if fluid_density lt 1.0  then begin
			d = ( (1.0 - fluid_density) * a*b*c )^0.333333
			bubble = replicate(d,3)
		endif else begin
			bubble[*] = 0.0
		endelse
		dense = 1.0
	endif

	if bubble[0] gt small then begin						; we have a vapor bubble
		cavity = [a,b,c]
		p = sort(cavity)
		if bubble[p[0]] gt cavity[p[0]] then begin			; smallest dimension first
			r = sqrt(bubble[p[0]]/cavity[p[0]])
			bubble[p[0]] = cavity[p[0]]
			bubble[p[1]] = bubble[p[1]] * r
			bubble[p[2]] = bubble[p[2]] * r
		endif
		if bubble[p[1]] gt cavity[p[1]] then begin
			r = bubble[p[1]]/cavity[p[1]]
			bubble[p[1]] = cavity[p[1]]
			bubble[p[2]] = bubble[p[2]] * r
		endif
		if bubble[p[2]] gt cavity[p[2]] then goto, bad_902
	endif

	f = 1.0
	s_cavity = path_integral( Bflat, Rx/2.,Ry/2., a/2., b/2., c/2., tol, E, $
					host_formula,host_density, ellipse, /set_cavity, host_weight=host_weight)

	t_total = path_integral( Bflat, Rx/2.,Ry/2., scale*a/2., scale*b/2., c/2., tol, E, $
					host_formula,host_density, ellipse, /infinite_range, host_weight=host_weight)

	if t_total lt small  then goto, bad_901

	s_bubble = 0.0
	if bubble[0] gt small then begin
		s_bubble = path_integral( Bflat, Rx/2.,Ry/2., bubble[0]/2., bubble[1]/2., bubble[2]/2., tol, E, $
	    			host_formula,host_density, ellipse, host_weight=host_weight)
	endif

	ratio = (s_cavity - s_bubble) / t_total

	if  dense*ratio lt small then goto, bad_903
	f = (1./ratio) * (model_density/dense)
	return, f

bad_901:
	warning, 'FLUID','zero fluid integral, check parameters.'
	error = 1
	return, 0.0
bad_902:
	warning, 'FLUID','bubble is too big, check parameters.'
	error = 1
	return, 0.0
bad_903:
	warning, 'FLUID','zero correction ratio, check parameters.'
	error = 1
	return, 0.0
	end


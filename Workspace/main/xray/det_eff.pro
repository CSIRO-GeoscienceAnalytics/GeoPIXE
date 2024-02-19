function eff_gamma, detectori, E

;	For gamma detector, also add in Compton and pair terms.
;	This assumes simple 1D integration.

	detector = detectori
	if size(detector,/tname) eq 'POINTER' then detector = *detector

	mu = atten( detector.crystal, E, /gamma)
	mup = atten( detector.crystal, E, /photo)

; Photoelectric:

	T = detector.crystal.thick
	effe = mup * (1.-exp(-mu*T)) / mu

; Compton:

	Ec1 = 1./( (1.-cos(20.0*!dtor))/511. + 1./E)
	Ec2 = 1./( (1.-cos(170.*!dtor))/511. + 1./E)
	alpha = E / 511.
	r = (1. + alpha*0.13) / (1. + alpha*1.98)

	muc1 = atten( detector.crystal, E, /compton)
	muc2 = r * muc1

	muct1 = atten( detector.crystal, Ec1, /gamma)
	muct2 = atten( detector.crystal, Ec2, /gamma)

	mup1 = atten( detector.crystal, Ec1, /photo)
	mup2 = atten( detector.crystal, Ec2, /photo)

	effc = ((mup1*muc1)/(mu-muct1)) * ((1.-exp(-muct1*T))/muct1 - (1.-exp(mu*T))/mu) + $
		((mup2*muc2)/(mu+muct2)) * ((1.-exp(-mu*T))/mu + exp(-(mu+muct2)*T)*(1.-exp(muct2*T))/muct2)

; Pair:

	Ec1 = 511.
	Ec2 = 511.

	muc1 = atten( detector.crystal, E, /pair)
	muc2 = muc1

	muct1 = atten( detector.crystal, Ec1, /gamma)
	muct2 = muct1

	mup1 = atten( detector.crystal, Ec1, /photo)
	mup2 = mup1

	effp = ((mup1*muc1)/(mu-muct1)) * ((1.-exp(-muct1*T))/muct1 - (1.-exp(mu*T))/mu) + $
		((mup2*muc2)/(mu+muct2)) * ((1.-exp(-mu*T))/mu + exp(-(mu+muct2)*T)*(1.-exp(muct2*T))/muct2)

	eff = effe + effc + effp^2

	print,'      E=',E
	print,'    old=',1.-exp(-mup*T)
	print,'    new=',eff
	print,'fraction:'
	print,'  photo=',effe/eff
	print,'compton=',effc/eff
	print,'   pair=',effp/eff

	return, eff
end

;------------------------------------------------------------------------------

function det_eff, detectori, E, skip_abs=skip_abs, pressure=pressure, temp=temp, $			; , full=full
						external_filters=filteri, _extra=extra

;	Return detector intrinsic efficiency for energies 'E'. E in keV.
;		/full				return full efficiency, relative to 4 pi
;		/skip_abs			do not include built-in or external absorber terms
;		filter				also include external filters
;
;		for exp polynomial model, always returns full efficiency
;
;	In case there are pinhole filters amongst the 'filters' as well as absorbers
;	in the detector, we must treat these together. This is why we have the
;	'external_filters' keyword here, to include external filters.
;
;	'filter'	is an array of filters, or a pointer to an array.
;
;	pressure		indicates fitting a spectrum with ambient conditions specified, with pressure (mbar) and
;	temp			temperature (C). Pass these onto to 'transmit' in filters and 'det-eff' (in 'array_yield').
;
;	'detector' is a struct (or a pointer to it):
;		{	PIGE:			1 for PIGE, 0 for PIXE
;
;							in PIXE mode:
;			absorbers:		array of {filter3, N:,Z:,F:,thick:, ...}
;			crystal:		{layer, N:,Z:,F:,thick:,name:}   in mg/cm^2
;			diameter:		diameter (mm)
;			density:		(g/cm^3)
;			distance:		(mm)
;			tilt:			(degrees)
;			source:			effective source size at target (mm)
;			Aeff:			experimental absorption coefficients (use 0.0)
;			Beff:			  "   (use 0.0)
;			Cohen:			1 for Cohen 0.717 factor, 0 for 1.0
;			Use_special1:	flag (1=ON)
;			Use_special2:	flag (1=ON)
;			Special1:		{Tb:,db:,Rest:}
;			Special2:		{c:,w:,h:, c2:,w2:,h2:}
;
;							in PIGE mode:
;			a[]:			6 polynomial terms (exponential of polynomial in log(E))
;			e_low:			low E of applicability
;			e_high:			high E of applicability (extrapolate log-log beyond)

	small = 1.0e-10
	use_external_filters = 0
	use_internal_filters = 0
	if n_elements(full) lt 1 then full=0
	if n_elements(skip_abs) lt 1 then skip_abs=0
	if n_elements(filteri) ge 1 then use_external_filters=1

	if n_elements(E) lt 1 then return, 0.0
	nen = n_elements(E)
	if (nen lt 1) then return, 0.0
	det_eff = E
	det_eff[*] = 1.0								; has same dimensions as E

	if n_elements(detectori) lt 1 then goto, done
	detector = detectori
	if size(detector,/tname) eq 'POINTER' then begin
		if ptr_valid(detector) eq 0 then goto, done
		detector = *detector
	endif
	if size(detector,/tname) ne 'STRUCT' then goto, done

	if use_external_filters then begin
		if n_elements(filteri) lt 1 then use_external_filters = 0
		filter = filteri
		if size(filter,/tname) eq 'POINTER' then begin
			if ptr_valid(filter) eq 0 then begin
				use_external_filters = 0
			endif else begin
				filter = *filter
			endelse
		endif
		if size(filter,/tname) ne 'STRUCT' then use_external_filters = 0
	endif
	if size(detector.absorbers,/tname) eq 'STRUCT' then use_internal_filters = 1

	tilt = 0.0
	t = where( tag_names(detector) eq 'TILT')
	if t[0] ne -1 then tilt = detector.tilt
	ct = cos(tilt/!radeg)

;	Gamma rays 

	if detector.pige then begin
		x = alog([detector.e_low,detector.e_high])
		xn = 1.0
		y = detector.a[0]
		m = 0.0
		for i=1L,5 do begin
			m = m + i*detector.a[i]*xn
			xn = xn*x
			y = y + detector.a[i]*xn
		endfor
		ylh = y

		y = 0.0
		xn = 1.0
		x = alog(E)
		for i=0L,5 do begin
			y = y + detector.a[i]*xn
			xn = xn*x
		endfor
		det_eff = exp(y)

		q = where(E gt detector.e_high)
		if q[0] ne -1 then begin
			y = m[1] * (x[q] - alog(detector.e_high)) + ylh[1]
			det_eff[q] = exp(y)
		endif

		q = where(E lt detector.e_low)
		if q[0] ne -1 then begin
			y = m[0] * (x[q] - alog(detector.e_low)) + ylh[0]
			det_eff[q] = exp(y)
		endif

		return, det_eff
	endif

;	X-rays (PIXE, XRF and SXRF)

	F_RESPONSE = 1.0													; ideal full-energy intrinsic response
	if detector.cohen eq 1 then F_RESPONSE = 0.717

	if skip_abs then begin
		F_ABS = 1.0
	endif else begin
		if use_internal_filters then begin
			absorb = (use_external_filters) ? [detector.absorbers,filter] : detector.absorbers
		endif else begin
			absorb = (use_external_filters) ? filter : 0
		endelse
		F_ABS = transmit( absorb, E, pressure=pressure, temp=temp, tilt=tilt, _extra=extra)			; absorbers
	endelse

	F_esc = 1. - escape_fraction( detector, E, tilt=tilt)				; escape losses

	UA = Atten( detector.crystal, E, /photo)							; detector (cm^2/mg)
	X_active = detector.crystal.thick / ct								; thickness in mg/cm^2

	X = EXP(double(-UA * X_active))
	Z = (( 1.-X - X*UA*X_active ) /   $									; effective depth
     		( 100.*detector.density*UA * (1.-X) )) > 0.0				; in mm

	if detector.correct_solid_angle then begin
		y = 1. + Z / detector.distance
		F_SOLID_ANGLE = 1./(y*y)										; effective solid-
	endif else begin													; angle correction
		F_SOLID_ANGLE = 1.
	endelse
																		
	y = detector.source/(detector.distance + Z)
	y = y*y
	F_SOURCE_SIZE = 2.D0*(1.D0 - 1.D0/SQRT(1.D0+y))/y					; finite source

	if (detector.aeff gt small) and (detector.beff gt small) then begin
		y = detector.crystal.thick / (100.*detector.density *ct)
		eff = 1. - EXP(double((-detector.Aeff)*y * E^(-detector.Beff)))	; intrinsic eff
	endif else begin
		eff = 1. - X
	endelse

;	if n_elements(x) gt 1 then begin
;		print,'debug array E ...'
;	endif
;------------------------------------------------------------------------------
;
;  Special modelling...
;
;  $1	Buried dead-layer in detector.
;		db	thickness of "front detector"
;		Tb	thickness of buried dead layer
;		rest	thickness of "back detector"
;
; NOTE	Check "detector_setup/update_detector_struct"
;		This may still have "specials" commented out.

	if detector.use_special1 then begin
	    Xa = 100. * detector.special1.db * detector.density
	    T1 = EXP(-UA*Xa)									; front
	    Eff1 = 1. - T1
;															; back
	    Xa = 100. * detector.special1.Tb * detector.density
	    T2 = EXP(-UA*Xa)

	    Xa = X_active - 100. * detector.density *  $
	    				(detector.special1.db + detector.special1.Tb)
	    Eff2 = 1. - EXP(-UA*Xa)

	    Eff = Eff1 + T1*T2 * Eff2
	endif

;  $2	X-ray Bragg scattering in detector (approx.)

	if detector.use_special2 then begin
	    T2 = 0.0
	    if detector.special2.c2 gt small then begin
			X = 1.664*(E - detector.special2.c2) / detector.special2.w2
			T2 = detector.special2.h2 * exp(-X*X)
	    endif
	    X = 1.664*(E - detector.special2.c) / detector.special2.w
	    Eff = Eff * (1. - detector.special2.h * exp(-X*X) - T2)
	endif

;------------------------------------------------------------------------------

	DET_EFF = F_abs * F_esc * F_RESPONSE * F_SOURCE_SIZE   $
				* F_SOLID_ANGLE * EFF							; effective eff

;	DET_EFF = F_esc * F_RESPONSE * F_SOURCE_SIZE * EFF

;	for i=0L,n_elements(e)-1 do begin
;		print,f_abs[i],f_esc[i],f_source_size[i],f_solid_angle[i], eff[i], det_eff[i]
;	endfor

	if full then begin

;		This uses the small solid-angle approximation: area/d^2
;		When should we switch to the full formula?
;			 2.* !pi * (1. - cos(angle)), where angle is cone half-angle (tan(angle)=r/d)

		omega = !pi * detector.diameter * detector.diameter / $
					(4.0 * detector.distance * detector.distance)
		det_eff = det_eff * omega/(4.*!pi)
	endif

done:
	RETURN, det_eff
	END

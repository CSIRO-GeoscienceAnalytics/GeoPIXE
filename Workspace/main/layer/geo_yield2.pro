function geo_yield2, formula, thick, microns=microns, density=density, weight=weight, energy=e_beami, $
			theta=theta, phi=phi, alpha=alpha, beta=beta, unknown=unknown, beam=beam, $
			z1=z1, a1=a1, state=state, select=select_z, gamma=gamma, $
			e_min=e_min, e_max=e_max, progress=do_progressi, error=error, $

			intensity=rel_int, lines=line_indx, e_lines=e_lines, n_lines=n_lines, z2=z2, $
			shell=shell, layers=layers, sec_yield=sec_yield, mu_zero=mu_zero, $
			x_slow=x, e_slow=e, xsect=xsect, yieldx=yieldx, cos_beam=cos_beam, $
			cmux=cmux, dydx=dydx, half_slice_id=hlid, flux=flux, $

;	Above args common to 'geo_array_yield' and 'geo_yield2'. Those below unique to 'geo_yield2':
;	They are used to bring in results (e.g. 'slices' and 'fluoro') from initial 'geo_yield2' 
;	(called from 'geo_array_yield') using central detector and including secondary fluorescence.

			force_trans=force_trans, reenter=reenter, sec_fl=sec_fl, $
			slices=slices, lid=lid, relmux=relmux, spec=spec, $
			branch_ratio=branch_ratio, e_bind=e_bind, fluoro=fluoro, jump=jump
;+
;	Build the PIXE, SXRF X-ray or PIGE gamma-ray yields, and optionally the relative intensities
;	z1=0,a1=0 to select photons; /gamma for PIGE.
;
;	The hierarchy of routines for yield calculation follows:
;		geo_array_yield			Accumulate yields across detector array.
;			geo_yield2			Initially for the central detector, including secondary fluorescence.
;			geo_yield2			In a loop over all detectors in an array, without sec. fluorescence.
;								( sec. fluor. enhancement provided by central detector model).
;				experiment_angles	Determine direction cosines for beam and detectors.
;				calc_slices		Construct incremental slices of sample from input layer specs.
;				slow_beam		Model slowing (or attenuation) of input beam.
;				calc_abs		Build array of mass-absorption coefficients for all lines.
;				calc_cross		Calculate cross-sections as beam slow for all elements.
;				calc_yield		Integrate X-ray yields over all slices.
;
;	Input:
;		formula[l]		array for chemical formulae for layers 'l'
;		thick[l]		thickness in mg/cm^2, or optionally in microns
;		weight[l]		formula multipliers in weight-% (default to atomic fraction)
;		microns[l]		single switch, or array (one for each layer),
;						selecting microns thickness (default is mg/cm^2)
;		density[l]		density of each layer (if microns is selected)
;		energy			energy of ion beam (MeV), or photon beam (keV), or use 'beam'
;		beam			general continuum beam struct, will replace 'energy'
;		z1				Z of beam particles
;		A1				A of beam
;		state			charge state of beam
;		theta			angle of X-ray detector to the beam in horizontal plane
;		phi				azimuthal angle of detector
;		alpha			rotation of target about Y axis
;		beta			tile of the target about the target centre line
;		unknown			number of the "UNKNOWN" layer, to weight relative intensities.
;		sec_fl=0		disables secondary fluorescence calculation.
;		select			specify a list of Z to calculate yields for (default 2-92).
;		/gamma			for PIGE yield calculation
;		/force_trans	force the inclusion of slices beyond range for a transmission target
;		/reenter		2nd pass into Geo_yield2 for yield only of more detector elements
;
;	Carry over for '/reenter' only:
;		slices			slices structures
;		lid				layer id for a slice index
;		relmux			relative mux
;		branch_ratio	branching ratios
;		e_bind			binding energies
;		fluoro			fluorescence yields
;		jump			jump ratios
;		spec			beam struct as a function of half-slice
;
;	Output:
;		intensity[k,i]  Relative intensities for X-ray line 'k', element 'i'
;						averaged over all layers, weighted by # atoms in each (LATER).
;		lines[k,i]		Line index for each line 'k', element 'i'
;		e_lines[k,i]	Energy of lines
;		n_lines[i]		Number of lines for each element 'i'
;		z2[i]			Atomic number for element 'i'
;		shell[i]		Shell used for element 'i' (note K, L, M done as separate "elements")
;		layers[l]		array of layer structs for sample layers
;		sec_yield[i,l]	PIXE yield component due to secondary fluorescence only.
;		mu_zero[k,i]	mass absorption coefficients for slice 0.
;
;	Diagnostics outputs for plot display:
;		x_slow[h]		distance (normal to surface) in half-slice steps 'h'
;		e_slow[h]		energy of beam in half-slice steps 'h'
;		half_slice_id[h]	layer ID of each half-slice
;		dydx[h]			stopping power at 'h'
;		xsect[i,h]		cross-sections for element 'i', half-slice steps 'h'
;		yieldx[h,i]		yield vs half-slice depth 'h', element 'i'
;		cmux[k,i,h]		cummulative mass-absorption by line 'k', element 'i' half-slice 'h'
;		cos_beam		cosine factor for beam input (need this to convert x normal to path)
;		flux[h]			decaying relative flux at each half-step (/photo only).
;
;	Return:
;		yield[i,l]		PIXE/SRXF yield total for element 'i', layer 'l'
;						Generally in units of counts/ppm/uC, unless continuum beam, then counts/ppm/s
;
;	‘Geo_yield2’ first constructs an array of ‘layer’ structs, to define the layered sample structure, 
;	using the input data for chemical formulae, thickness and density. It then calls ‘calc_slices2’ to 
;	sub-divide the coarse layers into smaller ‘slices’ that will be used for modelling energy loss (for 
;	ion beams), X-ray absorption and production yields. These ‘slices’ start small near layer boundaries 
;	and the sample surface, so that detail is not lost in the integration, and then grow away from boundaries 
;	for more efficient integration. The aim is the minimum number of slices that maintains energy loss 
;	and integration accuracy, while making the process as fast as possible. For an ion beam, it determines 
;	the slowing down of the beam through the slices (actually it uses half slices for integration steps) 
;	in ‘slow_beam’. In the case of a continuum source, the beam spectrum is modified for any beam filters 
;	in ‘harden_beam’.
;	
;	After all X-ray lines and relative intensities are retrieved using ‘get_lines’, in the case of a 
;	continuum beam, the XRF relative intensities get modified integrating over all energies in the beam. 
;	Then an array of mass absorption coefficients ‘cmux’ is generated for half slices to be used to model 
;	X-ray absorption through the sample slices. And X-ray production cross-sections are calculated for the 
;	beam energy (or range or energies). Next ‘calc_yield’ is called to integrate PIXE/SXRF yields using a 
;	4th order Runge Kutta integration through half slices, optionally including an inner loop to include 
;	secondary fluorescence effects. Finally, for a multi-layer sample structure, the overall X-ray relative 
;	intensities are determined by including contributions from all layers. These relative intensities will 
;	be used in X-ray spectrum fitting.
;-

;select_z = [14,17,19,20,25,26,29,30,33,35,37,38,42,47,50,51,56,56,82,0]
;select_z = [79]

COMPILE_OPT STRICTARR
ErrorNo = 0
common c_errors_1, catch_errors_on
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'Geo_yield2',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		goto, done
	endif
endif

progress_tlb = 0L
cancel = 0
error = 1
yield = 0

	if n_elements(do_progressi) lt 1 then do_progressi=0
	if n_elements(z1) lt 1 then z1 = 1					; beam Z
	if n_elements(a1) lt 1 then a1 = 1					; beam A
	if n_elements(state) lt 1 then state = 1.0			; charge state
	if n_elements(gamma) lt 1 then gamma = 0			; PIGE mode
	if n_elements(force_trans) lt 1 then force_trans = 0 ; force transmisson tgt mode off
	if n_elements(reenter) lt 1 then reenter = 0		; 2nd time through Geo_yield off

	if gamma then begin
		sec_fl = 0										; no secondary fluorescence
		if n_elements(e_min) lt 1 then e_min = 	50.0	; minimum gamma-ray line energy (keV)
		if n_elements(e_max) lt 1 then e_max = 	4000.0	; maximum gamma-ray line energy (keV)
	endif else begin
		if n_elements(sec_fl) lt 1 then sec_fl = 1		; secondary fluorescence on by default for PIXE, SXRF
		if n_elements(e_min) lt 1 then e_min = 	2.0		; minimum X-ray line energy (keV)
		if n_elements(e_max) lt 1 then e_max = 	48.0	; maximum X-ray line energy (keV)
	endelse
	
	if n_elements(beam) lt 1 then begin					; beam spectrum flags
		continuum = 0
		if n_elements(e_beami) lt 1 then e_beami = 3.0
		e_beam = e_beami
	endif else begin
		continuum = beam.continuum						; for a continuum source, e_beam is maximum energy
		e_beam = beam.energy							; only support continuum source for /photons
	endelse
	
	photo = 0
	if (a1 eq 0) and (z1 eq 0) then photo=1				; photons
	if photo eq 0 then continuum = 0
	if gamma then photo = 0
	if gamma then continuum = 0

	do_progress = do_progressi
	slice = (gamma eq 1) ? 0.005 : ((photo) ? 0.05 : 0.02*e_beam)	; smallest slice size to use (mg/cm2).  *** 21/8/14 CGR
;	slice = (gamma eq 1) ? 0.005 : 0.05*e_beam						; was a bit coarse for XRF

;	slice = 0.2 * slice									; *** use this only in debug for finer plots

	n = n_elements(formula)
	if (n lt 1) or (n_elements(thick) lt n) then begin
		warning, 'geo_yield','Insufficient layer data supplied.'
		goto, bad
	endif

	if do_progress then progress, tlb=progress_tlb, title='GeoPIXE: PIXE/SXRF Yield Calculation'

	if n_elements(weight) lt 1 then weight=intarr(n)
	if n_elements(weight) lt n then weight=replicate( weight[0], n)

	if n_elements(microns) lt 1 then microns=intarr(n)
	if n_elements(microns) lt n then microns=replicate( microns[0], n)

	if n_elements(density) lt 1 then density=-1.0
	if n_elements(density) lt n then density=replicate( density[0], n)

	if n_elements(theta) lt 1 then theta = ((photo eq 1) ? 90.0 : ((gamma eq 1) ? 0.0 : 135.0))
	if n_elements(phi) lt 1 then phi = 0.0
	if n_elements(alpha) lt 1 then alpha = ((photo eq 1) ? -45.0 : 0.0)
	if n_elements(beta) lt 1 then beta = 0.0
	if n_elements(flux) lt 1 then flux = 1.0

	if n_elements(unknown) lt 1 then unknown = 1

	unknown = unknown > 1
	thick = thick > 0.0
	state = state > 0.1
	if gamma then begin
		e_min = e_min > 50.0
		e_max = (e_max < 20000.0) > (e_min+100.0)
		if (z1 ne 1) or (a1 ne 1) then begin
			warning,'geo_yield','Gamma database only applies to proton beams.'
			goto, done
		endif
		if (e_beam lt 0.5) or (e_beam gt 6.0) then begin
			warning,'geo_yield','Beam energy is outside database range.'
			goto, done
		endif
	endif else begin
		e_min = e_min > 0.2
		e_max = (e_max < 200.0) > (e_min+1.0)
		if photo then begin
			if (e_beam lt 2.0) or (e_beam gt 100.0) then begin
				warning,'geo_yield','Beam energy is outside database range.'
				goto, done
			endif
		endif else begin
			if (e_beam lt (0.1*a1)) or (e_beam gt (6.0*a1)) then begin
				warning,'geo_yield','Beam energy is outside database range.'
				goto, done
			endif
		endelse
	endelse

;	Calculate cosines for beam and detector based on experimental angles ...

	aerror = experiment_angles( theta, phi, alpha, beta, cos_beam, cos_detector)
	if aerror then goto, bad

	if reenter then goto, yield

;-----------------------------------------------------------------------------------------------

;	Build layer structs from input data ...

	for i=0L,n-1 do begin
		if lenchr(formula[i]) lt 1 then goto, bad_layer
		if thick[i] lt 1.0e-10 then goto, bad_layer

		if i eq 0 then begin
			if microns[0] and (density[0] gt 0.0) then begin
				layers = make_layer( formula[0], thick[0], /microns, density=density[0], weight=weight[0], error=aerror)
				if aerror then goto, bad_layer
			endif else begin
				layers = make_layer( formula[0], thick[0], microns=microns[0], weight=weight[0], error=aerror )
				if aerror then goto, bad_layer
			endelse
		endif else begin
			if microns[i] and (density[i] gt 0.0) then begin
				layers = [layers, make_layer( formula[i], thick[i], /microns, density=density[i], weight=weight[i], error=aerror)]
				if aerror then goto, bad_layer
			endif else begin
				layers = [layers, make_layer( formula[i], thick[i], microns=microns[i], weight=weight[i], error=aerror )]
				if aerror then goto, bad_layer
			endelse
		endelse
	endfor

;	Construct variable thickness slices based on these layers. 'zmajor' returns the Z of all
;	sample major elements.

	slices = calc_slices2( layers, slice, layer_id=lid, zmajor=zmajor, photo=photo)

	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:1, size:5}, cancel=cancel
	if cancel then goto, done

;	Generate the slowing energy 'e' as a function of depth 'x' (normal to surface).
;	Also return the last slice used. For photons energy does not change.
;	In this case, flux decay calculated in Calc_Abs below.
;	Note that 'e','x' return is in half-slice steps

	e = slow_beam( z1,a1, e_beam, slices, last=last, distance=x, /normal, cos_beam=cos_beam, dydx=dydx)

	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:2, size:5}, cancel=cancel
	if cancel then goto, done

;	Truncate arrays at last slice used ...

	q = where( cos_detector lt 0.0, count)		; if cos_detector < 0 for any detector elements,
	if force_trans or (count gt 0) then begin	; include slices after proton range
		merge_slices, slices, last, lid			; but clump them together
	endif else begin
		slices = slices[0:last]
		lid = lid[0:last]						; layer ID from slice index
	endelse
	hlid = congrid(lid,2*n_elements(lid)+1)		; layer ID from half-slice index

;	Harden the beam spectrum (for a continnum source of photons).
;	Build list of all X-ray lines, for all shells and elements, for the energy range 'e_min, e_max' ...
;	Note that 'get_lines' does not return a proper 'branch_ratio' result for the continuum case.

	if continuum then begin

;		Normalize to 1 uC charge quivalent total beam flux? No, as "flux" is now dwell time (ms) for Maia
;		Mapper, need to report counts/ms instead of counts/uC.

		beam2 = beam
;		beam2.spectrum.data = beam2.spectrum.data * 6.242e+12 / total(beam2.spectrum.data)			; uC
		beam2.spectrum.data = beam2.spectrum.data * 1.0e-3											; ms

;		Fudge to test falling source spectrum effect on yields ...
;		Not in 'xos_transmission', which would require all source models to be recalculated (and saved?).
;		But either in source or here, need to rebuild all yield files.

;		if beam.poly.model eq 'XOS default' then begin
;			beam2.spectrum.data = beam2.spectrum.data * ((10./(beam2.spectrum.e > 10.))^4)
;			beam2.spectrum.data = beam2.spectrum.data * exp(-0.3*( (beam2.spectrum.e > 10.) - 10.))
;			beam2.spectrum.data = beam2.spectrum.data * exp(-0.2*( (beam2.spectrum.e > 10.) - 10.))
;			beam2.spectrum.data = beam2.spectrum.data * exp(-0.2*( (beam2.spectrum.e > 12.) - 12.))
;
;			beam2.spectrum.data = beam2.spectrum.data * exp(-0.19*( (beam2.spectrum.e > 10.) - 10.))
;		endif

		spec = harden_beam( beam2, slices, lid)		;, scale=scale)
		
;		print,'geo_yield2: scale for continuum spectrum =',scale
	endif

	e_lines = get_lines( e_min, e_max, select=select_z, branch_ratio=branch_ratio, $
				line_indx=line_indx, shell=shell, n_lines=n_lines, $
				e_bind=e_bind, fluoro=fluoro, jump=jump, z2=z2, gamma=gamma, $
				beam=spec, e_beam=e_beam, photo=photo )

;	For continuum case, need to build relative-intensities across the continuum spectrum,
;	weighted by the hardened spectrum at each (half) slice. Only call 'init_xrf_lines' once per energy.

	if continuum then begin
		nz = n_elements(z2)
		nlin = max(n_lines)
		nhs = n_elements(spec)
		rele = fltarr(nlin,nz,beam.spectrum.N)
		branch_ratio = fltarr(nlin,nz,nhs)
		klm = [0,1,4,9,16]
		for j=0,beam.spectrum.N-1 do begin
			init_xrf_lines, beam.spectrum.E[j]
			for i=0,nz-1 do begin
				m = n_lines[i]
				rele[0:m-1,i,j] = relative_intensity( z2[i], line_indx[0:m-1,i], photo=photo)
			endfor
		endfor
		for i=0,nz-1 do begin
			m = n_lines[i]
			xcs = photo_subshell( z2[i], klm[shell[i]], beam.spectrum.E)
			for k=0,nhs-1 do begin
				sum = total( spec[k].spectrum.data * xcs)
				for j=0,m-1 do begin
					if sum gt 1.0e-20 then begin
						branch_ratio[j,i,k] = total( spec[k].spectrum.data * xcs * rele[j,i,*]) / sum
					endif else begin
						branch_ratio[j,i,k] = 0.0
					endelse
				endfor
			endfor
		endfor
	endif
	
	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:3, size:5}, cancel=cancel
	if cancel then goto, done

	if n_lines[0] eq 0 then goto, bad

;	Build array of mass-absorption coefficients for all lines ...
;	Note that 'cmux' return is in half-slice steps. Also return 'flux' decaying, but only use for contiuum=0.
;	Need better database for gamma-rays.

	cmux = calc_abs( e_lines, z2, slices, lid, relmux=relmux, sec_fl=sec_fl, mu_zero=mu_zero, $
						gamma=gamma, photo=photo, e_beam=e_beam, flux=flux )
	flux = exp(-flux/cos_beam)

	if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:4, size:5}, cancel=cancel
	if cancel then goto, done

;	Calculate cross-sections for each element/shell ...
;	For ions at each proton energy, for continuum across spectrum energy, for mono photons at beam energy.
;	Use in different ways in 'calc_yield' for continuum versus ions/mono Xrays.

	if continuum then begin
		xsect = calc_cross( z1,a1, z2,shell, beam.spectrum.E, /photo, beam=beam)
	endif else begin
		if photo then begin
			xsect = calc_cross( z1,a1, z2,shell, replicate(e_beam,n_elements(e)), /photo)
		endif else begin
			xsect = calc_cross( z1,a1, z2,shell, e, gamma=gamma)
		endelse
	endelse

;-----------------------------------------------------------------------------------------------

;	Integrate PIXE/SXRF yields using a 4th order Runge Kutta integration,
;	and calculate relative intensities ...

yield:
	n_layers = n_elements(layers)

	yield = calc_yield( state, cos_beam, cos_detector, slices, z2, shell, e_lines, xsect, cmux, $
						branch_ratio, fluoro, jump, n_lines, n_layers, lid, e_bind, $
						intensity=intensity, relmux=relmux, sec_fl=sec_fl, beam=spec, $
						sec_yield=sec_yield, yieldx=yieldx, photo=photo, flux=flux, zero=zero )

;	Will need to form sum of layer yields, weighted by ppm of each element in each layer. 
;	This will also need a specification of the "UNKNOWN LAYER". In this layer only assume all 
;	missing (not in matrix) elements are 10 ppm.
;
;	Note that 'unknown' starts at 1, while the index here start at 0.

	if do_progress then progress, /complete, progress_tlb, 'Yield calculation completed.'

	linmax = n_elements( e_lines[*,0])
	n_els = n_elements( e_lines[0,*])

	mass_yield = fltarr(linmax,n_els)
	mass_tot = fltarr(n_els)

	for l=0L,n_layers-1 do begin
		n = layers[l].N
		A2 = mass( layers[l].Z[0:n-1])
		atot = total( A2 * layers[l].F[0:n-1])

		for i=0L,n_els-1 do begin
			q = where( z2[i] eq layers[l].Z[0:n-1])
			if q[0] ne -1 then begin
				ppm = 1.0e+6 * A2[q[0]] * layers[l].F[q[0]] / atot
			endif else begin
				if l eq (unknown-1) then begin
					ppm = 10.0
				endif else begin
					ppm = 0.0
				endelse
			endelse
			if finite(ppm) eq 0 then ppm=0.0

			mass_yield[*,i] = mass_yield[*,i] + intensity[*,i,l] * yield[i,l] * ppm
			mass_tot[i] = mass_tot[i] + ppm
		endfor
	endfor

;	For relative intensities, need to use the sum total of all layers ...

	major_yield = fltarr(n_els)
	rel_int = fltarr(linmax,n_els)
	for i=0L,n_els-1 do begin
		total_yield = mass_yield[*,i] / mass_tot[i]
		q = where( finite(total_yield) eq 0, nq)
		if nq gt 0 then begin
			total_yield[q]=0.0
		endif

		major_yield[i] = total_yield[0]
		rel_int[*,i] = total_yield / major_yield[i]
	endfor

	q = where( (zero eq 1) or (finite(rel_int[0,*]) eq 0) or (finite(rel_int[1,*]) eq 0))
	if q[0] ne -1 then begin
		yield[q,*] = 0.0
		major_yield[q] = 0.0
		rel_int[1:*,q] = 0.0
		rel_int[0,q] = 1.0
	endif
	q1 = where( (zero eq 0) and ((finite(rel_int[0,*]) eq 0) or (finite(rel_int[1,*]) eq 0)) )
	if q1[0] ne -1 then begin
		print,'geo_yield2: Undefined rel-ints - ',q1
	endif
	
;	Return the yields for the individual layers ...

	error = 0
	goto, done

bad_layer:
	warning,'geo_yield2',['Error in layer definition parameters.', '',$
			'Check for zero thickness or blank formula.', $
			'Check details for all layers.','','Abort yield calculation.'],/error
	goto, done
bad:
	warning,'geo_yield2',['Error in calculation parameters.','Abort yield calculation.'],/error

done:
	if do_progress then progress, /ending, progress_tlb
	return, yield
	end





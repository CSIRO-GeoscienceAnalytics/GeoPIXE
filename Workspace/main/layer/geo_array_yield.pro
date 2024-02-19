function geo_array_yield, formula, thick, microns=microns, density=density, weight=weight, energy=e_beami, $
			theta=theta, phi=phi, alpha=alpha, beta=beta, unknown=unknown, beam=beam, $
			z1=z1, a1=a1, state=state, sec_fl=sec_fl, select=select_z, gamma=gamma, $
			e_min=e_min, e_max=e_max, progress=do_progress, error=error, $
			detector=pdetector, layout=playout, ratio_yield=rY, array=array, $
			ratio_intensity=rIntensity, $

			intensity=rel_int, lines=line_indx, e_lines=e_lines, n_lines=n_lines, z2=z2, $
			shell=shell, layers=layers, sec_yield=sec_yield, mu_zero=mu_zero, $
			x_slow=x, e_slow=e, xsect=xsect, yieldx=yieldx, cos_beam=cos_beam, $
			cmux=cmux, dydx=dydx, half_slice_id=hlid, flux=flux
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
;	In ‘geo_array_yield’, the notation below uses ‘l’ for layer index, ‘k’ for line index, 
;	‘i’ for element index and ‘d’ for detector index.
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
;		array			array=1 denotes a detector array, for which detector and layout should be supplied
;		detector		pointer to detector struct
;		layout			pointer to layout struct
;
;	Output:
;		intensity[k,i]  Relative intensities for X-ray line 'k', element 'i', 'central detector,
;						averaged over all layers, weighted by # atoms in each (LATER).
;		ratio_intensity[d,k,i]   Relative intensities for X-ray line 'k', element 'i',
;						detector 'd'
;		lines[k,i]		Line index for each line 'k', element 'i'
;		e_lines[k,i]	Energy of lines
;		n_lines[i]		Number of lines for each element 'i'
;		z2[i]			Atomic number for element 'i'
;		shell[i]		Shell used for element 'i' (note K, L, M done as separate "elements")
;		layers[l]		array of layer structs for sample layers
;		sec_yield[i,l]	Yield component due to secondary fluorescence only.
;		mu_zero[k,i]	mass absorption coefficients for slice 0.
;		ratio_yield[d,k]	ratio of yield for detector 'd' to generic yield, element 'k'
;							must be in normal detector number order.
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
;		yield[i,l]		Yield total for element 'i', layer 'l'
;						Generally in units of counts/ppm/uC, unless continuum beam, then counts/ppm/s
;
;	Passed via '/renter' between calls to 'geo_yield2' only:
;		slices			slices structures
;		lid				layer id for a slice index
;		relmux			relative mux
;		branch_ratio	branching ratios
;		e_bind			binding energies
;		fluoro			fluorescence yields
;		jump			jump ratios
;		spec			beam struct as a function of half-slice
;
;	‘geo_array_yield’ determines cosine factors for incoming beam and detected outgoing X-rays 
;	using ‘experiment_angles’ and calls ‘geo_yield2’ to model yields for the central detector, 
;	including secondary fluorescence. Then it calls ‘geo_yield2’ in a loop over all other detectors 
;	in a detector array, not including secondary fluorescence. It simply adopts the secondary fluorescence 
;	enhancement factor from the central detector result. It uses ‘detector_geometry’ to provide the 
;	effective geometry (theta, phi) of every detector in the array, given the global angles and tilt 
;	of the array as a whole. It forms the array ‘rIntensity’ as the ratio of the intensities across 
;	the array relative to the central detector.
;	
;	‘Geo_yield2’ returns rel_int[k,i] for element ‘i’, line ‘k’ for ‘central’ detector. Then for a 
;	detector array, ‘Geo_yield2’ is called again to return rel_inti[k,i] for each detector. Need to 
;	form weighted average across the array using rY[d,i] for element ‘i’, detector ‘d’ to weight 
;	rel_inti[*,i] and sum over all ‘d’. But, like yields, this will need to be done just prior to 
;	the fit using the current detector selections (done in ‘array_yields’). Here we need to just pass 
;	back the individual rel_int’s for each detector to be saved in the yields file.
;-

COMPILE_OPT STRICTARR

;select_z = [14,17,19,20,25,26,29,30,33,35,37,38,42,47,50,51,56,56,82,0]
;select_z = [79]
;select_z = [24,26,82]
;select_z = [26,29,79]
;e_min = 4.

;	beam = make_tube_spectrum()							; make a test continuum spectrum ...   ????	

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
		warning,'Geo_array_yield',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		error = 1
		goto, done
	endif
endif
if n_elements(array) eq 0 then array=0

	if n_elements(beam) lt 1 then begin					; beam spectrum flags
		continuum = 0
		if n_elements(e_beami) lt 1 then e_beami = 3.0
		e_beam = e_beami
	endif else begin
		continuum = beam.continuum						; for a continuum source, e_beam is maximum energy
		e_beam = beam.energy							; only support continuum source for /photons
		if (e_beam lt 0.1) and (n_elements(e_beami) eq 1) then begin
			e_beam = e_beami
			beam.energy = e_beam
		endif
	endelse

if n_elements(do_progress) lt 1 then do_progress=0
progress_tlb = 0L
if array then begin
	if ptr_valid(pdetector) and ptr_valid(playout) then begin
		if (size(*pdetector, /tname) eq 'STRUCT') and (size(*playout, /tname) eq 'STRUCT') then begin
			array = (*pdetector).array
		endif else array = 0
	endif else array = 0
endif

;	Show progress here in first "geo_yield2", which includes sec. fluor.

;	Calculate cosines for beam and detector based on experimental angles ...

	aerror = experiment_angles( theta, phi, alpha, beta, cos_beam, cos_detector)
	if aerror then goto, bad

	force_trans = 0
	q = where( cos_detector lt 0.0, count)		; if cos_detector < 0 for any detector elements,
	if (count gt 0) then force_trans=1			; include slices after proton range

; generic yields, using 'central' detector, with sec_fl

	yield = geo_yield2( formula, thick, microns=microns, density=density, weight=weight, energy=e_beam, $
			theta=theta, phi=phi, alpha=alpha, beta=beta, unknown=unknown, beam=beam, $
			z1=z1, a1=a1, state=state, /sec_fl, select=select_z, gamma=gamma, $
			e_min=e_min, e_max=e_max, force_trans=force_trans, progress=do_progress, error=error, $

			intensity=rel_int, lines=line_indx, e_lines=e_lines, n_lines=n_lines, z2=z2, $
			shell=shell, layers=layers, sec_yield=sec_yield, mu_zero=mu_zero, $
			x_slow=x, e_slow=e, xsect=xsect, yieldx=yieldx, cos_beam=cos_beam, $
			cmux=cmux, dydx=dydx, half_slice_id=hlid, flux=flux, $

			slices=slices, lid=lid, relmux=relmux, spec=spec, $
			branch_ratio=branch_ratio, e_bind=e_bind, fluoro=fluoro, jump=jump)
	if error then goto, bad

; secondary fluorescence enhancement factors

	enhance = yield / (yield-sec_yield)
	n_els = n_elements( yield[*,0])
	linmax = n_elements( e_lines[*,0])

	error = 0
	if array eq 0 then goto, done

; Calculate yields across the array. Use no sec_fl, /reenter.
; Take care with detector order. Want ratio_yield output to be in normal detector number order,
; but detector_geometry returns in CSV table order. Use (*playout).ref to reorder below.
; Show progress here only for loop over "geo_yield2" for all detectors.

	g = detector_geometry( playout, (*pdetector).distance, theta, phi, tilt=(*pdetector).tilt, error=error)
	if error then goto, bad

	if do_progress then progress, tlb=progress_tlb, title='GeoPIXE: PIXE/SXRF Array Yield Calculation'
	rY = fltarr((*playout).N, n_els)
	rIntensity = fltarr( (*playout).N, linmax, n_els)
	
	for i=0L,(*playout).N-1 do begin
		cancel = 0
		if do_progress then progress, /update, progress_tlb, {unit:0, value:0, current:i, size:(*playout).N}, cancel=cancel
		if cancel then goto, done

		yieldi = geo_yield2( formula, thick, microns=microns, density=density, weight=weight, energy=e_beam, $
				theta=g[i].theta, phi=g[i].phi, alpha=alpha, beta=beta, unknown=unknown, beam=beam, $
				z1=z1, a1=a1, state=state, sec_fl=0, select=select_z, gamma=gamma, $
				e_min=e_min, e_max=e_max, force_trans=force_trans, error=error, $
	
				intensity=rel_inti, lines=line_indx, e_lines=e_lines, n_lines=n_lines, z2=z2, $
				shell=shell, layers=layers, mu_zero=mu_zero, $
				x_slow=x, e_slow=e, xsect=xsect, cos_beam=cos_beam, $
				cmux=cmux, dydx=dydx, half_slice_id=hlid, flux=flux, $
	
				/reenter, slices=slices, lid=lid, relmux=relmux, spec=spec, $
				branch_ratio=branch_ratio, e_bind=e_bind, fluoro=fluoro, jump=jump)
		if error then goto, bad

		q = where( yield[*,unknown-1] gt 1.0e-10)
		if q[0] ne -1 then begin
			rY[i,*] = 1.0
			rY[i,q] = yieldi[q,unknown-1] * enhance[q,unknown-1] / yield[q,unknown-1]
		endif
		rIntensity[i,*,*] = rel_inti[*,*] / rel_int[*,*]
	endfor

; 'rY', 'rIntensity' above and 'g' use the CSV index order. Re-order to normal detector index order ...
; Note that ref is a little padded, so need to specify index range for it.

	rY[*,*] = rY[ (*playout).ref[(*playout).start + indgen((*playout).N)], * ]	
	rIntensity[*,*,*] = rIntensity[ (*playout).ref[(*playout).start + indgen((*playout).N)], *,* ]	

	q = where( finite(rIntensity) eq 0, nq)
	if nq gt 0 then rIntensity[q]=0.0
	
	if do_progress then progress, /complete, progress_tlb, 'Yield calculation completed.'
	error = 0
	goto, done

bad:
	warning,'geo_array_yield',['Error in calculation parameters.','Abort yield calculation.'],/error

done:
	if do_progress then progress, /ending, progress_tlb
	return, yield
	end

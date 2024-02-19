function array_yield, pdetector=pdetector, playout=playout, pfilter=pfilter, $
				array=array, n_els=n_els, Y=Y, Energy=E, charge=charge, $
				active=active, n_det=n_det, ratio_yield=rY, rGamma=rGamma, $
				E_line=e_line, ratio_intensity=rIntensity, correct_intensity=cIntensity, n_lines=n_lines, $
				theta=theta, phi=phi, multiplicity=multiplicity, totGamma=totGamma, $
				counts_per_ppm_uc=counts_per_ppm_uc, refit=refit, use_last=use_last, $
				pressure=pressure, temp=temp, error=error
				
; Calculate the yield from an array detector, based on *pdetector and *playout parameters,
; positioned at 'theta' and 'phi' in angle, for energies 'E', charge 'charge',
; correcting input generic (central detector) yields 'Y' for variation across the detector,
; for 'active' detectors and apply filters and intrinsic efficiency too.
;
; This entails accumulating contributions to efficiency, solid-angle, filter
; absorption and variations in yields with take-off angle across the array,
; only adding in detectors that are 'active' in the spectral data.
;
; Input:
;	pdetector	pointer to detector struct
;	playout		pointer to detector array layout
;	pfilter		pointer to external filter
;	array		bool flags an array detector
;	active		list of active detector channels
;	n_det		total number of detector channels (includes inactive)
;	n_els		# elements
;	n_lines		# lines per element
;	e_line		line energies per element per line index
;	E			major lines energies per element
;	Y			yields per element
;	theta		detector centre theta angle
;	phi			detector centre phi angle
;	charge		beam charge
;
;	rY			relative yields per element and detector (not input if /refit or /use_last)
;	rIntensity	relative intensities relative to 'central' detector for all lines, elements
;
;	multiplicity	fall-back (some old data), replaced by sum of 'rGamma' to form 'totGamma'
;
;	pressure		indicates fitting a spectrum with ambient conditions specified, with pressure (mbar) and
;	temp			temperature (C). Pass these onto to 'transmit' in filters and 'det-eff' (in 'array_yield').
;
;	refit		flags doing a refit of an existing fit results
;	use_last	use previous calculation of array corrections to save time
;
; Input (if /refit):
; 	rGamma		relative sensitivity factors per element and detector (ignore 'rY')
;
; Output:
;	counts_per_ppm_uc	effective total yields per element
;	cIntensity	correction factor across array to line intensities per line per element
;				(these are applied to (*peaks).intensity on return)
;	totGamma	sum of rGamma for used detectors (effective multiplcity factor)
;	error		flags an error
;
; Output (if refit=0):
; 	rGamma		relative sensitivity factors per element and detector
;
; 'ratio_yield' is assumed to be calculated elsewhere (layer_setup) and input.
; If 'refit' is set, it is assumed that 'rGamma' is input (and 'rY' is ignored),
; otherwise, 'rGamma' is calculated here and output.
; If 'use_last' is set, it is assumed that 'rGamma' is stored as 'wGamma' in common
; from a previous call to array_yield (same for 'wIntensity').
;
; 'rY' are relative yields per element and detector (normal detector order)
; 'rGamma are relative factors per element and detector including yield, efficiency,
; filters and solid-angle (normal detector order).
;
; 'rIntensity' are the input relative intensities relative to 'central' detector for all lines
; across all detectors. 'e_line' are all the line energies to use to correct overall
; relative intensities for rFilt, rOmega and rEff effects across array. The corrections are
; output as 'cIntensity'.
; 
; 'multiplicity' is largely obsolete and used in some old data. It is replaced now by the
; sum of 'rGamma' to form 'totGamma'.
; 
; Need to take care with detector index. The index in the layout array is CSV row index, not
; the detector number. Need to translate or re-sort to correct order. Hence, return 'rGamma'
; in normal detector order. 'rY' input is in detector order, and 'rGamma' output should be
; in detector order too.

COMPILE_OPT STRICTARR
if n_elements(charge) lt 1 then charge=1.0
if n_elements(array) lt 1 then array=0
if n_elements(refit) lt 1 then refit=0
if n_elements(use_last) lt 1 then use_last=0
if n_elements(active) lt 1 then active=0
if n_elements(n_det) lt 1 then n_det=1
if n_elements(multiplicity) lt 1 then multiplicity=1

; Assume that PIGE detector efficiency is TOTAL, including solid-angle.
; Therefore, set omega to 4 pi. PIXE efficiency is intrinsic only, so set Omega to solid angle (msr).

; For a single detector, or the first, reference detector in an array, calculate solid-angle and efficiency
; using the normal 'diameter' and 'distance' parameters. For a valid array, calculate ratios to this for
; each detector element below in the multiplicity factors.

error = 1
ErrorNo = 0
common c_errors_1, catch_errors_on
if n_elements(catch_errors_on) lt 1 then catch_errors_on=0
if catch_errors_on then begin
	Catch, ErrorNo
	if (ErrorNo ne 0) then begin
		Catch, /cancel
		on_error, 1
		help, calls = s
		n = n_elements(s)
		c = 'Call stack: '
		if n gt 2 then c = [c, s[1:n-2]]
		warning,'array_yield',['IDL run-time error caught.', '', $
				'Error:  '+strtrim(!error_state.name,2), $
				!Error_state.msg,'',c], /error
		MESSAGE, /RESET
		return, 0
	endif
endif
common c_fit_array_yield_1, wGamma, wIntensity

		if (*pdetector).pige eq 1 then begin
			omega = 4.0 * !pi * 1000.
		endif else begin
			omega = solid_angle( (*pdetector).distance, (*pdetector).diameter, tilt=(*pdetector).tilt, $
						array=0, shape=(*pdetector).shape)
		endelse

		eff = det_eff( pdetector, e, external_filters=pfilter, pressure=pressure, temp=temp, gamma=(*pdetector).pige)
		T = 1.
		counts_per_ppm_uc = float(omega * eff * T * Y)

; 'Multiplicity' is used for a detector array using generic single-detector solid-angle and efficiency.
; If a full-array 'layout' specification is available, then these multiplicity factors can be calculated
; from the individual detector geometry and relative yields.

		yield = counts_per_ppm_uc * charge

		if array then begin
			if ptr_valid(pdetector) and ptr_valid(playout) then begin
				if (size(*pdetector, /tname) ne 'STRUCT') or (size(*playout, /tname) ne 'STRUCT') then array=0
			endif else array = 0
			if array eq 0 then warning,'array_yield',['Array detector, but error with structure pointers.', $
												'Array function disabled in X-ray spectrum fit.']
		endif

		if array then begin
			ok = intarr(n_det)
			ok[ active] = 1
			if use_last or refit then begin						; use array corrections from last fit
				if (refit eq 0) then rGamma = wGamma
				goto, cont	
			endif
			
; Veto detector channels that have been selected as disabled in the layout ...
; This is not correct. Since we are fitting the pactive detector channel data in a sum spectrum,
; we need to normalize to these channels. We can't veto more, as here ...
;			if (*playout).veto then begin
;				q = where( (*playout).data.bad ge 1)
;			endif else begin
;				q = where( (*playout).data.bad ge 2)
;			endelse
;			if q[0] ne -1 then ok[q] = 0

; Note that 'detector_geometry' output struct vector 'g' is in CSV table index order.
; All the quantities calculated from it (rOmega, rFilt, rEff) will be in this order too.
; Must re-order later.

			g = detector_geometry( playout, (*pdetector).distance, theta, $
										phi, tilt=(*pdetector).tilt, error=err)
			if err then goto, bad_geometry
	
; Solid-angle ratios rOmega (CSV index order) ...

			rOmega = fltarr(n_det)
			darea = (*playout).data.width * (*playout).data.height
			if (*pdetector).shape eq 0 then darea = darea * !pi/4.

			rOmega = 1000. * (darea*cos(g.tilt/!radeg)/(g.R*g.R)) / omega
	
; Filter ratios (CSV index order) ...
; Pinholes for an array detector are assumed to be applied to each element, and should
; be specified in the detector set-up, not here.

			q = where( (*pfilter).pinhole ne 0)					; check for illegal pinhole filters on array
			if q[0] ne -1 then goto, bad_filter

; This uses global tilt, and distance from centre tilt, but NOT local tilt, as in layout file.
; The filters are assumed to be straight across the front of a non-flat detector, and not
; following the contours of the detector array. For filters that DO follow these contours,
; put the filter in the Detector 'absorber' specification, not here in 'filters'.
; Note that none of these external filters should be a 'pin-hole' filter.

			pt = ptr_new( *playout)
			(*pt).data.tilt = 0.0
			g2 = detector_geometry( pt, (*pdetector).distance, theta, $
										phi, tilt=(*pdetector).tilt, error=err)
			ptr_free, pt
			if n_elements(g2) lt n_det then goto, bad_mismatch

;			Filter attenuation ratios for major line 'e'

			rFilt = fltarr(n_det,n_els)
			Filt0 = transmit( pfilter, pressure=pressure, temp=temp, e)
			for i=0L,n_det-1 do begin
				rFilt[i,*] = transmit( pfilter, e, pressure=pressure, temp=temp, tilt=g2[i].tilt) / Filt0
			endfor

;			Filter attenuation ratios for all lines 'e_line', relative to major line

			nk = n_elements( rIntensity[0,*,0])
			rFilt_all = fltarr(n_det,nk,n_els)
			Filt0_all = transmit( pfilter, e_line)
			for i=0L,n_det-1 do begin
				rFilt_all[i,*,*] = transmit( pfilter, e_line, pressure=pressure, temp=temp, tilt=g2[i].tilt) / Filt0_all
			endfor
			for k=nk-1,0,-1 do begin
				rFilt_all[*,k,*] = rFilt_all[*,k,*] / rFilt_all[*,0,*]
			endfor

; Efficiency ratios (CSV index order) ...
; This does follow the detector contours, and uses the local tilt - in 'g'.
; Need to calculate these for local tilt. Do this by copying detector struct, and modifying
; tilt and distance (weak effect on some geometry corrections).

			rGamma = fltarr(n_det,n_els)
			small = 1.0e-10
			rEff = fltarr(n_det,n_els)
			Eff0 = det_eff( pdetector, e, pressure=pressure, temp=temp) > small
			rEff_all = fltarr(n_det,nk,n_els)
			Eff0_all = det_eff( pdetector, e_line, pressure=pressure, temp=temp) > small
			Eff0_all = reform( Eff0_all, nk,n_els)

			det = *pdetector
			for i=0L,n_det-1 do begin
				det.distance = g[i].R
				det.tilt = g[i].tilt
				rEff[i,*] = (det_eff( det, e, pressure=pressure, temp=temp) > small) / Eff0

;				Efficiency ratios for all lines 'e_line', relative to major line

				rEff_all[i,*,*] = (det_eff( det, e_line, pressure=pressure, temp=temp) > small) / Eff0_all

; Take care here as rY and rIntensity are input in normal detector order. Use (*playout).data.index to
; order in CSV index order to match rOmega, rEff, rFilt here. Offset by (playout).start, which
; is the detector starting number. Maia uses a start of 0, but other devices do not.
;
;	List of all CSV table indices referenced by ref, with 'start' offset:
;		ref = (*playout).ref[ (*playout).start + indgen((*playout).N) ]
;
;	Detector index corrected for non-zero 'start':
;		(*playout).data[i].index - (*playout).start

				rGamma[i,*] = rY[ (*playout).data[i].index - (*playout).start,*] * rOmega[i] * rEff[i,*] * rFilt[i,*]
			endfor

			for k=nk-1,0,-1 do begin
				rEff_all[*,k,*] = rEff_all[*,k,*] / rEff_all[*,0,*]
			endfor

; Form relative-intensity correction factors. 
 
			wIntensity = rFilt_all[*,*,*] * rEff_all[*,*,*] * rIntensity[ (*playout).data.index - (*playout).start,*,*]
 
; 'playout' and 'g' use the CSV index order. Re-order to normal detector index order for output ...
; Remember that 'ref' is conversion from detector number to CSV index. But detector numbers start at 'start'.

			ref = (*playout).ref[ (*playout).start + indgen((*playout).N) ]
			
			rGamma[*,*] = rGamma[ ref, * ]					; Re-order into normal detector order
			wIntensity[*,*,*] = wIntensity[ ref, *,* ]
			wGamma = rGamma

cont:
			totGamma = fltarr(n_els)						; "effective multiplicity"
			q = where( ok eq 1)
			for i=0L,n_els-1 do begin
				totGamma[i] = total( rGamma[q,i])
			endfor

			yield = yield * totGamma
			
			if refit eq 0 then begin
				nk1 = max(n_lines) > 1						; build weighted corrections to
				cIntensity = replicate(1.,nk1,n_els)		; relative intensities
				for i=0L,n_els-1 do begin
					tG = total(rGamma[q,i])
					if n_lines[i] ge 1 then begin
						for k=0L,n_lines[i]-1 do begin
							cIntensity[k,i] = total( rGamma[q,i] * wIntensity[q,k,i]) / tG
						endfor
					endif
				endfor
			endif
			
;			for i=0L,(*playout).N-1 do begin
;				print,' Layout geometry:',strtrim(i,2),' Index=',strtrim((*playout).data[i].index,2),' R= ',str_tidy(g[i].R),' Theta= ',str_tidy(g[i].theta), $
;						' Phi= ',str_tidy(g[i].phi),' Tilt= ',str_tidy(g[i].tilt),' F-Tilt= ',str_tidy(g2[i].tilt)
;			endfor
;			print, 'pActive =', active
;			print, 'totGamma=', totGamma
;			print, 'N active=', n_elements(active)
;			print, 'totGamma/nDet=', totGamma/float(n_elements(where(ok eq 1)))
;			print, 'Some values fof element #5 ...'
;			print, 'rFilt (min, max)=',min(rFilt[*,5]), max(rFilt[*,5])
;			print, 'rEff (min, max)=',min(rEff[*,5]), max(rEff[*,5])
;			print, 'rOmega (min, max)=',min(rOmega), max(rOmega)
;			print, 'rY (min, max)=',min(ry[*,5]), max(ry[*,5])
;			print, 'rGamma (min, max)=',min(rGamma[*,5]), max(rGamma[*,5])

		endif else begin
			if refit eq 0 then begin
				rGamma = 0.0
				cIntensity = 1.0
			endif
			totGamma = 1.0
			n_det = 1
			yield = yield * (multiplicity > 1)
		endelse
		
error = 0
return, yield

bad_filter:
	message = ['Pin-hole "filters" are illegal with detector arrays.', $
				'However, they can be built into the detector "absorbers".']
	warning, 'array_yield', message
	error = 1
	return, 0
bad_mismatch:
	warning, 'array_yield', 'mismatch between old and new detector parameters'
	error = 1
	return, 0
bad_geometry:
	warning, 'array_yield', 'error from "detector_geometry"'
	error = 1
	return, 0
end

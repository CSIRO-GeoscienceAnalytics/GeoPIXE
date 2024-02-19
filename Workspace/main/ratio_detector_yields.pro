function ratio_detector_yields, pdetector, playout, pfilter, peaks, $
					active1, active2

; Calculate the ratio of array yields for 'active1' / 'active2' groups of detectors in
; an array. All elements are output as 'ratio'.
; 
; Calculate the yield from an array detector, based on *pdetector and *playout parameters,
; positioned at 'theta' and 'phi' in angle, for energies 'E', charge 'charge',
; correcting input generic (central detector) yields 'Y' for variation across the detector,
; for 'active1' and 'active2' detectors and apply filters and intrinsic efficiency too.
; All yield calculation results from *peaks.
;
; This entails accumulating contributions to efficiency, solid-angle, filter
; absorption and variations in yields with take-off angle across the array,
; only adding in detectors that are 'active' in the spectral data.
;
; 'ratio_yield' is assumed to be calculated elsewhere (layer_setup) and input.
; 'rY' are relative yields per element and detector (normal detector order)
; 'rGamma are relative factors per element and detector including yield, efficiency,
; filters and solid-angle (normal detector order). 'rGamma' is calculated in 'array_yield'.
;
; 'rIntensity' are the relative intensities relative to 'central' detector for all lines
; across all detectors. 'e_line' are all the line energies to use to correct overall
; relative intensities for rFilt and rEff effects across array. 

COMPILE_OPT STRICTARR
	small = 1.0e-6

	array = (*peaks).array
	n_det = n_elements((*peaks).ratio_yield[*,0])
	Yyield = (*peaks).yield[*,(*peaks).unknown-1]

; All array code assumes vectors in detector number order. Hence, (*peaks).ratio_yields,
; (*peaks).ratio_intensity and rGamma should all be in this order.

	nk = max( (*peaks).n_lines) > 1
	rY = (*peaks).ratio_yield[*,*]
	major = major_line((*peaks).z, (*peaks).shell)
	e = e_line( (*peaks).z, major)

; Need to take care with detector index in 'array_yield'. The index in the layout array is 
; CSV row index, not the detector number. Need to translate or re-sort to correct order. 
; Hence, 'rY' input is in detector order, and 'rGamma' returned in detector order too.

	yield1 = array_yield( pdetector=pdetector, playout=playout, pfilter=pfilter, $
				Energy=e, array=array, charge=1.0, active=active1, n_det=n_det, $
				E_line=(*peaks).e, ratio_intensity=(*peaks).ratio_intensity, $
				n_lines=(*peaks).n_lines, ratio_yield=rY, Y=Yyield, n_els=(*peaks).n_els, $
				theta=(*peaks).theta, phi=(*peaks).phi, error=error)
	if error then goto, bad_array_yield

	yield2 = array_yield( pdetector=pdetector, playout=playout, pfilter=pfilter, $
				Energy=e, array=array, charge=1.0, active=active2, n_det=n_det, $
				E_line=(*peaks).e, ratio_intensity=(*peaks).ratio_intensity, $
				n_lines=(*peaks).n_lines, ratio_yield=rY, Y=Yyield, n_els=(*peaks).n_els, $
				theta=(*peaks).theta, phi=(*peaks).phi, error=error)
	if error then goto, bad_array_yield

	ratio = yield1 / yield2						; 'outer' / 'inner' yields

; Remove ratios for tiny yields and divide by zero ...
	
	q = where( finite(ratio), nq, complement=qc, ncomplement=nqc)
	if nqc gt 0 then ratio[qc] = 0.

	q = where( (yield1 lt small) or (yield2 lt small), nq)
	if nq gt 0 then ratio[q] = 0.

	return, ratio

bad_array_yield:
	warning,'ratio_detector_yields','Error in "array_yield".'
	return, 0.
end
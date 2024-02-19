function boost_back, spec, low, high, cal_a, cal_b, detectori, filters, yields, $
		boost=boost, in=in, out=out, factor=factor, sxrf=sxrf

	COMPILE_OPT STRICTARR
	if n_elements(detectori) lt 1 then detectori=0
	detector = detectori[0]
	if size(detector,/tname) eq 'POINTER' then detector = (*detector)[0]
	if n_elements(filters) lt 1 then filters=0
	if n_elements(yields) lt 1 then yields=0

	if n_elements(boost) lt 1 then boost=1
	if n_elements(sxrf) lt 1 then sxrf=0
	if n_elements(out) lt 1 then out=0
	in = 1
	if (out eq 1) then in=0

	x = low + indgen(high-low+1)
	e = cal_a * x + cal_b								; energy
	n = n_elements(spec)

	if (size(detector,/tname) eq 'STRUCT') and (sxrf eq 0) then begin
		trans_detector = det_eff( detector, e, /skip_abs) 	; detector efficiency (less absorbers)
	endif else begin
		trans_detector = 1.0
	endelse
	if boost then begin
		if ptr_valid(filters) then begin
			trans_filters = transmit(filters, e) 		; filter transmission
		endif else begin
			trans_filters = 1.0
		endelse

		if ptr_valid(yields) then begin
			layer = (*yields).layers[0]
			mux = atten(layer,e) * layer.thick
			xmu = 1./(1.+1./mux)
			trans_sample = exp(double(-xmu)) 			; sample self-absorption
		endif else begin
			trans_sample = 1.0
		endelse
	endif else begin
		trans_filters = 1.0
		trans_sample = 1.0
	endelse

	veto_all_absorb = 0								; veto all absorption (e.g. for Monte Carlo)

	trans = trans_detector * trans_filters * trans_sample

	if veto_all_absorb then begin
		trans[*] = 1.0
	endif else begin
		q = where( finite(trans))
		if q[0] ne -1 then begin
			trans[q] = smooth(trans[q], (0.16/cal_a) > 2)
		endif else begin
			trans[*] = 1.0
		endelse
	endelse

	precision = machar(/double)
	base = 1.0e+15 * precision.xmin
	q = where( (finite(trans) eq 0) or (trans lt 1.0e+6*base) )
	if q[0] ne -1 then trans[q] = 1.0e+6*base

;	This controls the "softness" of the low energy boost
;	May need this as an accessible control to tailor Boost

	cap = 100.									; was 100. (use 10 for Rio)
	if e[0] lt 0.5 then cap=5.					; fudge for Rio EMP

	trans = 1./ (cap*atan( (1./trans)/cap))

	factor = fltarr( n)
	factor[*] = 1.0
	factor[x] = trans

	e = cal_a * findgen(n) + cal_b
	q = where(e gt 0.305)
	xmin = q[0]>0
	x0 = xmin > x[0]
	factor[0:((x0-1)>0)] = factor[x0]
	factor[((high+1)<(n-1)):*] = factor[high]

	if in then begin
		s = spec * factor
	endif else begin
		s = spec / factor
	endelse
	return, s
	end

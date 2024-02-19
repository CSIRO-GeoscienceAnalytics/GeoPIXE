;
; See main routine "Strip_Clip" at end of this file ...
;
;----------------------------------------------------------------------------

pro extend_ends, spec, low, high, error=error, boost=boost

;	Extend ends, up ramp at low end, flat at top, for now.
;	This effectively anchors the ends in place.

	COMPILE_OPT STRICTARR
	common c_strip_clip_1, xmin

	n = n_elements(spec)
	if high le low+10 then begin
		error = 1
		return
	endif

	ns = round((low-1)/10 > 2) < 4
	x = indgen(low)

		slope = (alog(mean(spec[low+1:low+ns])) - alog(mean(spec[low+ns+1:low+2*ns]))) / float(ns)

;		ramp = exp( alog(mean(spec[low:low+3])>0.0) + slope * (low - x) )
		ramp = exp( alog(spec[low]>0.0) + slope * (low - x) )

		q = where( finite(spec[low:*]) and (spec[low:*] gt 0.1))
		if q[0] eq -1 then begin
			error = 1
			return
		endif
		ok = low + q[0]
		q = where( finite(ramp) eq 0)
		if q[0] ne -1 then ramp[q]=spec[ok]

;		spec[x] = spec[x] > (ramp > mean(spec[low:(low+ns)]))
		spec[x] = spec[x] > (ramp > spec[low])

	x = high+1 + indgen(n-high)
	spec[x] = spec[x] > (mean(spec[high-10:high])>0.0)
;	spec[x] = mean(spec[high-10:high]) > 0.0

	q = where( finite(spec) eq 0)
	if q[0] ne -1 then spec[q]=0.0
	q = where( spec gt 0)
	if q[0] ne -1 then xmin = q[0]
	error = 0
	return
	end

;----------------------------------------------------------------------------

function half_curve, x, n_curve, curve

	COMPILE_OPT STRICTARR
	c_low = -1.5								; PIXE range
	c_high = 1.1
;	c_low = 0.5									; SXRF range?
;	c_high = 1.5
	c_mid = 0.5*(c_low + c_high)				; mid-point in curve
	c_range = 0.5 * (c_high-c_low)				; range of curvature

	n = n_elements(x)
	half = replicate( 0.5D0, n)

	if n_curve gt 0 then begin
		r = double(x)/float(n_curve)
		q = where((r gt c_low) and (r lt c_high))
		if q[0] ne -1 then begin
			y = (r[q] - c_mid) / c_range
			half[q] = 0.5D0 * (1. + (curve-1.) * (1.-y*y))
		endif
	endif else begin
		half = half * curve
	endelse

	return, half
	end

;----------------------------------------------------------------------------

pro simple_strip, spec, lowi, highi, cal_a, cal_b, w0, w1, curve, $
				sample_width_scaling, passes, n_curve

	COMPILE_OPT STRICTARR
	common c_strip_clip_1, xmin

;	low = lowi + 10								; don't strip first 10 channels
;	high = highi - 10							; or last 10
	low = lowi
	high = highi

	n = n_elements(spec)
	x = low + indgen(high-low+1)				; the channels to do
	e = cal_a * x + cal_b						; energy
	w = sqrt(w0 + w1*e) / cal_a					; FWHM (channels)
	off = sample_width_scaling*w
	if high-low le 2.*max(off) then begin
		low = (low-max(off)) > 0
		high = (high+max(off)) < (n-1)
		x = low + indgen(high-low+1)
		e = cal_a * x + cal_b
		w = sqrt(w0 + w1*e) / cal_a
		off = sample_width_scaling*w
	endif

	t = spec
	half = half_curve( x, n_curve, curve)

	if n_curve eq 0 then begin
		rf = 1.0
		f = 0.0
	endif else begin
		s = sample_width_scaling > 2.0
		f = 2.0 ^ ( -(2.*s)^2)					; as in original
		rf = 1./(1. - 2.*f)
	endelse

	for i=0L,passes-1 do begin
		y = half * (spec[(x-off)>xmin] + spec[x+off])
;		y = half * (spec[(x-off)>0] + spec[x+off])
		y = rf * ( y - 2.*f * spec[x])
		t[x] = (y < spec[x]) 			;> 0.1

		y = half * (t[(x-off)>xmin] + t[x+off])
;		y = half * (t[(x-off)>0] + t[x+off])
		y = rf * ( y - 2.*f * t[x])
		spec[x] = (y < t[x]) 			;> 0.1
	endfor

	return
	end

;----------------------------------------------------------------------------

function trim_seb, spec, low, high, cal_a, cal_b, in=in, out=out, factor=seb

	COMPILE_OPT STRICTARR
	if n_elements(out) lt 1 then out=0
	in = 1
	if (out eq 1) then in=0

	x = low + indgen(high-low+1)
	e = cal_a * x + cal_b							; energy

	alpha = 0.7										; strength
	a = 2.5
	b = 1.5

	e1 = (e-a)/b
	seb = 1. - alpha * exp(-e1*e1)

	s = spec
	if in then begin
		s[x] = spec[x] * seb
	endif else begin
		s[x] = spec[x] / seb
	endelse

	q = where( finite(s) eq 0)
	if q[0] ne -1 then s[q]=0.0
	return, s
	end

;----------------------------------------------------------------------------

function un_back, spec, guide, low, high, factor=factor, in=in, out=out

	COMPILE_OPT STRICTARR
	if n_elements(boost) lt 1 then boost=1
	if n_elements(out) lt 1 then out=0
	in = 1
	if (out eq 1) then in=0

	x = low + indgen(high-low+1)

	factor = (0.3 > guide[x])		; / 1000.0

	s = spec
	if in then begin
		s[x] = spec[x] * factor
	endif else begin
		s[x] = spec[x] / factor
	endelse
	s[0:x[0]] = s[x[0]]

	q = where( finite(s) eq 0)
	if q[0] ne -1 then s[q]=0.0
	return, s
	end

;----------------------------------------------------------------------------

function strip_clip, pspec, elow, ehigh, passes=passesi, filters=filters, yields=yields, $
		detector=detectori, boost=boost, hybrid=hybrid, trim_seb=trim_seb, $
		def_passes=def_passes, w1=w1, w0=w0, curve=curve, sxrf=sxrf, silent=silent

;	Do the SNIP background strip
;	(C.G. Ryan, E. Clayton, W.L. Griffin, S.H. Sie and D.R. Cousens, 1988, "SNIP, a 
;	Statistics Sensitive Background Treatment for the Quantitative Analysis of PIXE Spectra
;	in Geoscience Applications", Nucl. Instr. and Meth. in Phys. Res. B34, 396-402)
;	
;	see new test version in 'Test_back_plugin'
;
;	pspec		pointer to spectrum structure (see DEFINE.pro)
;	elow		low energy marker
;	ehigh		high energy
;	passes		number of passes through SNIP
;	filters		filters to use with Boost
;	yields		structure contains sample composition for self-absorption correction
;	detector	detector struct, use with Boost
;	boost		=1 switches on Boost mode
;	hybrid		=1 (default) uses more passes at high energy
;	trim_seb	=1 remove secondary electron bremsstralung hump as well
;	def_passes	default number of passes, default to 2*passes
;	w0,w1		offset and gain for peak width (defaults to parameters in 'detector')
;	curve		optional curvature parameter, default=1.0003
;	sxrf		enable certain SXRF options (e.g. no Ge edge from detector).
;	/silent		suppress error messages (e.g. in Fit All loop)

	COMPILE_OPT STRICTARR
	common c_strip_clip_1, xmin

	if n_elements(pspec) lt 1 then return,0
	if n_elements(detectori) lt 1 then detectori=0
	detector = detectori[0]
	if size(detector,/tname) eq 'POINTER' then begin
		if ptr_valid(detector) then detector = (*detector)[0]
	endif
	if size(detector,/tname) eq 'STRUCT' then begin
		if n_elements(w1) lt 1 then w1 = detector.w1
		if n_elements(w0) lt 1 then w0 = detector.w0
	endif else begin
		if n_elements(w1) lt 1 then w1 = 0.003
		if n_elements(w0) lt 1 then w0 = (0.16)*(0.16) - w1*5.895
		detector = 0
	endelse
	if n_elements(filters) lt 1 then filters=0
	if n_elements(yields) lt 1 then yields=0

	if n_elements(passesi) lt 1 then passesi = 8
	if n_elements(def_passes) lt 1 then def_passes = 2*passesi
	if n_elements(curve) lt 1 then curve = 1.0003
	if n_elements(sxrf) lt 1 then sxrf=0
	if n_elements(hybrid) lt 1 then hybrid=1
	if n_elements(boost) lt 1 then boost=0
	if n_elements(trim_seb) lt 1 then trim_seb=0
	if n_elements(silent) lt 1 then silent=1

;	elow = elow > 0.603							; 0.6 keV minimum (also in pixe_fit)
;	elow = elow > 0.31							; 0.3 keV minimum (also in pixe_fit)
	elow = elow > 0.15							; 0.15 keV minimum (also in pixe_fit)

	passes = passesi
	if boost then passes = passes/2

	sample_width_scaling = 2.0					; Clip FWHM scaling width
	n_wiggles_trim = 4							; Reduce wiggles as last passes

	if ptr_valid(pspec) eq 0 then goto, bad_ptr
	cal_a = (*pspec).cal.poly[1]
	cal_b = (*pspec).cal.poly[0]
	ehigh = ehigh > (elow+2.0)

	spec = *(*pspec).data
	n = n_elements(spec)
	t = reform(float(spec))
	low = ((elow-cal_b)/cal_a) > 1
	high = ((ehigh-cal_b)/cal_a) < (n-2)
	if (n lt 50) or (high lt low+50) then goto, bad_exit

	n_trans = (e_inflection(filters) - cal_b)/cal_a
	if hybrid then begin
		n_trans_low = fix(0.8 * n_trans) > low
		n_trans_high = fix(1.2 * n_trans) < high
	endif
	n_curve = n_trans

;.....................................................................................
;	Veto ends, if there are only zero channels

;	warning,'strip_back','veto ends ...'		; debug
	veto_ends, t, low, high, error=error		; move low, high if zero channels
	if error then goto, spec_gone				; low, high MUST remain fixed from here ...

	x = low + indgen(high-low+1)
	e = cal_a * x + cal_b						; energy
	w = sqrt(w0 + w1*e) / cal_a					; FWHM (channels)
	wm = w[(high-low)/5] < (high-low)/2

	AF = (w1 * cal_a) / (cal_a*cal_a)			; FWHM**2 (channels)
	BF = (w0 + w1 * cal_b) / (cal_a*cal_a)
	t1 = t
;	t1 = median(t, 0.3*mean(w) > 3)
	err = low_stats_filter( t1,t, n_elements(t), low,high, AF,BF)

	t0 = t										; save for 2nd pass (un-back boost)
;	see new test version in 'Test_back_plugin'

;.....................................................................................
;	Remove secondary electron bremsstrahlung hump

	if trim_seb then begin
		t = trim_seb( t, low, high, cal_a, cal_b, /out, factor=seb)
	endif

;.....................................................................................
;	Extend the ends to a constant at high, and a ramp at low energy

	extend_ends, t, low, high, error=error, boost=boost
	if error then goto, spec_gone

;.....................................................................................
;	BOOST out (filters, matrix optional, detector always)

	emin = ( cal_a*low+cal_b - 0.2 ) > 0.31
	xmin = fix((emin - cal_b) / cal_a) > 2
	
	t = boost_back( t, xmin-2, high, cal_a, cal_b, detector, filters, yields, $
						/out, boost=boost, factor=trans, sxrf=sxrf)

;	print,'SNIP: low,high=',low,high,' n_curve=',n_curve,' E=',n_curve*cal_a+cal_b
;	print,'          passes=',passes,' boost=',boost,' hybrid=',hybrid
;	print,'          pass 1:', low, n_trans_high
;	print,'          pass 2:', n_trans_low, high

;.....................................................................................
;	Transform to log-log space ...

	t = alog( (alog((t>0.0D0) + 1.0D0) > 0.0D0) + 1.0D0)

;.....................................................................................
;	Do the main strips ...

;wset,0
;plot,t[0:600]

;	warning,'strip_back','strip clip ...'		; debug
	if hybrid then begin
		if n_trans_high gt low+50 then begin
			simple_strip, t, low, n_trans_high, cal_a, cal_b, w0, w1, curve, $
					sample_width_scaling, passes, n_curve
		endif
		if high gt n_trans_low+50 then begin
			simple_strip, t, n_trans_low, high, cal_a, cal_b, w0, w1, curve, $
					sample_width_scaling, def_passes, n_curve
		endif
	endif else begin
		simple_strip, t, low, high, cal_a, cal_b, w0, w1, curve, $
				sample_width_scaling, passes, n_curve
	endelse

;.....................................................................................
;	More strips with decreasing widths to reduce wiggles

	nm = 4
	if n_wiggles_trim lt 1 then nm=2
	wiggle_passes = ((n_wiggles_trim+1)/4) > 1

	r = 1.0
	for i=0L,nm-1 do begin
		simple_strip, t, low, high, cal_a, cal_b, w0, w1, 1.0, $
				r*sample_width_scaling, wiggle_passes, 0
		r = 0.7*r
	endfor

skip:

;.....................................................................................
;	Transform back from log-log space

;	q = where( w ge 5.0)
;	if q[0] ne -1 then begin
;		t[q] = median(t[q], mean(w[q]) > 3)
;	endif
	back = float( exp( exp(t) -1.0D0) - 1.0D0 ) > 0.1

;oplot,back[0:100],linestyle=1

;.....................................................................................
;	BOOST in (filters optional, matrix later, detector always)

	back = back * trans
	if trim_seb then back[x] = back[x] * seb

;.......................................................................................
;	Do 2nd pass in boost mode as a ratio to first iteration

;	warning,'strip_back','boost ...'		; debug
	if boost then begin
		back = un_back( t0, back, low, high, /out, factor=uback)

;wset,1
;plot,back[0:100],yrange=[0,20]

		simple_strip, back, low, high, cal_a, cal_b, w0, w1, curve, $
				sample_width_scaling, passes, n_curve

		nm = 4
		if n_wiggles_trim lt 1 then nm=2
		wiggle_passes = (n_wiggles_trim+1)/4 > 1

		r = 1.0
		for i=0L,nm-1 do begin
			simple_strip, back, low, high, cal_a, cal_b, w0, w1, 1.0, $
					r*sample_width_scaling, wiggle_passes, 0
			r = 0.7*r
		endfor

		back[x] = back[x] * uback
	endif

	back[0:low-1] = back[low]
	back[high+1:*] = back[high]
	q = where( finite(back) eq 0)
	if q[0] ne -1 then back[q]=0.0
	back = median(back, wm > 2)
	back[0:low-2] = 0.0
	back[high+2:*] = 0.0

;.....................................................................................
;	Put it a fit overlay ...

;	warning,'strip_back','done ...'		; debug
	fit = define(/spectrum)
	fit.source = (*pspec).source
	fit.label = 'SNIP'
	fit.cal.poly[0] = cal_b
	fit.cal.poly[1] = cal_a
	fit.cal.units = (*pspec).cal.units
	fit.cal.order = 1
	fit.comment = 'SNIP background algorithm'

	fit.size = (*pspec).size
	fit.data = ptr_new( back, /no_copy)

	return, ptr_new( fit, /no_copy)

bad_ptr:
	warning, 'strip_clip', 'bad spectrum pointer'
	goto, bad_exit
spec_gone:
	if (silent eq 0) then begin
		warning, 'strip_clip', ['Too few channels selected or zero spectrum data, or','bad or missing energy calibration.','', $
							'Check energy range for fit.','Set this using the VIEW markers.']
	endif
	goto, bad_exit

bad_exit:
	return, ptr_new()
	end

;-------------------------------------------------------------------------------------

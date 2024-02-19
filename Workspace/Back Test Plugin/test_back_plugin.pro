
function test_back_plugin, pspec, elow, ehigh, passes=passesi, filters=filters, yields=yields, $
		detector=detectori, boost=boost, title=title, curve=curve, trim_seb=trim_seb, sxrf=sxrf

;	Test background plugin - just does the SNIP background, as in Strip_Clip
;
;  Plugins should be compiled in IDLDE and saved as a SAV file.
;  Only compile routines for ONE plugin and save using the command:
;  "SAVE, /routines, filename='test_back_plugin.sav'" for a "fred_spectrum_plugin" plugin.
;  To ensure this, exit IDLDE and start it again to compile and save another plugin.
;  
;  Don't use "resolve_all" as it will embed any routines found in the plugin.
;  If you need to call a routine not used in GeoPIXE, then add an explicit command to the
;  code like: "resolve_routine, 'lmfit', /is_function"
;
;  NOTE: It is important to ensure that ONLY routines for ONE plugin is in each SAV file.
;  Otherwise, unexpected results may result when the SAV files are restored at run-time.
;
;	Do the SNIP background strip
;
;	pspec		pointer to spectrum structure (see DEFINE.pro)
;	elow		low energy marker
;	ehigh		high energy
;	passes		number of passes through SNIP
;	filters		filters to use with Boost
;	yields		structure contains sample composition for self-absorption correction
;	detector	detector struct, use with Boost
;	title		just return title text
;	boost		=1 switches on Boost mode
;
;	trim_seb	=1 tries to remove secondary electron bremsstralung hump as well
;	curve		optional curvature parameter, default=1.0003
;	sxrf		enable certain SXRF options (e.g. no Ge edge from detector).

COMPILE_OPT STRICTARR

if arg_present(title) then begin
	title = 'SNIP2'				; return the menu title for this plugin
	return, 0
endif

	common c_strip_clip_1, xmin

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
	if n_elements(hybrid) lt 1 then hybrid=1
	if n_elements(boost) lt 1 then boost=0
	if n_elements(trim_seb) lt 1 then trim_seb=0
	if n_elements(sxrf) lt 1 then sxrf=0

	elow = elow > 0.15							; 0.15 keV minimum (also in pixe_fit)

	passes = passesi
	if boost then passes = passes/2

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

	sample_width_scaling = 2.0					; Clip FWHM scaling width
	n_wiggles_trim = 4							; Reduce wiggles as last pass

	n_trans = (e_inflection(filters) - cal_b)/cal_a
	if hybrid then begin
		n_trans_low = fix(0.8 * n_trans) > low
		n_trans_high = fix(1.2 * n_trans) < high
	endif
	n_curve = n_trans

	x = low + indgen(high-low+1)
	e = cal_a * x + cal_b						; energy
	w = sqrt(w0 + w1*e) / cal_a					; FWHM (channels)
	wm = w[(high-low)/5] < (high-low)/2

	AF = (w1 * cal_a) / (cal_a*cal_a)			; FWHM**2 (channels)
	BF = (w0 + w1 * cal_b) / (cal_a*cal_a)

	goto, do_SNIP
	
;-------------------------------------------------------------------------------------------
;	Test code ...

;	Top-hat filter ...

	iwm = round(wm) > 2
	middle = iwm
	wings = iwm
	scale = float(2*middle + 2*wings)
	kernel = [replicate(-1.,wings),replicate(+2.,middle),replicate(-1.,wings)]
	tophat = convol( t, kernel, scale, /center, /edge_truncate)
	
	
	goto, finish
	
;-------------------------------------------------------------------------------------------
;	SNIP test code ...

;	Veto ends, if there are only zero channels

do_SNIP:
	veto_ends, t, low, high, error=error		; move low, high if zero channels
	if error then goto, spec_gone				; low, high MUST remain fixed from here ...

	t1 = t
;	t1 = median(t, 0.3*mean(w) > 3)
	err = low_stats_filter( t1,t, n_elements(t), low,high, AF,BF)

	t0 = t										; save for 2nd pass (un-back boost)

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
	xmin = fix((emin - cal_b) / cal_a) > 0

	t = boost_back( t, xmin-2, high, cal_a, cal_b, detector, filters, yields, $
						/out, boost=boost, factor=trans, sxrf=sxrf)

;	print,'SNIP: low,high=',low,high,' n_curve=',n_curve,' E=',n_curve*cal_a+cal_b
;	print,'          passes=',passes,' boost=',boost,' hybrid=',hybrid
;	if hybrid then begin
;		print,'          pass 1:', low, n_trans_high, ' passes =',passes
;		print,'          pass 2:', n_trans_low, high, ' passes =',def_passes
;	endif

;.....................................................................................
;	Transform to log-log space ...

	t = alog( (alog((t>0.0D0) + 1.0D0) > 0.0D0) + 1.0D0)

;.....................................................................................
;	Do the main strips ...

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
	wiggle_passes = (n_wiggles_trim+1)/4 > 1

	r = 1.0
	for i=0,nm-1 do begin
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

;.....................................................................................
;	BOOST in (filters optional, matrix later, detector always)

	back = back * trans
	if trim_seb then back[x] = back[x] * seb

;.......................................................................................
;	Do 2nd pass in boost mode as a ratio to first iteration

	if boost then begin
		back = un_back( t0, back, low, high, /out, factor=uback)

		simple_strip, back, low, high, cal_a, cal_b, w0, w1, curve, $
				sample_width_scaling, passes, n_curve

		nm = 4
		if n_wiggles_trim lt 1 then nm=2
		wiggle_passes = ((n_wiggles_trim+1)/4) > 1

		r = 1.0
		for i=0,nm-1 do begin
			simple_strip, back, low, high, cal_a, cal_b, w0, w1, 1.0, $
					r*sample_width_scaling, wiggle_passes, 0
			r = 0.7*r
		endfor

		back[x] = back[x] * uback
	endif
	
; end SNIP
;-------------------------------------------------------------------------------------------

finish:
	back[0:low-1] = back[low]
	back[high+1:*] = back[high]
	q = where( finite(back) eq 0)
	if q[0] ne -1 then back[q]=0.0
	back = median(back, wm > 2)
	back[0:low-1] = 0.0
	back[high+1:*] = 0.0

;	Put it a fit overlay ...

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
	warning, 'test_back_plugin', 'bad spectrum pointer'
	goto, bad_exit
spec_gone:
	warning, 'test_back_plugin', ['Null spectrum, or','bad or missing energy calibration.']
	goto, bad_exit

bad_exit:
	return, ptr_new()
	end

;-------------------------------------------------------------------------------------

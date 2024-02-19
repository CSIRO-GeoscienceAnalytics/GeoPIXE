function bayes_back_plugin, pspec, elow, ehigh, passes=passesi, filters=filters, detector=detector, $
		boost=boost, curve=curve, yields=yields, title=title

;		basis_set=bsi, n_var=nvi, type_of_problem=topi

;	pspec				pointer to spectrum structure (see DEFINE.pro)
;	elow				low energy marker
;	ehigh				high energy
;	passes				number of passes through SNIP
;	filters				filters to use with Boost
;	yields				structure contains sample composition for self-absorption correction
;	detector			detector struct, use with Boost
;	title				just return title text
;	boost				=1 switches on Boost mode
;	curve				optional curvature parameter (not used in Bauer, only in SNIP)
;
;	basis_set			1:Legendre 2:spline
;	n_var				expansion order of background polynomial
;	type_of_problem 	1:Linear 2:Scaling 3:polynomial

COMPILE_OPT STRICTARR
common c_working_dir, geopixe_root

if arg_present(title) then begin
	title = 'Bayes'								; return the menu title for this plugin
	return, 0L
endif

	if n_elements(passesi) lt 1 then passesi = 10
	if n_elements(bsi) lt 1 then bsi = 2
	if n_elements(nvi) lt 1 then nvi = 15
	if n_elements(topi) lt 1 then topi = 3

	elow = elow > 0.603							; 0.6 keV minimum (also in pixe_fit)
	passes = passesi

	if ptr_valid(pspec) eq 0 then goto, bad_ptr
	cal_a = (*pspec).cal.poly[1]
	cal_b = (*pspec).cal.poly[0]
	ehigh = ehigh > (elow+2.0)

	spec = *(*pspec).data
	n = n_elements(spec)
	low = (elow-cal_b)/cal_a > 1
	high = (ehigh-cal_b)/cal_a < n-2
	if (n lt 50) or (high lt low+50) then goto, bad_exit
	x = low + indgen(high-low+1)

;	BOOST out (remove effects of filters, matrix optional, detector always)

	spec = boost_back( spec, low, high, cal_a, cal_b, detector, filters, yields, $
						/out, boost=boost, factor=trans)
	back = spec
	
;.....................................................................................
;	Do your custom background algorithm here ...

	case !version.os_family of
		'Windows': begin
			s = call_external(geopixe_root+'bayes_back.dll','BGSUB',spec,n,bsi,nvi,passes,topi,back)
			end
		else: begin
			warning,'bayes_back_plugin','No Bayesian library for this platform.'
			end
	endcase

;.....................................................................................
;	BOOST in (restore absorption of filters, matrix, detector)

	back[x] = back[x] * trans

	back[0:low-1] = 0.0
	back[high+1:*] = 0.0
	q = where( finite(back) eq 0)
	if q[0] ne -1 then back[q]=0.0

;.....................................................................................
;	Put it a fit overlay ...
;	This assumes that 'back' and 'spec' have the same energy calibration.

	fit = define(/spectrum)
	fit.source = (*pspec).source
	fit.label = 'Bayes Back'
	fit.cal.poly[0] = cal_b
	fit.cal.poly[1] = cal_a
	fit.cal.units = (*pspec).cal.units
	fit.cal.order = 1
	fit.comment = 'Bayes Back - background algorithm'

	fit.size = n_elements(back)
	fit.data = ptr_new( back, /no_copy)

	return, ptr_new( fit, /no_copy)

bad_ptr:
	warning, 'bayes_back_plugin', 'bad spectrum pointer'
	goto, bad_exit

bad_exit:
	return, ptr_new()
	end
